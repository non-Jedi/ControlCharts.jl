# This file is part of ControlCharts.jl.
#
# ControlCharts.jl is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# ControlCharts.jl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.

# * Types.jl
module Types

export ControlChart, calculate, ShewartSeries, Shewart,
    TabularCumulativeSum, CUSUM, BasicExponentiallyWeightedMovingAverage, EWMA,
    MovingCenterlineExponentiallyWeightedMovingAverage, MCEWMA

import ..RepeatArrays: RepeatVector
import Statistics: mean, std
import Optim

# Each kind of control chart will have a "control series" representing the actual
# source data to be plotted on the given kind of control chart. We
# will call it a ~ControlSeries~.

"Represents a series of values to be turned into a control chart."
abstract type ControlSeries{T,V} end

# When ~calculate~ is called on this ~ControlSeries~, a ~ControlChart~
# will be generated with any values not in the actual data calculated
# as needed. This ~ControlChart~ object can then be plotted directly.

"""
    ControlChart(z, lcl, ucl, control_series)

Represents a control chart.

Vectors in tuple `x` is the main series to plot while `lcl` and `ucl` are
vectors defining points where each point in each vector in `x` must
not exceed in the negative and positive directions. This function is
returned by calling `calculate` on a `ControlSeries`.
"""
struct ControlChart{T, V <: AbstractVector{T}, VCL <: AbstractVector{T},
                    C <: ControlSeries{T,V}, Z <: NamedTuple}
    z::Z
    LCL::VCL
    UCL::VCL
    control_series::C
end#struct

"""
    ControlChart(c)

Calls `calculate` on control series `c`.
"""
ControlChart(c::ControlSeries) = calculate(c)

"""
    calculate(control_series)

Returns a `ControlChart` for a given series.
"""
function calculate end

# ** Shewart Charts
# *** Types

# TODO: docstring
struct ShewartSeries{
    T, V<:AbstractVector{T}
} <: ControlSeries{T,V}
    x::V
    L::Float64
    μ::T
    σ::T
    function ShewartSeries(
        x::V, l::Float64, μ::T, σ::T
    ) where {T, V<:AbstractVector{T}}
        if l <= 0
            throw(DomainError(l, "Control limit width must be positive"))
        elseif σ < 0
            throw(DomainError(σ, "Standard deviation must be positive"))
        else
            new{T, V}(x, l, μ, σ)
        end#if
    end#constructor
end

const Shewart = ShewartSeries

function ShewartSeries(
    x::AbstractVector{T2}, l::Real, μ::T1, σ::T1
) where {T1, T2}
    T = promote_type(T1, T2)
    xt = similar(x, T)
    @inbounds xt .= x
    ShewartSeries(xt, convert(Float64, l), convert(T, μ), convert(T, σ))
end#constructor

ShewartSeries(x::AbstractVector; l=3.0, μ=mean(x), σ=std(x)) = ShewartSeries(x, l, μ, σ)

# *** calculate
function calculate(cs::ShewartSeries)
    lcl = RepeatVector(cs.μ - cs.L * cs.σ, length(cs.x))
    ucl = RepeatVector(cs.μ + cs.L * cs.σ, length(cs.x))
    centerline = RepeatVector(cs.μ, length(cs.x))
    ControlChart((x=cs.x, μ=centerline), lcl, ucl, cs)
end#function

# ** Cumulative Sum Charts (CUSUM)
# *** Types

abstract type CumulativeSum{T,V} <: ControlSeries{T,V} end

struct TabularCumulativeSum{T, V<:AbstractVector{T}} <: CumulativeSum{T,V}
    x::V
    K::T
    H::T
    μ::T
end#struct

"""
    CUSUM(x, K, H, μ)

Create tabular Cumulative Sum series on variable `x`.

`K` is the "reference value" (or "allowance" or "slack value"), `H` is
the "decision interval" (to establish control limits) and `μ` is the
target value.

# References

- Introduction to Statistical Quality Control
  - Chapter
    - 9.1.2: The Tabular or Algorithmic CUSUM for Monitoring the Process Mean
  - Pages
    - 417--421
  - Author:
    - Montgomery, Douglas C.
  - ISBN:
    - 978-1-118-14681-1
  - Publisher
    - John Wiley & Sons, Inc.
  - Year
    - 2013
"""
const CUSUM = TabularCumulativeSum

function CUSUM(x::AbstractVector{V}, k::K, h::H, μ::MU) where {V, K, H, MU}
    T = promote_type(promote_type(promote_type(V, K), H), MU)
    xt = similar(x, T)
    @inbounds xt .= x
    CUSUM(x::AbstractVector{T},
          convert(T, k)::T, convert(T, h), convert(T, μ)::T)
end#constructor

# *** calculate

function calculate(series::CUSUM{T,V}) where {T,V}
    c⁻ = similar(series.x)
    c⁺ = similar(series.x)

    if !isempty(series.x)
        # TODO: This assumes arrays are one-indexed
        @inbounds c⁻[1] = min(zero(T), series.x[1] - series.μ + series.K)
        @inbounds c⁺[1] = max(zero(T), series.x[1] - series.μ - series.K)
        @inbounds for i in 2:length(c⁺)
            c⁻[i] = min(zero(T), series.x[i] - series.μ + series.K + c⁻[i-1])
            c⁺[i] = max(zero(T), series.x[i] - series.μ - series.K + c⁺[i-1])
        end#for
    end#if

    lcl = RepeatVector(-series.H, length(series.x))
    ucl = RepeatVector(+series.H, length(series.x))

    ControlChart((C⁻=c⁻, C⁺=c⁺), lcl, ucl, series)
end#function

# ** Exponentially Weighted Moving Average Chart (EWMA)
# *** Basic EWMA
# **** Types
abstract type ExponentiallyWeightedMovingAverage{T,V} <: ControlSeries{T,V} end

struct BasicExponentiallyWeightedMovingAverage{
    T, V<:AbstractVector{T}
} <: ExponentiallyWeightedMovingAverage{T,V}
    x::V
    λ::Float64
    L::Float64
    μ::T
    σ::T
    function BasicExponentiallyWeightedMovingAverage(
        x::V, λ::Float64, l::Float64, μ::T, σ::T
    ) where {T, V<:AbstractVector{T}}
        if λ <= 0 || λ > 1
            throw(DomainError(λ, "Weight must be 0 < λ <= 1"))
        elseif l <= 0
            throw(DomainError(l, "Control limit width must be positive"))
        elseif σ < 0
            throw(DomainError(σ, "Standard deviation must be positive"))
        else
            new{T, V}(x, λ, l, μ, σ)
        end#if
    end#constructor
end#struct

"""
    EWMA(x, λ, l, μ, σ)

Exponentially weighted moving average series on `x`.

Each point `z` in the EWMA is calculated according to:

```
z[0] = μ
z[i] = λ * x[i] + (1 - λ) * z[i-1]
```

Where `λ` is a constant between 0 and 1.

The LCL and UCL are calculated according to:

```
cl[i] = L * σ * sqrt(λ * (1 - (1 - λ)^(2i)) / (2 - λ))
LCL[i] = μ - cl[i]
UCL[i] = μ + cl[i]
```

where `L` is a constant, `σ` is the standard deviation of `x`, and `μ`
is the process target.

# References

- Introduction to Statistical Quality Control
  - Chapter
    - 9.2: The Exponentially Weighted Moving Average Control Chart
  - Pages
    - 433--442
  - Author:
    - Montgomery, Douglas C.
  - ISBN:
    - 978-1-118-14681-1
  - Publisher
    - John Wiley & Sons, Inc.
  - Year
    - 2013
"""
const EWMA = BasicExponentiallyWeightedMovingAverage

# try to make sure EWMA works even if types aren't as expected
function EWMA(
    x::AbstractVector{T2}, λ::Real, l::Real, μ::T1, σ::T1
) where {T1, T2}
    T = promote_type(T1, T2)
    xt = similar(x, T)
    @inbounds xt .= x
    EWMA(xt, convert(Float64, λ), convert(Float64, l),
         convert(T, μ), convert(T, σ))
end#constructor

"""
    EWMA(x; λ=0.2, l=3.0, μ=mean(x), σ=std(x))
"""
EWMA(x::AbstractVector; λ=0.2, L=3.0, μ=mean(x), σ=std(x)) = EWMA(x, λ, L, μ, σ)

# **** calculate

function predict_ewma(x::AbstractVector{T}, λ::Float64, μ::T) where T
    z = similar(x)

    if !isempty(x)
        # TODO: This assumes arrays are one-indexed
        @inbounds z[1] = λ * x[1] + (1 - λ)μ
        @inbounds for i in 2:length(z)
            z[i] = λ * x[i] + (1 - λ)*z[i-1]
        end#for
    end#if
    z
end#function

function calculate(cs::EWMA)
    z = predict_ewma(cs.x, cs.λ, cs.μ)

    lcl = similar(cs.x)
    ucl = similar(cs.x)
    let σ = cs.σ, λ = cs.λ, l = cs.L, μ = cs.μ
        @inbounds @simd for i in 1:length(lcl)
            cl = l*σ*sqrt(λ*(1 - (1 - λ)^(2i)) / (2 - λ))
            lcl[i] = Ref(μ) .- cl
            ucl[i] = Ref(μ) .+ cl
        end#for
    end#let
    ControlChart((z=z,), lcl, ucl, cs)
end#function

# *** Moving Centerline

struct MovingCenterlineExponentiallyWeightedMovingAverage{
    T, V <: AbstractVector{T}
} <: ExponentiallyWeightedMovingAverage{T,V}
    x::V
    λ::Float64
    L::Float64
    μ::T
    σ::T
    function MovingCenterlineExponentiallyWeightedMovingAverage(
        x::V, λ::Float64, l::Float64, μ::T, σ::T
    ) where {T, V<:AbstractVector{T}}
        if λ <= 0 || λ > 1
            throw(DomainError(λ, "Weight must be 0 < λ <= 1"))
        elseif l <= 0
            throw(DomainError(l, "Control limit width must be positive"))
        elseif σ < 0
            throw(DomainError(σ, "Standard deviation must be positive"))
        else
            new{T, V}(x, λ, l, μ, σ)
        end#if
    end#constructor
end#struct

"""
    MCEWMA(x; L=3.0, μ=mean(x), σ, λ)

Exponentially weighted moving average series with moving center-line on `x`.

Each point `z` in the EWMA is calculated according to:

```
z[0] = μ
z[i] = λ * x[i] + (1 - λ) * z[i-1]
```

Where `μ` is the chosern starting value of `z`, and `λ` is a constant between 0
and 1. If not specified, `λ` is chosen as the value that minimizes the sum of
squared prediction error `e[t]^2`.

```
e[t] = x[t] - z[t]
```

The LCL and UCL are calculated at point `t` as:

```
lcl[t+1] = z[t] - L*σ
ucl[t+1] = z[t] + L*σ
```

Where `L` is a constant and `σ` is the standard deviation of the
errors `e[t]`. If not specified, `σ` is calculated as:

```
σ = sum(e.^2) / length(x)
```

# References

- Introduction to Statistical Quality Control
  - Chapter
    - 10.4: SPC with Autocorrelated Process Data
  - Pages
    - 468--471
  - Author:
    - Montgomery, Douglas C.
  - ISBN:
    - 978-1-118-14681-1
  - Publisher
    - John Wiley & Sons, Inc.
  - Year
    - 2013
- Optim: A Mathematical Optimization Package for Julia
  - Author
    - Mogensen, Patrick Kofod
    - Riseth, Asbjørn Nilsen
  - Journal
    - Journal of Open Source Software
  - Year
    - 2018
  - Volume
    - 3
  - Number
    - 24
  - Pages
    - 615
  - DOI
    - 10.21105/joss.00615
  - URL
    - https://github.com/JuliaNLSolvers/Optim.jl
"""
const MCEWMA = MovingCenterlineExponentiallyWeightedMovingAverage

function MCEWMA(
    x::AbstractVector{T1}; λ=nothing, L=3.0,
    μ::T2=mean(x), σ=nothing
) where {T1, T2}
    T = promote_type(T1, T2)

    x₁ = similar(x, T)
    @inbounds x₁ .= x

    μ₁ = convert(T, μ)

    λ₁ = if isnothing(λ)
        # Find optimal value of λ using nonlinear optimization
        min_objective(λ::Float64) = sum(zip(x₁, predict_ewma(x₁, λ, μ₁))) do (i, j)
            (i - j)^2
        end#sum

        res = Optim.optimize(min_objective, 0.0, 1.0)
        if !Optim.converged(res)
            error("Optim: Unable to find minimum λ value")
        end#if

        Optim.minimizer(res)
    else
        convert(Float64, λ)
    end#if

    L₁ = convert(Float64, L)

    σ₁ = if isnothing(σ)
        sqrt(λ₁ / length(x))
    else
        convert(T, σ)
    end#if

    MCEWMA(x₁, λ₁, L₁, μ₁, σ₁)
end#function

end#module Types
