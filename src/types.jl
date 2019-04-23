# This file is part of ControlSeriess.jl.
#
# ControlSeriess.jl is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# ControlSeriess.jl is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.

module Types

export ControlChart, calculate,
    TabularCumulativeSum, CUSUM, ExponentiallyWeightedMovingAverage, EWMA

import Statistics: mean, std

"Represents a series of values to be turned into a control chart."
abstract type ControlSeries{T,V} end

"""
    ControlChart(z, lcl, ucl, control_series)

Represents a control chart.

Vectors in tuple `x` is the main series to plot while `lcl` and `ucl` are
vectors defining points where each point in each vector in `x` must
not exceed in the negative and positive directions. This function is
returned by calling `calculate` on a `ControlSeries`.
"""
struct ControlChart{T, V <: AbstractVector{T}, C <: ControlSeries{T,V},
                    N, Z <: NTuple{N,V}}
    z::Z
    LCL::V
    UCL::V
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

# Shewart Charts
#---------------

abstract type ShewartSeries{T,V} <: ControlSeries{T,V} end

# Cumulative Sum Charts (CUSUM)
#------------------------------

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
    xt .= x
    CUSUM(x::AbstractVector{T},
          convert(T, k)::T, convert(T, h), convert(T, μ)::T)
end#constructor

function calculate(series::CUSUM{T,V}) where {T,V}
    cpositive = similar(series.x)
    cnegative = similar(series.x)

    cpositive[1] = max(zero(T), series.x[1] - series.μ - series.K)
    cnegative[1] = min(zero(T), series.x[1] - series.μ + series.K)
    for i in 2:length(cpositive)
        cpositive[i] = max(zero(T),
                           series.x[i] - series.μ - series.K + cpositive[i-1])
        cnegative[i] = min(zero(T),
                           series.x[i] - series.μ + series.K + cnegative[i-1])
    end#for

    lcl = similar(series.x)
    ucl = similar(series.x)
    lcl .= Ref(-series.H)
    ucl .= Ref(+series.H)

    ControlChart((cnegative, cpositive), lcl, ucl, series)
end#function

# Exponentially Weighted Moving Average Chart (EWMA)
#---------------------------------------------------

struct ExponentiallyWeightedMovingAverage{T, V<:AbstractVector{T}} <: ControlSeries{T,V}
    x::V
    λ::Float64
    L::Float64
    μ::T
    σ::T
    function ExponentiallyWeightedMovingAverage(
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

Represents an exponentially weighted moving average series on `x`.

Each point `z` in the EWMA is calculated according to:

```
z[0] = μ
z[i] = λ * x[i] + (1 - λ) * z[i-1]
```

where `λ` is a constant.

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
const EWMA = ExponentiallyWeightedMovingAverage

# try to make sure EWMA works even if types aren't as expected
function EWMA(
    x::AbstractVector{T2}, λ::Real, l::Real, μ::T1, σ::T1
) where {T1, T2}
    T = promote_type(T1, T2)
    xt = similar(x, T)
    xt .= x
    EWMA(xt, convert(Float64, λ), convert(Float64, l),
         convert(T, μ), convert(T, σ))
end#constructor

"""
    EWMA(x; λ=0.2, l=3.0, μ=mean(x), σ=std(x))
"""
EWMA(x::AbstractVector; λ=0.2, L=3.0, μ=mean(x), σ=std(x)) = EWMA(x, λ, L, μ, σ)

function calculate(cs::EWMA)
    z = similar(cs.x)

    z[1] = cs.λ * cs.x[1] + (1 - cs.λ)cs.μ
    for i in 2:length(z)
        z[i] = cs.λ * cs.x[i] + (1 - cs.λ)*z[i-1]
    end#for

    lcl = similar(cs.x)
    ucl = similar(cs.x)
    let σ = cs.σ, λ = cs.λ, l = cs.L, μ = cs.μ
        cl = (l*σ*sqrt(λ*(1 - (1 - λ)^(2i)) / (2 - λ)) for i in 1:length(lcl))
        lcl .= Ref(μ) .- cl
        ucl .= Ref(μ) .+ cl
    end#let
    ControlChart((z,), lcl, ucl, cs)
end#function

end#module Types
