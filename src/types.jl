# This file is part of ControlCharts.jl.
#
# ControlCharts.jl is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# ControlCharts.jl is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.

module Types

export ShewartChart, CumulativeSumChart, CUSUM,
    ExponentiallyWeightedMovingAverageChart, EWMA

abstract type ControlChart end

abstract type ShewartChart <: ControlChart end

abstract type CumulativeSumChart <: ControlChart end
const CUSUM = CumulativeSumChart

struct ExponentiallyWeightedMovingAverageChart{T, V<:AbstractVector{T}} <: ControlChart
    λ::Float64
    μ::T
    x::V
    function ExponentiallyWeightedMovingAverageChart(
        λ::Float64, μ::T, x::V
    ) where {T, V<:AbstractVector{T}}
        if 0 < λ <= 1
            new{T, V}(λ, μ, x)
        else
            throw(DomainError(λ, "Weight must be 0 < λ <= 1"))
        end#if
    end#constructor
end#struct
const EWMA = ExponentiallyWeightedMovingAverageChart

function EWMA(λ, μ::T1, x::AbstractVector{T2}) where {T1, T2}
    T = promote_type(T1, T2)
    xt = similar(x, T)
    xt .= x
    EWMA(Float64(λ), T(μ), xt)
end#function

end#module Types
