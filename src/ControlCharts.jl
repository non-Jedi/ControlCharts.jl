module ControlCharts

export Stat, Geom, plot

import Gadfly: plot

include("types.jl")

module Stat

import Gadfly
import Gadfly: input_aesthetics, output_aesthetics
import Gadfly.Stat: apply_statistic
import SplitApplyCombine: group
import Statistics: mean, std

abstract type ControlChart <: Gadfly.StatisticElement end
abstract type ShewartChart <: ControlChart end

struct XBarS{T<:Union{Nothing,Real}} <: ShewartChart
    centerline::T
    sigma_limit::Float64
end
XBarS(; centerline=nothing, sigma_limit=3) = XBarS(centerline, sigma_limit)

input_aesthetics(stat::XBarS) = [:x, :y]
output_aesthetics(stat::XBarS) = [:x, :y, :ymin, :ymax, :yintercept]

"""
    Stat.xbars

Transform `y` grouped by `x` into aesthetics for an XbarS chart.
"""
const xbars = XBarS

function apply_statistic(stat::XBarS,
                         scales::Dict{Symbol, Gadfly.ScaleElement},
                         coord::Gadfly.CoordinateElement,
                         aes::Gadfly.Aesthetics)
    groups = group(i -> i[1], i -> i[2], zip(aes.x, aes.y))
    x = collect(keys(groups))
    center = isnothing(stat.centerline) ? mean(aes.y) : stat.centerline
    s = mean(std.(values(group); mean=center))
    y = mean.(values(group))
    sorted = sort(hcat(x, y))

    aes.x = sorted[:,1]
    aes.y = sorted[:,2]
    aes.ymin = aes.y .- stat.sigma_limit * s
    aes.ymax = aes.y .+ stat.sigma_limit * s
    aes.yintercept = [center,]
end#function

end#module

module Geom

import Compose: compose, context
import Gadfly
import Gadfly.Geom: line, point, ribbon, render, element_aesthetics
import ..Stat

abstract type ControlChart <: Gadfly.GeometryElement end

struct ShewartChart{T<:Union{Real,Nothing}} <: ControlChart
    center::T
end
ShewartChart(; center=nothing) = ShewartChart(center)

"""
    Geom.shewart[(; center=nothing)]

Control chart of the deviation of `y` aesthetic from `center` ordered by `x`.

Control limits will be taken from `ymin` and `ymax`.
"""
const shewart = ShewartChart

element_aesthetics(::ShewartChart) = [:x, :y, :ymin, :ymax]

end#module

end#module
