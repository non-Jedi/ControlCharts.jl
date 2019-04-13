module ControlCharts

#module Calc
#
#function grouped_std(x, groups)
#    @assert length(x) == length(groups)
#
#end#module

export Stat, Geom

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

import Colors: Color, RGBA
import Compose: compose!, context, line, linewidth, stroke, strokedash, svgclass
import Gadfly
import Gadfly.Geom: element_aesthetics, inherit, render, Shape
import Measures: Measure
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

function render(geom::ShewartChart, theme::Gadfly.Theme, aes::Gadfly.Aesthetics)
    Gadfly.assert_aesthetics_defined("Geom.xbars", aes,
                                     :x, :y, :ymin, :ymax)
    Gadfly.assert_aesthetics_equal_length("Geom.xbars", aes,
                                          :x, :y, :ymin, :ymax)

    n = length(aes.x)
    default_aes = Gadfly.Aesthetics()
    default_aes.color = fill(RGBA{Float32}(theme.default_color), n)
    default_aes.size = Measure[theme.point_size]
    aes = inherit(aes, default_aes)

    root = context()
    compose!(root, (context(), line(collect(zip(aes.x, aes.y))),
                    stroke([first(aes.color)]),
                    strokedash(Gadfly.get_stroke_vector(theme.line_style[1])),
                    svgclass("geometry"), linewidth(theme.line_width)))
    compose!(root, (context(),
                    (context(), Shape.circle(aes.x, aes.y, aes.size), svgclass("marker")),
                    fill(aes.color), stroke(aes.color)))
    root
end#function

end#module

end#module
