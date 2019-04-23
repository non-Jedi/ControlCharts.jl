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

using ControlCharts.Types

@testset "Constructors" begin
    @test_throws DomainError EWMA([1,2,3,4], 1.1, 3, 2, 1)
    @test_throws DomainError EWMA([1,2,3,4], 2, 3, 2, 1)
    @test_throws DomainError EWMA([1,2,3,4], -5, 3, 2, 1)
    @test_throws DomainError EWMA([1,2,3,4], 0.5, -3, 2, 1)
    @test_throws DomainError EWMA([1,2,3,4], 0.5, 3, 2, -1)
end#@testset

const x = [09.45,
           07.99,
           09.29,
           11.66,
           12.16,
           10.18,
           08.04,
           11.46,
           09.20,
           10.34]
const cpositive = [0,
                   0,
                   0,
                   1.16,
                   2.82,
                   2.50,
                   0.04,
                   1.00,
                   0,
                   0]
const cnegative = -[0.05,
                    1.56,
                    1.77,
                    0,
                    0,
                    0,
                    1.46,
                    0,
                    0.30,
                    0]
const cusum = CUSUM(x, 0.5, 5, 10)
const cusum_chart = calculate(cusum)
const cusum_chart2 = ControlChart(cusum)

@testset "CUSUM" begin
    @test all(cusum_chart.z[1] .≈ cnegative)
    @test all(cusum_chart.z[2] .≈ cpositive)
    @test all(cusum_chart.LCL .≈ -5)
    @test all(cusum_chart.UCL .≈ 5)
    @test all(cusum_chart.z[1] .≈ cusum_chart2.z[1])
end#@testset

const z_ewma = [09.94500,
                09.74950,
                09.70355,
                09.89920,
                10.12530,
                10.13070,
                09.92167,
                10.07550,
                09.98796,
                10.02320]
const cl_ewma = [0.27,
                 0.363248,
                 0.424003,
                 0.467462,
                 0.499902,
                 0.52471,
                 0.543976,
                 0.559095,
                 0.571048,
                 0.580549]

const ewma = EWMA(x, 0.1, 2.7, 10, 1)
const ewma_chart = calculate(ewma)

@testset "EWMA" begin
    @test all(isapprox.(ewma_chart.z[1], z_ewma, atol=0.0001))
    @test all(isapprox.(ewma_chart.LCL, 10 .- cl_ewma, atol=0.0001))
    @test all(isapprox.(ewma_chart.UCL, 10 .+ cl_ewma, atol=0.0001))
    # test kwarg form of EWMA
    @test EWMA([1,2,3]).λ ≈ 0.2
    @test EWMA([1,2,3]; λ=0.15).λ ≈ 0.15
    @test EWMA([1,2,3]).L ≈ 3
    @test EWMA([1,2,3]; L=4).L ≈ 4
    @test EWMA([1,2,3]).μ ≈ 2
    @test EWMA([1,2,3]; μ=2.5).μ ≈ 2.5
    @test EWMA([1,2,3]).σ ≈ 1
    @test EWMA([1,2,3]; σ=1.1).σ ≈ 1.1
end#@testset
