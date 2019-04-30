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

module RepeatArrays

export RepeatArray, RepeatVector

import Base: size, getindex, IndexStyle, IndexLinear, iterate, length

struct RepeatArray{T,N} <: AbstractArray{T,N}
    value::T
    dims::NTuple{N,Int}
end#struct

const RepeatVector{T} = RepeatArray{T,1}
RepeatVector(v::T, n::Int) where T = RepeatArray{T,1}(v, (n,))

size(a::RepeatArray) = a.dims
function getindex(a::RepeatArray, i::Int)
    @boundscheck checkbounds(a, i)
    a.value
end#function
IndexStyle(::Type{RepeatArray{T,N}}) where {T,N} = IndexLinear()

end#module RepeatArrays
