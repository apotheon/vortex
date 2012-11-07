--[[ Vortex 0.1 table runtime module

 Author: q66 <quaker66@gmail.com>
 The table specific things for Vortex runtime. Appends to the default runtime
 namespace.

 Available under the terms of the MIT license.
]]

local M = require("rt.core")

local select = select
M.__vx_tbl_append = function(tbl, ...)
    for i =1, select("#", ...) do
        tbl[#tbl + 1] = select(i, ...)
    end
end

-- XXX: optimize? assume hash keys?
M.__vx_tbl_join = function(a, b)
    local ret, alen = {}, #a
    for i = 1, alen do
        ret[i] = a[i]
    end
    for i = 1, #b do
        ret[alen + i] = b[i]
    end
    return ret
end

M.__vx_tbl_concat = _G.table.concat
M.__vx_tbl_insert = _G.table.insert
M.__vx_tbl_remove = _G.table.remove
M.__vx_tbl_unpack = _G.table.unpack

-- TODO: sorting algorithms and other table functions