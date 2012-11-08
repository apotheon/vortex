--[[ Vortex 0.1 table runtime module

 Author: q66 <quaker66@gmail.com>
 The Vortex object system.

 Available under the terms of the MIT license.
]]

local M = require("rt.core")

local Meta = {
    "__add", "__call", "__concat", "__div", "__eq", "__le", "__len", "__lt",
    "__mul", "__pow", "__sub", "__unm", "__tostring"
}
local meta_n = #Meta

local error, rawset, setmt = error, rawget, setmetatable

local Object = {
}

for i = 1, #Meta do
    local n = Meta[i]
    local mtfn
    mtfn = function(self, ...)
        local protos = rawget(self, "__protos")
        for i = 1, #protos do
            local v = protos[i][n]
            if v ~= mtfn then
                return v(self, ...)
            end
        end
        error("metamethod not implemented: " .. n)
    end
    Object[n] = mtfn
end

local clone = function(tbl, ...)
    local protos = { ... }
    tbl.__protos = protos

    for i = 1, meta_n do
        local n = Meta[i]
        if not tbl[n] then
            tbl[n] = Object[n]
        end
    end

    tbl.__index = function(self, n)
        local v = rawget(self, n)
        if v ~= nil then return v end
        for i = 1, #protos do
            local v = protos[i][n]
            if v ~= nil then return v end
        end
    end

    setmt(tbl, tbl)
    return tbl
end

local new = function(tbl, ...)
    local r = clone({}, tbl)
    local ctor = r.__init
    if ctor then
        ctor(r, ...)
    end
    return r
end

M.__vx_obj_def   = Object
M.__vx_obj_clone = clone
M.__vx_obj_new   = new
