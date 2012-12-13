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

local error, rawequal, rawget, rawset, setmt
    = error, rawequal, rawget, rawset, setmetatable

local Object = {
}

for i = 1, #Meta do
    local n = Meta[i]
    local mtfn
    if n == "__tostring" then
        mtfn = function(self, ...)
            local protos = rawget(self, "__protos")
            for i = 1, #protos do
                local v = protos[i][n]
                if v ~= mtfn then
                    return v(self, ...)
                end
            end
            return "object"
        end
    else
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
    end
    Object[n] = mtfn
end

-- sort of inefficient variant, make non-recursive later?
local is_a; is_a = function(self, base)
    if rawequal(self, base) then return true end
    local pts = rawget(self, "__protos")
    if not pts then return false end
    for i = 1, #pts do
        if is_a(pts[i], base) then
            return true
        end
    end
    return false
end
Object.is_a = is_a
M.__vx_obj_is_a = is_a

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

local Super_MT = {
    __index = function(self, n)
        local protos, obj = self.protos, self.obj
        for i = 1, #protos do
            local v = protos[i][n]
            if v ~= nil then
                return function(inst, ...)
                    return v((inst == self) and obj or inst, ...)
                end
            end
        end
    end
}

M.__vx_obj_super = function(obj, obj2)
    if obj2 == nil then
        obj2 = obj
        obj  = obj.__protos[1]
    end
    return setmt({ protos = obj.__protos, obj = obj2 }, Super_MT)
end
