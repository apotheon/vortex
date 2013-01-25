--[[ Vortex 0.1 strict globals

 Author: q66 <quaker66@gmail.com>
 Handles the Vortex global variable behavior, which is "strict" by default.
 All global variables must be declared before usage or assignment.

 Available under the terms of the MIT license.
]]

local M = require("rt.core")
local E = M.__vx_def_env

local rawget, rawset = rawget, rawset
local getinfo, error, select = debug.getinfo, error, select

local mt = {}
setmetatable(E, mt)

mt.__gdecl = {}
local what = function()
    local info = getinfo(3, "S")
    return info and info.what or "C"
end

mt.__newindex = function(self, n, v)
    if not mt.__gdecl[n] then
        if what() ~= "C" then
            error("assign to undeclared variable '" .. n .. "'")
        end
        self.__gdecl[n] = true
    end
    rawset(self, n, v)
end

mt.__index = function(self, n)
    if not mt.__gdecl[n] and what() ~= "C" then
        error("access to undeclared variable '" .. n .. "'")
    end
    return rawget(self, n)
end

M.__vx_gdecl = function(...)
    local gdecl = mt.__gdecl
    for i = 1, select("#", ...) do
        gdecl[select(i, ...)] = true
    end
end
