--[[ Vortex 0.1 default script environment

 Author: q66 <quaker66@gmail.com>
 Provides the default environment under which all Vortex scripts run.

 Available under the terms of the MIT license.
]]

local M = require("rt.core")

local pairs = pairs
local ipairs = ipairs
local pcall = pcall
local floor = math.floor

local env = {}
M.__vx_def_env = env

env.assert   = M.__vx_assert
env.error    = M.__vx_error
env.get_mt   = M.__vx_getmt
env.set_mt   = M.__vx_setmt
env.print    = M.__vx_print
env.select   = M.__vx_select
env.tostr    = M.__vx_tostr
env.tonum    = M.__vx_tonum
env.pcall    = M.__vx_pcall
env.xpcall   = M.__vx_xpcall
env.raw_eq   = M.__vx_raweq
env.raw_get  = M.__vx_rawget
env.raw_set  = M.__vx_rawset
env.raw_len  = M.__vx_rawlen
env.require  = M.__vx_require
env.gcollect = M.__vx_gcollect
env.type     = M.__vx_type
env.pairs    = M.__vx_pairs
env.ipairs   = M.__vx_ipairs
env.setfenv  = M.__vx_env_set
env.getfenv  = M.__vx_env_get

env.unpack   = M.__vx_tbl_unpack

-- temporary until the standard library is in
env.table    = _G.table
env.string   = _G.string
env.debug    = _G.debug
env.math     = _G.math
env.os       = _G.os
env.io       = _G.io

-- Vortex builtin functions
env.super = M.__vx_obj_super
env.first = M.__vx_list_first
env.rest  = M.__vx_list_rest
env.map   = M.__vx_list_map
env.dict  = function(tbl)
    local r = {}
    for i, v in ipairs(tbl) do
        r[v[1]] = v[2]
    end
    return r
end
env.zip = function(a, b)
    local r = {}
    for i = 1, #a do
        r[i] = { a[i], b[i] }
    end
    return r
end
env.items = function(tbl)
    local r = {}
    for k, v in pairs(tbl) do
        r[#r + 1] = { k, v }
    end
    return r
end
env.filter = function(fun, tbl)
    local r = {}
    for i = 1, #tbl do
        local v = tbl[i]
        if fun(v) then r[#r + 1] = v end
    end
    return r
end
env.isstr   = function(s) return type(s) == "string" end
env.isnum   = function(n) return type(n) == "number" end
env.isfun   = function(f) return type(f) == "function" end
env.isbool  = function(b) return type(b) == "boolean" end
env.isnil   = function(n) return n == nil end
env.inarray = function(arr, idx)
    return ((type(idx) == "number") and floor(idx) == idx
             and idx >= 1 and idx <= #arr)
end
env.tconc   = _G.table.concat
env.floor   = floor
env.ceil    = math.ceil
env.resume  = M.__vx_coro_resume

-- the parser
local parser = M.__vx_parser
env.parser   = parser

local pparse, pbuild = parser.parse, parser.build
local lload, concat = _G.load, table.concat
local load = function(ld, src, mode, _env)
    if type(ld) ~= "string" then
        local s = ld()
        if s then
            local t, i = { s }, 2
            while true do
                local c = ld()
                if c then t[i] = c else break end
                i = i + 1
            end
            ld = concat(t)
        else
            ld = nil
        end
    end
    local stat, ret = pcall(pparse, ld)
    if not stat then return nil, ret end
    stat, ret = pcall(pbuild, ret)
    if not stat then return nil, ret end
    return lload(ret, src, mode, _env or env)
end
env.load = load

local io_open = io.open
env.loadfile = function(fname, mode, _env)
    local stat, sr = pcall(io_open, fname, "rb")
    if not stat then return nil, sr end
    local str = sr:read("*all")
    sr:close()
    return load(str, "@" .. fname, mode, _env)
end

env._L       = _G
env._G       = env
env._R       = M
env._VERSION = "Vortex 0.1"

env.arg = arg