--[[ Vortex 0.1 env runtime module

 Author: q66 <quaker66@gmail.com>
 Provides environment facilities for Lua 5.2 and LuaJIT. Appends to the
 default runtime namespace.

 Available under the terms of the MIT license.
]]

local M = require("rt.core")

if _VERSION == "Lua 5.2" then
    local gu, su, gi, uj = debug.getupvalue, debug.setupvalue,
                           debug.getinfo, debug.upvaluejoin

    local error, type = error, type

    local env_lookup = function(f)
        local name, val
        local up, unknown = 0
        repeat
            up = up + 1
            name, val = gu(f, up)
            if name == "" then
                unknown = true
            end
        until name == "_ENV" or not name
        if name ~= "_ENV" then
            up = nil
            if unknown then
                error("upvalues not readable when debug info missing", 3)
            end
        end
        return name == "_ENV" and up, val, unknown
    end

    local env_helper = function(f, name)
        local t = type(f)
        if t == "number" then
            if f < 0 then
                error(("bad argument #1 to '%s' (level must be non-negative")
                    :format(name), 3)
            elseif f < 1 then
                error("thread environments unsupported", 3)
            end
            f = gi(f + 2, "f").func
        elseif t ~= "function" then
            error(("bad argument #1 to '%s' (number expected, got %s)")
                :format(name, type(f)), 2)
        end
        return f
    end

    M.__vx_env_set = function(f, t)
        f = env_helper(f, "setfenv")
        local up, val, unknown = env_lookup(f)
        if up then
            uj(f, up, function() return up end, 1)
            su(f, up, t)
        else
            local w = gi(f, "S").what
            if w ~= "Lua" and w ~= "main" then error(
                "'setfenv' cannot change environment of the given object", 2
            ) end
        end
        return f
    end

    M.__vx_env_get = function(f)
        if f == 0 or not f then return _G end
        f = env_helper(f, "getfenv")
        local up, val = env_lookup(f)
        if not up then return _G end
        return val
    end
else
    M.__vx_env_get = getfenv
    M.__vx_env_set = setfenv
end