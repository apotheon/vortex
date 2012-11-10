--[[ Vortex 0.1 list runtime module

 Author: q66 <quaker66@gmail.com>
 Lisp-like list implementation for Vortex.

 Available under the terms of the MIT license.
]]

local M = require("rt.core")

local getmt, setmt, tostr, type = getmetatable, setmetatable, tostring, type
local ipairs = ipairs

local list_tostring
list_tostring = function(l)
    if type(l) ~= "table" then
        return tostr(l)
    end
    return "(" .. tostr(l[1]) .. (l[2] and
        (", " .. list_tostring(l[2]) .. ")") or ")")
end

local list_pairs = function(self)
    local idx = 0
    return function()
        if self then
            local it = self[1]
            self = self[2]
            idx = idx + 1
            return idx, it
        end
    end
end

local List_MT = {
    __len = function(self)
        local i, x = 0, self
        while x do i, x = i + 1, x[2] end
        return i
    end,

    __tostring = function(self)
        return "list: " .. list_tostring(self)
    end,

    __pairs = list_pairs,
    __ipairs = list_ipairs,

    __eq = function(self, lst)
        return (self[1] == lst[1]) and (self[2] == lst[2])
    end
}

local list_new
list_new = function(first, ...)
    return setmt({ first, ... and list_new(...) or nil }, List_MT)
end

M.__vx_list_first = function(list)
    return list[1]
end
M.__vx_list_rest = function(list)
    return (getmt(list) == List_MT) and list[2] or { unpack(list, 2) }
end
M.__vx_list_cons = function(head, tail)
    return setmt({ head, tail }, List_MT)
end
M.__vx_list_new = list_new

local list_map
list_map = function(fun, lst)
    return setmt({ fun(lst[1]), lst[2] and list_map(fun, lst[2]) or nil },
        List_MT)
end

M.__vx_list_map = function(fun, lst)
    if getmt(lst) == List_MT then
        return list_map(fun, lst)
    else
        local ret = {}
        for i = 1, #lst do
            ret[i] = fun(lst[i])
        end
        return ret
    end
end
