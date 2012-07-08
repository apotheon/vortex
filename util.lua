--[[ Buttscript 0.1 utilities

 Author: q66 <quaker66@gmail.com>
 Available under the terms of the MIT license.
]]

local strstream = function(str)
    return string.gmatch(str, "([%z\1-\127\194-\244][\128-\191]*)")
end

local ofstream = function(f)
    return function(...)
        f:write(...)
    end
end

local ifstream = function(f)
    local str = f:read("*all")
    return strstream(str)
end

local identchars = {
    ["_"] = true
}

local keywordchars = {
    ["@"] = true,
    ["#"] = true
}

local identkeywordchars = {
    ["_"] = true,
    ["@"] = true,
    ["#"] = true
}

local is_ascii = function(ch)
    local   i = ch:byte()
    return (i >= 0 and i <= 127)
end

local is_alpha = function(ch)
    local   i = ch:byte()
    return (i >= 65 and i <= 90) or (i >= 97 and i <= 122)
end

local is_alnum = function(ch)
    local   i = ch:byte()
    return (i >= 48 and i <= 57) or (i >= 65 and i <= 90)
        or (i >= 97 and i <= 122)
end

local is_digit = function(ch)
    local   i = ch:byte()
    return (i >= 48 and i <= 57)
end

local io_stderr = io.stderr
local io_write  = io.write

local is_a = function(inst, base)
    local  cl = getmetatable(inst).__index
    while  cl do
        if cl == base then return true end
        cl = cl.base_class
    end

    return false
end

local get_class = function(inst)
    return getmetatable(inst).__index
end

local Object = {
    __call = function(self, ...)
        local ret = setmetatable({}, { 
            __index    = self,
            __tostring = self.__tostring or function(self)
                return self.name
            end
        })

        if  self.__init then
            self.__init(ret, ...)
        end

        return ret
    end,

    __tostring = function(self)
        return self.name
    end
}

local class = function(t)
    t = t or {}

    if t.base then return setmetatable(t, {
        __index    = t.base,
        __call     = Object.__call,
        __tostring = Object.__tostring
    }) end

    t.is_a      = is_a
    t.get_class = get_class
    return setmetatable(t, Object)
end

return {
    file_istream  = ifstream,
    file_ostream  = ofstream,
    string_stream = strstream,

    is_newline = function(ch)
        return (ch == "\n" or ch == "\r")
    end,

    is_white = function(ch)
        return (ch == " " or ch == "\f" or ch == "\t" or ch == "\v")
    end,

    is_ascii = is_ascii,
    is_alpha = is_alpha,
    is_alnum = is_alnum,
    is_digit = is_digit,

    is_ident = function(ch)
        return is_alnum(ch) or (ch == "_")
    end,

    is_keyword = function(ch)
        return is_alnum(ch) or (keywordchars[ch] ~= nil)
    end,

    is_ident_keyword = function(ch)
        return is_alnum(ch) or (identkeywordchars[ch] ~= nil)
    end,

    fatal = function(msg)
        io_stderr:write(msg, "\n")
        os.exit(1)
    end,

    class = class
}
