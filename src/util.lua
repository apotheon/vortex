--[[ Vortex utility library
    A bunch of utilities used across the Vortex compiler.

    Author: q66 <quaker66@gmail.com>
    License: MIT
]]

-- support Lua and LuaJIT
local bit = require (_VERSION == "Lua 5.2" and "bit32" or "bit")

--
-- stream utilities
--

local strstream = function(str)
    return string.gmatch(str, ".")
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

--
-- character utilities
--

-- is the given char ascii?
local is_ascii = function(ch)
    local   i = ch:byte()
    return (i >= 0 and i <= 127)
end

-- is it alphabetic?
local is_alpha = function(ch)
    local   i = ch:byte()
    return (i >= 65 and i <= 90) or (i >= 97 and i <= 122)
end

-- or alphanumeric?
local is_alnum = function(ch)
    local   i = ch:byte()
    return (i >= 48 and i <= 57) or (i >= 65 and i <= 90)
        or (i >= 97 and i <= 122)
end

-- is it a digit?
local is_digit = function(ch)
    local   i = ch:byte()
    return (i >= 48 and i <= 57)
end

--
-- object system
--

local Object = {
    -- instantiation of clones
    __call = function(self, ...)
        local r = {
            __index = self, __proto = self, __call = self.__call,
            __tostring = self.__inst_tostring or self.__tostring,
            name = self.name
        }

        setmetatable(r, r)
        if self.__init then self.__init(r, ...) end
        return r
    end,

    -- cloning of any clone
    clone = function(self, tbl)
        tbl = tbl or {}
        tbl.__index, tbl.__proto, tbl.__call = self, self, self.__call
        if not tbl.__tostring then tbl.__tostring = self.__tostring end

        setmetatable(tbl, tbl)
        return tbl
    end,

    -- checks if an object is a child of another object (deep down
    -- to the structure, checks all the parents)
    is_a = function(self, base)
        if self == base then return true end

        local pt, is = self.__proto, pt == base
        while not is and pt do
            pt, is = pt.__proto, pt == base
        end
        return is
    end,

    __tostring = function(self)
        return ("Object: %s"):format(self.name or "unnamed")
    end
}

-- is the given thing an array?
local is_array = function(tbl)
    local i = #tbl
    for _ in pairs(tbl) do
        i = i - 1
        if i < 0 then
            return false, #tbl
        end
    end
    return true, #tbl
end

-- a neat table serializer
local serialize
serialize = function(tbl, pretty, indent)
    pretty = pretty or false
    indent = indent or 4

    local enc
    enc = function(tbl, tables, ind)
        local assoc, narr = is_array(tbl)
        local ret         = {}
        tables = tables  or {}

        assoc = not assoc

        for k, v in (assoc and pairs or ipairs)(tbl) do
            local skip = false
            local tk   = type(k)

            if tk == "string" and k:sub(1, 2) == "__" then
                skip = true
            end

            if not skip then
                local elem

                local t = type(v)

                if assoc then
                    assert(tk == "string" or tk == "number", 
                        "only string and number keys allowed for serialization"
                    )

                    if tk == "string" then
                        if not loadstring(k .. "=nil") then
                            elem = { "[\"", k, "\"]",
                                pretty and " = " or "=", true }
                        else
                            elem = { k, pretty and " = " or "=", true }
                        end
                    else
                        elem = { "[", tostring(k), "]",
                            pretty and " = " or "=", true }
                    end
                else
                    elem = { true }
                end

                if t == "table" then
                    -- the table references itself, infinite recursion
                    -- do not permit such behavior
                    if v == tbl or tables[v] then
                        elem[#elem] = "\"" .. tostring(v) .. "\""
                    else
                        tables[v] = true
                        elem[#elem] =
                            enc(v, tables, assoc and ind + indent or ind)
                    end
                elseif t == "number" then
                    elem[#elem] = tostring(v)
                else
                    elem[#elem] = "\"" .. tostring(v) .. "\""
                end

                if assoc and pretty then
                    table.insert(ret, "\n" .. (" "):rep(ind)
                        .. table.concat(elem))
                else
                    table.insert(ret, table.concat(elem))
                end
            end
        end

        if pretty then
            if assoc then
                table.insert(ret, "\n" .. (" "):rep(ind - indent))
                return "{" .. table.concat(ret, ",") .. "}"
            -- special case - an array containing one table, don't add spaces
            elseif #tbl == 1 and type(tbl[1] == "table") then
                return "{" .. table.concat(ret, ", ") .. "}"
            end

            return "{ " .. table.concat(ret, ", ") .. " }"
        end

        return "{" .. table.concat(ret, ",") .. "}"
    end

    local t = type(tbl)

    if t ~= "table" then
        if t == "number" then
            return tostring(tbl)
        else
            return "\"" .. tostring(tbl) .. "\""
        end
    end

    return enc(tbl, nil, indent)
end

local randomseed, random
do
    local phi = 0x9E3779B9
    local q, c = { true, true, true }, 362436

    local bxor, band, rshift = bit.bxor, bit.band, bit.rshift
    local floor = math.floor

    -- seeds a number for the RNG
    randomseed = function(x)
        local i

        q[1] = x
        q[2] = x + phi
        q[3] = x + phi * 2

        for i = 4, 4096 do
            q[i] = bxor(q[i - 3], q[i - 2], phi, i - 1)
        end
    end

    local i = 4095

    -- a PRNG - complementary MWC
    random = function()
        local t, a = nil, 18782
        local x, r = nil, 0xFFFFFFFE

        i = band(i + 1, 4095)
        t = a * q[i + 1] + c
        c = rshift(t, 32)
        x = t + c
        if  x < c then
            x = x + 1
            c = c + 1
        end

        local  ret = r - x
        q[i + 1] = ret
        return floor(ret)
    end
end

-- generates a random number from a to b
local rnd = function(a, b)
    if not b then
        return random() % a
    else
        return random() % b + a
    end
end

local syms = {}
META.cgen.syms = syms

local chars = "1234567890abcdefghijklmnopqrstuvwxyz"
local clen = #chars

local sch, tc = string.char, table.concat

local hash = function(ch)
    return "_" .. ch:byte() .. "d"
end

-- "hashes" a string so that it becomes a valid Lua identifier
local hash_sym = function(sym)
    local r = sym:gsub("[^a-zA-Z0-9_]", hash)
    return r
end

-- generates an unique symbol name with a given suffix
local unique_sym = function(suffix)
    local st = syms[suffix]
    if not st then
        st = {}
        syms[suffix] = st
    end

    local uq
    local t = { true, true, true, true }
    repeat
        t[1], t[2], t[3], t[4] =
            sch(chars:byte(rnd(1, clen))), sch(chars:byte(rnd(1, clen))),
            sch(chars:byte(rnd(1, clen))), sch(chars:byte(rnd(1, clen)))
        uq = table.concat(t)
    until not st[uq]

    return tc { "__", uq, "_", hash_sym(suffix) }
end

local io_stderr = io.stderr
local io_write  = io.write

return {
    file_istream  = ifstream,
    file_ostream  = ofstream,
    string_stream = strstream,

    -- checks whether the value is a newline character (CR or LF)
    is_newline = function(ch)
        return (ch == "\n" or ch == "\r")
    end,

    -- checks whether the value is a whitespace 
    is_white = function(ch)
        return (ch == " " or ch == "\f" or ch == "\t" or ch == "\v")
    end,

    is_ascii = is_ascii,
    is_alpha = is_alpha,
    is_alnum = is_alnum,
    is_digit = is_digit,

    -- checks if the given character can be in an identifier
    is_ident = function(ch)
        return is_alnum(ch) or (ch == "_") or (ch == "?")
    end,

    -- checks if the given character can be in a keyword
    is_keyword = function(ch)
        return is_alnum(ch) or (ch == "_") or (ch == "?")
                            or (ch == "@") or (ch == "#")
    end,

    -- fatal error, prints to stderr and exits
    fatal = function(msg)
        io_stderr:write(msg, "\n")
        os.exit(1)
    end,

    Object = Object,

    is_array = is_array,
    serialize = serialize,

    randomseed = randomseed,
    random = rnd,
    unique_sym = unique_sym,
    hash_sym = hash_sym,

    parse_args = function(argv)
        local ret = {}

        local val = false
        for i = 1, #argv do
            local opt = argv[i]
            if opt == "-o" then
                val = true
            elseif val then
                opt:gsub("(.+)=(.+)", function(a, b)
                    ret[#ret + 1] = { a, b }
                end)
                val = false
            else
                ret[#ret + 1] = opt
            end
        end

        return ret
    end
}
