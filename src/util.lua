--[[ Vortex utility library
    A bunch of utilities used across the Vortex compiler.

    Author: q66 <quaker66@gmail.com>
    License: MIT
]]

-- support Lua and LuaJIT
local bit = require (_VERSION == "Lua 5.2" and "bit32" or "bit")

META = {
    general = {
        version = 0.01
    },
    lexer = {
    },
    parser = {
    },
    cgen = {
        indent = 4
    }
}

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

-- hex digit?
local is_hex_digit = function(ch)
    local i = ch:byte()
    return (i >= 48 and i <= 57) or (i >= 65 and i <= 70)
        or (i >= 97 and i <= 102)
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
serialize = function(tbl, pretty, indent, simp)
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
                local noq
                if simp then
                    v, noq = simp(v)
                end
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
                elseif v == nil or t == "number" or t == "boolean" or noq then
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

    if tbl == nil then return "nil" end

    local t = type(tbl)

    if t ~= "table" then
        if t == "number" or t == "boolean" then
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

    randomseed(0)
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

local get_syms = function(suffix)
    return syms[suffix]
end

local io_stderr = io.stderr
local io_write  = io.write

local Stack = Object:clone {
    __len_fn = function(self)
        return self.length
    end,

    __init = function(self)
        self.length, self.__len  = 0, self.__len_fn
    end,

    push = function(self, item)
        self.length = self.length + 1
        self[self.length] = item
        return item
    end,

    pop = function(self)
        local len = self.length
        local it  = self[len]
        self[len] = nil
        self.length = len - 1
        return it
    end,

    top = function(self)
        return self[self.length]
    end
}

local getopt = function(arglist, shortargs, longargs)
    shortargs = shortargs or ""
    longargs  = longargs  or {}
    local len = #arglist
    local opts, args = {}, {}

    local vo = {}
    for i = 1, #longargs do
        local larg = longargs[i]
        if larg:find(".+=") then
            larg = larg:sub(1, #larg - 1)
            vo[larg] = true
        else
            vo[larg] = false
        end
    end
    for ch in shortargs:gmatch("([%w]:?)") do
        if #ch == 2 then
            vo[ch:sub(1, 1)] = true
        else
            vo[ch] = false
        end
    end

    -- want_val: false coming from short arg, true from long arg, nil otherwise
    local want_val, opt
    local skip = false
    for i = 1, len do
        local str = arglist[i]

        if want_val ~= nil then
            opt[2] = str
            opts[#opts + 1] = opt
            opt, str, want_val = nil, nil, nil
            goto getopt_cycle
        end

        if not skip and str == "--" then
            skip = true
            goto getopt_cycle
        end

        if skip then
            args[#args + 1] = str
            goto getopt_cycle
        end

        -- try long arguments - the form --foo value, --foo=value, --foo
        local mstr = str:match("^%-%-(.+)$")
        if mstr then
            local a, b = mstr:match("([%w-]+)=(.+)")
            if not a then a = mstr end
            opt = { a, b }

            local valued = vo[a]
            if valued == nil then
                error("unrecognized option '--" .. a .. "'", 0)
            elseif valued == false and b then
                error("option '--" .. a .. "' must not have an argument", 0)
            elseif valued == true and not b then
                want_val = true
            else
                if not b then opt[2] = "" end
                opts[#opts + 1] = opt
                opt = nil
            end
            goto getopt_cycle
        end

        -- short arguments - the form -x, -x value
        mstr = str:match("^%-(%w)$")
        if mstr then
            opt = { mstr }
            local valued = vo[mstr]
            if valued == nil then
                error("invalid option -- " .. mstr, 0)
            elseif valued then
                want_val = false
            else
                opt[2] = ""
                opts[#opts + 1] = opt
                opt = nil
            end
            goto getopt_cycle
        end

        args[#args + 1] = str
        ::getopt_cycle::
    end
    if want_val == true then
        error("option '--" .. opt[1] .. "' requires an argument", 0)
    elseif want_val == false then
        error("option requires an argument -- " .. opt[1], 0)
    end
    return opts, args
end

local tb = debug.traceback
local traceback = function(msg)
    return msg:match("^.+%.lua:%d+: .*$") and tb(msg) or msg
end

local xpcall = xpcall
local vxpcall = function(fun, ...)
    return xpcall(fun, traceback, ...)
end

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
    is_hex_digit = is_hex_digit,

    -- checks if the given character can be in an identifier
    is_ident = function(ch)
        return is_alnum(ch) or (ch == "_")
    end,

    -- fatal error, prints to stderr and exits
    fatal = function(msg)
        error(msg, 0)
    end,

    traceback = traceback,
    vxpcall = vxpcall,

    Object = Object,
    Stack  = Stack,

    is_array = is_array,
    serialize = serialize,

    randomseed = randomseed,
    random = rnd,
    unique_sym = unique_sym,
    get_syms = get_syms,
    hash_sym = hash_sym,

    getopt = getopt,

    map = function(tbl, fun)
        local r = {}
        for i = 1, #tbl do
            r[i] = fun(tbl[i])
        end
        return r
    end,

    band = bit.band,
    bor  = bit.bor,
    bnot = bit.bnot,
    lshift = bit.lshift,
    rshift = bit.rshift
}
