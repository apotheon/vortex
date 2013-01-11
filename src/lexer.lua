--[[ Vortex 0.1 lexer

 Author: q66 <quaker66@gmail.com>
 Inspired by Lua's lexer (most notably in string parsing)

 Available under the terms of the MIT license.
]]

local util = require("util")

local is_newline   = util.is_newline
local is_white     = util.is_white
local is_ascii     = util.is_ascii
local is_ident     = util.is_ident
local is_alnum     = util.is_alnum
local is_digit     = util.is_digit
local is_hex_digit = util.is_hex_digit 
local fatal        = util.fatal
local concat       = table.concat
local Stack        = util.Stack

local wrap, yield = coroutine.wrap, coroutine.yield

-- all of the core vortex keywords
local keywords = {
    ["and"    ] = true,
    ["as"     ] = true,
    ["break"  ] = true,
    ["case"   ] = true,
    ["cfn"    ] = true,
    ["clone"  ] = true,
    ["coro"   ] = true,
    ["cycle"  ] = true,
    ["do"     ] = true,
    ["else"   ] = true,
    ["enum"   ] = true,
    ["false"  ] = true,
    ["fn"     ] = true,
    ["for"    ] = true,
    ["glob"   ] = true,
    ["goto"   ] = true,
    ["if"     ] = true,
    ["in"     ] = true,
    ["let"    ] = true,
    ["match"  ] = true,
    ["module" ] = true,
    ["new"    ] = true,
    ["nil"    ] = true,
    ["not"    ] = true,
    ["or"     ] = true,
    ["quote"  ] = true,
    ["rec"    ] = true,
    ["redo"   ] = true,
    ["result" ] = true,
    ["return" ] = true,
    ["seq"    ] = true,
    ["true"   ] = true,
    ["unless" ] = true,
    ["unquote"] = true,
    ["when"   ] = true,
    ["while"  ] = true,
    ["with"   ] = true,
    ["yield"  ] = true,

    ["__FILE__"] = true,
    ["__LINE__"] = true
}

-- true - binary op, false - unary op
local kwops = {
    ["band"] = true,
    ["bor" ] = true,
    ["bxor"] = true,
    ["bnot"] = false,
    ["asr" ] = true,
    ["bsr" ] = true,
    ["bsl" ] = true
}

local lex_error = function(ls, msg, value)
    msg = ("%s:%d: %s"):format(ls.source, ls.line_number, msg)
    if value then
        msg = msg .. " near '" .. value .. "'"
    end
    fatal(msg)
end

local syntax_error = function(ls, msg)
    lex_error(ls, msg, ls.token.value or ls.token.name)
end

local next_char = function(ls)
    local c = ls.reader()
    ls.current = c
    return c
end
local next_line = function(ls)
    local prev = ls.current
    assert(is_newline(ls.current))

    -- \n or \r
    next_char(ls)
    -- \n\r or \r\n
    if is_newline(ls.current) and ls.current ~= prev then
        next_char(ls)
    end

    ls.line_number = ls.line_number + 1
end

local save_and_next_char = function(ls, buf)
    table.insert(buf, ls.current)
    next_char(ls)
end

local lex

local read_number = function(ls, float)
    local buf = float and { ".", ls.current } or { ls.current }
    next_char(ls)
    while ls.current and (is_alnum(ls.current) or ls.current == ".") do
        save_and_next_char(ls, buf)
    end

    local str = concat(buf)
    if not tonumber(str) then
        lex_error(ls, "malformed number", str)
    end
    yield("<number>", str)
end

local read_long_comment

read_long_comment = function(ls)
    local finished = false
    while ls.current do
        -- nesting? get rid of the nested comment recursively.
        if ls.current == "/" then
            next_char(ls)
            if ls.current == "*" then
                next_char(ls)
                read_long_comment(ls)
            end
        end

        -- follow newlines for proper debug info
        if is_newline(ls.current) then
            next_line(ls)
        end

        -- end comments if required
        if ls.current == "*" then
            next_char(ls)
            if ls.current == "/" then
                next_char(ls)
                finished = true
                break
            end
        end

        next_char(ls)
    end
    -- here we should have the token right after the comment ready
    if not finished then
        lex_error(ls, "unfinished long comment", "<eos>")
    end
end

local char = string.char
local read_hex_esc = function(ls)
    local r = ""
    for i = 2, 3 do
        local c = next_char(ls)
        r = r .. c
        if not is_hex_digit(c) then
            lex_error(ls, "hexadecimal digit expected", "x" .. r)
        end
    end
    return char(tonumber("0x" .. r))
end

local read_dec_esc = function(ls)
    local r = ""
    for i = 1, 3 do
        local curr = ls.current
        if not is_digit(curr) then
            break
        end
        r = r .. curr
        next_char(ls)
    end
    local n = tonumber(r)
    if n > 255 then
        lex_error(ls, "decimal escape too large", r)
    end
    return char(n)
end

local read_string = function(ls, pre)
    local delim, long = ls.current
    if next_char(ls) == delim then
        if next_char(ls) == delim then
            next_char(ls)
            long = true
        -- empty strings
        else
            yield("<begstring>")
            yield("<string>", "")
            yield("<endstring>")
            return nil
        end
    end
    pre = pre or {}
    local raw, exp = pre.raw, pre.exp
    local buf = {}
    yield("<begstring>")
    while true do
        local curr = ls.current
        if not long then
            if curr == delim then
                break
            end
        elseif curr == delim then
            curr = next_char(ls)
            if curr == delim then
                curr = next_char(ls)
                if curr == delim then
                    break
                else
                    buf[#buf + 1] = delim .. delim
                end
            else
                buf[#buf + 1] = delim
            end
        end

        if not curr then
            lex_error(ls, "unfinished string", "<eos>")
        elseif is_newline(curr) and not long then
            lex_error(ls, "unfinished string", concat(buf))
        elseif curr == "\\" then
            local curr = next_char(ls)
            if curr == "a" then
                buf[#buf + 1] = raw and "\\a" or "\a"
                next_char(ls)
            elseif curr == "b" then
                buf[#buf + 1] = raw and "\\b" or "\b"
                next_char(ls)
            elseif curr == "f" then
                buf[#buf + 1] = raw and "\\f" or "\f"
                next_char(ls)
            elseif curr == "n" then
                buf[#buf + 1] = raw and "\\n" or "\n"
                next_char(ls)
            elseif curr == "r" then
                buf[#buf + 1] = raw and "\\r" or "\r"
                next_char(ls)
            elseif curr == "t" then
                buf[#buf + 1] = raw and "\\t" or "\t"
                next_char(ls)
            elseif curr == "v" then
                buf[#buf + 1] = raw and "\\v" or "\v"
                next_char(ls)
            elseif curr == "x" then
                buf[#buf + 1] = raw and "\\x" or read_hex_esc(ls, raw)
                next_char(ls)
            elseif curr == "z" then
                while is_newline(ls.current) or is_white(ls.current) do
                    if is_newline(ls.current) then
                        next_line(ls)
                    else
                        next_char(ls)
                    end
                end
            elseif curr == "$" and exp then
                buf[#buf + 1] = raw and "\\$" or "$"
                next_char(ls)
            elseif curr == "\n" or curr == "\r" then
                next_line(ls)
                if raw then
                    buf[#buf + 1] = long and "\\\n" or "\\"
                end
            elseif curr == "\\" or curr == '"' or curr == "'" then
                buf[#buf + 1] = raw and ("\\" .. curr) or curr
                next_char(ls)
            elseif not curr then
                lex_error(ls, "unfinished string", "<eos>")
            elseif raw then
                buf[#buf + 1] = "\\"
            else
                if not is_digit(curr) then
                    lex_error(ls, "invalid escape sequence", curr)
                end
                buf[#buf + 1] = read_dec_esc(ls)
            end
        elseif curr == "$" and exp then
            local str = concat(buf)
            buf = {}
            yield("<string>", str)
            local c = next_char(ls)
            if c == "(" then
                yield("$(")
                next_char(ls)
                if not lex(ls, true) then
                    lex_error(ls, "unfinished string", "<eos>")
                end
            elseif is_ident(c) and not is_digit(c) then
                yield("$")
                local id = { c }
                next_char(ls)
                while ls.current and is_ident(ls.current) do
                    save_and_next_char(ls, id)
                end
                yield("<ident>", concat(id))
            else
                lex_error(ls, "expected identifier", curr)
            end
        else
            save_and_next_char(ls, buf)
        end
    end
    next_char(ls)
    yield("<string>", concat(buf))
    yield("<endstring>")
end

lex = function(ls, instr)
    local lvl = 0
    while true do
        local curr = ls.current
        if not curr then
            break
        elseif instr and lvl == 0 and curr == ")" then
            next_char(ls)
            yield(")")
            return true
        end

        if is_newline(curr) then
            next_line(ls)
        elseif is_white(curr) then
            next_char(ls)
        -- (, balancing checks - when delim is )
        elseif curr == "(" then
            next_char(ls)
            lvl = lvl + 1
            yield("(")
        -- )
        elseif curr == ")" then
            next_char(ls)
            lvl = lvl - 1
            yield(")")
        -- =, ==, >, >=, <, <=, !, !=, %, %=
        elseif curr == "=" or curr == ">" or curr == "<" or curr == "!"
        or curr == "%" then
            next_char(ls)
            if ls.current == "=" then
                next_char(ls)
                yield(curr .. "=")
            else
                yield(curr)
            end
        -- +, +=, ++, ++=, *, *=, **, **=
        elseif curr == "+" or curr == "*" then
            next_char(ls)
            if ls.current == curr then
                next_char(ls)
                if ls.current == "=" then
                    next_char(ls)
                    yield(curr .. curr .. "=")
                else
                    yield(curr .. curr)
                end
            elseif ls.current == "=" then
                next_char(ls)
                yield(curr .. "=")
            else
                yield(curr)
            end
        -- -, -=, ->
        elseif curr == "-" then
            next_char(ls)
            if ls.current == ">" then
                next_char(ls)
                yield("->")
            elseif ls.current == "=" then
                next_char(ls)
                yield("-=")
            else
                yield("-")
            end
        -- /, /=, // (short comments), /* */ (long comments)
        elseif curr == "/" then
            next_char(ls)
            -- short comments
            if ls.current == "/" then
                -- skip until eol/eos
                while ls.current and not is_newline(ls.current) do
                    next_char(ls)
                end
            -- long comments
            elseif ls.current == "*" then
                next_char(ls)
                read_long_comment(ls)
            -- /=
            elseif ls.current == "=" then
                next_char(ls)
                yield("/=")
            -- /
            else
                yield("/")
            end
        -- ., .., ..., numbers
        elseif curr == "." then
            next_char(ls)
            -- ... or ..
            if ls.current == "." then
                next_char(ls)
                -- ...
                if ls.current == "." then
                    next_char(ls)
                    yield("...")
                -- ..
                else
                    yield("..")
                end
            -- .
            elseif not is_digit(ls.current) then
                yield(".")
            -- .num
            else
                read_number(ls, true)
            end
        -- numbers
        elseif is_digit(curr) then
            read_number(ls)
        -- : or ::
        elseif curr == ":" then
            next_char(ls)
            if ls.current == ":" then
                next_char(ls)
                yield("::")
            else
                yield(":")
            end
        -- $ or $(
        elseif curr == "$" then
            next_char(ls)
            if ls.current == "(" then
                next_char(ls)
                yield("$(")
            else
                if not is_ident(ls.current) then
                    lex_error(ls, "expected identifier", "$")
                end
                yield("$")
            end
        -- strings
        elseif curr == '"' or curr == "'" then
            read_string(ls)
        -- keywords, identifiers
        elseif is_ident(curr) then
            local buf = { curr }
            next_char(ls)

            local raw, exp = 0, 0
            if curr == "e" or curr == "E" then
                exp = exp + 1
            elseif curr == "r" or curr == "R" then
                raw = raw + 1
            end
            local strp = raw ~= 0 or exp ~= 0

            while ls.current and is_ident(ls.current) do
                if strp then
                    local curr = ls.current
                    if curr == "e" or curr == "E" then
                        exp = exp + 1
                    elseif curr == "r" or curr == "R" then
                        raw = raw + 1
                    else
                        raw, exp, strp = nil, nil, false
                    end
                end
                save_and_next_char(ls, buf)
            end

            local str = concat(buf)
            local kwop = kwops[str]
            -- a keyworded op
            if kwop == true or kwop == false then
                -- true kwops allow compound variants
                if kwop == true and ls.current == "=" then
                    next_char(ls)
                    yield(str .. "=")
                else
                    yield(str)
                end
            -- a keyword
            elseif keywords[str] then
                yield(str)
            -- a potential string
            elseif strp then
                local curr = ls.current
                if curr == '"' or curr == "'" then
                    if raw > 1 or exp > 1 then
                        lex_error(ls, "invalid string prefix", str)
                    end
                    read_string(ls, { raw = raw ~= 0, exp = exp ~= 0 })
                -- an identifier
                else
                    yield("<ident>", str)
                end
            -- an identifier
            else
                yield("<ident>", str)
            end
        -- any other singlechar token
        else
            next_char(ls)
            yield(curr)
        end
    end
    yield("<eos>")
end

local State_MT = {
    __index = {
        -- retrieves a new token
        -- uses previous lookahead if available
        get = function(self)
            self.last_line = self.line_number

            local tok = self.token
            local lah = self.ltoken

            if lah.name then
                tok.name, tok.value = lah.name, lah.value
                lah.name, lah.value = nil, nil
            else
                tok.name, tok.value = self:lex()
            end

            return tok.name
        end,

        -- retrieves a lookahead token when required
        lookahead = function(self)
            local lah = self.ltoken
            lah.name, lah.value = self:lex()
            return lah.name
        end
    }
}

local skip_bom = function(rdr)
    local c = rdr()
    if c ~= 0xEF then return c end
    c = rdr()
    if c ~= 0xBB then return c end
    c = rdr()
    if c ~= 0xBF then return c end
    return rdr()
end

local skip_shebang = function(rdr)
    local c = skip_bom(rdr)
    if c == "#" then
        repeat
            c = rdr()
        until c == "\n" or c == "\r" or not c
        local e = c
        c = rdr()
        if (e == "\n" and c == "\r") or (e == "\r" and c == "\n") then
            c = rdr()
        end
    end
    return c
end

return {
    -- sets up the lexer state
    init = function(fname, reader)
        return setmetatable({
            reader      = reader,   -- the input character stream, UTF-8 aware
            token       = {         -- the token, contains name and its
                name    = nil,      -- semantic value (i.e. a string literal)
                value   = nil
            },
            ltoken   = {            -- the lookahead token, used when required
                name    = nil,
                value   = nil 
            },
            source      = fname,    -- the source (a filename or stdin or w/e)
            current     = skip_shebang(reader), -- the current character
            line_number = 1,        -- the current line number
            last_line   = 1,        -- previous line number
            lex         = wrap(lex) -- the lexer coroutine used by this state
        }, State_MT)
    end,

    syntax_error = syntax_error
}
