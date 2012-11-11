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

-- all of the core vortex keywords
local keywords = {
    ["and"   ] = true,
    ["as"    ] = true,
    ["break" ] = true,
    ["case"  ] = true,
    ["cfn"   ] = true,
    ["clone" ] = true,
    ["coro"  ] = true,
    ["cycle" ] = true,
    ["do"    ] = true,
    ["else"  ] = true,
    ["false" ] = true,
    ["fn"    ] = true,
    ["for"   ] = true,
    ["glob"  ] = true,
    ["goto"  ] = true,
    ["if"    ] = true,
    ["in"    ] = true,
    ["let"   ] = true,
    ["match" ] = true,
    ["mod"   ] = true,
    ["new"   ] = true,
    ["nil"   ] = true,
    ["not"   ] = true,
    ["or"    ] = true,
    ["quote" ] = true,
    ["rec"   ] = true,
    ["return"] = true,
    ["seq"   ] = true,
    ["true"  ] = true,
    ["when"  ] = true,
    ["while" ] = true,
    ["yield" ] = true,

    ["__FILE__"] = true,
    ["__LINE__"] = true
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

local read_number = function(ls, token)
    local buf = { ls.current }
    next_char(ls)
    while ls.current and (is_alnum(ls.current) or ls.current == ".") do
        save_and_next_char(ls, buf)
    end

    local str = table.concat(buf)
    if not tonumber(str) then
        lex_error(ls, "malformed number", str)
    end

    token.value = str
end

local read_long_comment

read_long_comment = function(ls)
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
                break
            end
        end

        next_char(ls)
    end
    -- here we should have the token right after the comment ready
    if not ls.current then
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

local cw, cy = coroutine.wrap, coroutine.yield
local readstr = function(ls, prefixes, delim, long)
    cy()
    local buf, levels, sp = {}, {}, prefixes or {}
    local raw, expand = sp.raw, sp.expand

    cy("<string>", nil)
    while true do
        local curr = ls.current
        if not long then
            if curr == delim then
                break
            end
        else
            if curr == delim then
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
                next_char(ls)
                while is_newline(ls.current) or is_white(ls.current) do
                    if is_newline(ls.current) then
                        next_line(ls)
                    else
                        next_char(ls)
                    end
                end
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
        elseif curr == "[" or curr == "]" then
            local c = curr
            save_and_next_char(ls, buf)
            local lev = 0
            while ls.current == "=" do
                lev = lev + 1
                save_and_next_char(ls, buf)
            end
            if ls.current == c then
                levels[lev] = true
            end
            save_and_next_char(ls, buf)
        elseif curr == "$" and expand then
            local str = concat(buf)
            buf = {}
            cy("<string>", str)
            cy("$", nil)
            local c = next_char(ls)
            local tok = {}
            if c == "(" then
                cy("(", nil)
                next_char(ls)
                repeat
                    local name = lex(ls, tok, true)
                    cy(name, tok.value)
                until ls.current == ")"
                cy(")", nil)
                next_char(ls)
            else
                local name = lex(ls, tok, true)
                if name == "<ident>" then
                    cy(name, tok.value)
                else
                    buf[#buf + 1] = "$" .. (tok.value or name)
                end
            end
        else
            save_and_next_char(ls, buf)
        end
    end
    next_char(ls)
    cy("<string>", concat(buf))
    return "<string>", levels
end

local read_string = function(ls, prefixes)
    local delim, long = ls.current
    if next_char(ls) == delim then
        if next_char(ls) == delim then
            next_char(ls)
            long = true
        end
    end
    local coro = cw(readstr)
    coro(ls, prefixes, delim, long)
    return coro
end

local str_readers = Stack()
lex = function(ls, token, instr)
    token.value = nil
    while true do
        if not instr then
            local rdr = str_readers:top()
            if rdr then
                local tok, val = rdr()
                -- terminating token
                if type(val) == "table" then
                    str_readers:pop()
                end
                token.value = val
                return tok
            end
        end
        local curr = ls.current

        -- end of stream
        if not curr then
            return "<eos>"
        end

        -- line breaks
        if is_newline(curr) then
            next_line(ls)
        -- spaces
        elseif is_white(curr) then
            next_char(ls)
        -- comments
        -- strings, long strings

        -- = or ==
        elseif curr == "=" then
            next_char(ls)
            if ls.current ~= "=" then return "=" end
            next_char(ls)
            return "=="
        -- > or >=, >>, >>= 
        elseif curr == ">" then
            next_char(ls)
            -- >>, >>=
            if ls.curent == ">" then
                next_char(ls)
                if ls.current == "=" then
                    next_char(ls)
                    return ">>="
                else
                    return ">>"
                end
            -- >=
            elseif ls.current == "=" then
                next_char(ls)
                return ">="
            -- >
            else
                return ">"
            end
        -- < , <=, <<, <<=
        elseif curr == "<" then
            next_char(ls)
            -- <<, <<=
            if ls.curent == "<" then
                next_char(ls)
                if ls.current == "=" then
                    next_char(ls)
                    return "<<="
                else
                    return "<<"
                end
            -- >=
            elseif ls.current == "=" then
                next_char(ls)
                return "<="
            -- >
            else
                return "<"
            end
        -- ! or !=
        elseif curr == "!" then
            next_char(ls)
            if ls.current ~= "=" then return "!" end
            next_char(ls)
            return "!="

        -- +, +=, ++, ++=
        elseif curr == "+" then
            next_char(ls)

            -- ++, ++=
            if ls.current == "+" then
                next_char(ls)
                if ls.current == "=" then
                    next_char(ls)
                    return "++="
                end
                return "++"
            -- +=
            elseif ls.current == "=" then
                next_char(ls)
                return "+="
            -- +
            else
                return "+"
            end
        -- -, -=, ->
        elseif curr == "-" then
            next_char(ls)

            -- ->
            if ls.current == ">" then
                next_char(ls)
                return "->"
            -- -=
            elseif ls.current == "=" then
                next_char(ls)
                return "-="
            -- -
            else
                return "-"
            end
        -- *, *=, **, **=
        elseif curr == "*" then
            next_char(ls)

            -- **, **=
            if ls.current == "*" then
                next_char(ls)
                if ls.current == "=" then
                    next_char(ls)
                    return "**="
                end
                return "**"
            -- *=
            elseif ls.current == "=" then
                next_char(ls)
                return "*="
            -- *
            else
                return "*"
            end
        -- %, %=
        elseif curr == "%" then
            next_char(ls)
            if ls.current ~= "=" then return "%" end
            next_char(ls)
            return "%="

        -- &, &=
        elseif curr == "&" then
            next_char(ls)
            if ls.current ~= "=" then return "&" end
            next_char(ls)
            return "&="
        -- |, |=
        elseif curr == "|" then
            next_char(ls)
            if ls.current ~= "=" then return "|" end
            next_char(ls)
            return "|="
        -- ^, ^=
        elseif curr == "^" then
            next_char(ls)
            if ls.current ~= "=" then return "^" end
            next_char(ls)
            return "^="

        -- /, /=, // (short comments), /* */ (long comments)
        elseif curr == "/" then
            next_char(ls)

            -- short comments
            if ls.current == "/" then
                -- skip until EOL, EOS
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
                return "/="
            -- /
            else
                return "/"
            end

        -- ., .., ..., numbers
        elseif curr == "." then
            local buf = { curr }
            next_char(ls)
            if ls.current == "." then
                next_char(ls)
                if ls.current == "." then
                    next_char(ls)
                    return "..."
                end
                return ".."
            elseif not is_digit(ls.current) then
                return "."
            else
                read_number(ls, token)
                return "<number>"
            end

        -- numbers
        elseif is_digit(curr) then
            read_number(ls, token)
            return "<number>"

        -- : or ::
        elseif curr == ":" then
            next_char(ls)
            if ls.current ~= ":" then return ":" end
            next_char(ls)
            return "::"

        -- strings
        elseif curr == '"' or curr == "'" then
            local rdr = str_readers:push(read_string(ls))
            -- the "start" token
            return rdr()

        -- keywords, identifiers, single-char tokens
        else
            -- keyword or identifier
            if is_ident(curr) then
                local buf = { curr }
                next_char(ls)

                local raw, expand = 0, 0
                if curr == "e" or curr == "E" then
                    expand = expand + 1
                elseif curr == "r" or curr == "R" then
                    raw = raw + 1
                end
                local strp = raw ~= 0 or expand ~= 0

                while ls.current and is_ident(ls.current) do
                    if strp then
                        local curr = ls.current
                        if curr == "e" or curr == "E" then
                            expand = expand + 1
                        elseif curr == "r" or curr == "R" then
                            raw = raw + 1
                        else
                            raw, expand, strp = nil, nil, false
                        end
                    end
                    save_and_next_char(ls, buf)
                end

                local str = table.concat(buf)
                if keywords[str] then
                    return  str
                else
                    if strp then
                        local curr = ls.current
                        if curr == '"' or curr == "'" then
                            if raw > 1 or expand > 1 then
                                lex_error(ls, "invalid string prefix", str)
                            end

                            local rdr = str_readers:push(read_string(ls, {
                                raw = raw ~= 0, expand = expand ~= 0
                            }))
                            -- the "start" token
                            return rdr()
                        end
                    end
                    token.value = str
                    return "<ident>"
                end
            -- other single char tokens
            else
                next_char(ls)
                return curr
            end
        end
    end
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
                tok.name = lex(self, tok)
            end

            return tok.name
        end,

        -- retrieves a lookahead token when required
        lookahead = function(self)
            local lah = self.ltoken
            lah.name = lex(self, lah)
            return lah.name
        end
    }
}

return {
    -- sets up the lexer state
    init = function(fname, reader)
        return setmetatable({
            reader      = reader,   -- the input character stream, UTF-8 aware
            token       = {         -- the token, contains name and its
                name    = nil,      -- semantic value (i.e. a string literal)
                value   = nil
            },
            ltoken   = {         -- the lookahead token, used when required
                name    = nil,
                value   = nil 
            },
            source      = fname,    -- the source (a filename or stdin or w/e)
            current     = reader(), -- the current character (from reader)
            line_number = 1,        -- the current line number
            last_line   = 1         -- previous line number
        }, State_MT)
    end,

    syntax_error = syntax_error
}
