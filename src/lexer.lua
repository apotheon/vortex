--[[ Vortex 0.1 lexer

 Author: q66 <quaker66@gmail.com>
 Inspired by Lua's lexer (most notably in string parsing)

 Available under the terms of the MIT license.
]]

local util = require("util")

local is_newline = util.is_newline
local is_white   = util.is_white
local is_ascii   = util.is_ascii
local is_ident   = util.is_ident
local is_keyword = util.is_keyword
local is_alnum   = util.is_alnum
local is_digit   = util.is_digit
local fatal      = util.fatal

-- all of the core vortex keywords
local keywords = {
    ["again"] = true, ["and"  ] = true, ["break"] = true, ["case"  ] = true,
    ["cfn"  ] = true, ["coro" ] = true, ["do"   ] = true, ["else"  ] = true,
    ["false"] = true, ["fn"   ] = true, ["for"  ] = true, ["glob"  ] = true,
    ["goto" ] = true, ["if"   ] = true, ["in"   ] = true, ["let"   ] = true,
    ["match"] = true, ["mod"  ] = true, ["nil"  ] = true, ["not"   ] = true,
    ["or"   ] = true, ["quote"] = true, ["rec"  ] = true, ["return"] = true,
    ["seq"  ] = true, ["true" ] = true, ["while"] = true, ["yield" ] = true,

    ["__FILE__"] = true, ["__LINE__"] = true
}

local lex_error = function(ls, msg, value)
    msg = ("%s:%d:%d: %s"):format(ls.source, ls.line_number, ls.line_pos, msg)
    if value then
        msg = msg .. " near '" .. value .. "'"
    end
    fatal(msg)
end

local syntax_error = function(ls, msg)
    lex_error(ls, msg, ls.token.value or ls.token.name)
end

local next_char = function(ls)
    ls.current  = ls.reader()
    ls.line_pos = ls.line_pos + 1
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
    ls.line_pos    = 0
end

local save_and_next_char = function(ls, buf)
    table.insert(buf, ls.current)
    next_char(ls)
end

local read_number = function(ls, token)
    local buf = { ls.current }
    next_char(ls)
    while is_alnum(ls.current) or ls.current == "." do
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

local skip_sep = function(ls, buf)
    local cnt  = 0
    local curr = ls.current
    assert(curr == "[" or curr == "]")

    save_and_next_char(ls, buf)

    while ls.current == "=" do
        save_and_next_char(ls, buf)
        cnt = cnt + 1
    end

    return ls.current == curr and cnt or -cnt - 1
end

local read_long_string = function(ls, nsep, buf)
    save_and_next_char(ls, buf)
    if is_newline(ls.current) then
        next_line(ls)
    end

    while true do
        local  curr = ls.current
        if not curr then
            lex_error(ls, "unfinished long string", "<eos>")
        elseif is_newline(curr) then
            table.insert(buf, "\n")
            next_line(ls)
        elseif curr == "]" then
            if skip_sep(ls, buf) == nsep then
                -- skip ]
                save_and_next_char(ls, buf)
                break
            end
        else
            save_and_next_char(ls, buf)
        end
    end
end

local read_string = function(ls, delim, buf)
    save_and_next_char(ls, buf)

    while ls.current ~= delim do
        local  curr = ls.current

        -- these errors have to be handled during lexing (line numbers etc.)
        -- other errors are handled by Lua
        if not curr then
            lex_error(ls, "unfinished string", "<eos>")
        -- newline without escape means error
        elseif is_newline(curr) then
            lex_error(ls, "unfinished string", table.concat(buf))
        -- escape sequences
        elseif curr == "\\" then
            -- save the \, it has to be in the source
            save_and_next_char(ls, buf)

            local curr = ls.current

            -- newlines - insert a real newline
            if is_newline(curr) then
                table.insert(buf, "\n")
                next_char(ls)
            -- EOS - error
            elseif not curr then
                lex_error(ls, "unfinished string", "<eos>")
            -- otherwise save - will be handled by Lua
            else
                save_and_next_char(ls, buf)
            end
        -- other characters are saved as usual
        else
            save_and_next_char(ls, buf)
        end
    end

    -- remove the ending delimiter
    save_and_next_char(ls, buf)
end

local lex = function(ls, token)
    token.value = nil
    while true do
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

        -- +, +=, ++
        elseif curr == "+" then
            next_char(ls)

            -- ++
            if ls.current == "+" then
                next_char(ls)
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
        -- *, *=, **
        elseif curr == "*" then
            next_char(ls)

            -- **
            if ls.current == "*" then
                next_char(ls)
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

        -- long strings or [
        elseif curr == "[" then
            local buf  = {}
            local nsep = skip_sep(ls, buf)
            if    nsep >= 0 then
                read_long_string(ls, nsep, buf)
                token.value = table.concat(buf)
                return "<string>"
            elseif nsep == -1 then
                return "["
            else
                lex_error(ls, "invalid long string delimiter",
                    table.concat(buf))
            end

        -- short strings
        elseif curr == '"' or curr == "'" then
            local buf  = {}
            read_string(ls, curr, buf)
            token.value = table.concat(buf)
            return "<string>"

        -- keywords, identifiers, single-char tokens
        else
            -- keyword or identifier
            if is_ident(curr) or is_keyword(curr) then
                local buf = { curr }
                next_char(ls)

                while is_ident(ls.current) or is_keyword(ls.current) do
                    save_and_next_char(ls, buf)
                end

                local str = table.concat(buf)
                if keywords[str] then
                    return  str
                else
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
            last_line   = 1,        -- previous line number
            line_pos    = 0         -- position on the line
        }, State_MT)
    end,

    syntax_error = syntax_error
}
