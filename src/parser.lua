local lexer   = require("lexer")
local util    = require("util")

-- t[1] > t[2] == right-associative, otherwise left-associative
local Binary_Ops = {
    -- all the assignment operators have the same, lowest precedence
    ["="  ] = { 2,  1  }, ["+=" ] = { 2,  1  }, ["-=" ] = { 2,  1  },
    ["*=" ] = { 2,  1  }, ["/=" ] = { 2,  1  }, ["%=" ] = { 2,  1  },
    ["^=" ] = { 2,  1  }, ["&=" ] = { 2,  1  }, ["|=" ] = { 2,  1  },
    ["<<="] = { 2,  1  }, [">>="] = { 2,  1  },

    -- followed by logical operators or, and
    ["or" ] = { 3,  3  }, ["and"] = { 4,  4  },

    -- eq / neq comparison
    ["==" ] = { 5,  5  }, ["!=" ] = { 5,  5  },

    -- other comparisons
    ["<"  ] = { 6,  6  }, ["<=" ] = { 6,  6  }, [">"  ] = { 6,  6 },
    [">=" ] = { 6,  6  },

    -- concat
    [".." ] = { 8,  7  },

    -- bitwise ops
    ["|"  ] = { 9,  9  }, ["^"  ] = { 10, 10 }, ["&"  ] = { 11, 11 },
    [">>" ] = { 12, 12 }, ["<<" ] = { 12, 12 },

    -- arithmetic ops
    ["+"  ] = { 13, 13 }, ["-"  ] = { 13, 13 }, ["*"  ] = { 14, 14 },
    ["/"  ] = { 14, 14 }, ["%"  ] = { 14, 14 },

    -- join and cons have the same precedence
    ["++" ] = { 15, 15 }, ["::" ] = { 15, 15 },

    -- unary ops come now, but are in their own table
    -- and the last one - pow
    ["**" ] = { 18, 17 }
}

local Unary_Ops = {
    ["-"  ] = 16, ["not"] = 16, ["#"  ] = 16, ["~"  ] = 16
}

local Ass_Ops = {
    ["="  ] = true, ["+=" ] = true, ["-=" ] = true, ["*=" ] = true,
    ["/=" ] = true, ["%=" ] = true, ["^=" ] = true, ["&=" ] = true,
    ["|=" ] = true, ["<<="] = true, [">>="] = true
}

local syntax_error = lexer.syntax_error
local case_class = util.case_class
local unique_sym = util.unique_sym
local hash_sym = util.hash_sym
local map = util.map

local assert_tok = function(ls, tok, allow)
    local n = ls.token.name
    if n ~= tok and (allow == nil or n ~= allow) then
        syntax_error(ls, "unexpected symbol")
    end
end

local concat = table.concat

local Scope = util.Object:clone {
    __init = function(self, fs, indent)
        self.body, self.fstate, self.indent, self.locked
            = {}, fs, indent, false
    end,

    push = function(self, stat)
        if self.locked then return nil end
        local body = self.body
        body[#body + 1] = stat
    end,

    lock = function(self)
        self.locked = true
    end,

    build = function(self)
        local ind = self.indent * META.cgen.indent
        return (" "):rep(ind) .. concat(self.body, "\n" .. (" "):rep(ind))
    end
}

local Function_State = Scope:clone {
}

local gen_local = function(names, vals, ltype)
    if ltype == "glob" then
        return concat { names, " = ", vals or "nil" }
    elseif not vals then
        return concat { "local ", names }
    elseif ltype == "rec" then
        return concat { "local ", names, "; ", names, " = ", vals }
    else
        return concat { "local ", names, " = ", vals }
    end
end

local gen_ass = function(names, vals)
    return concat { names, " = ", vals }
end

local gen_binexpr = function(op, lhs, rhs)
    return concat { lhs, " ", op, " ", rhs }
end

local gen_unexpr = function(op, rhs)
    return concat { op, " ", rhs }
end

local gen_seq = function(lst)
    return concat(lst, ", ")
end

local gen_if = function(cond, tsc, fsc)
    local rt = { "if ", cond, " then\n", tsc:build() }
    if fsc then
        rt[#rt + 1] = "\n" .. (" "):rep((tsc.indent - 1) * META.cgen.indent)
        rt[#rt + 1] = "else\n" .. fsc:build()
    end
    rt[#rt + 1] = "\n" .. (" "):rep((tsc.indent - 1) * META.cgen.indent)
    return concat(rt) .. "end"
end

local gen_while = function(cond, body)
    return concat { "while ", cond, " do\n", body:build(), "\n",
        (" "):rep((body.indent - 1) * META.cgen.indent) } .. "end"
end

local gen_block = function(sc)
    return concat { "do\n", sc:build(), "\n", (" "):rep(
        (sc.indent - 1) * META.cgen.indent) } .. "end"
end

local gen_fun = function(params, body)
    return concat { "function(", params, ")\n", body:build(), "\n",
        (" "):rep((body.indent - 1) * META.cgen.indent), "end" }
end

local gen_ret = function(vals)
    return concat { "return ", vals }
end

local gen_call = function(expr, params)
    return concat { expr, "(", params, ")" }
end

local gen_indent = function(lvl)
    return (" "):rep(lvl * META.cgen.indent)
end

-- classes

local Expr = util.Object:clone {
    name = "Expr",

    is_lvalue = function(self)
        return false
    end,

    is_scoped = function(self)
        return false
    end,

    generate = function(self, sc, kwargs)
    end,

    to_lua = function(self, i)
        return "Expr()"
    end
}

local Call_Expr

local Symbol_Expr = Expr:clone {
    name = "Symbol_Expr",

    __init = function(self, sym)
        self.symbol = sym
    end,

    generate = function(self, sc, kwargs)
        return self.symbol
    end,

    is_lvalue = function(self)
        return true
    end,

    to_lua = function(self, i)
        return ("Symbol_Expr(%q)"):format(self.symbol)
    end
}

local Value_Expr = Expr:clone {
    name = "Value_Expr",

    __init = function(self, val)
        self.value = val
    end,

    generate = function(self, sc, kwargs)
        return self.value
    end,

    to_lua = function(self, i)
        return ("Value_Expr(%s)"):format(self.value)
    end
}

local Return_Expr = Expr:clone {
    name = "Return_Expr",

    __init = function(self, exprs)
        self.exprs = exprs
    end,

    generate = function(self, sc, kwargs)
        local exprs = self.exprs
        local len   = #exprs

        local exps = {}
        for i = 1, len do
            exps[#exps + 1] = exprs[i]:generate(sc, {
                no_local = (i == len)
            })
        end
        sc:push(gen_ret(gen_seq(exps)))
        sc:lock()
    end,

    to_lua = function(self, i)
        local exprs
        if #self.exprs == 0 then
            exprs = "{}"
        else
            local exprt = map(self.exprs, function(expr)
                return expr:to_lua(i + 1) end)
            exprs = "{\n" .. gen_indent(i + 1)
                .. concat(exprt, ",\n" .. gen_indent(i + 1))
                .. "\n" .. gen_indent(i) .. "}"
        end
        return ("Return_Expr(%s)"):format(exprs)
    end
}

local Block_Expr = Expr:clone {
    name = "Block_Expr",

    __init = function(self, exprs)
        self.exprs = exprs
    end,

    generate = function(self, sc, kwargs)
        local exprs = self.exprs
        local len   = #exprs

        local scope = (kwargs.statement or not kwargs.no_scope) and
            Scope(sc.fstate, sc.indent + 1) or sc

        for i = 1, len - 1 do
            scope:push(exprs[i]:generate(scope, {
                statement = true
            }))
        end

        if kwargs.return_val and exprs[len]:is_scoped() then
            sc:push(exprs[len]:generate(sc, {
                return_val = true
            }))
        elseif kwargs.no_scope then
            if sc:is_a(Function_State) or kwargs.return_val then
                sc:push(gen_ret(exprs[len]:generate(sc, {
                    no_local = true
                })))
            else
                return exprs[len]:generate(sc, {
                    no_local = true
                })
            end
        elseif kwargs.statement then
            scope:push(exprs[len]:generate(scope, {
                statement = true
            }))
            sc:push(gen_block(scope))
        else
            local sym = unique_sym("block")
            sc:push(gen_local(sym))
            scope:push(gen_ass(sym, exprs[len]:generate(scope, {
                no_local = true
            })))
            sc:push(gen_block(scope))
            return sym
        end
    end,

    is_scoped = function(self)
        return true
    end,

    to_lua = function(self, i)
        local exprs
        if #self.exprs == 0 then
            exprs = "{}"
        else
            local exprt = map(self.exprs, function(expr)
                return expr:to_lua(i + 1) end)
            exprs = "{\n" .. gen_indent(i + 1)
                .. concat(exprt, ",\n" .. gen_indent(i + 1))
                .. "\n" .. gen_indent(i) .. "}"
        end
        return ("Block_Expr(%s)"):format(exprs)
    end
}

local Binary_Expr
Binary_Expr = Expr:clone {
    name = "Binary_Expr",

    __init = function(self, op, lhs, rhs)
        self.op, self.lhs, self.rhs = op, lhs, rhs
    end,

    generate = function(self, sc, kwargs)
        local lhs = self.lhs:generate(sc, {})

        local op = self.op
        if Ass_Ops[op] then
            if op == "=" then
                sc:push(gen_ass(lhs, rhs))
            else
                sc:push(gen_ass(lhs, Binary_Expr(op:sub(1, #op - 1),
                    self.lhs, self.rhs):generate(sc, { no_local = true })))
            end
            return lhs
        end

        local rhs = self.rhs:generate(sc, {})

        if kwargs.no_local then
            return gen_binexpr(self.op, lhs, rhs)
        end

        local sym = unique_sym(self.op)
        sc:push(gen_local(sym, gen_binexpr(self.op, lhs, rhs)))
        return sym
    end,

    is_lvalue = function(self)
        if Ass_Ops[self.op] then
            return true
        end
        return false
    end,

    to_lua = function(self, i)
        return ("Binary_Expr(%q, %s, %s)"):format(self.op,
            self.lhs:to_lua(i + 1), self.rhs:to_lua(i + 1))
    end
}

local Unary_Expr = Expr:clone {
    name = "Unary_Expr",

    __init = function(self, op, lhs)
        self.op, self.rhs = op, rhs
    end,

    generate = function(self, sc, kwargs)
        local rhs = self.rhs:generate(sc, {})

        if kwargs.no_local then
            return gen_unexpr(self.op, rhs)
        end

        local sym = unique_sym(self.op)
        sc:push(gen_local(sym, gen_unexpr(self.op, rhs)))
        return sym
    end,

    to_lua = function(self, i)
        return ("Unary_Expr(%q, %s)"):format(self.op,
            self.rhs:to_lua(i + 1))
    end
}

local Let_Expr = Expr:clone {
    name = "Let_Expr",

    __init = function(self, ltype, idents, assign)
        self.type, self.idents, self.assign = ltype, idents, assign
    end,

    generate = function(self, sc, kwargs)
        local syms = {}
        local len = #self.assign

        for i = 1, len - 1 do
            local nd = self.assign[i]
            syms[#syms + 1] = nd:generate(sc, {})
        end

        syms[#syms + 1] = self.assign[len]:generate(sc, {
            no_local = true
        })

        local idents = gen_seq(self.idents)
        syms = gen_seq(syms)

        sc:push(gen_local(idents, syms, self.type))

        if not kwargs.statement then
            return idents
        end
    end,

    to_lua = function(self, i)
        local idents = #self.idents == 0 and "{}"
            or "{ " .. concat(map(self.idents,
                function(n) return '"' .. n .. '"' end), ", ") .. " }"

        local assign
        if #self.assign == 0 then
            assign = "{}"
        else
            local exprs = map(self.assign, function(expr)
                return expr:to_lua(i + 1) end)
            assign = "{\n" .. gen_indent(i + 1)
                .. concat(exprs, ",\n" .. gen_indent(i + 1))
                .. "\n" .. gen_indent(i) .. "}"
        end
        return ("Let_Expr(%q, %s, %s)"):format(self.type, idents, assign)
    end
}

local Function_Expr = Expr:clone {
    name = "Function_Expr",

    __init = function(self, params, defaults, body)
        self.params, self.defaults, self.body = params, defaults, body
    end,

    generate = function(self, sc, kwargs)
        local fs = Function_State(sc.fstate, sc.indent + 1)

        local args,  defs  = self.params, self.defaults
        local nargs, ndefs = #args, #defs

        if args[#args] == "..." then
            nargs = nargs - 1
        end
        local pargs = nargs - ndefs

        local np = {}
        for i = 1, pargs do
            np[#np + 1] = args[i]
        end

        if (ndefs > 0 or args[#args] == "...") then
            np[#np + 1] = "..."
        end

        if ndefs > 0 then
            local nd = unique_sym("#")
            fs:push(gen_local(nd, "select('#', ...)"))

            for i = 1, ndefs do
                local name = args[pargs + i]
                fs:push(gen_local(name))

                local tsc = Scope(fs, fs.indent + 1)
                tsc:push(gen_ass(name, defs[i]:generate(tsc, {
                    no_local = true
                })))

                local fsc = Scope(fs, fs.indent + 1)
                fsc:push(gen_ass(name, "select(" .. i .. ", ...)"))

                fs:push(gen_if(gen_binexpr("<", nd, i), tsc, fsc))
            end
        end

        -- avoid temps
        local body = self.body
        if body:is_a(Block_Expr) then
            body:generate(fs, {
                no_scope = true,
                return_val = true
            })
        else
            fs:push(gen_ret(body:generate(fs, {
                no_local = true,
                return_val = true
            })))
        end
        return gen_fun(gen_seq(np), fs)
    end,

    is_scoped = function(self)
        return true
    end,

    to_lua = function(self, i)
        local params = #self.params == 0 and "{}"
            or "{ " .. concat(map(self.params,
                function(n) return '"' .. n .. '"' end), ", ") .. " }"

        local defaults
        if #self.defaults == 0 then
            defaults = "{}"
        else
            local exprs = map(self.defaults, function(expr)
                return expr:to_lua(i + 1) end)
            defaults = "{\n" .. gen_indent(i + 1)
                .. concat(exprs, ",\n" .. gen_indent(i + 1))
                .. "\n" .. gen_indent(i) .. "}"
        end
        return ("Function_Expr(%s, %s, %s)"):format(params, defaults,
            self.body:to_lua(i + 1))
    end
}

local If_Expr = Expr:clone {
    name = "If_Expr",

    __init = function(self, cond, tval, fval)
        self.cond, self.tval, self.fval = cond, tval, fval
    end,

    generate = function(self, sc, kwargs)
        local stat, rval, tsc, fsc, tval, fval, tscoped, fscoped
            = kwargs.statement, kwargs.return_val

        tscoped = self.tval:is_scoped()
        tsc  = Scope(sc.fstate, sc.indent + 1)
        tval = self.tval:generate(tsc, {
            no_scope  = true,
            no_local  = true,
            statement = stat,
            return_val = rval
        })

        if self.fval then
            fscoped = self.fval:is_scoped()
            fsc  = Scope(sc.fstate, sc.indent + 1)
            fval = self.fval:generate(fsc, {
                no_scope  = true,
                no_local  = true,
                statement = stat,
                return_val = rval
            })
        end

        if stat or rval then
            tsc:push((rval and not tscoped) and gen_ret(tval) or tval)
            if fsc then fsc:push((rval and not fscoped)
                and gen_ret(fval) or fval) end

            sc:push(gen_if(self.cond:generate(sc, {
                no_local = true
            }), tsc, fsc))
        else
            local sym = unique_sym("if")

            tsc:push(gen_ass(sym, tval))
            if fsc then fsc:push(gen_ass(sym, fval)) end

            sc:push(gen_local(sym))
            sc:push(gen_if(self.cond:generate(sc, {
                no_local = true
            }), tsc, fsc))

            return sym
        end
    end,

    is_scoped = function(self)
        return true
    end,

    to_lua = function(self, i)
        return ("If_Expr(%s, %s, %s)"):format(
            self.cond:to_lua(i + 1), self.tval:to_lua(i + 1),
            self.fval:to_lua(i + 1))
    end
}

local While_Expr = Expr:clone {
    name = "While_Expr",

    __init = function(self, cond, body)
        self.cond, self.body = cond, body
    end,

    generate = function(self, sc, kwargs)
        local bsc = Scope(sc.fstate, sc.indent + 1)

        self.body:generate(bsc, {
            statement = true,
            no_scope  = true
        })

        sc:push(gen_while(self.cond:generate(sc, {
            no_local = true
        }), bsc))
    end,

    is_scoped = function(self)
        return true
    end,

    to_lua = function(self, i)
        return ("While_Expr(%s, %s)"):format(
            self.cond:to_lua(i + 1), self.body:to_lua(i + 1))
    end
}

local Seq_Expr = Expr:clone {
    name = "Seq_Expr",

    __init = function(self, expr)
        self.expr = expr
    end,

    generate = function(self, sc, kwargs)
    end,

    to_lua = function(self, i)
        return ("Seq_Expr(%s)"):format(self.expr:to_lua(i + 1))
    end
}

local Quote_Expr = Expr:clone {
    name = "Quote_Expr",

    __init = function(self, expr)
        self.expr = expr
    end,

    generate = function(self, sc, kwargs)
        return self.expr:to_lua(sc.indent)
    end,

    to_lua = function(self, i)
        return ("Quote_Expr(%s)"):format(self.expr:to_lua(i + 1))
    end
}

Call_Expr = Expr:clone {
    name = "Call_Expr",

    __init = function(self, expr, params)
        self.expr, self.params = expr, params
    end,

    generate = function(self, sc, kwargs)
        local syms = {}
        local len  = #self.params

        for i = 1, len do
            local par = self.params[i]
            syms[#syms + 1] = par:generate(sc, {
                no_local = par:is_a(Call_Expr)
            })
        end

        syms = gen_seq(syms)

        local expr = self.expr:generate(sc, {
            no_local = true
        })

        if kwargs.statement then
            sc:push(gen_call(expr, syms))
        elseif kwargs.no_local then
            return gen_call(expr, syms)
        else
            local sym = unique_sym("call")
            sc:push(gen_local(sym, gen_call(expr, syms)))
            return sym
        end
    end,

    to_lua = function(self, i)
        local params
        if #self.params == 0 then
            params = "{}"
        else
            local exprs = map(self.params, function(expr)
                return expr:to_lua(i + 1) end)
            params = "{\n" .. gen_indent(i + 1)
                .. concat(exprs, ",\n" .. gen_indent(i + 1))
                .. "\n" .. gen_indent(i) .. "}"
        end
        return ("Call_Expr(%s, %s)"):format(self.expr:to_lua(i + 1), params)
    end
}

local parse_binexpr
local parse_expr
local parse_exprlist

local parse_prefixexpr = function(ls)
    local tok = ls.token.name
    if tok == "<ident>" then
        local v = ls.token.value
        ls:get()
        return Symbol_Expr(v)
    else
        syntax_error(ls, "unexpected symbol")
    end
end

local parse_subexpr = function(ls)
    local tok = ls.token.name
    if tok == "(" then
        ls:get()
        local v = parse_expr(ls)
        if ls.token.name ~= ")" then syntax_error(ls, "missing ')'") end
        ls:get()
        return v
    elseif not tok or Binary_Ops[tok] then
        syntax_error(ls, "unexpected symbol")
    elseif Unary_Ops[tok] then
        ls:get()
        return Unary_Expr(tok, parse_binexpr(ls, Unary_Ops[tok]))
    elseif tok == "<number>" or tok == "<string>" then
        local v = ls.token.value
        ls:get()
        return Value_Expr(v)
    elseif tok == "nil" or tok == "true" or tok == "false" then
        ls:get()
        return Value_Expr(tok)
    -- handle calls here; we parse prefix expressions, which can always
    -- be called when it comes to syntax (semantically that's a different
    -- case)
    else
        local v = parse_prefixexpr(ls)
        if ls.token.name == "(" then
            ls:get()
            if ls.token.name == ")" then
                ls:get()
                return Call_Expr(v, {})
            end
            local list = parse_exprlist(ls)
            assert_tok(ls, ")")
            ls:get()
            return Call_Expr(v, list)
        end
        return v
    end
end

parse_binexpr = function(ls, mp)
    mp = mp or 1
    local lhs = parse_subexpr(ls)
    while true do
        local cur = ls.token.name

        local t = Binary_Ops[cur]
        if not cur or not t or t[1] < mp then break end

        local op, p1, p2 = cur, t[1], t[2]

        if lhs and (Ass_Ops[op] and not lhs:is_lvalue()) then
            syntax_error(ls, "expected lvalue")
        end

        ls:get()
        local rhs = parse_binexpr(ls, p1 > p2 and p1 or p2)

        lhs = Binary_Expr(op, lhs, rhs)
    end
    return lhs
end

local parse_identlist = function(ls)
    local tok, ids = ls.token, {}
    repeat
        assert_tok(ls, "<ident>")
        ids[#ids + 1] = tok.value
    until ls:get() ~= "," or ls:get() ~= "<ident>"
    return ids
end

parse_exprlist = function(ls)
    local tok, exprs = ls.token, {}
    repeat
        exprs[#exprs + 1] = parse_expr(ls)
    until tok.name ~= "," or not ls:get()
    return exprs
end

local endargs = { ["<ident>"] = true, ["..."] = true }
local parse_arglist = function(ls)
    local tok = ls.token
    local tn  = tok.name

    if tn == "->" or tn == "{" then
        return {}, {}
    end

    if tn == "..." then
        return { "..." }, {}
    end

    local ids, defs = {}
    repeat
        if tok.name == "..." then
            ids[#ids + 1] = "..."
            ls:get()
            break
        end

        assert_tok(ls, "<ident>")
        ids[#ids + 1] = tok.value

        if ls:lookahead() == "=" then
            ls:get() ls:get()
            if not defs then defs = {} end
            defs[#defs + 1] = parse_expr(ls)
        elseif defs then
            ls:get()
            assert_tok(ls, "=")
        else
            ls:get()
        end
    until tok.name ~= "," or not endargs[ls:get()]

    if not defs then defs = {} end
    return ids, defs
end

local parse_block = function(ls)
    ls:get()
    local tok, exprs = ls.token, {}

    if tok.name == "}" then
        ls:get()
        return Block_Expr(exprs)
    end

    repeat
        exprs[#exprs + 1] = parse_expr(ls)
    until tok.name == "}"

    assert_tok(ls, "}")
    ls:get()
    return Block_Expr(exprs)
end

local parse_function = function(ls)
    ls:get()
    local ids, defs = parse_arglist(ls)

    if ls.token.name == "{" then
        return Function_Expr(ids, defs, parse_block(ls))
    end

    assert_tok(ls, "->")
    ls:get()

    return Function_Expr(ids, defs, parse_expr(ls))
end

local parse_sequence = function(ls)
    ls:get()

    if ls.token.name == "{" then
        return Seq_Expr(parse_block(ls))
    end

    assert_tok(ls, "->")
    ls:get()

    return Seq_Expr(parse_expr(ls))
end

local parse_quote = function(ls)
    ls:get()

    if ls.token.name == "{" then
        return Quote_Expr(parse_block(ls))
    end

    assert_tok(ls, "->")
    ls:get()

    return Quote_Expr(parse_expr(ls))
end

local parse_if = function(ls)
    ls:get()
    local cond = parse_expr(ls)
    local tok  = ls.token

    local tval
    if tok.name == "{" then
        tval = parse_block(ls)
    else
        assert_tok(ls, "->")
        ls:get()
        tval = parse_expr(ls)
    end

    if tok.name == "else" then
        ls:get()
        if tok.name == "{" then
            return If_Expr(cond, tval, parse_block(ls))
        else
            if tok.name == "->" then
                ls:get()
            end
            return If_Expr(cond, tval, parse_expr(ls))
        end
    end

    return If_Expr(cond, tval, nil)
end

local parse_while = function(ls)
    ls:get()
    local cond = parse_expr(ls)
    local tok  = ls.token

    local body
    if tok.name == "{" then
        body = parse_block(ls)
    else
        assert_tok(ls, "->")
        ls:get()
        body = parse_expr(ls)
    end

    return While_Expr(cond, body)
end

local parse_return = function(ls)
    ls:get()
    if ls.token.name == "(" then
        ls:get()
        local exprs = parse_exprlist(ls)
        assert_tok(ls, ")")
        ls:get()
        return Return_Expr(exprs)
    end
    return Return_Expr({ parse_expr(ls) })
end

-- main expression parsing function
parse_expr = function(ls)
    local tok  = ls.token
    local name = tok.name

    if name == "fn" then
        return parse_function(ls)
    elseif name == "let" then
        ls:get()

        local ltype
        if tok.name == "rec" or tok.name == "glob" then
            ltype = tok.name
            ls:get()
        end

        if tok.name ~= "<ident>" then
            syntax_error(ls, "unexpected symbol")
        end

        local ids = parse_identlist(ls)
        local exprs
        if tok.name == "=" then
            ls:get()
            exprs = parse_exprlist(ls)
        end

        return Let_Expr(ltype or "default", ids, exprs)
    elseif name == "seq" then
        return parse_sequence(ls)
    elseif name == "quote" then
        return parse_quote(ls)
    elseif name == "if" then
        return parse_if(ls)
    elseif name == "while" then
        return parse_while(ls)
    elseif name == "return" then
        return parse_return(ls)
    elseif name == "{" then
        return parse_block(ls)
    elseif name == "__FILE__" then
        ls:get()
        return Value_Expr('"' .. ls.source .. '"')
    elseif name == "__LINE__" then
        ls:get()
        return Value_Expr(ls.line_number)
    else
        return parse_binexpr(ls)
    end
end

return {
    parse = function(fname, reader, writer)
        -- init the lexer
        local ls = lexer.init(fname, reader)

        -- first token
        ls:get()

        -- parse it recursively
        local ast = {}
        while ls.token.name ~= "<eos>" do
            ast[#ast + 1] = parse_expr(ls)
        end

        util.randomseed(os.clock() * os.time())

        local ms = Scope(nil, 0)
        -- generate the code
        for i = 1, #ast do
            ast[i]:generate(ms, {
                statement = true
            })
        end
        local str = ms:build()

        local f = io.open(fname, "r")
        print("-- input -- ")
        print(f:read("*all"))
        f:close()

        print("-- output --")
        print(str)
        print("\n-- test --")
        local f, err = loadstring(str)
        if not f then
            print("ERROR: " .. err)
        else
            f()
        end

        --print("------ Serialized AST ------")
        --print(util.serialize(ast, true))
    end
}
