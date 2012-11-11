local lexer   = require("lexer")
local util    = require("util")

-- t[1] > t[2] == right-associative, otherwise left-associative
local Binary_Ops = {
    -- all the assignment operators have the same, lowest precedence
    ["="  ] = { 2,  1  }, ["+=" ] = { 2,  1  }, ["-=" ] = { 2,  1  },
    ["*=" ] = { 2,  1  }, ["/=" ] = { 2,  1  }, ["%=" ] = { 2,  1  },
    --["^=" ] = { 2,  1  }, ["&=" ] = { 2,  1  }, ["|=" ] = { 2,  1  },
    --["<<="] = { 2,  1  }, [">>="] = { 2,  1  },
    ["++="] = { 2,  1  }, ["::="] = { 2,  1  }, ["**="] = { 2,  1  },

    -- followed by logical operators or, and
    ["or" ] = { 3,  3  }, ["and"] = { 4,  4  },

    -- eq / neq comparison
    ["==" ] = { 5,  5  }, ["!=" ] = { 5,  5  },

    -- other comparisons
    ["<"  ] = { 6,  6  }, ["<=" ] = { 6,  6  }, [">"  ] = { 6,  6 },
    [">=" ] = { 6,  6  },

    -- concat
    ["~"  ] = { 8,  7  },

    -- bitwise ops
    --["|"  ] = { 9,  9  }, ["^"  ] = { 10, 10 }, ["&"  ] = { 11, 11 },
    --[">>" ] = { 12, 12 }, ["<<" ] = { 12, 12 },

    -- arithmetic ops
    ["+"  ] = { 13, 13 }, ["-"  ] = { 13, 13 }, ["*"  ] = { 14, 14 },
    ["/"  ] = { 14, 14 }, ["%"  ] = { 14, 14 },

    -- join is left associative, cons is right associative
    ["++" ] = { 15, 15 }, ["::" ] = { 16, 15 },

    -- unary ops come now, but are in their own table
    -- and the last one - pow
    ["**" ] = { 19, 18 }
}

local Unary_Ops = {
    ["-"  ] = 17, ["not"] = 17, ["#"  ] = 17, ["~"  ] = 17
}

local Ass_Ops = {
    ["="  ] = true, ["+=" ] = true, ["-=" ] = true, ["*=" ] = true,
    ["/=" ] = true, ["%=" ] = true, ["^=" ] = true, ["&=" ] = true,
    ["|=" ] = true, ["<<="] = true, [">>="] = true
}

local syntax_error = lexer.syntax_error
local case_class = util.case_class
local unique_sym = util.unique_sym
local get_syms = util.get_syms
local hash_sym = util.hash_sym
local map = util.map
local Stack = util.Stack

local get_rt_fun

local assert_tok = function(ls, ...)
    local n = ls.token.name
    for i = 1, select("#", ...) do
        if select(i, ...) == n then return nil end
    end
    syntax_error(ls, "unexpected symbol")
end

local assert_check = function(ls, cond, msg)
    if not cond then
        syntax_error(ls, msg)
    end
end

local push_curline = function(ls)
    ls.ndstack:push({ first_line = ls.line_number })
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

    merge = function(self, sc)
        if self.locked then return nil end
        local bd1, bd2 = self.body, sc.body
        local len1, len2 = #bd1, #bd2
        for i = 1, len2 do
            bd1[len1 + i] = bd2[i]
        end
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
    __init = function(self, indent)
        Scope.__init(self, self, indent)
    end
}

local gen_str = function(str, level)
    local ind = ("="):rep(level or 0)
    return concat { "([", ind, "[", str, "]", ind, "])" }
end

local gen_local = function(names, vals)
    if not vals then
        return concat { "local ", names }
    else
        return concat { "local ", names, " = ", vals }
    end
end

local gen_ass = function(names, vals)
    return concat { names, " = ", vals }
end

local gen_binexpr = function(op, lhs, rhs)
    if op == "~" then
        op = ".."
    elseif op == "++" then
        return concat { get_rt_fun("tbl_join"), "(", lhs, ", ", rhs, ")" }
    elseif op == "::" then
        return concat { get_rt_fun("list_cons"), "(", lhs, ", ", rhs, ")" }
    elseif op == "!=" then
        op = "~="
    end
    return concat { "(", lhs, " ", op, " ", rhs, ")" }
end

local gen_unexpr = function(op, rhs)
    return concat { "(", op, " ", rhs, ")" }
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

local gen_require = function(name)
    return concat { 'require("', name, '")' }
end

local gen_call = function(expr, params)
    return concat { expr, "(", params, ")" }
end

local gen_index = function(sym, idx)
    return concat { sym, "[", idx, "]" }
end

local gen_table = function(sc, t)
    local ind = (" "):rep((sc.indent + 1) * META.cgen.indent)
    local tbl = { "{\n" }

    local len = #t
    for i = 1, len do
        local e = i % 4
        if e == 1 then
            tbl[#tbl + 1] = ind
        end

        tbl[#tbl + 1] = t[i]
        if i ~= len then tbl[#tbl + 1] = "," end
        if e == 0 and i ~= len then
            tbl[#tbl + 1] = "\n"
        elseif i ~= len then
            tbl[#tbl + 1] = " "
        end
    end

    tbl[#tbl + 1] = "\n" .. (" "):rep((sc.indent) * META.cgen.indent) .. "}"
    return concat(tbl)
end

local gen_list = function(sc, syms)
    local fun = get_rt_fun("list_new")
    -- TODO: break lines maybe?
    return concat { fun, "(", gen_seq(syms), ")" }
end

local gen_indent = function(lvl)
    return (" "):rep(lvl * META.cgen.indent)
end

local gen_label = function(name)
    return "::" .. name .. "::"
end

local gen_goto = function(label)
    return "goto " .. label
end

local TAG_NUMBER  = 1
local TAG_STRING  = 2
local TAG_BOOLEAN = 3
local TAG_NIL     = 4
local TAG_INVALID = 5

-- classes

local Expr = util.Object:clone {
    name = "Expr",

    __init = function(self, ps)
        if not ps then return nil end
        local dinfo = ps.ndstack:pop()
        dinfo.last_line, dinfo.source = ps.line_number, ps.source
        self.dinfo = dinfo
    end,

    is_lvalue = function(self)
        return false
    end,

    is_scoped = function(self)
        return false
    end,

    is_multret = function(self)
        return false
    end,

    generate = function(self, sc, kwargs)
    end,

    to_lua = function(self, i)
        return "Expr()"
    end
}

local Call_Expr
local Value_Expr

local Symbol_Expr = Expr:clone {
    name = "Symbol_Expr",

    __init = function(self, ps, sym, rt)
        Expr.__init(self, ps)
        self.symbol, self.rt = sym, rt or false
    end,

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        local sym = self.symbol
        return self.rt and get_rt_fun(sym) or sym
    end,

    is_lvalue = function(self)
        return true
    end,

    to_lua = function(self, i)
        return ("Symbol_Expr(%q)"):format(self.symbol)
    end
}

local Index_Expr = Expr:clone {
    name = "Index_Expr",

    __init = function(self, ps, expr, iexpr)
        Expr.__init(self, ps)
        self.expr, self.iexpr = expr, iexpr
    end,

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        local ex, iex
        if self.iexpr:is_a(Value_Expr) then
            iex = self.iexpr:generate(sc, {})
        else
            local sym = unique_sym("index")
            sc:push(gen_local(sym, self.iexpr:generate(sc, {})))
            iex = sym
        end
        -- no need to check for value expr here because we are
        -- sure it isn't one (not permitted by the syntax)
        local sym = unique_sym("expr")
        sc:push(gen_local(sym, self.expr:generate(sc, {})))
        ex = sym
        return gen_index(ex, iex), ex
    end,

    is_lvalue = function(self)
        return true
    end,

    to_lua = function(self, i)
        return ("Index_Expr(%q, %s)"):format(self.symbol,
            self.expr:to_lua(i + 1))
    end
}

local to_tag = function(tn)
    if tn == "<string>" then
        return TAG_STRING
    elseif tn == "<number>" then
        return TAG_NUMBER
    elseif tn == "true" or tn == "false" then
        return TAG_BOOLEAN
    elseif tn == "nil" then
        return TAG_NIL
    else
        return TAG_INVALID
    end
end

Value_Expr = Expr:clone {
    name = "Value_Expr",

    __init = function(self, ps, tag, val, level)
        Expr.__init(self, ps)
        self.tag, self.value, self.level = tag, val, level
    end,

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        local tag, v = self.tag, self.value
        if tag == TAG_STRING then
            return gen_str(v, self.level)
        end
        return self.value
    end,

    is_tag = function(self, tag)
        return self.tag == tag
    end,

    to_lua = function(self, i)
        return ("Value_Expr(%s)"):format(self.value)
    end
}

local Vararg_Expr = Expr:clone {
    name = "Vararg_Expr",

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        local fs = sc:is_a(Function_State) and sc or sc.fstate
        local sl = get_rt_fun("select")
        return sl .. "(" .. (fs.ndefargs + 1) .. ", ...)"
    end,

    to_lua = function(self, i)
        return "Vararg_Expr()"
    end
}

local Return_Expr = Expr:clone {
    name = "Return_Expr",

    __init = function(self, ps, exprs)
        Expr.__init(self, ps)
        self.exprs = exprs
    end,

    generate = function(self, sc, kwargs)
        local exprs = self.exprs
        local len   = #exprs

        local exps = {}
        for i = 1, len do
            local expr = exprs[i]
            if expr:is_a(Value_Expr) or i == len then
                exps[#exps + 1] = expr:generate(sc, {})
            else
                local sym = unique_sym("ret")
                sc:push(gen_local(sym, expr:generate(sc, {})))
                exps[#exps + 1] = sym
            end
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

local Yield_Expr = Expr:clone {
    name = "Yield_Expr",

    __init = function(self, ps, exprs)
        Expr.__init(self, ps)
        self.exprs = exprs
    end,

    generate = function(self, sc, kwargs)
        local cy = get_rt_fun("coro_yield")
        local exprs = self.exprs
        local len   = #exprs

        local exps = {}
        for i = 1, len do
            local expr = exprs[i]
            if expr:is_a(Value_Expr) or i == len then
                exps[#exps + 1] = expr:generate(sc, {})
            else
                local sym = unique_sym("yield")
                sc:push(gen_local(sym, expr:generate(sc, {})))
                exps[#exps + 1] = sym
            end
        end
        if kwargs.statement then
            sc:push(gen_call(cy, gen_seq(exps)))
        else
            return gen_call(cy, gen_seq(exps))
        end
    end,

    is_multret = function(self)
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
        return ("Yield_Expr(%s)"):format(exprs)
    end
}

local Block_Expr = Expr:clone {
    name = "Block_Expr",

    __init = function(self, ps, exprs, vexpr)
        Expr.__init(self, ps)
        self.exprs, self.vexpr = exprs, vexpr
    end,

    generate = function(self, sc, kwargs)
        local exprs, vexpr = self.exprs, self.vexpr
        local len   = #exprs

        local no_scope = kwargs.no_scope
        local scope = (not no_scope) and Scope(sc.fstate, sc.indent + 1) or sc

        for i = 1, len do
            exprs[i]:generate(scope, {
                statement = true
            })
        end

        if not vexpr then
            if not no_scope then
                sc:push(gen_block(scope))
            end
            return nil
        end

        if kwargs.return_val and vexpr:is_scoped() then
            sc:push(vexpr:generate(sc, {
                return_val = true
            }))
        elseif no_scope then
            if sc:is_a(Function_State) or kwargs.return_val then
                sc:push(gen_ret(vexpr:generate(sc, {})))
            else
                return vexpr:generate(sc, {
                    statement = kwargs.statement
                })
            end
        elseif kwargs.statement then
            scope:push(vexpr:generate(scope, {
                statement = true
            }))
            sc:push(gen_block(scope))
        else
            local sym = unique_sym("block")
            sc:push(gen_local(sym))
            if vexpr:is_multret() then
                local fsc = Scope(scope.fstate, scope.indent + 1)
                fsc:push(gen_ret(vexpr:generate(scope, {})))
                scope:push(gen_ass(sym, gen_fun("", fsc)))
                sc:push(gen_block(scope))
                return gen_call(sym, "")
            else
                scope:push(gen_ass(sym, vexpr:generate(scope, {})))
                sc:push(gen_block(scope))
                return sym
            end
        end
    end,

    is_scoped = function(self)
        return true
    end,

    is_multret = function(self)
        local vexpr = self.vexpr
        return vexpr and vexpr:is_multret()
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

local Table_Expr = Expr:clone {
    name = "Table_Expr",

    __init = function(self, ps, contents)
        Expr.__init(self, ps)
        self.contents = contents
    end,

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        local tbl = self.contents

        sc.indent = sc.indent + 1
        local kvs = {}
        local len = #tbl
        for i = 1, len do
            local pair = tbl[i]
            local ke, ve = pair[1], pair[2]
            if (type(ke) == "number") then
                if i == len or ve:is_a(Value_Expr) then
                    kvs[#kvs + 1] = ve:generate(sc, {})
                else
                    local sym = unique_sym("arr")
                    sc:push(gen_local(sym, ve:generate(sc, {})))
                    kvs[#kvs + 1] = sym
                end
            elseif ke:is_a(Value_Expr) then
                if i == len or ve:is_a(Value_Expr) then
                    kvs[#kvs + 1] = gen_ass("[" .. ke:generate(sc, {}) .. "]",
                        ve:generate(sc, {}))
                else
                    local sym = unique_sym("map")
                    sc:push(gen_local(sym, ve:generate(sc, {})))
                    kvs[#kvs + 1] = gen_ass("[" .. ke:generate(sc, {}) .. "]",
                        sym)
                end
            else
                local ksym = unique_sym("key")
                sc:push(gen_local(ksym, ke:generate(sc, {})))
                if i == len or ve:is_a(Value_Expr) then
                    kvs[#kvs + 1] = gen_ass("[" .. ksym .. "]",
                        ve:generate(sc, {}))
                else
                    local sym = unique_sym("map")
                    sc:push(gen_local(sym, ve:generate(sc, {})))
                    kvs[#kvs + 1] = gen_ass("[" .. sym .. "]",
                        sym)
                end
            end
        end
        sc.indent = sc.indent - 1

        return gen_table(sc, kvs)
    end,

    to_lua = function(self, i)
        local array, assarr
        if #self.array == 0 then
            array = "{}"
        else
            local exprs = map(self.array, function(expr)
                return expr:to_lua(i + 1) end)
            array = "{\n" .. gen_indent(i + 1)
                .. concat(exprs, ",\n" .. gen_indent(i + 1))
                .. "\n" .. gen_indent(i) .. "}"
        end
        if #self.map == 0 then
            assarr = "{}"
        else
            local exprs = map(self.map, function(expr)
                return expr:to_lua(i + 1) end)
            assign = "{\n" .. gen_indent(i + 1)
                .. concat(exprs, ",\n" .. gen_indent(i + 1))
                .. "\n" .. gen_indent(i) .. "}"
        end
        return ("Table_Expr(%s, %s)"):format(array, assarr)
    end
}

local List_Expr = Expr:clone {
    name = "List_Expr",

    __init = function(self, ps, contents)
        Expr.__init(self, ps)
        self.contents = contents
    end,

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        local lst = self.contents

        local syms, len = {}, #lst
        for i = 1, len do
            local expr = lst[i]
            if i == len or expr:is_a(Value_Expr) then
                syms[i] = expr:generate(sc, {})
            else
                local sym = unique_sym("list")
                sc:push(gen_local(sym, expr:generate(sc, {})))
                syms[i] = sym
            end
        end

        return gen_list(sc, syms)
    end,

    to_lua = function(self, i)
    end
}

local Object_Expr = Expr:clone {
    name = "Object_Expr",

    __init = function(self, ps, parents, body)
        Expr.__init(self, ps)
        self.parents, self.body = parents, body
    end,

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end

        local pars, body = self.parents, self.body
        local syms, len = {}, #pars
        for i = 1, len do
            if i == len then
                syms[i] = pars[i]:generate(sc, {})
            else
                local sym = unique_sym("par")
                sc:push(gen_local(sym, pars[i]:generate(sc, {})))
                syms[i] = sym
            end
        end

        local fun, kvs = get_rt_fun("obj_clone"), {}
        for i = 1, #body do
            local pair = body[i]
            local ke, ve = pair[1], pair[2]
            if ke:is_a(Value_Expr) then
                if i == len or ve:is_a(Value_Expr) then
                    kvs[#kvs + 1] = gen_ass("[" .. ke:generate(sc, {}) .. "]",
                        ve:generate(sc, {}))
                else
                    local sym = unique_sym("obj")
                    sc:push(gen_local(sym, ve:generate(sc, {})))
                    kvs[#kvs + 1] = gen_ass("[" .. ke:generate(sc, {}) .. "]",
                        sym)
                end
            else
                local ksym = unique_sym("key")
                sc:push(gen_local(ksym, ke:generate(sc, {})))
                if i == len or ve:is_a(Value_Expr) then
                    kvs[#kvs + 1] = gen_ass("[" .. ksym .. "]",
                        ve:generate(sc, {}))
                else
                    local sym = unique_sym("map")
                    sc:push(gen_local(sym, ve:generate(sc, {})))
                    kvs[#kvs + 1] = gen_ass("[" .. ksym .. "]",
                        sym)
                end
            end
        end
        return gen_call(fun, gen_seq({ gen_table(sc, kvs), gen_seq(syms) }))
    end
}

local New_Expr = Expr:clone {
    name = "New_Expr",

    __init = function(self, ps, expr, exprlist)
        Expr.__init(self, ps)
        self.expr, self.exprlist = expr, exprlist
    end,

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end

        local fun = get_rt_fun("obj_new")
        local expr, el = self.expr, self.exprlist
        local sym = unique_sym("new")
        sc:push(gen_local(sym, expr:generate(sc, {})))

        local len, t = #el, { sym }
        for i = 1, len do
            local exp = el[i]
            if i == len or exp:is_a(Value_Expr) then
                t[i + 1] = exp:generate(sc, {})
            else
                local sym = unique_sym("ctor")
                sc:push(gen_local(sym, exp:generate(sc, {})))
                t[i + 1] = sym
            end
        end
        return gen_call(fun, gen_seq(t))
    end
}

local Binary_Expr
Binary_Expr = Expr:clone {
    name = "Binary_Expr",

    __init = function(self, ps, op, lhs, rhs)
        Expr.__init(self, ps)
        self.op, self.lhs, self.rhs = op, lhs, rhs
    end,

    generate = function(self, sc, kwargs)
        local op = self.op
        if Ass_Ops[op] then
            local lhs, sym = self.lhs, unique_sym("rhs")
            local iv = lhs:generate(sc, {})
            if op == "=" then
                sc:push(gen_local(sym, self.rhs:generate(sc, {})))
            else
                sc:push(gen_local(sym, gen_binexpr(op:sub(1, #op - 1), iv,
                    self.rhs:generate(sc, {}))))
            end
            sc:push(gen_ass(iv, sym))
            if not kwargs.statement then
                return sym, av
            end
        end

        local lhs = self.lhs:generate(sc, {})
        local rhs = self.rhs:generate(sc, {})
        if not kwargs.statement then
            return gen_binexpr(self.op, lhs, rhs)
        end
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

    __init = function(self, ps, op, rhs)
        Expr.__init(self, ps)
        self.op, self.rhs = op, rhs
    end,

    generate = function(self, sc, kwargs)
        local rhs = self.rhs:generate(sc, {})
        return gen_unexpr(self.op, rhs)
    end,

    to_lua = function(self, i)
        return ("Unary_Expr(%q, %s)"):format(self.op,
            self.rhs:to_lua(i + 1))
    end
}

local Function_Expr = Expr:clone {
    name = "Function_Expr",

    __init = function(self, ps, params, defaults, body)
        Expr.__init(self, ps)
        ps.fnstack:pop()
        self.params, self.defaults, self.body = params, defaults, body
    end,

    generate = function(self, sc, kwargs)
        local fs = Function_State(sc.indent + 1)

        local args,  defs  = self.params, self.defaults
        local nargs, ndefs = #args, #defs

        fs.nargs = nargs
        fs.ndefargs = ndefs

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
            local sl = get_rt_fun("select")
            local nd
            if defs[ndefs] ~= "..." then
                nd = unique_sym("#")
                fs:push(gen_local(nd, gen_call(sl, gen_seq({ "'#'", "..." }))))
            end

            for i = 1, ndefs do
                local name = args[pargs + i]
                local val  = defs[i]
                if val == "..." then
                    fs:push(gen_local(name, gen_table(fs, { gen_call(sl,
                        gen_seq({ i, "..." })) })))
                    break
                end
                fs:push(gen_local(name))

                local tsc = Scope(fs, fs.indent + 1)
                tsc:push(gen_ass(name, defs[i]:generate(tsc, {})))

                local fsc = Scope(fs, fs.indent + 1)
                fsc:push(gen_ass(name, gen_call(sl, gen_seq({ i, "..." }))))

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
            local ret = body:generate(fs, {
                return_val = true
            })
            if ret then fs:push(gen_ret(ret)) end
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

    __init = function(self, ps, cond, tval, fval)
        Expr.__init(self, ps)
        self.cond, self.tval, self.fval = cond, tval, fval
    end,

    generate = function(self, sc, kwargs)
        local stat, rval, tsc, fsc, tval, fval, tscoped, fscoped
            = kwargs.statement, kwargs.return_val

        local tv, fv = self.tval, self.fval

        tscoped = tv:is_scoped()
        tsc  = Scope(sc.fstate, sc.indent + 1)
        tval = tv:generate(tsc, {
            no_scope  = true,
            statement = stat,
            return_val = rval
        })

        if fv then
            fscoped = fv:is_scoped()
            fsc  = Scope(sc.fstate, sc.indent + 1)
            fval = fv:generate(fsc, {
                no_scope  = true,
                statement = stat,
                return_val = rval
            })
        end

        if stat or rval then
            tsc:push((rval and not tscoped) and gen_ret(tval) or tval)
            if fsc then fsc:push((rval and not fscoped)
                and gen_ret(fval) or fval) end

            sc:push(gen_if(self.cond:generate(sc, {}), tsc, fsc))
        else
            local sym = unique_sym("if")

            sc:push(gen_local(sym))
            if self:is_multret() then
                local tfsc = Scope(tsc.fstate, tsc.indent + 1)
                tfsc:push(gen_ret(tval))
                tsc:push(gen_ass(sym, gen_fun("", tfsc)))
                if fv then
                    local ffsc = Scope(fsc.fstate, fsc.indent + 1)
                    ffsc:push(gen_ret(fval))
                    fsc:push(gen_ass(sym, gen_fun("", ffsc)))
                end
                sc:push(gen_if(self.cond:generate(sc, {}), tsc, fsc))
                return gen_call(sym, "")
            else
                tsc:push(gen_ass(sym, tval))
                if fsc then fsc:push(gen_ass(sym, fval)) end
                sc:push(gen_if(self.cond:generate(sc, {}), tsc, fsc))
                return sym
            end
        end
    end,

    is_scoped = function(self)
        return true
    end,

    is_multret = function(self)
        local fv = self.fval
        return self.tval:is_multret() or (fv and fv:is_multret())
    end,

    to_lua = function(self, i)
        return ("If_Expr(%s, %s, %s)"):format(
            self.cond:to_lua(i + 1), self.tval:to_lua(i + 1),
            self.fval:to_lua(i + 1))
    end
}

local And_Pattern = Expr:clone {
    name = "And_Pattern",

    __init = function(self, ps, lhs, rhs, as, cond)
        Expr.__init(self, ps)
        self.lhs, self.rhs, self.cond, self.as = lhs, rhs, cond, as
    end,

    generate = function(self, sc, kwargs)
        return gen_binexpr("and", self.lhs:generate(sc, kwargs),
                                  self.rhs:generate(sc, kwargs))
    end
}

local Or_Pattern = Expr:clone {
    name = "Or_Pattern",

    __init = function(self, ps, lhs, rhs, as, cond)
        Expr.__init(self, ps)
        self.lhs, self.rhs, self.cond, self.as = lhs, rhs, cond, as
    end,

    generate = function(self, sc, kwargs)
        return gen_binexpr("or", self.lhs:generate(sc, kwargs),
                                 self.rhs:generate(sc, kwargs))
    end
}

local Expr_Pattern = Expr:clone {
    name = "Expr_Pattern",

    __init = function(self, ps, expr, as, cond)
        Expr.__init(self, ps)
        self.expr, self.cond, self.as = expr, cond, as
    end,

    generate = function(self, sc, kwargs)
        return gen_binexpr("==", self.expr:generate(sc, {}), kwargs.expr)
    end
}

local Variable_Pattern = Expr:clone {
    name = "Variable_Pattern",

    __init = function(self, ps, var, as, cond)
        Expr.__init(self, ps)
        self.var, self.cond, self.as = var, cond, as
    end,

    generate = function(self, sc, kwargs)
        if kwargs.decl then
            sc:push(gen_local(self.var))
            return nil
        elseif kwargs.no_local then
            sc:push(gen_ass(self.var, kwargs.expr))
        else
            sc:push(gen_local(self.var, kwargs.expr))
        end
        if kwargs.let then return self.var end
    end
}

local Wildcard_Pattern = Expr:clone {
    name = "Wildcard_Pattern",

    __init = function(self, ps, as, cond)
        Expr.__init(self, ps)
        self.cond, self.as = cond, as
    end,

    generate = function(self, sc, kwargs)
    end
}

local Table_Pattern = Expr:clone {
    name = "Table_Pattern",

    __init = function(self, ps, contents, as, cond)
        Expr.__init(self, ps)
        self.contents, self.cond, self.as = contents, cond, as
    end,

    generate = function(self, sc, kwargs)
        local tfun = get_rt_fun("type")
        local tbl, expr = self.contents, kwargs.expr
        local mn, ret = 0

        if kwargs.decl then
            for i = 1, #tbl do
                tbl[i][2]:generate(sc, { decl = true })
            end
            return nil
        elseif kwargs.let then
            local ret = {}
            for i = 1, #tbl do
                local it = tbl[i]
                local k, v = it[1], it[2]
                if type(k) == "number" then
                    mn = mn + 1
                end
                local el = gen_index(expr, k)
                ret[i] = v:generate(sc, { expr = el, let = true,
                    no_local = kwargs.no_local })
            end
            return gen_seq(ret)
        end

        local ns = Scope(sc.fstate, sc.indent)
        for i = 1, #tbl do
            local it = tbl[i]
            local k, v = it[1], it[2]
            if type(k) == "number" then
                mn = mn + 1
            end
            local el = gen_index(expr, k)
            local pv = v:generate(ns, { expr = el,
                next_arm = kwargs.next_arm })
            ret = ret and gen_binexpr("and", ret, pv) or pv
        end

        local ts = Scope(sc.fstate, sc.indent + 1)
        ts:push(gen_goto(kwargs.next_arm))
        sc:push(gen_if(gen_binexpr("or",
            gen_binexpr("!=", gen_call(tfun, expr), gen_str("table")),
            gen_binexpr("!=", gen_unexpr("#", expr), mn)), ts))
        sc:merge(ns)

        return ret
    end
}

local Cons_Pattern = Expr:clone {
    name = "Cons_Pattern",

    __init = function(self, ps, head, tail, as, cond)
        Expr.__init(self, ps)
        self.head, self.tail, self.cond, self.as = head, tail, cond, as
    end,

    generate = function(self, sc, kwargs)
        local tfun = get_rt_fun("type")
        local first, rest = get_rt_fun("list_first"), get_rt_fun("list_rest")
        local head, tail, expr = self.head, self.tail, kwargs.expr

        if kwargs.decl then
            sc:push(gen_local(gen_seq({ head, tail })))
            return nil
        elseif kwargs.let then
            local seq = gen_seq({ head, tail })
            sc:push(gen_local(seq))
            sc:push(gen_ass(h, gen_call(first, expr)))
            sc:push(gen_ass(t, gen_call(last,  expr)))
            return seq
        end

        local ts = Scope(sc.fstate, sc.indent + 1)
        ts:push(gen_goto(kwargs.next_arm))
        sc:push(gen_if(gen_binexpr("!=", gen_call(tfun, expr),
            gen_str("table")), ts))
        sc:push(gen_local(head, gen_call(first, expr)))
        sc:push(gen_if(gen_binexpr("==", head, "nil"), ts))
        sc:push(gen_local(tail, gen_call(rest,  expr)))
    end
}

local Match_Expr = Expr:clone {
    name = "Match_Expr",

    __init = function(self, ps, exprlist, body)
        Expr.__init(self, ps)
        self.exprlist, self.body = exprlist, body
    end,

    generate = function(self, sc, kwargs)
        local stat, rval = kwargs.statement, kwargs.return_val

        local al = self.body
        local narms = #al

        -- find the longest arm
        local alen = 1
        for i = 1, narms do
            local l = #al[i][1]
            if l > alen then alen = l end
        end

        local el = self.exprlist
        local elen, exps = #el, {}
        for i = 1, elen - 1 do
            local s = unique_sym("expr")
            sc:push(gen_local(s, el[i]:generate(sc, {})))
            exps[i] = s
        end
        local last = el[elen]
        if last:is_multret() then
            local lseq
            for i = elen, alen do
                local sym = unique_sym("expr")
                lseq = lseq and gen_seq({ lseq, sym }) or sym
                exps[i] = sym
            end
            sc:push(gen_local(lseq, last:generate(sc, {})))
        else
            local s = unique_sym("expr")
            sc:push(gen_local(s, last:generate(sc, {})))
            exps[elen] = s
        end

        local msym, multret
        if not stat and not rval then
            msym = unique_sym("match")
            sc:push(gen_local(msym))
            multret = self:is_multret()
        end

        local armlb, elb = unique_sym("lbl"), unique_sym("lbl")
        for i = 1, narms do
            local arm = al[i]
            local pl, bd = arm[1], arm[2]
            local ptrns = {}

            local asc = Scope(sc.fstate, sc.indent + 1)
            sc:push(gen_label(armlb))
            armlb = (i ~= narms) and unique_sym("lbl") or elb
            local n = 1
            for i = 1, #pl do
                local pt = pl[i]
                local expr = exps[i] or "nil"
                local v = pt:generate(asc, {
                    expr = expr, next_arm = armlb
                })
                local cond = pt.cond
                if cond then
                    local ts = Scope(asc.fstate, asc.indent + 1)
                    ts:push(gen_goto(armlb))
                    asc:push(gen_if(gen_unexpr("not", cond:generate(asc, {})),
                        ts))
                end
                local as = pt.as
                if as then
                    asc:push(gen_local(as:generate(asc, {}), expr))
                end
                if v then
                    ptrns[n] = v
                    n = n + 1
                end
            end

            local cond
            for i = 1, n - 1 do
                local c = ptrns[i]
                cond = cond and gen_binexpr("and", cond, c) or c
            end

            if cond then
                local csc = Scope(asc.fstate, asc.indent + 1)
                csc:push(gen_goto(armlb))
                asc:push(gen_if(gen_unexpr("not", cond), csc))
            end

            local aval = bd:generate(asc, {
                no_scope = true,
                statement = stat,
                return_val = rval
            })

            if stat or rval then
                asc:push((rval and not bd:is_scoped())
                    and gen_ret(aval) or aval)
            elseif multret then
                local fsc = Scope(asc.fstate, asc.indent + 1)
                fsc:push(gen_ret(aval))
                asc:push(gen_ass(msym, gen_fun("", fsc)))
            else
                asc:push(gen_ass(msym, aval))
            end
            sc:push(gen_block(asc))
            sc:push(gen_goto(elb))
        end
        sc:push(gen_label(elb))
        return multret and gen_call(msym, "") or msym
    end,

    is_scoped = function(self)
        return true
    end,

    is_multret = function(self)
        local body = self.body
        for i = 1, #body do
            if body[i][2]:is_multret() then return true end
        end
        return false
    end,

    to_lua = function(self, i)
        return ("If_Expr(%s, %s, %s)"):format(
            self.cond:to_lua(i + 1), self.tval:to_lua(i + 1),
            self.fval:to_lua(i + 1))
    end
}

local Let_Expr = Expr:clone {
    name = "Let_Expr",

    __init = function(self, ps, ltype, patterns, assign)
        Expr.__init(self, ps)
        self.type, self.patterns, self.assign = ltype, patterns, assign
    end,

    generate = function(self, sc, kwargs)
        local ptrns, assign, syms = self.patterns, self.assign, {}
        local len, plen = #assign, #ptrns
        local tp = self.type

        -- generate declarations
        if tp == "rec" then
            for i = 1, plen do
                ptrns[i]:generate(sc, {
                    decl = true,
                    let  = true
                })
            end
        end

        for i = 1, len - 1 do
            local expr = assign[i]
            if expr:is_a(Value_Expr) then
                syms[i] = expr:generate(sc, {})
            else
                local sym = unique_sym("let")
                sc:push(gen_local(sym, expr:generate(sc, {})))
                syms[i] = sym
            end
        end
        local last = assign[len]
        if last and last:is_multret() then
            local lseq
            for i = len, plen do
                local sym = unique_sym("let")
                lseq = lseq and gen_seq({ lseq, sym }) or sym
                syms[i] = sym
            end
            sc:push(gen_local(lseq, assign[len]:generate(sc, {})))
        elseif last then
            if last:is_a(Value_Expr) then
                syms[len] = last:generate(sc, {})
            else
                local sym = unique_sym("let")
                sc:push(gen_local(sym, last:generate(sc, {})))
                syms[len] = sym
            end
        end

        local rids = {}
        for i = 1, plen do
            rids[#rids + 1] = ptrns[i]:generate(sc, {
                expr = syms[i], let = true, no_local = tp ~= nil
            })
        end

        if not kwargs.statement then
            return gen_seq(rids)
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

local While_Expr = Expr:clone {
    name = "While_Expr",

    __init = function(self, ps, cond, body)
        Expr.__init(self, ps)
        self.cond, self.body = cond, body
    end,

    generate = function(self, sc, kwargs)
        local bsc = Scope(sc.fstate, sc.indent + 1)

        local lbl = unique_sym("lbl")
        local lbeg, lend = lbl .. "_beg", lbl .. "_end"
        bsc:push(gen_label(lbeg))

        local tsc = Scope(sc.fstate, bsc.indent + 1)
        tsc:push(gen_goto(lend))
        bsc:push(gen_if(gen_unexpr("not", self.cond:generate(bsc, {})), tsc))
        self.body:generate(bsc, {
            statement = true,
            no_scope  = true
        })
        bsc:push(gen_goto(lbeg))
        bsc:push(gen_label(lend))

        sc:push(gen_block(bsc))
    end,

    is_scoped = function(self)
        return true
    end,

    to_lua = function(self, i)
        return ("While_Expr(%s, %s)"):format(
            self.cond:to_lua(i + 1), self.body:to_lua(i + 1))
    end
}

local Do_While_Expr = Expr:clone {
    name = "Do_While_Expr",

    __init = function(self, ps, cond, body)
        Expr.__init(self, ps)
        self.cond, self.body = cond, body
    end,

    generate = function(self, sc, kwargs)
        local bsc = Scope(sc.fstate, sc.indent + 1)

        local lbl = unique_sym("lbl")
        bsc:push(gen_label(lbl))
        self.body:generate(bsc, {
            statement = true,
            no_scope  = true
        })
        local tsc = Scope(sc.fstate, bsc.indent + 1)
        tsc:push(gen_goto(lbl))
        bsc:push(gen_if(self.cond:generate(bsc, {}), tsc))

        sc:push(gen_block(bsc))
    end,

    is_scoped = function(self)
        return true
    end,

    to_lua = function(self, i)
        return ("While_Expr(%s, %s)"):format(
            self.cond:to_lua(i + 1), self.body:to_lua(i + 1))
    end
}

local For_Expr = Expr:clone {
    name = "For_Expr",

    __init = function(self, ps, idents, exprs, body)
        Expr.__init(self, ps)
        self.idents, self.exprs, self.body = idents, exprs, body
    end,

    generate = function(self, sc, kwargs)
        local bsc = Scope(sc.fstate, sc.indent + 1)

        local fsym, ssym, varsym
            = unique_sym("f"), unique_sym("s"), unique_sym("var")

        local lbl = unique_sym("lbl")
        local lbeg, lend = lbl .. "_beg", lbl .. "_end"

        local exps = {}
        local el = self.exprs
        for i = 1, #el do
            exps[i] = el[i]:generate(bsc, {})
        end
        bsc:push(gen_local(gen_seq({ fsym, ssym, varsym }), gen_seq(exps)))
        bsc:push(gen_label(lbeg))

        local ids = self.idents
        bsc:push(gen_local(gen_seq(ids), gen_call(fsym,
            gen_seq({ ssym, varsym }))))

        local tsc = Scope(sc.fstate, bsc.indent + 1)
        tsc:push(gen_goto(lend))
        bsc:push(gen_if(gen_binexpr("==", ids[1], "nil"), tsc))
        bsc:push(gen_ass(varsym, ids[1]))
        self.body:generate(bsc, {
            statement = true,
            no_scope  = true
        })
        bsc:push(gen_goto(lbeg))
        bsc:push(gen_label(lend))

        sc:push(gen_block(bsc))
    end,

    is_scoped = function(self)
        return true
    end,

    to_lua = function(self, i)
        return ("While_Expr(%s, %s)"):format(
            self.cond:to_lua(i + 1), self.body:to_lua(i + 1))
    end
}

local For_Range_Expr = Expr:clone {
    name = "For_Range_Expr",

    __init = function(self, ps, ident, first, last, step, body)
        Expr.__init(self, ps)
        self.ident, self.first, self.last, self.step, self.body
            = ident, first, last, step, body
    end,

    generate = function(self, sc, kwargs)
        local bsc = Scope(sc.fstate, sc.indent + 1)
        local tonum = get_rt_fun("tonum")
        local rterr = get_rt_fun("error")

        local varsym, limsym, stepsym
            = unique_sym("var"), unique_sym("lim"), unique_sym("step")

        local lbl = unique_sym("lbl")
        local lbeg, lend = lbl .. "_beg", lbl .. "_end"

        local ex1, ex2, ex3 = self.first:generate(bsc, {}),
            self.last:generate(bsc, {}), self.step:generate(bsc, {})

        bsc:push(gen_local(gen_seq({ varsym, limsym, stepsym }),
            gen_seq({ gen_call(tonum, ex1),
                      gen_call(tonum, ex2),
                      gen_call(tonum, ex3) })))

        local ivs, lms, sts = Scope(sc.fstate, bsc.indent + 1),
            Scope(sc.fstate, bsc.indent + 1), Scope(sc.fstate, bsc.indent + 1)

        local di = self.dinfo
        local src, linenum = di.source, di.first_line
        ivs:push(gen_call(rterr, '"'.."'for' initial value must be a number"
            ..'"'))
        lms:push(gen_call(rterr, '"'.."'for' limit must be a number"..'"'))
        sts:push(gen_call(rterr, '"'.."'for' step must be a number"..'"'))

        bsc:push(gen_if(gen_binexpr("==", varsym,  "nil"), ivs))
        bsc:push(gen_if(gen_binexpr("==", limsym,  "nil"), lms))
        bsc:push(gen_if(gen_binexpr("==", stepsym, "nil"), sts))

        bsc:push(gen_label(lbeg))

        local tsc = Scope(sc.fstate, bsc.indent + 1)
        tsc:push(gen_goto(lend))
        bsc:push(gen_if(gen_unexpr("not", gen_binexpr("or",
            gen_binexpr("and",
                gen_binexpr(">", stepsym, "0"),
                gen_binexpr("<=", varsym, limsym)),
            gen_binexpr("and",
                gen_binexpr("<=", stepsym, "0"),
                gen_binexpr(">=", varsym, limsym)
            ))), tsc))
        bsc:push(gen_local(self.ident, varsym))
        self.body:generate(bsc, {
            statement = true,
            no_scope  = true
        })
        bsc:push(gen_ass(varsym, gen_binexpr("+", varsym, stepsym)))
        bsc:push(gen_goto(lbeg))
        bsc:push(gen_label(lend))

        sc:push(gen_block(bsc))
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

    __init = function(self, ps, expr)
        Expr.__init(self, ps)
        ps.fnstack:pop()
        self.expr = expr
    end,

    generate = function(self, sc, kwargs)
        local sq, cc = get_rt_fun("seq_create"), get_rt_fun("coro_create")

        local fs = Scope(sc.fstate, sc.indent + 1)
        local body = self.expr
        if body:is_a(Block_Expr) then
            body:generate(fs, {
                no_scope = true,
                return_val = true
            })
        else
            local ret = body:generate(fs, {
                return_val = true
            })
            if ret then fs:push(gen_ret(ret)) end
        end
        return gen_call(sq, gen_call(cc, gen_fun("", fs)))
    end,

    is_multret = function(self)
        return true
    end,

    to_lua = function(self, i)
        return ("Seq_Expr(%s)"):format(self.expr:to_lua(i + 1))
    end
}

local Pack_Expr = Expr:clone {
    name = "Pack_Expr",

    __init = function(self, ps, exprlist)
        Expr.__init(self, ps)
        self.exprlist = exprlist
    end,

    generate = function(self, sc, kwargs)
        local el, exprs = self.exprlist, {}
        for i = 1, #el do
            exprs[i] = el[i]:generate(sc, {})
        end
        return gen_seq(exprs)
    end,

    is_multret = function(self)
        return true
    end,
}

local Quote_Expr = Expr:clone {
    name = "Quote_Expr",

    __init = function(self, ps, expr)
        Expr.__init(self, ps)
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

    __init = function(self, ps, expr, params, method)
        Expr.__init(self, ps)
        self.expr, self.params, self.method = expr, params,
            method or false
    end,

    generate = function(self, sc, kwargs)
        local syms = {}
        local len  = #self.params
        local method = self.method

        local expr, slf = self.expr:generate(sc, {})
        if method then
            syms[1] = slf
        end
        local off = method and 1 or 0
        for i = 1, len do
            local par = self.params[i]
            if par:is_a(Value_Expr) or i == len then
                syms[i + off] = par:generate(sc, {})
            else
                local sym = unique_sym("arg")
                sc:push(gen_local(sym, par:generate(sc, {})))
                syms[i + off] = sym
            end
        end
        syms = gen_seq(syms)

        if kwargs.statement then
            sc:push(gen_call(expr, syms))
        else
            return gen_call(expr, syms)
        end
    end,

    is_multret = function(self)
        return true
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

local parse_expr
local parse_exprlist
local parse_pattern_list
local parse_pattern

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
local parse_arglist = function(ls, first)
    local tok = ls.token
    local tn  = tok.name

    if tn == "->" or tn == "{" or tn ~= "<ident>" then
        return { first }, {}
    end

    if tn == "..." then
        ls:get()
        return (first and { first, "..." } or { "..." }), {}
    end

    local ids, defs = { first }
    repeat
        if tok.name == "..." then
            ids[#ids + 1] = "..."
            ls:get()
            break
        end

        assert_tok(ls, "<ident>")
        ids[#ids + 1] = tok.value

        local lah = ls:lookahead()
        if lah == "=" then
            ls:get() ls:get()
            if not defs then defs = {} end
            defs[#defs + 1] = parse_expr(ls)
        elseif lah == "..." then
            ls:get() ls:get()
            if not defs then defs = {} end
            defs[#defs + 1] = lah
            break
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
    push_curline(ls)
    ls:get()
    local tok, exprs = ls.token, {}

    if tok.name == "}" then
        ls:get()
        return Block_Expr(ls, exprs)
    end

    if tok.name == "->" then
        goto block_endexpr
    end

    repeat
        exprs[#exprs + 1] = parse_expr(ls)
    until tok.name == "}" or tok.name == "->"

    ::block_endexpr::
    if tok.name == "->" then
        ls:get()
        if tok.name == "(" then
            ls:get()
            local el = parse_exprlist(ls)
            assert_tok(ls, ")")
            ls:get()
            push_curline(ls)
            assert_tok(ls, "}")
            ls:get()
            return Block_Expr(ls, exprs, Pack_Expr(ls, el))
        else
            local exp = parse_expr(ls)
            assert_tok(ls, "}")
            ls:get()
            return Block_Expr(ls, exprs, exp)
        end
    end

    assert_tok(ls, "}")
    ls:get()
    return Block_Expr(ls, exprs)
end

local parse_table = function(ls)
    push_curline(ls)
    ls:get()
    local tok, tbl = ls.token, {}

    if tok.name == "]" then
        ls:get()
        return Table_Expr(ls, tbl)
    end

    local idx = 1
    repeat
        if tok.name == "<ident>" and ls:lookahead() == "=" then
            local name = Value_Expr(nil, TAG_STRING, tok.value)
            ls:get() ls:get()
            tbl[#tbl + 1] = { name, parse_expr(ls) }
        elseif tok.name == "$" then
            local expr = parse_expr(ls)
            if tok.name == "=" then
                ls:get()
                tbl[#tbl + 1] = { expr, parse_expr(ls) }
            else
                tbl[#tbl + 1] = { idx, expr }
                idx = idx + 1
            end
        else
            tbl[#tbl + 1] = { idx, parse_expr(ls) }
            idx = idx + 1
        end
        if tok.name ~= "," then
            assert_tok(ls, "]")
        else
            ls:get()
        end
    until tok.name == "]"

    assert_tok(ls, "]")
    ls:get()

    return Table_Expr(ls, tbl)
end

local parse_list = function(ls)
    push_curline(ls)
    ls:get()
    ls:get()
    local tok = ls.token

    if tok.name == ":" then
        ls:get()
        assert_tok(ls, "]")
        ls:get()
        return List_Expr(ls, {})
    end

    local el = parse_exprlist(ls)

    assert_tok(ls, ":")
    ls:get()
    assert_tok(ls, "]")
    ls:get()
    return List_Expr(ls, el)
end

local parse_match = function(ls)
    push_curline(ls)
    ls:get()
    local el  = parse_exprlist(ls)
    local tok = ls.token

    local inb = false
    if tok.name == "{" then
        ls:get()
        inb = true
    else
        assert_tok(ls, "->")
        ls:get()
    end

    local body = parse_match_body(ls)
    if inb then
        assert_tok(ls, "}")
        ls:get()
    end

    return Match_Expr(ls, el, body)
end

local parse_match_body

local parse_function = function(ls, obj)
    local tok = ls.token
    push_curline(ls)
    ls:get()

    local ltype
    if tok.name == "rec" or tok.name == "glob" then
        if obj then
            assert_tok(ls, "<ident>")
        end
        ltype = tok.name
        ls:get()
    end

    local noself
    if obj and tok.name == "@" then
        ls:get()
        noself = true
    end

    local tbl, slf, name, ids, defs
    if tok.name == "<ident>" then
        local lah = ls:lookahead()
        if lah == "(" then
            push_curline(ls)
            if not obj then
                push_curline(ls)
            end
            name = tok.value
            ls:get()
            ls:get()
            ids, defs = parse_arglist(ls, (obj and not noself)
                and "self" or nil)
            assert_tok(ls, ")")
            ls:get()
        elseif lah == ":" or lah == "." then
            if obj then
                ls:get()
                assert_tok(ls, "(")
            end
            if ltype then
                syntax_error(ls, "method with '" .. ltype .. "'")
            end
            push_curline(ls)
            if not obj then
                push_curline(ls)
                push_curline(ls)
                push_curline(ls)
            end
            tbl, slf = tok.value, (lah == ":" and true or false)
            ls:get()
            ls:get()
            assert_tok(ls, "<ident>")
            name = tok.value
            ls:get()
            assert_tok(ls, "(")
            ls:get()
            ids, defs = parse_arglist(ls, slf and "self" or nil)
            assert_tok(ls, ")")
            ls:get()
        elseif not ltype then
            if obj then
                ls:get()
                assert_tok(ls, "(")
            end
            local v = tok.value
            ls:get()
            if tok.name == "..." then
                ls:get()
                ids, defs = { v }, { "..." }
            elseif tok.name ~= "," then
                ids, defs = { v }, {}
            else
                ls:get()
                ids, defs = parse_arglist(ls, v)
            end
        else
            ls:get()
            assert_tok(ls, "(")
        end
    elseif not ltype then
        if obj then
            assert_tok(ls, "<ident>")
        end
        local par = false
        if tok.name == "(" then
            ls:get()
            par = true
        end
        ids, defs = parse_arglist(ls)
        if par then
            assert_tok(ls, ")")
            ls:get()
        end
    else
        assert_tok(ls, "<ident>")
    end

    ls.fnstack:push({ vararg = ids[#ids] == "..." })

    if tok.name == "{" then
        local lah, body = ls:lookahead()
        if lah == "|" or lah == "case" then
            ls:get()
            push_curline(ls)
            local el = {}
            for i = 1, #ids do
                local n = ids[i]
                if n == "..." then break end
                el[i] = Symbol_Expr(nil, n)
            end
            body = Match_Expr(ls, el, parse_match_body(ls))
            assert_tok(ls, "}")
            ls:get()
        else
            body = parse_block(ls)
        end
        local fnexpr = Function_Expr(ls, ids, defs, body)
        if name then
            if obj then
                return Value_Expr(ls, TAG_STRING, name), fnexpr
            elseif tbl then
                return Binary_Expr(ls, "=", Index_Expr(ls,
                    Symbol_Expr(ls, tbl), Value_Expr(ls, TAG_STRING, name)),
                        fnexpr)
            else
                return Let_Expr(ls, ltype, { Variable_Pattern(ls, name) },
                    { fnexpr })
            end
        end
        return fnexpr
    end

    assert_tok(ls, "->")
    ls:get()

    local n, fnexpr = tok.name
    if n == "|" or n == "case" then
        push_curline(ls)
        local el = {}
        for i = 1, #ids do
            local n = ids[i]
            if n == "..." then break end
            el[i] = Symbol_Expr(nil, n)
        end
        fnexpr = Function_Expr(ls, ids, defs,
            Match_Expr(ls, el, parse_match_body(ls)))
    elseif n == "(" then
        ls:get()
        local el = parse_exprlist(ls)
        assert_tok(ls, ")")
        ls:get()
        push_curline(ls)
        fnexpr = Function_Expr(ls, ids, defs, Pack_Expr(ls, el))
    else
        fnexpr = Function_Expr(ls, ids, defs, parse_expr(ls))
    end

    if name then
        if obj then
            return Value_Expr(ls, TAG_STRING, name), fnexpr
        elseif tbl then
            return Binary_Expr(ls, "=", Index_Expr(ls,
                Symbol_Expr(ls, tbl), Value_Expr(ls, TAG_STRING, name)),
                    fnexpr)
        else
            return Let_Expr(ls, ltype, { Variable_Pattern(ls, name) },
                { fnexpr })
        end
    end
    return fnexpr
end

local parse_let = function(ls)
    local tok = ls.token
    push_curline(ls)
    ls:get()

    local ltype
    if tok.name == "rec" or tok.name == "glob" then
        ltype = tok.name
        ls:get()
    end

    local ptrns
    if tok.name == "(" then
        ls:get()
        ptrns = parse_pattern_list(ls, true)
        assert_tok(ls, ")")
        ls:get()
    else
        ptrns = { parse_pattern(ls, true) }
    end

    local exprs
    if tok.name == "=" then
        ls:get()
        if tok.name == "(" then
            ls:get()
            exprs = parse_exprlist(ls)
            assert_tok(ls, ")")
            ls:get()
        else
            exprs = { parse_expr(ls) }
        end
    else
        exprs = {}
    end

    return Let_Expr(ls, ltype, ptrns, exprs)
end

local parse_sequence = function(ls)
    push_curline(ls)
    ls:get()
    ls.fnstack:push({ vararg = false })

    local tok = ls.token
    if tok.name == "{" then
        return Seq_Expr(ls, parse_block(ls))
    end

    assert_tok(ls, "->")
    ls:get()

    if tok.name == "(" then
        ls:get()
        local el = parse_exprlist(ls)
        assert_tok(ls, ")")
        ls:get()
        push_curline(ls)
        return Seq_Expr(ls, Pack_Expr(ls, el))
    end

    return Seq_Expr(ls, parse_expr(ls))
end

local parse_quote = function(ls)
    push_curline(ls)
    ls:get()

    local tok = ls.token
    if tok.name == "{" then
        return Quote_Expr(ls, parse_block(ls))
    end

    assert_tok(ls, "->")
    ls:get()

    if tok.name == "(" then
        ls:get()
        local el = parse_exprlist(ls)
        assert_tok(ls, ")")
        ls:get()
        push_curline(ls)
        return Quote_Expr(ls, Pack_Expr(ls, el))
    end

    return Quote_Expr(ls, parse_expr(ls))
end

local parse_if = function(ls)
    push_curline(ls)
    ls:get()
    local cond = parse_expr(ls)
    local tok  = ls.token

    local tval
    if tok.name == "{" then
        tval = parse_block(ls)
    else
        assert_tok(ls, "->")
        ls:get()

        if tok.name == "(" then
            ls:get()
            local el = parse_exprlist(ls)
            assert_tok(ls, ")")
            ls:get()
            push_curline(ls)
            tval = Pack_Expr(ls, el)
        else
            tval = parse_expr(ls)
        end
    end

    if tok.name == "else" then
        ls:get()
        if tok.name == "{" then
            return If_Expr(ls, cond, tval, parse_block(ls))
        else
            if tok.name == "->" then
                ls:get()
            end
            if tok.name == "(" then
                ls:get()
                local el = parse_exprlist(ls)
                assert_tok(ls, ")")
                ls:get()
                push_curline(ls)
                return If_Expr(ls, cond, tval, Pack_Expr(ls, el))
            else
                return If_Expr(ls, cond, tval, parse_expr(ls))
            end
        end
    end

    return If_Expr(ls, cond, tval, nil)
end

local parse_when = function(ls, let)
    if let then return nil end
    if ls.token.name == "when" then
        ls:get()
        return parse_expr(ls)
    end
end

local parse_as = function(ls, let)
    if let then return nil end
    local tok = ls.token
    if tok.name == "as" then
        ls:get()
        assert_tok(ls, "<ident>")
        local v = tok.value
        ls:get()
        return Symbol_Expr(nil, v)
    end
end

local parse_table_pattern

parse_pattern = function(ls, let)
    local tok = ls.token
    local tn  = tok.name
    if (tn == "$" or tn == "<string>" or tn == "<number>"
    or  tn == "true" or tn == "false" or tn == "nil") and not let then
        push_curline(ls)
        local exp
        if tn == "$" then
            exp = parse_expr(ls)
        else
            push_curline(ls)
            local v = tok.value or tn
            ls:get()
            exp = Value_Expr(ls, to_tag(tn), v)
        end
        return Expr_Pattern(ls, exp, parse_as(ls), parse_when(ls))
    elseif tn == "<ident>" then
        push_curline(ls)
        local v = tok.value
        ls:get()
        if tok.name == "::" then
            ls:get()
            assert_tok(ls, "<ident>")
            local v2 = tok.value
            ls:get()
            return Cons_Pattern(ls, v, v2, parse_as(ls, let),
                parse_when(ls, let))
        end
        if v == "_" then
            return Wildcard_Pattern(ls, parse_as(ls, let),
                parse_when(ls, let))
        else
            return Variable_Pattern(ls, v, parse_as(ls, let),
                parse_when(ls, let))
        end
    elseif tn == "[" then
        return parse_table_pattern(ls, let)
    else
        syntax_error(ls, "pattern expected")
    end
end

parse_table_pattern = function(ls, let)
    push_curline(ls)
    ls:get()
    local tok, tbl = ls.token, {}

    if tok.name == "]" then
        ls:get()
        return Table_Pattern(ls, {}, parse_as(ls, let), parse_when(ls, let))
    end

    local tok = ls.token
    local idx = 1
    repeat
        if tok.name == "<ident>" and ls:lookahead() == "=" then
            local name = Value_Expr(nil, TAG_STRING, tok.value)
            ls:get() ls:get()
            tbl[#tbl + 1] = { name, parse_pattern(ls, let) }
        elseif tok.name == "$" then
            local expr = parse_expr(ls)
            assert_tok(ls, "=")
            ls:get()
            tbl[#tbl + 1] = { expr, parse_pattern(ls, let) }
        else
            tbl[#tbl + 1] = { idx, parse_pattern(ls, let) }
            idx = idx + 1
        end
        if tok.name ~= "," then
            assert_tok(ls, "]")
        else
            ls:get()
        end
    until tok.name == "]"

    assert_tok(ls, "]")
    ls:get()

    return Table_Pattern(ls, tbl, parse_as(ls, let), parse_when(ls, let))
end

local Pattern_Ops = {
    ["or"] = { 1, Or_Pattern }, ["and"] = { 2, And_Pattern }
}

local parse_patternprec

local parse_subpattern = function(ls)
    local tok = ls.token.name
    if tok == "(" then
        ls:get()
        local v = parse_patternprec(ls)
        if ls.token.name ~= ")" then syntax_error(ls, "missing ')'") end
        ls:get()
        return v
    else
        return parse_pattern(ls)
    end
end

parse_patternprec = function(ls, mp)
    local curr = ls.ndstack
    local len = #curr
    push_curline(ls)
    mp = mp or 1
    local lhs = parse_subpattern(ls)
    while true do
        local cur = ls.token.name
        local t = Pattern_Ops[cur]
        if not cur or not t or t[1] < mp then break end
        ls:get()
        local rhs = parse_patternprec(ls, t[1])
        lhs = t[2](ls, lhs, rhs)
        push_curline(ls)
    end
    for i = 1, #curr - len do curr:pop() end
    return lhs
end

parse_pattern_list = function(ls, let)
    local tok, ptrns = ls.token, {}
    repeat
        ptrns[#ptrns + 1] = let and parse_pattern(ls, let)
            or parse_patternprec(ls)
    until tok.name ~= "," or not ls:get()
    return ptrns
end

parse_match_body = function(ls)
    local ret = {}
    local tok = ls.token
    assert_tok(ls, "|", "case")
    repeat
        ls:get()
        local pl, body = parse_pattern_list(ls)
        if tok.name == "{" then
            body = parse_block(ls)
        else
            assert_tok(ls, "->")
            ls:get()
            if tok.name == "(" then
                ls:get()
                local el = parse_exprlist(ls)
                assert_tok(ls, ")")
                ls:get()
                push_curline(ls)
                body = Pack_Expr(ls, el)
            else
                body = parse_expr(ls)
            end
        end
        ret[#ret + 1] = { pl, body }
    until tok.name ~= "|" and tok.name ~= "case"
    return ret
end

local parse_match = function(ls)
    push_curline(ls)
    ls:get()
    local el  = parse_exprlist(ls)
    local tok = ls.token

    local inb = false
    if tok.name == "{" then
        ls:get()
        inb = true
    else
        assert_tok(ls, "->")
        ls:get()
    end

    local body = parse_match_body(ls)
    if inb then
        assert_tok(ls, "}")
        ls:get()
    end

    return Match_Expr(ls, el, body)
end

local parse_while = function(ls)
    push_curline(ls)
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

    return While_Expr(ls, cond, body)
end

local parse_dowhile = function(ls)
    push_curline(ls)
    ls:get()
    local body
    local tok = ls.token
    if tok.name == "{" then
        body = parse_block(ls)
    else
        assert_tok(ls, "->")
        ls:get()
        body = parse_expr(ls)
    end
    assert_tok(ls, "while")
    ls:get()
    return Do_While_Expr(ls, parse_expr(ls), body)
end

local parse_for = function(ls)
    push_curline(ls)
    ls:get()
    local tok, ident, body, ids, exprs, first, last, step = ls.token
    assert_tok(ls, "<ident>")

    -- for range
    local range = (ls:lookahead() == "=")
    if range then
        ident = tok.value
        ls:get() ls:get()

        first = parse_expr(ls)
        assert_tok(ls, "..")
        ls:get()

        last = parse_expr(ls)
        if tok.name == "," then
            ls:get()
            step = parse_expr(ls)
        else
            step = Value_Expr(nil, TAG_NUMBER, 1)
        end
    else
        ids = parse_identlist(ls)
        assert_tok(ls, "in")
        ls:get()
        exprs = parse_exprlist(ls)
    end

    if tok.name == "{" then
        body = parse_block(ls)
    else
        assert_tok(ls, "->")
        ls:get()
        body = parse_expr(ls)
    end

    if range then
        return For_Range_Expr(ls, ident, first, last, step, body)
    else
        return For_Expr(ls, ids, exprs, body)
    end
end

local parse_return = function(ls)
    push_curline(ls)
    ls:get()
    if ls.token.name == "(" then
        ls:get()
        local exprs = parse_exprlist(ls)
        assert_tok(ls, ")")
        ls:get()
        return Return_Expr(ls, exprs)
    end
    return Return_Expr(ls, { parse_expr(ls) })
end

local parse_yield = function(ls)
    push_curline(ls)
    ls:get()
    if ls.token.name == "(" then
        ls:get()
        local exprs = parse_exprlist(ls)
        assert_tok(ls, ")")
        ls:get()
        return Yield_Expr(ls, exprs)
    end
    return Yield_Expr(ls, { parse_expr(ls) })
end

local parse_primaryexpr

local parse_object = function(ls)
    push_curline(ls)
    ls:get()
    local tok, parent = ls.token

    local el
    if tok.name == "(" then
        ls:get()
        el = parse_exprlist(ls, true)
        assert_tok(ls, ")")
        ls:get()
    elseif tok.name ~= "{" then
        el = { parse_expr(ls, true) }
    end

    assert_tok(ls, "{")
    ls:get()
    if tok.name == "}" then
        ls:get()
        return Object_Expr(ls, el and el or {
            Symbol_Expr(nil, "obj_def", true) }, {})
    end

    local tbl = {}
    repeat
        if tok.name == "fn" then
            tbl[#tbl + 1] = { parse_function(ls, true) }
        elseif tok.name == "<ident>" then
            local kexpr = Value_Expr(nil, TAG_STRING, tok.value)
            ls:get()
            assert_tok(ls, "=")
            ls:get()
            tbl[#tbl + 1] = { kexpr, parse_expr(ls) }
        else
            assert_tok(ls, "$")
            local kexpr = parse_expr(ls)
            assert_tok(ls, "=")
            ls:get()
            tbl[#tbl + 1] = { kexpr, parse_expr(ls) }
        end
        if tok.name == "," then
            ls:get()
        end
    until tok.name == "}"

    assert_tok(ls, "}")
    ls:get()
    return Object_Expr(ls, el and el or {
            Symbol_Expr(nil, "obj_def", true) }, tbl)
end

local parse_new = function(ls)
    push_curline(ls)
    ls:get()
    local tok = ls.token
    local pex = parse_primaryexpr(ls)
    assert_tok(ls, "(")
    ls:get()
    if tok.name == ")" then
        ls:get()
        return New_Expr(ls, pex, {})
    end
    local el = parse_exprlist(ls)
    assert_tok(ls, ")")
    ls:get()
    return New_Expr(ls, pex, el)
end

local parse_binexpr

parse_primaryexpr = function(ls)
    local tok = ls.token
    if tok.name == "(" then
        ls:get()
        local exp = parse_expr(ls)
        if tok.name ~= ")" then
            syntax_error(ls, "missing ')'")
        end
        ls:get()
        return exp
    elseif tok.name == "<ident>" then
        push_curline(ls)
        local v = tok.value
        ls:get()
        return Symbol_Expr(ls, v)
    elseif tok.name == "@" then
        push_curline(ls)
        push_curline(ls)
        ls:get()
        push_curline(ls)
        assert_tok(ls, "<ident>")
        local v = tok.value
        ls:get()
        return Index_Expr(ls, Symbol_Expr(ls, "self"),
            Value_Expr(ls, TAG_STRING, v))
    else
        syntax_error(ls, "unexpected symbol")
    end
end

local parse_suffixedexpr = function(ls)
    local tok = ls.token

    local exp = parse_primaryexpr(ls)
    while true do
        if tok.name == "." or tok.name == ":" then
            local mcall = (tok.name == ":")
            push_curline(ls)
            ls:get()
            assert_tok(ls, "<ident>")
            push_curline(ls)
            local s = tok.value
            ls:get()
            if mcall then
                push_curline(ls)
                assert_tok(ls, "(")
                ls:get()
                local el
                if tok.name == ")" then
                    ls:get()
                    el = {}
                else
                    el = parse_exprlist(ls)
                    assert_tok(ls, ")")
                    ls:get()
                end
                exp = Call_Expr(ls, Index_Expr(ls, exp,
                    Value_Expr(ls, TAG_STRING, s)), el, true)
            else
                exp = Index_Expr(ls, exp, Value_Expr(ls, TAG_STRING, s))
            end
        elseif tok.name == "[" then
            push_curline(ls)
            ls:get()
            local e = parse_expr(ls)
            assert_tok(ls, "]")
            ls:get()
            exp = Index_Expr(ls, exp, e)
        elseif tok.name == "(" then
            push_curline(ls)
            ls:get()
            local el
            if tok.name == ")" then
                ls:get()
                el = {}
            else
                el = parse_exprlist(ls)
                assert_tok(ls, ")")
                ls:get()
            end
            exp = Call_Expr(ls, exp, el)
        else
            return exp
        end
    end
end

local parse_simpleexpr = function(ls)
    local tok = ls.token
    local name = tok.name

    if name == "fn" then
        return parse_function(ls)
    elseif name == "let" then
        return parse_let(ls)
    elseif name == "seq" then
        return parse_sequence(ls)
    elseif name == "quote" then
        return parse_quote(ls)
    elseif name == "if" then
        return parse_if(ls)
    elseif name == "match" then
        return parse_match(ls)
    elseif name == "while" then
        return parse_while(ls)
    elseif name == "do" then
        return parse_dowhile(ls)
    elseif name == "for" then
        return parse_for(ls)
    elseif name == "return" then
        return parse_return(ls)
    elseif name == "yield" then
        return parse_yield(ls)
    elseif name == "clone" then
        return parse_object(ls)
    elseif name == "new" then
        return parse_new(ls)
    elseif name == "{" then
        return parse_block(ls)
    elseif name == "[" then
        if ls:lookahead() == ":" then
            return parse_list(ls)
        else
            return parse_table(ls)
        end
    elseif name == "__FILE__" then
        push_curline(ls)
        ls:get()
        return Value_Expr(ls, TAG_STRING, ls.source)
    elseif name == "__LINE__" then
        push_curline(ls)
        ls:get()
        return Value_Expr(ls, TAG_NUMBER, ls.line_number)
    elseif name == "..." then
        assert_check(ls, ls.fnstack:top().vararg,
            "cannot use '...' outside a vararg function")
        push_curline(ls)
        ls:get()
        return Vararg_Expr(ls)
    elseif Unary_Ops[name] then
        push_curline(ls)
        ls:get()
        return Unary_Expr(ls, name, parse_binexpr(ls, Unary_Ops[name]))
    elseif name == "<number>" then
        push_curline(ls)
        local v = tok.value
        ls:get()
        return Value_Expr(ls, TAG_NUMBER, v)
    elseif name == "<string>" then
        push_curline(ls)
        ls:get() -- consume the "start" token

        local levels, exprs, value = {}, { true }
        while true do
            -- potential string end? but we have to assume implicit
            -- constant concatenation
            local tn, tv = tok.name, tok.value
            if tn == "<string>" and type(tv) == "table" then
                -- append levels
                for k, v in pairs(tv) do
                    levels[k] = true
                end
                ls:get()
                -- end of the string
                if tok.name ~= "<string>" then
                    break
                end
                -- consume next "start" token
                ls:get()
            elseif tn == "$" then
                exprs[#exprs + 1] = parse_expr(ls)
                value = value and (value .. "%s") or "%s"
            else
                value = value and (value .. tv) or tv
                ls:get()
            end
        end

        local level = 0
        while levels[level] do
            level = level + 1
        end
        if #exprs > 1 then
            push_curline(ls)
            exprs[1] = Value_Expr(ls, TAG_STRING, value, level)
            return Call_Expr(ls, Symbol_Expr(nil, "str_fmt", true), exprs)
        end
        return Value_Expr(ls, TAG_STRING, value, level)
    elseif name == "nil" or name == "true" or name == "false" then
        push_curline(ls)
        ls:get()
        return Value_Expr(ls, to_tag(name), name)
    else
        return parse_suffixedexpr(ls)
    end
end

parse_binexpr = function(ls, mp)
    local curr = ls.ndstack
    local len = #curr
    push_curline(ls)
    mp = mp or 1
    local lhs = parse_simpleexpr(ls)
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

        lhs = Binary_Expr(ls, op, lhs, rhs)
        push_curline(ls)
    end
    for i = 1, #curr - len do curr:pop() end
    return lhs
end

parse_expr = function(ls)
    local tok = ls.token
    if tok.name == "$" then
        ls:get()
        if tok.name == "<ident>" then
            push_curline(ls)
            local v = tok.value
            ls:get()
            return Symbol_Expr(ls, v)
        end
        assert_tok(ls, "(")
        ls:get()
        local expr = parse_binexpr(ls)
        assert_tok(ls, ")")
        ls:get()
        return expr
    end
    return parse_binexpr(ls)
end

local parse = function(fname, reader)
    local ls = lexer.init(fname, reader)
    ls.ndstack, ls.fnstack = Stack(), Stack()
    -- global scope
    ls.fnstack:push({ vararg = false })
    ls:get()

    local ast = {}
    while ls.token.name ~= "<eos>" do
        ast[#ast + 1] = parse_expr(ls)
    end

    return ast
end

local build = function(ast)
    util.randomseed(os.clock() * os.time())

    local ms = Scope(nil, 0)

    local rts = unique_sym("rt")
    local hdr = { gen_local(rts, gen_require("rt_init")) }
    local rtcache = {}
    get_rt_fun = function(name)
        local n = rtcache[name]
        if not n then
            local sym = unique_sym("rtfn")
            hdr[#hdr + 1] = gen_local(sym, rts .. ".__vx_" .. name)
            rtcache[name] = sym
            return sym
        end
        return n
    end

    -- generate the code
    for i = 1, #ast do
        ast[i]:generate(ms, {
            statement = true
        })
    end
    local se, de = get_rt_fun("env_set"), get_rt_fun("def_env")
    hdr[#hdr + 1] = gen_call(se, gen_seq({ "1", de }))

    return concat(hdr, "\n") .. "\n" .. ms:build()
end

return {
    parse = parse,
    build = build
}
