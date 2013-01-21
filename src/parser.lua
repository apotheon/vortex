local lexer   = require("lexer")
local util    = require("util")

local M = {}

-- t[1] > t[2] == right-associative, otherwise left-associative
local Binary_Ops = {
    -- assignment ops are left out, they're right associative and have the
    -- lowest precedence; we're leaving them out of the precedence parser
    -- in order to gain flexibility and allow for pack exprs like in let.

    -- logical operators or, and
    ["or"] = { 1, 1 }, ["and"] = { 2, 2 },

    -- eq / neq comparison
    ["=="] = { 3, 3 }, ["!="] = { 3, 3 },

    -- other comparisons
    ["<" ] = { 4, 4 }, ["<="] = { 4, 4 }, [">"] = { 4, 4 },
    [">="] = { 4, 4 },

    -- concat
    ["~"] = { 6, 5 },

    -- bitwise ops
    ["bor"] = { 7,  7  }, ["bxor"] = { 8,  8  }, ["band"] = { 9,  9  },
    ["asr"] = { 10, 10 }, ["bsr" ] = { 10, 10 }, ["bsl" ] = { 10, 10 },

    -- arithmetic ops
    ["+"] = { 11, 11 }, ["-"] = { 11, 11 }, ["*"] = { 12, 12 },
    ["/"] = { 12, 12 }, ["%"] = { 12, 12 },

    -- join is left associative, cons is right associative
    ["++"] = { 13, 13 }, ["::"] = { 14, 13 },

    -- unary ops come now, but are in their own table
    -- and the last one - pow
    ["**"] = { 17, 16 }
}

local Unary_Ops = {
    ["-"  ] = 15, ["not"] = 15, ["#"  ] = 15, ["bnot"] = 15
}

local Ass_Ops = {
    ["="  ] = true, ["+=" ] = true, ["-=" ] = true, ["*=" ] = true,
    ["/=" ] = true, ["%=" ] = true, ["++="] = true, ["::="] = true,
    ["**="] = true,

    ["band="] = true, ["bor="] = true, ["bxor="] = true,
    ["asr=" ] = true, ["bsr="] = true, ["bsl=" ] = true,
}

local syntax_error = lexer.syntax_error
local case_class = util.case_class
local unique_sym = util.unique_sym
local get_syms = util.get_syms
local hash_sym = util.hash_sym
local map = util.map
local Stack = util.Stack
local serialize = util.serialize

local get_rt_fun
local lazy_rt_fun = function(name)
    return function() return get_rt_fun(name) end
end

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
    indent = 0,

    push = function(self, stat)
        if self.locked then return nil end
        local body = self.body
        body[#body + 1] = stat
        return stat
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
M.Scope = Scope

-- new function scope eliminates extra data
local new_fn_scope = function(sc, noinc)
    local ret = sc:clone()
    ret.fstate = ret
    ret.indent = noinc and sc.indent or sc.indent + 1
    ret.body, ret.locked = {}, false
    ret.data = {}
    return ret
end
M.new_fn_scope = new_fn_scope

local new_scope = function(sc, fs, noinc)
    local ret = sc:clone()
    ret.fstate = fs or sc.fstate
    ret.indent = noinc and sc.indent or sc.indent + 1
    ret.body, ret.locked = {}, false
    return ret
end
M.new_scope = new_scope

local gen_str = function(str)
    local used = {}
    for x in str:gmatch("%[(=*)%[") do
        used[#x] = true
    end
    for x in str:gmatch("%](=*)%]") do
        used[#x] = true
    end
    if str:sub(#str) == "]" then
        used[0] = true
    end
    local lvl = 0
    while true do
        if used[lvl] then lvl = lvl + 1 else break end
    end
    local ind = ("="):rep(lvl or 0)
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
    elseif op == "bor" then
        return concat { get_rt_fun("bor"), "(", lhs, ", ", rhs, ")" }
    elseif op == "bxor" then
        return concat { get_rt_fun("bxor"), "(", lhs, ", ", rhs, ")" }
    elseif op == "band" then
        return concat { get_rt_fun("band"), "(", lhs, ", ", rhs, ")" }
    elseif op == "asr" then
        return concat { get_rt_fun("arsh"), "(", lhs, ", ", rhs, ")" }
    elseif op == "bsr" then
        return concat { get_rt_fun("rsh"), "(", lhs, ", ", rhs, ")" }
    elseif op == "bsl" then
        return concat { get_rt_fun("lsh"), "(", lhs, ", ", rhs, ")" }
    elseif op == "!=" then
        op = "~="
    end
    return concat { "(", lhs, " ", op, " ", rhs, ")" }
end

local gen_unexpr = function(op, rhs)
    if op == "bnot" then
        return concat { get_rt_fun("bnot"), "(", rhs, ")" }
    end
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

local gen_dindex = function(sym, idx)
    return concat { sym, ".", idx }
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

local gen_string = function(str)
    return '"' .. str .. '"'
end

-- classes

local mskip = {
    ["__call"] = true, ["__index"] = true, ["__proto"   ] = true,
    ["dinfo" ] = true, ["name"   ] = true, ["__tostring"] = true
}

local Expr = util.Object:clone {
    name = "Expr",

    __init = function(self, ps)
        if not ps then return nil end
        if not ps.ndstack then
            self.dinfo = ps
        else
            local dinfo = ps.ndstack:pop()
            dinfo.last_line, dinfo.source = ps.line_number, ps.source
            self.dinfo = dinfo
        end
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

    to_lua = function(self, sc, kwargs)
        local params = { serialize(self.dinfo) }
        for i = 1, #self do
            local v = self[i]
            if type(v) == "table" and v.to_lua then
                params[#params + 1] = v:to_lua(sc, kwargs)
            else
                params[#params + 1] = serialize(v, nil, nil, function(v)
                    if type(v) == "table" and v.to_lua then
                        return v:to_lua(sc, kwargs), true
                    else
                        return v
                    end
                end)
            end
        end
        local slf = get_rt_fun("parser")
        return gen_index(slf, gen_string(self.name)) .. "("
            .. concat(params, ",") .. ")"
    end,

    is_ending = function(self)
        return false
    end
}
M.Expr = Expr

local Call_Expr
local Value_Expr

local gen_ctor = function(n, fun)
    return function(self, ps, ...)
        Expr.__init(self, ps)
        if fun then fun(self, ps) end
        for i = 1, n or select("#", ...) do
            self[i] = select(i, ...)
        end
    end
end

local gen_evalorder = function(expr, sc, symprefix, kwargs, cond)
    if expr:is_a(Value_Expr) or (cond == nil and true or cond) then
        return expr:generate(sc, kwargs)
    elseif kwargs.statement then
        expr:generate(sc, kwargs)
    else
        local sym = unique_sym(symprefix)
        sc:push(gen_local(sym, expr:generate(sc, kwargs)))
        return sym
    end
end

local gen_withtemp = function(expr, sc, symprefix, kwargs)
    local sym = unique_sym(symprefix)
    sc:push(gen_local(sym, expr:generate(sc, kwargs)))
    return sym
end

-- { symbol }
local Symbol_Expr = Expr:clone {
    name = "Symbol_Expr",
    __init = gen_ctor(1),

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        local r = self[1]
        return type(r) ~= "string" and r() or r
    end,

    is_lvalue = function(self)
        return true
    end
}
M.Symbol_Expr = Symbol_Expr

-- { expr, index }
local Index_Expr = Expr:clone {
    name = "Index_Expr",
    __init = gen_ctor(2),

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        return gen_index(gen_withtemp(self[1], sc, "expr", {}),
            gen_evalorder(self[2], sc, "index", {}))
    end,

    is_lvalue = function(self)
        return true
    end
}
M.Index_Expr = Index_Expr

-- { value }
Value_Expr = Expr:clone {
    name = "Value_Expr",
    __init = gen_ctor(1),

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        local v = self[1]
        if type(v) == "string" then return gen_str(v) end
        return tostring(v)
    end
}
M.Value_Expr = Value_Expr

-- {}
local Vararg_Expr = Expr:clone {
    name = "Vararg_Expr",

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        return gen_call(get_rt_fun("select"),
            gen_seq({ sc.fstate.ndefargs + 1, "..."}))
    end
}
M.Vararg_Expr = Vararg_Expr

-- { expr1, expr2, expr3, ... }
local Result_Expr = Expr:clone {
    name = "Result_Expr",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        local len, exprs = #self, {}
        for i = 1, len do
            exprs[i] = gen_evalorder(self[i], sc, "ret", kwargs, i == len)
        end
        if kwargs.return_val then
            sc:push(gen_ret(gen_seq(exprs)))
            sc:lock()
        elseif kwargs.statement then
            return nil
        else
            return gen_seq(exprs)
        end
    end,

    is_multret = function(self)
        return (#self > 1) or (self[1]:is_multret())
    end,

    is_ending = function(self)
        return true
    end
}
M.Return_Expr = Return_Expr

-- { expr1, expr2, expr3, ... }
local Return_Expr = Expr:clone {
    name = "Return_Expr",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        local len, exprs = #self, {}
        for i = 1, len do
            exprs[i] = gen_evalorder(self[i], sc, "ret", {}, i == len)
        end
        sc:push(gen_ret(gen_seq(exprs)))
        sc:lock()
    end,

    is_ending = function(self)
        return true
    end
}
M.Return_Expr = Return_Expr

-- { expr1, expr2, expr3, ... }
local Yield_Expr = Expr:clone {
    name = "Yield_Expr",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        local cy = get_rt_fun("coro_yield")
        local len, exprs = #self, {}
        for i = 1, len do
            exprs[i] = gen_evalorder(self[i], sc, "yield", {}, i == len)
        end
        if kwargs.statement then
            sc:push(gen_call(cy, gen_seq(exprs)))
        else
            return gen_call(cy, gen_seq(exprs))
        end
    end,

    is_multret = function(self)
        return (#self > 0) or (self[1]:is_multret())
    end
}
M.Yield_Expr = Yield_Expr

-- { expr1, expr2, expr3, ... }
local Do_Expr = Expr:clone {
    name = "Do_Expr",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        local len = #self
        local no_scope = kwargs.no_scope
        local scope = (not no_scope) and new_scope(sc) or sc

        for i = 1, len - 1 do
            self[i]:generate(scope, { statement = true })
        end
        local vexpr = self[len]
        -- no need to handle this for other "statements" - the parser only
        -- allows result/return at the end of the block
        if not vexpr:is_a(Result_Expr) and not vexpr:is_a(Return_Expr) then
            vexpr:generate(scope, { statement = true })
            if not no_scope then sc:push(gen_block(scope)) end
            return nil
        end

        if kwargs.return_val and vexpr:is_scoped() then
            sc:push(vexpr:generate(sc, { return_val = true }))
        elseif no_scope then
            if sc.fstate == sc or kwargs.return_val then
                sc:push(gen_ret(vexpr:generate(sc, {})))
            else
                return vexpr:generate(sc, { statement = kwargs.statement })
            end
        elseif kwargs.statement then
            scope:push(vexpr:generate(scope, { statement = true }))
            sc:push(gen_block(scope))
        else
            local sym = unique_sym("block")
            sc:push(gen_local(sym))
            if vexpr:is_multret() then
                local fsc = new_scope(scope)
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
        local vexpr = self[1]
        return vexpr and vexpr:is_multret()
    end
}
M.Do_Expr = Do_Expr

-- { { key, val }, { key, val }, ... }
local Table_Expr = Expr:clone {
    name = "Table_Expr",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end

        sc.indent = sc.indent + 1
        local kvs = {}
        local len = #self
        for i = 1, len do
            local pair = self[i]
            local ke, ve = pair[1], pair[2]
            if (type(ke) == "number") then
                kvs[#kvs + 1] = gen_evalorder(ve, sc, "arr", {}, i == len)
            elseif ke:is_a(Value_Expr) then
                kvs[#kvs + 1] = gen_ass("[" .. ke:generate(sc, {}) .. "]",
                    gen_evalorder(ve, sc, "map", {}, i == len))
            else
                local ksym = unique_sym("key")
                sc:push(gen_local(ksym, ke:generate(sc, {})))
                kvs[#kvs + 1] = gen_ass("[" .. ksym .. "]",
                    gen_evalorder(ve, sc, "map", {}, i == len))
            end
        end
        sc.indent = sc.indent - 1

        return gen_table(sc, kvs)
    end
}
M.Table_Expr = Table_Expr

-- { ex1, ex2, ex3, ... }
local List_Expr = Expr:clone {
    name = "List_Expr",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        local syms, len = {}, #self
        for i = 1, len do
            syms[i] = gen_evalorder(self[i], sc, "list", {}, i == len)
        end
        return gen_list(sc, syms)
    end
}
M.List_Expr = List_Expr

-- { parents, ctor, { name, expr }, { name, expr }, ... }
local Object_Expr = Expr:clone {
    name = "Object_Expr",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end

        local pars, ctor = self[1], self[2]

        local syms, len = {}, #pars
        for i = 1, len do
            if i == len then
                syms[i] = pars[i]:generate(sc, {})
            else
                local sym = unique_sym("parent")
                sc:push(gen_local(sym, pars[i]:generate(sc, {})))
                syms[i] = sym
            end
        end

        local fun, kvs = get_rt_fun("obj_clone"), {}

        if #ctor ~= 0 then
            local asrt = get_rt_fun("assert")
            local pars = { "self" }
            local ns   = new_scope(sc)
            ns.indent = ns.indent + 1
            for i = 1, #ctor do
                local it = ctor[i]
                local name, cond = it[1], it[2]
                pars[i + 1] = name
                if cond then
                    ns:push(gen_call(asrt, cond:generate(ns, {})))
                end
                ns:push(gen_ass(gen_dindex("self", name), name))
            end
            kvs[#kvs + 1] = gen_ass("__init", gen_fun(gen_seq(pars), ns))
        end

        for i = 3, #self do
            local pair = self[i]
            local ke, ve = pair[1], pair[2]
            if ke:is_a(Value_Expr) then
                kvs[#kvs + 1] = gen_ass("[" .. ke:generate(sc, {}) .. "]",
                    gen_evalorder(ve, sc, "obj", {}, i == len))
            else
                local ksym = unique_sym("key")
                sc:push(gen_local(ksym, ke:generate(sc, {})))
                kvs[#kvs + 1] = gen_ass("[" .. ksym .. "]",
                    gen_evalorder(ve, sc, "obj", {}, i == len))
            end
        end

        return gen_call(fun, gen_seq({ gen_table(sc, kvs), gen_seq(syms) }))
    end
}
M.Object_Expr = Object_Expr

-- { expr, ex1, ex2, ex3, ... }
local New_Expr = Expr:clone {
    name = "New_Expr",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end

        local fun = get_rt_fun("obj_new")
        local sym = unique_sym("new")
        sc:push(gen_local(sym, self[1]:generate(sc, {})))

        local len, t = #self, { sym }
        for i = 2, len do
            t[i] = gen_evalorder(self[i], sc, "ctor", {}, i == len)
        end
        return gen_call(fun, gen_seq(t))
    end
}
M.New_Expr = New_Expr

-- { op, lhs, rhs }
local Binary_Expr = Expr:clone {
    name = "Binary_Expr",
    __init = gen_ctor(3),

    generate = function(self, sc, kwargs)
        local op, lhs, rhs = self[1], self[2], self[3]
        if not Ass_Ops[op] then
            if not kwargs.statement then
                return gen_binexpr(op, lhs:generate(sc, {}),
                    rhs:generate(sc, {}))
            end
            return nil
        end

        local lel = {}
        if not lhs.name then
            for i = 1, #lhs do
                lel[i] = lhs[i]:generate(sc, {})
            end
        else
            lel[1] = lhs:generate(sc, {})
        end
        local ret = gen_seq(lel)

        if op == "=" then
            local rh
            if not rhs.name then
                local exs = {}
                for i = 1, #rhs do
                    exs[i] = rhs[i]:generate(sc, {})
                end
                rh = gen_seq(exs)
            else
                rh = rhs:generate(sc, {})
            end
            sc:push(gen_ass(ret, rh))
        else
            local bop = op:sub(1, #op - 1)

            local rel = {}
            if not rhs.name then
                for i = 1, #rhs do
                    local le, ge = lel[i], rhs[i]:generate(sc, {})
                    rel[i] = le and gen_binexpr(bop, le, ge) or ge
                end
            else
                local le, ge = lel[1], rhs:generate(sc, {})
                rel[1] = le and gen_binexpr(bop, le, ge) or ge
            end

            local rlen = #rel
            local d = #lel - rlen
            if d > 0 then
                for i = rlen + 1, rlen + d do
                    rel[i] = lel[i]
                end
            end

            sc:push(gen_ass(ret, gen_seq(rel)))
        end

        if not kwargs.statement then
            return ret
        end
    end,

    is_lvalue = function(self)
        return Ass_Ops[self[1]] and true or false
    end,

    is_multret = function(self)
        return (Ass_Ops[self[1]] and self[2]:is_multret())
    end
}
M.Binary_Expr = Binary_Expr

-- { op, rhs }
local Unary_Expr = Expr:clone {
    name = "Unary_Expr",
    __init = gen_ctor(2),

    generate = function(self, sc, kwargs)
        if kwargs.statement then return nil end
        return gen_unexpr(self[1], self[2]:generate(sc, {}))
    end
}
M.Unary_Expr = Unary_Expr

-- { args, defargs, body }
local Function_Expr = Expr:clone {
    name = "Function_Expr",

    __init = function(self, ps, params, defaults, body)
        Expr.__init(self, ps)
        ps.fnstack:pop()
        ps.lpstack:pop()
        self[1], self[2], self[3] = params, defaults, body
    end,

    generate = function(self, sc, kwargs)
        local fs = new_fn_scope(sc)

        local args,  defs  = self[1], self[2]
        local nargs, ndefs = #args, #defs

        if args[#args] == "..." then
            nargs = nargs - 1
            fs.vararg = true
        end

        fs.nargs, fs.ndefargs = nargs, ndefs
        local pargs = nargs - ndefs

        local np = {}
        for i = 1, pargs do np[#np + 1] = args[i] end
        if (ndefs > 0 or args[#args] == "...") then np[#np + 1] = "..." end

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

                local tsc = new_scope(fs)
                tsc:push(gen_ass(name, defs[i]:generate(tsc, {})))

                local fsc = new_scope(fs)
                fsc:push(gen_ass(name, gen_call(sl, gen_seq({ i, "..." }))))

                fs:push(gen_if(gen_binexpr("<", nd, i), tsc, fsc))
            end
        end

        -- avoid temps
        local body = self[3]
        if body:is_a(Do_Expr) then
            body:generate(fs, {
                no_scope = true, return_val = true
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
    end
}
M.Function_Expr = Function_Expr

-- { cond, tval, fval }
local If_Expr = Expr:clone {
    name = "If_Expr",
    __init = gen_ctor(3),

    generate = function(self, sc, kwargs)
        local stat, rval, tsc, fsc, tval, fval, tscoped, fscoped
            = kwargs.statement, kwargs.return_val

        local tv, fv = self[2], self[3]

        tscoped = tv:is_scoped()
        tsc  = new_scope(sc)
        tval = tv:generate(tsc, {
            no_scope  = true, statement = stat, return_val = rval
        })

        if fv then
            fscoped = fv:is_scoped()
            fsc  = new_scope(sc)
            fval = fv:generate(fsc, {
                no_scope  = true, statement = stat, return_val = rval
            })
        end

        if stat or rval then
            tsc:push((rval and not tscoped) and gen_ret(tval) or tval)
            if fsc then fsc:push((rval and not fscoped)
                and gen_ret(fval) or fval) end

            sc:push(gen_if(self[1]:generate(sc, {}), tsc, fsc))
        else
            local sym = unique_sym("if")

            sc:push(gen_local(sym))
            if self:is_multret() then
                local tfsc = new_scope(tsc)
                tfsc:push(gen_ret(tval))
                tsc:push(gen_ass(sym, gen_fun("", tfsc)))
                if fv then
                    local ffsc = new_scope(fsc)
                    ffsc:push(gen_ret(fval))
                    fsc:push(gen_ass(sym, gen_fun("", ffsc)))
                end
                sc:push(gen_if(self[1]:generate(sc, {}), tsc, fsc))
                return gen_call(sym, "")
            else
                tsc:push(gen_ass(sym, tval))
                if fsc then fsc:push(gen_ass(sym, fval)) end
                sc:push(gen_if(self[1]:generate(sc, {}), tsc, fsc))
                return sym
            end
        end
    end,

    is_scoped = function(self)
        return true
    end,

    is_multret = function(self)
        local fv = self[3]
        return self[2]:is_multret() or (fv and fv:is_multret())
    end
}
M.If_Expr = If_Expr

-- { cond, pattern }
local When_Pattern = Expr:clone {
    name = "When_Pattern",
    __init = gen_ctor(2),

    generate = function(self, sc, kwargs)
        local v = self[2]:generate(sc, kwargs)
        local ts = new_scope(sc)
        ts:push(gen_goto(kwargs.next_arm))
        sc:push(gen_if(gen_unexpr("not", self[1]:generate(sc, {})), ts))
        return v
    end
}
M.When_Pattern = When_Pattern

-- { cond, pattern }
local Unless_Pattern = Expr:clone {
    name = "Unless_Pattern",
    __init = gen_ctor(2),

    generate = function(self, sc, kwargs)
        local v = self[2]:generate(sc, kwargs)
        local ts = new_scope(sc)
        ts:push(gen_goto(kwargs.next_arm))
        sc:push(gen_if(self[1]:generate(sc, {}), ts))
        return v
    end
}
M.Unless_Pattern = Unless_Pattern

-- { name, pattern }
local As_Pattern = Expr:clone {
    name = "When_Pattern",
    __init = gen_ctor(2),

    generate = function(self, sc, kwargs)
        local v = self[2]:generate(sc, kwargs)
        sc:push(gen_local(self[1], kwargs.expr))
        return v
    end
}
M.As_Pattern = As_Pattern

-- { lhs, rhs }
local And_Pattern = Expr:clone {
    name = "And_Pattern",
    __init = gen_ctor(2),

    generate = function(self, sc, kwargs)
        return gen_binexpr("and", self[1]:generate(sc, kwargs) or "nil",
                                  self[2]:generate(sc, kwargs) or "nil")
    end
}
M.And_Pattern = And_Pattern

-- { lhs, rhs }
local Or_Pattern = Expr:clone {
    name = "Or_Pattern",
    __init = gen_ctor(2),

    generate = function(self, sc, kwargs)
        return gen_binexpr("or", self[1]:generate(sc, kwargs) or "nil",
                                 self[2]:generate(sc, kwargs) or "nil")
    end
}
M.Or_Pattern = Or_Pattern

-- { expr }
local Expr_Pattern = Expr:clone {
    name = "Expr_Pattern",
    __init = gen_ctor(1),

    generate = function(self, sc, kwargs)
        return gen_binexpr("==", self[1]:generate(sc, {}), kwargs.expr)
    end
}
M.Expr_Pattern = Expr_Pattern

-- { var }
local Variable_Pattern = Expr:clone {
    name = "Variable_Pattern",
    __init = gen_ctor(1),

    generate = function(self, sc, kwargs)
        local var = self[1]
        if kwargs.decl then
            sc:push(gen_local(var))
            return nil
        elseif kwargs.no_local then
            sc:push(gen_ass(var, kwargs.expr))
        else
            sc:push(gen_local(var, kwargs.expr))
        end
        if kwargs.let then return var end
    end
}
M.Variable_Pattern = Variable_Pattern

-- {}
local Wildcard_Pattern = Expr:clone {
    name = "Wildcard_Pattern",
    generate = function(self, sc, kwargs)
    end
}
M.Wildcard_Pattern = Wildcard_Pattern

-- { { key, pattern }, { key, pattern }, ... }
local Table_Pattern = Expr:clone {
    name = "Table_Pattern",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        local tfun = get_rt_fun("type")
        local expr = kwargs.expr
        local mn, ret = 0

        if kwargs.decl then
            for i = 1, #self do
                self[i][2]:generate(sc, { decl = true })
            end
            return nil
        elseif kwargs.let then
            local ret = {}
            for i = 1, #self do
                local it = self[i]
                local k, v = it[1], it[2]
                if type(k) == "number" then
                    mn = mn + 1
                else
                    k = k:generate(sc, {})
                end
                local el = gen_index(expr, k)
                ret[i] = v:generate(sc, { expr = el, let = true,
                    no_local = kwargs.no_local })
            end
            return gen_seq(ret)
        end

        local ns = new_scope(sc, nil, true)
        for i = 1, #self do
            local it = self[i]
            local k, v = it[1], it[2]
            if type(k) == "number" then
                mn = mn + 1
            else
                k = k:generate(sc, {})
            end
            local el = gen_index(expr, k)
            local pv = v:generate(ns, { expr = el,
                next_arm = kwargs.next_arm })
            ret = ret and gen_binexpr("and", ret, pv) or pv
        end

        local ts = new_scope(sc)
        ts:push(gen_goto(kwargs.next_arm))
        sc:push(gen_if(gen_binexpr("or",
            gen_binexpr("!=", gen_call(tfun, expr), gen_str("table")),
            gen_binexpr("!=", gen_unexpr("#", expr), mn)), ts))
        sc:merge(ns)

        return ret
    end
}
M.Table_Pattern = Table_Pattern

-- { expr, { key, pattern }, { key, pattern }, ... }
local Object_Pattern = Expr:clone {
    name = "Object_Pattern",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        if kwargs.decl then
            local exs = {}
            for i = 2, #self do
                exs[i] = self[i][1]:generate(sc, {})
            end
            sc:push(gen_local(gen_seq(exs)))
            return nil
        elseif kwargs.let then
            local expr, exs = kwargs.expr, {}
            for i = 1, #self do
                local it = self[i]
                local n, k = it[1]:generate(sc, {}), it[2]:generate(sc, {})
                sc:push(gen_local(n, gen_index(expr, k)))
                exs[i] = n
            end
            return gen_seq(exs)
        end

        local ns = new_scope(sc, nil, true)
        local expr, nl = kwargs.expr, kwargs.no_local
        for i = 2, #self do
            local it = self[i]
            local n, k = it[1]:generate(sc, {}), it[2]:generate(sc, {})
            ns:push((nl and gen_ass or gen_local)(n, gen_index(expr, k)))
        end

        local tfun, isafun = get_rt_fun("type"), get_rt_fun("obj_is_a")
        local ts = new_scope(sc)
        ts:push(gen_goto(kwargs.next_arm))
        sc:push(gen_if(gen_binexpr("or",
            gen_binexpr("!=", gen_call(tfun, expr), gen_str("table")),
            gen_unexpr("not", gen_call(isafun, gen_seq({ expr,
                self[1]:generate(sc, {}) })))), ts))
        sc:merge(ns)
    end
}
M.Object_Pattern = Object_Pattern

-- { head, tail }
local Cons_Pattern = Expr:clone {
    name = "Cons_Pattern",
    __init = gen_ctor(2),

    generate = function(self, sc, kwargs)
        local tfun = get_rt_fun("type")
        local first, rest = get_rt_fun("list_first"), get_rt_fun("list_rest")
        local head, tail, expr = self[1], self[2], kwargs.expr

        if kwargs.decl then
            head:generate(sc, { decl = true })
            tail:generate(sc, { decl = true })
            return nil
        elseif kwargs.let then
            local sym = unique_sym("cons")
            local hsym, tsym = sym .. "_h", sym .. "_t"
            sc:push(gen_local(hsym, gen_call(first, expr)))
            sc:push(gen_local(tsym, gen_call(rest, expr)))
            local h  = head:generate(sc, { let = true, expr = hsym })
            local t  = tail:generate(sc, { let = true, expr = tsym })

            return gen_seq({ h, t })
        end

        local ts = new_scope(sc)
        ts:push(gen_goto(kwargs.next_arm))
        sc:push(gen_if(gen_binexpr("!=", gen_call(tfun, expr),
            gen_str("table")), ts))
        local narm = kwargs.next_arm
        local sym = unique_sym("cons")
        local hsym, tsym = sym .. "_h", sym .. "_t"
        sc:push(gen_local(hsym, gen_call(first, expr)))
        sc:push(gen_local(tsym, gen_call(rest, expr)))
        head:generate(sc, { expr = hsym, next_arm = narm })
        tail:generate(sc, { expr = tsym, next_arm = narm })
    end
}
M.Cons_Pattern = Cons_Pattern

-- { { ex1, ex2, ex3, ... }, { { pt1, pt2, pt3, ... }, expr }, ... }
local Match_Expr = Expr:clone {
    name = "Match_Expr",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        local stat, rval = kwargs.statement, kwargs.return_val
        local narms = #self

        -- find the longest arm
        local alen = 1
        for i = 2, narms do
            local l = #self[i][1]
            if l > alen then alen = l end
        end

        local el = self[1]
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
        for i = 2, narms do
            local arm = self[i]
            local pl, bd = arm[1], arm[2]
            local ptrns = {}

            local asc = new_scope(sc)
            sc:push(gen_label(armlb))
            armlb = (i ~= narms) and unique_sym("lbl") or elb
            local n = 1
            for i = 1, #pl do
                local pt = pl[i]
                local expr = exps[i] or "nil"
                local v = pt:generate(asc, {
                    expr = expr, next_arm = armlb
                })
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
                local csc = new_scope(asc)
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
                local fsc = new_scope(asc)
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
        for i = 2, #self do
            if self[i][2]:is_multret() then return true end
        end
        return false
    end
}
M.Match_Expr = Match_Expr

-- { type, { pattern1, pattern2, pattern3, ... }, { ex1, ex2, ex3, ... } }
local Let_Expr = Expr:clone {
    name = "Let_Expr",
    __init = gen_ctor(3),

    generate = function(self, sc, kwargs)
        local ptrns, assign, syms = self[2], self[3], {}
        local len, plen = #assign, #ptrns
        local tp = self[1]

        -- generate declarations
        if tp == "rec" then
            for i = 1, plen do
                ptrns[i]:generate(sc, { decl = true, let = true })
            end
        end

        for i = 1, len - 1 do
            syms[i] = gen_evalorder(assign[i], sc, "let", {})
        end
        local last = assign[len]
        if last:is_multret() then
            local lseq
            for i = len, plen do
                local sym = unique_sym("let")
                lseq = lseq and gen_seq({ lseq, sym }) or sym
                syms[i] = sym
            end
            sc:push(gen_local(lseq, last:generate(sc, {})))
        else
            syms[len] = gen_evalorder(last, sc, "let", {})
        end

        local rids = {}
        for i = 1, plen do
            rids[#rids + 1] = ptrns[i]:generate(sc, {
                expr = syms[i], let = true, no_local = tp ~= "def"
            })
        end

        if not kwargs.statement then
            return gen_seq(rids)
        end
    end
}
M.Let_Expr = Let_Expr

-- { type, { pt1, pt2, pt3, ... }, { ex1, ex2, ex3, ... }, body }
local With_Expr = Expr:clone {
    name = "With_Expr",
    __init = gen_ctor(4),

    generate = function(self, sc, kwargs)
        local scope = new_scope(sc)
        Let_Expr.generate(self, scope, {})

        local stat, rval = kwargs.statement, kwargs.return_val
        local body = self[4]

        local bscoped = body:is_scoped()
        local val = body:generate(scope, {
            no_scope = true, statement = stat, return_val = rval
        })

        if stat or rval then
            scope:push((rval and not bscoped) and gen_ret(val) or val)
            sc:push(gen_block(scope))
        else
            local sym = unique_sym("with")
            sc:push(gen_local(sym))
            if self:is_multret() then
                local fsc = new_scope(scope)
                fsc:push(gen_ret(val))
                scope:push(gen_ass(sym, gen_fun("", fsc)))
                sc:push(gen_block(scope))
                return gen_call(sym, "")
            else
                scope:push(gen_ass(sym, val))
                sc:push(gen_block(scope))
                return sym
            end
        end
    end
}
M.With_Expr = With_Expr

-- { precond, postcond, body }
local Loop_Expr = Expr:clone {
    name = "Loop_Expr",
    __init = gen_ctor(3, function(self, ps) ps.lpstack:pop() end),

    generate = function(self, sc, kwargs)
        local bsc = new_scope(sc)

        local lbl = unique_sym("lbl")
        local lbeg, lend = lbl .. "_beg", lbl .. "_end"
        bsc.data = {
            loop_start = lbeg,
            loop_inc   = lbeg,
            loop_end   = lend
        }
        bsc:push(gen_label(lbeg))

        local prec = self[1]
        if prec then
            local tsc = new_scope(bsc)
            tsc:push(gen_goto(lend))
            bsc:push(gen_if(gen_unexpr("not", prec:generate(bsc, {})), tsc))
        end

        self[3]:generate(bsc, { statement = true, no_scope = true })

        local postc = self[2]
        if postc then
            local tsc = new_scope(bsc)
            tsc:push(gen_goto(lbeg))
            bsc:push(gen_if(postc:generate(bsc, {}), tsc))
        else
            bsc:push(gen_goto(lbeg))
        end

        bsc:push(gen_label(lend))
        sc:push(gen_block(bsc))
    end,

    is_scoped = function(self)
        return true
    end
}
M.Loop_Expr = Loop_Expr

-- { idents, exprs, body }
local For_Expr = Expr:clone {
    name = "For_Expr",
    __init = gen_ctor(3, function(self, ps) ps.lpstack:pop() end),

    generate = function(self, sc, kwargs)
        local bsc = new_scope(sc)

        local fsym, ssym, varsym
            = unique_sym("f"), unique_sym("s"), unique_sym("var")

        local lbl = unique_sym("lbl")
        local lbeg, linc, lend = lbl .. "_beg", lbl .. "_inc", lbl .. "_end"
        bsc.data = {
            loop_start = lbeg,
            loop_inc   = linc,
            loop_end   = lend
        }

        local exps = {}
        local el = self[2]
        for i = 1, #el do
            exps[i] = el[i]:generate(bsc, {})
        end
        bsc:push(gen_local(gen_seq({ fsym, ssym, varsym }), gen_seq(exps)))
        bsc:push(gen_label(linc))

        local ids = self[1]
        bsc:push(gen_local(gen_seq(ids), gen_call(fsym,
            gen_seq({ ssym, varsym }))))

        bsc:push(gen_label(lbeg))
        local tsc = new_scope(bsc)
        tsc:push(gen_goto(lend))
        bsc:push(gen_if(gen_binexpr("==", ids[1], "nil"), tsc))
        bsc:push(gen_ass(varsym, ids[1]))
        self[3]:generate(bsc, { statement = true, no_scope = true })
        bsc:push(gen_goto(linc))
        bsc:push(gen_label(lend))

        sc:push(gen_block(bsc))
    end,

    is_scoped = function(self)
        return true
    end
}
M.For_Expr = For_Expr

-- { ident, first, last, step, body }
local For_Range_Expr = Expr:clone {
    name = "For_Range_Expr",
    __init = gen_ctor(5, function(self, ps) ps.lpstack:pop() end),

    generate = function(self, sc, kwargs)
        local bsc = new_scope(sc)
        local tonum = get_rt_fun("tonum")
        local rterr = get_rt_fun("error")

        local varsym, limsym, stepsym
            = unique_sym("var"), unique_sym("lim"), unique_sym("step")

        local lbl = unique_sym("lbl")
        local lbeg, linc, lend = lbl .. "_beg", lbl .. "_inc", lbl .. "_end"
        bsc.data = {
            loop_start = lbeg,
            loop_inc   = linc,
            loop_end   = lend
        }

        local ex1, ex2, ex3 = self[2]:generate(bsc, {}),
            self[3]:generate(bsc, {}), self[4]:generate(bsc, {})

        bsc:push(gen_local(gen_seq({ varsym, limsym, stepsym }),
            gen_seq({ gen_call(tonum, ex1),
                      gen_call(tonum, ex2),
                      gen_call(tonum, ex3) })))

        local ivs, lms, sts = new_scope(bsc),
            new_scope(bsc), new_scope(bsc)

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

        local tsc = new_scope(bsc)
        tsc:push(gen_goto(lend))
        bsc:push(gen_if(gen_unexpr("not", gen_binexpr("or",
            gen_binexpr("and",
                gen_binexpr(">", stepsym, "0"),
                gen_binexpr("<=", varsym, limsym)),
            gen_binexpr("and",
                gen_binexpr("<=", stepsym, "0"),
                gen_binexpr(">=", varsym, limsym)
            ))), tsc))
        bsc:push(gen_local(self[1], varsym))
        self[5]:generate(bsc, { statement = true, no_scope = true })
        bsc:push(gen_label(linc))
        bsc:push(gen_ass(varsym, gen_binexpr("+", varsym, stepsym)))
        bsc:push(gen_goto(lbeg))
        bsc:push(gen_label(lend))

        sc:push(gen_block(bsc))
    end,

    is_scoped = function(self)
        return true
    end
}
M.For_Range_Expr = For_Range_Expr

-- {}
local Break_Expr = Expr:clone {
    name = "Break_Expr",

    generate = function(self, sc, kwargs)
        sc:push(gen_goto(sc.data.loop_end))
    end
}
M.Break_Expr = Break_Expr

-- {}
local Cycle_Expr = Expr:clone {
    name = "Cycle_Expr",

    generate = function(self, sc, kwargs)
        sc:push(gen_goto(sc.data.loop_inc))
    end
}
M.Cycle_Expr = Cycle_Expr

-- {}
local Redo_Expr = Expr:clone {
    name = "Redo_Expr",

    generate = function(self, sc, kwargs)
        sc:push(gen_goto(sc.data.loop_start))
    end
}
M.Redo_Expr = Redo_Expr

-- { expr }
local Seq_Expr = Expr:clone {
    name = "Seq_Expr",
    __init = gen_ctor(1, function(self, ps)
        ps.fnstack:pop()
        ps.lpstack:pop()
    end),

    generate = function(self, sc, kwargs)
        local sq, cc = get_rt_fun("seq_create"), get_rt_fun("coro_create")
        local fs, body = new_scope(sc), self[1]
        if body:is_a(Do_Expr) then
            body:generate(fs, {
                no_scope = true, return_val = true
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
    end
}
M.Seq_Expr = Seq_Expr

-- { expr }
local Quote_Expr = Expr:clone {
    name = "Quote_Expr",
    __init = gen_ctor(1),

    generate = function(self, sc, kwargs)
        return self[1]:to_lua(sc, kwargs)
    end
}
M.Quote_Expr = Quote_Expr

-- { expr }
local Unquote_Expr = Expr:clone {
    name = "Unquote_Expr",
    __init = gen_ctor(1),

    generate = function(self, sc, kwargs)
        return self[1]:generate(sc, kwargs)
    end,

    to_lua = function(self, sc, kwargs)
        local slf = get_rt_fun("parser")
        return gen_index(slf, gen_string("Value_Expr")) .. "("
            .. serialize(self.dinfo) .. ", "
            .. self[1]:generate(sc, kwargs) .. ")"
    end
}
M.Unquote_Expr = Unquote_Expr

-- { { name, expr }, { name, expr }, { name, expr }, ... }
local Enum_Expr = Expr:clone {
    name = "Enum_Expr",
    __init = gen_ctor(),

    __init = function(self, ps, enum)
        Expr.__init(self, ps)
        self.enum = enum
    end,

    generate = function(self, sc, kwargs)
        local sym = kwargs.assign
        if sym then
            sc:push(gen_ass(sym, gen_table(sc, {})))
        else
            sym = unique_sym("enum")
            sc:push(gen_local(sym, gen_table(sc, {})))
        end

        local isym, incr = unique_sym("enumi"), 1
        local it = self[1]
        local name, expr = it[1], it[2]
        if expr then
            sc:push(gen_local(isym, expr:generate(sc, {})))
        else
            sc:push(gen_local(isym, incr - 1))
        end
        sc:push(gen_ass(gen_index(sym, gen_string(name)), isym))
        for i = 2, #self do
            local it = self[i]
            local name, expr = it[1], it[2]
            if expr then
                sc:push(gen_ass(isym, expr:generate(sc, {})))
                incr = 0
            end
            sc:push(gen_ass(gen_index(sym, gen_string(name)),
                gen_binexpr("+", isym, incr)))
            incr = incr + 1
        end
        return sym
    end
}
M.Enum_Expr = Enum_Expr

-- { mcall, expr, ex1, ex2, ex3, ... }
Call_Expr = Expr:clone {
    name = "Call_Expr",
    __init = gen_ctor(),

    generate = function(self, sc, kwargs)
        local syms = {}
        local mcall, len = self[1], #self

        local expr, n = nil, 0
        if mcall then
            local sym = unique_sym("self")
            sc:push(gen_local(sym, mcall:generate(sc, {})))
            syms[1], expr = sym, gen_index(sym, self[2]:generate(sc, {}))
            n = 1
        else
            expr = self[2]:generate(sc, {})
        end

        for i = 3, len do
            syms[i + n - 2] = gen_evalorder(self[i], sc, "arg", {}, i == len)
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
    end
}
M.Call_Expr = Call_Expr

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

parse_exprlist = function(ls, lv)
    local tok, exprs = ls.token, {}
    repeat
        local ex = parse_expr(ls)
        if lv and not ex:is_lvalue() then
            syntax_error(ls, "expected lvalue")
        end
        exprs[#exprs + 1] = ex
    until tok.name ~= "," or not ls:get()
    return exprs
end

local endargs = { ["<ident>"] = true, ["..."] = true }
local parse_arglist = function(ls, first)
    local tok = ls.token
    local tn  = tok.name

    if tn == "->" or tn == "do" or tn ~= "<ident>" then
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

local parse_table = function(ls)
    push_curline(ls)
    ls:get()
    local tok, tbl = ls.token, {}

    if tok.name == "]" then
        ls:get()
        return Table_Expr(ls)
    end

    local idx = 1
    repeat
        if tok.name == "<ident>" and ls:lookahead() == ":" then
            local name = Value_Expr(nil, tok.value)
            ls:get() ls:get()
            tbl[#tbl + 1] = { name, parse_expr(ls) }
        elseif tok.name == "$" or tok.name == "$(" then
            local expr = parse_expr(ls)
            if tok.name == ":" then
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

    return Table_Expr(ls, unpack(tbl))
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
        return List_Expr(ls)
    end

    local el = parse_exprlist(ls)

    assert_tok(ls, ":")
    ls:get()
    assert_tok(ls, "]")
    ls:get()
    return List_Expr(ls, unpack(el))
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
    ls.lpstack:push(false)

    if tok.name ~= "do" then
        assert_tok(ls, "->")
        ls:get()
    end

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
            Match_Expr(ls, el, unpack(parse_match_body(ls))))
    else
        fnexpr = Function_Expr(ls, ids, defs, parse_expr(ls))
    end

    if name then
        if obj then
            return Value_Expr(ls, name), fnexpr
        elseif tbl then
            return Binary_Expr(ls, "=", Index_Expr(ls,
                Symbol_Expr(ls, tbl), Value_Expr(ls, name)),
                    fnexpr)
        else
            return Let_Expr(ls, ltype or "def",
                { Variable_Pattern(ls, name) }, { fnexpr })
        end
    end
    return fnexpr
end

local parse_let_with = function(ls, with)
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

    if with then
        if tok.name ~= "do" then
            assert_tok(ls, "->")
            ls:get()
        end
        return With_Expr(ls, ltype or "def", ptrns, exprs, parse_expr(ls))
    end

    return Let_Expr(ls, ltype or "def", ptrns, exprs)
end

local parse_set = function(ls)
    local tok = ls.token
    push_curline(ls)
    ls:get()

    local lhs
    if tok.name == "(" then
        ls:get()
        lhs = parse_exprlist(ls, true)
        assert_tok(ls, ")")
        ls:get()
    else
        lhs = parse_expr(ls)
        if not lhs:is_lvalue() then
            syntax_error(ls, "expected lvalue")
        end
    end

    local op = tok.name
    if not Ass_Ops[op] then
        syntax_error(ls, "unexpected symbol")
    end
    ls:get()

    local rhs
    if tok.name == "(" then
        ls:get()
        rhs = parse_exprlist(ls)
        assert_tok(ls, ")")
        ls:get()
    else
        rhs = parse_expr(ls)
    end

    return Binary_Expr(ls, op, lhs, rhs)
end

local parse_sequence = function(ls)
    push_curline(ls)
    ls:get()
    ls.fnstack:push({ vararg = false })
    ls.lpstack:push(false)

    if ls.token.name ~= "do" then
        assert_tok(ls, "->")
        ls:get()
    end

    return Seq_Expr(ls, parse_expr(ls))
end

local parse_quote = function(ls)
    push_curline(ls)
    ls:get()

    if ls.token.name ~= "do" then
        assert_tok(ls, "->")
        ls:get()
    end

    return Quote_Expr(ls, parse_expr(ls))
end

local parse_unquote = function(ls)
    push_curline(ls)
    ls:get()

    if ls.token.name ~= "do" then
        assert_tok(ls, "->")
        ls:get()
    end

    return Unquote_Expr(ls, parse_expr(ls))
end

local parse_enum = function(ls)
    push_curline(ls)
    ls:get()

    local tok = ls.token
    assert_tok(ls, "->")
    ls:get()
    assert_tok(ls, "(")
    ls:get()

    local t = {}
    repeat
        assert_tok(ls, "<ident>")
        local name = tok.value
        ls:get()
        if tok.name == "=" then
            ls:get()
            t[#t + 1] = { name, parse_expr(ls) }
        else
            t[#t + 1] = { name, nil }
        end
    until tok.name ~= "," or ls:get() ~= "<ident>"

    assert_tok(ls, ")")
    ls:get()

    return Enum_Expr(ls, unpack(t))
end

local parse_if = function(ls)
    push_curline(ls)
    ls:get()
    local cond = parse_expr(ls)
    local tok  = ls.token

    if tok.name ~= "do" then
        assert_tok(ls, "->")
        ls:get()
    end
    local tval = parse_expr(ls)

    if tok.name == "else" then
        ls:get()
        if tok.name == "->" then ls:get() end
        return If_Expr(ls, cond, tval, parse_expr(ls))
    end

    return If_Expr(ls, cond, tval, nil)
end

local parse_compound_pattern = function(ls, let)
    local tok, tbl, idx = ls.token, {}, 1
    repeat
        if tok.name == "<ident>" and ls:lookahead() == ":" then
            local name = Value_Expr(nil, tok.value)
            ls:get() ls:get()
            tbl[#tbl + 1] = { name, parse_pattern(ls, let) }
        elseif tok.name == "$" or tok.name == "$(" then
            local expr = parse_expr(ls)
            assert_tok(ls, ":")
            ls:get()
            tbl[#tbl + 1] = { expr, parse_pattern(ls, let) }
        else
            tbl[#tbl + 1] = { idx, parse_pattern(ls, let) }
            idx = idx + 1
        end
    until tok.name ~= "," or not ls:get()
    return tbl
end

local parse_table_pattern = function(ls, let)
    push_curline(ls)
    ls:get()
    local tok = ls.token

    if tok.name == "]" then
        ls:get()
        return Table_Pattern(ls)
    end

    local tbl = parse_compound_pattern(ls, let)
    assert_tok(ls, "]")
    ls:get()

    return Table_Pattern(ls, unpack(tbl))
end

local parse_object_pattern = function(ls)
    local tok, tbl = ls.token, {}
    repeat
        assert_tok(ls, "<ident>")
        local name = tok.value
        ls:get()
        local ex
        if tok.name == ":" then
            ls:get()
            if tok.name == "$" or tok.name == "$(" then
                ex = parse_expr(ls)
            else
                assert_tok(ls, "<ident>")
                ex = Value_Expr(nil, tok.value)
                ls:get()
            end
        end
        tbl[#tbl + 1] = { Symbol_Expr(nil, name),
            ex or Value_Expr(nil, name) }
    until tok.name ~= "," or not ls:get()
    return tbl
end

-- cons pattern is right associative
local Pattern_Ops = {
    ["or"] = { 1, 1, Or_Pattern }, ["and"] = { 2, 2, And_Pattern },
    ["::"] = { 4, 3, Cons_Pattern }
}

local parse_suffixedpattern
local parse_primarypattern = function(ls, let)
    local tok = ls.token
    local tn = tok.name
    if tn == "(" then
        ls:get()
        local pt = parse_pattern(ls, let)
        if tok.name ~= ")" then
            syntax_error(ls, "missing ')'")
        end
        ls:get()
        -- cons pattern is a binary pattern, but it can have when/unless/as
        -- too, so handle it here (only when in parens, we don't want any
        -- ambiguity)
        if pt:is_a(Cons_Pattern) then
            pt = parse_suffixedpattern(ls, let, pt)
        end
        return pt
    elseif tn == "<string>" or tn == "<number>" or tn == "true"
    or tn == "false" or tn == "nil" and not let then
        push_curline(ls)
        push_curline(ls)
        local v = tok.value or tn
        ls:get()
        if tn == "<number>" then
            v = tonumber(v)
        elseif tn == "true" then
            v = true
        elseif tn == "false" then
            v = false
        elseif tn == "nil" then
            v = nil
        end
        return Expr_Pattern(ls, Value_Expr(ls, v))
    elseif tn == "$" or tn == "$(" and not let then
        push_curline(ls)
        local exp = parse_expr(ls)
        if tok.name == "(" then
            ls:get()
            local tbl = (tok.name == ")") and {} or parse_object_pattern(ls)
            assert_tok(ls, ")")
            ls:get()
            return Object_Pattern(ls, exp, unpack(tbl))
        end
        return Expr_Pattern(ls, exp)
    elseif tn == "<ident>" then
        push_curline(ls)
        push_curline(ls)
        local v = tok.value
        ls:get()
        if v == "_" then
            ls.ndstack:pop()
            return Wildcard_Pattern(ls)
        elseif tok.name == "(" then
            ls:get()
            local tbl = (tok.name == ")") and {} or parse_object_pattern(ls)
            assert_tok(ls, ")")
            ls:get()
            return Object_Pattern(ls, Symbol_Expr(ls, v), unpack(tbl))
        else
            ls.ndstack:pop()
            return Variable_Pattern(ls, v)
        end
    elseif tn == "[" then
        return parse_table_pattern(ls, let)
    else
        syntax_error(ls, "pattern expected")
    end
end

parse_suffixedpattern = function(ls, let, pt)
    local tok = ls.token
    local pat = pt or parse_primarypattern(ls, let)
    local hasas, haswhen, hasunless = false, false, false
    while true do
        if tok.name == "when" and not haswhen and not let then
            push_curline(ls)
            ls:get()
            pat, haswhen = When_Pattern(ls, parse_expr(ls), pat), true
        elseif tok.name == "unless" and not hasunless and not let then
            push_curline(ls)
            ls:get()
            pat, hasunless = Unless_Pattern(ls, parse_expr(ls), pat), true
        elseif tok.name == "as" and not hasas then
            push_curline(ls)
            ls:get()
            assert_tok(ls, "<ident>")
            local v = tok.value
            ls:get()
            pat, hasas = As_Pattern(ls, v, pat), true
        else
            return pat
        end
    end
end

local parse_patternprec
parse_patternprec = function(ls, mp, let)
    local curr = ls.ndstack
    local len = #curr
    push_curline(ls)
    mp = mp or 1
    local lhs = parse_suffixedpattern(ls, let)
    while true do
        local cur = ls.token.name
        if let and cur ~= "::" then break end
        local t = Pattern_Ops[cur]
        if not cur or not t or t[1] < mp then break end
        ls:get()
        local p1, p2 = t[1], t[2]
        local rhs = parse_patternprec(ls, p1 > p2 and p1 or p2, let)
        lhs = t[3](ls, lhs, rhs)
        push_curline(ls)
    end
    for i = 1, #curr - len do curr:pop() end
    return lhs
end

parse_pattern = function(ls, let)
    return parse_patternprec(ls, nil, let)
end

parse_pattern_list = function(ls, let)
    local tok, ptrns = ls.token, {}
    repeat
        ptrns[#ptrns + 1] = parse_pattern(ls, let)
    until tok.name ~= "," or not ls:get()
    return ptrns
end

parse_match_body = function(ls)
    local ret = {}
    local tok = ls.token
    assert_tok(ls, "|", "case")
    repeat
        ls:get()
        local pl = parse_pattern_list(ls)
        if tok.name ~= "do" then
            assert_tok(ls, "->")
            ls:get()
        end
        ret[#ret + 1] = { pl, parse_expr(ls) }
    until tok.name ~= "|" and tok.name ~= "case"
    return ret
end

local parse_match = function(ls)
    push_curline(ls)
    ls:get()
    local el  = parse_exprlist(ls)
    local tok = ls.token

    assert_tok(ls, "->")
    ls:get()
    return Match_Expr(ls, el, unpack(parse_match_body(ls)))
end

local parse_do = function(ls)
    push_curline(ls)
    ls:get()
    local tok, exprs = ls.token, {}

    if tok.name == ";;" or tok.name == "end" then
        ls:get()
        return Do_Expr(ls)
    end

    while true do
        local ex = parse_expr(ls)
        exprs[#exprs + 1] = ex
        if tok.name == ";" then
            ls:get()
        elseif ex:is_ending() or tok.name == ";;" or tok.name == "end" then
            break
        end
    end

    assert_tok(ls, ";;", "end")
    ls:get()
    return Do_Expr(ls, unpack(exprs))
end

local parse_loop = function(ls)
    push_curline(ls)
    ls:get()
    local tok = ls.token

    local precond
    if tok.name == "while" then
        ls:get()
        precond = parse_expr(ls)
    end

    if ls.token.name ~= "do" then
        assert_tok(ls, "->")
        ls:get()
    end

    ls.lpstack:push(true)
    local body = parse_expr(ls)

    local postcond
    if tok.name == "while" then
        ls:get()
        postcond = parse_expr(ls)
    end

    return Loop_Expr(ls, precond, postcond, body)
end

local parse_for = function(ls)
    push_curline(ls)
    ls:get()
    local tok, ident, ids, exprs, first, last, step = ls.token
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
            step = Value_Expr(nil, 1)
        end
    else
        ids = parse_identlist(ls)
        assert_tok(ls, "in")
        ls:get()
        exprs = parse_exprlist(ls)
    end

    ls.lpstack:push(true)
    if tok.name ~= "do" then
        assert_tok(ls, "->")
        ls:get()
    end

    if range then
        return For_Range_Expr(ls, ident, first, last, step, parse_expr(ls))
    else
        return For_Expr(ls, ids, exprs, parse_expr(ls))
    end
end

local parse_result = function(ls)
    push_curline(ls)
    ls:get()
    if ls.token.name == "(" then
        ls:get()
        local exprs = parse_exprlist(ls)
        assert_tok(ls, ")")
        ls:get()
        return Result_Expr(ls, unpack(exprs))
    end
    return Result_Expr(ls, parse_expr(ls))
end

local parse_return = function(ls)
    push_curline(ls)
    ls:get()
    if ls.token.name == "(" then
        ls:get()
        local exprs = parse_exprlist(ls)
        assert_tok(ls, ")")
        ls:get()
        return Return_Expr(ls, unpack(exprs))
    end
    return Return_Expr(ls, parse_expr(ls))
end

local parse_yield = function(ls)
    push_curline(ls)
    ls:get()
    if ls.token.name == "(" then
        ls:get()
        local exprs = parse_exprlist(ls)
        assert_tok(ls, ")")
        ls:get()
        return Yield_Expr(ls, unpack(exprs))
    end
    return Yield_Expr(ls, parse_expr(ls))
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
    elseif tok.name ~= ";;" and tok.name ~= "end" and tok.name ~= "->"
    and tok.name ~= "[" then
        el = { parse_primaryexpr(ls) }
    end

    -- implicit constructors
    local cargs
    if tok.name == "[" then
        ls:get()

        cargs = {}
        repeat
            assert_tok(ls, "<ident>")
            local v = tok.value
            ls:get()
            cargs[#cargs + 1] = { v, --[[parse_when(ls)]] }
         until tok.name ~= "," or ls:get() ~= "<ident>"

        assert_tok(ls, "]")
        ls:get()
    end

    if (cargs and tok.name ~= "->") or tok.name == ";;"
    or tok.name == "end" then
        if tok.name == ";;" or tok.name == "end" then
            ls:get()
        end
        return Object_Expr(ls, el and el or {
            Symbol_Expr(nil, lazy_rt_fun("obj_def")) }, cargs or {})
    end

    assert_tok(ls, "->")
    ls:get()

    local tbl = {}
    repeat
        if tok.name == "fn" then
            tbl[#tbl + 1] = { parse_function(ls, true) }
        elseif tok.name == "<ident>" then
            local kexpr = Value_Expr(nil, tok.value)
            ls:get()
            assert_tok(ls, "=")
            ls:get()
            tbl[#tbl + 1] = { kexpr, parse_expr(ls) }
        else
            assert_tok(ls, "$", "$(")
            local kexpr = parse_expr(ls)
            assert_tok(ls, "=")
            ls:get()
            tbl[#tbl + 1] = { kexpr, parse_expr(ls) }
        end
        if tok.name == ";" then
            ls:get()
        end
    until tok.name == ";;" or tok.name == "end"

    assert_tok(ls, ";;", "end")
    ls:get()
    return Object_Expr(ls, el and el or {
            Symbol_Expr(nil, lazy_rt_fun("obj_def")) }, cargs or {},
                unpack(tbl))
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
        return New_Expr(ls, pex)
    end
    local el = parse_exprlist(ls)
    assert_tok(ls, ")")
    ls:get()
    return New_Expr(ls, pex, unpack(el))
end

local parse_binexpr
local parse_simpleexpr

parse_primaryexpr = function(ls)
    local tok = ls.token
    local tn = tok.name
    if tn == "(" then
        ls:get()
        local exp = parse_expr(ls)
        if tok.name ~= ")" then
            syntax_error(ls, "missing ')'")
        end
        ls:get()
        return exp
    elseif tn == "[" then
        if ls:lookahead() == ":" then
            return parse_list(ls)
        else
            return parse_table(ls)
        end
    elseif tn == "$(" then
        ls:get()
        local exp = parse_expr(ls)
        if tok.name ~= ")" then
            syntax_error(ls, "missing ')'")
        end
        ls:get()
        return exp
    elseif tn == "$" then
        push_curline(ls)
        ls:get()
        assert_tok(ls, "<ident>")
        local v = tok.value
        ls:get()
        return Symbol_Expr(ls, v)
    elseif tn == "<ident>" then
        push_curline(ls)
        local v = tok.value
        ls:get()
        return Symbol_Expr(ls, v)
    elseif tn == "<number>" then
        push_curline(ls)
        local v = tok.value
        ls:get()
        return Value_Expr(ls, tonumber(v))
    elseif tn == "<begstring>" then
        push_curline(ls)
        ls:get() -- consume the "start" token

        local exprs, value = { true }
        while true do
            -- potential string end? but we have to assume implicit
            -- constant concatenation
            local tn, tv = tok.name, tok.value
            if tn == "<endstring>" then
                ls:get()
                if tok.name ~= "<begstring>" then
                    break
                end
                -- consume next "start" token
                ls:get()
            elseif tn == "$" or tn == "$(" then
                exprs[#exprs + 1] = parse_expr(ls)
                value = value and (value .. "%s") or "%s"
            else
                value = value and (value .. tv) or tv
                ls:get()
            end
        end

        if #exprs > 1 then
            push_curline(ls)
            exprs[1] = Value_Expr(ls, value)
            return Call_Expr(ls, false, Symbol_Expr(nil,
                lazy_rt_fun("str_fmt")), unpack(exprs))
        end
        return Value_Expr(ls, value)
    elseif tn == "nil" or tn == "true" or tn == "false" then
        push_curline(ls)
        ls:get()
        local v
        if tn == "true" then
            v = true
        elseif tn == "false" then
            v = false
        end
        return Value_Expr(ls, v)
    elseif tn == "@" then
        push_curline(ls)
        push_curline(ls)
        ls:get()
        push_curline(ls)
        assert_tok(ls, "<ident>")
        local v = tok.value
        ls:get()
        return Index_Expr(ls, Symbol_Expr(ls, "self"),
            Value_Expr(ls, v))
    elseif Unary_Ops[tn] then
        push_curline(ls)
        ls:get()
        return Unary_Expr(ls, tn, parse_binexpr(ls, Unary_Ops[tn]))
    else
        syntax_error(ls, "unexpected symbol")
    end
end

local parse_suffixedexpr
parse_suffixedexpr = function(ls)
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
                exp = Call_Expr(ls, exp, Value_Expr(ls, s), unpack(el))
            else
                exp = Index_Expr(ls, exp, Value_Expr(ls, s))
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
            exp = Call_Expr(ls, false, exp, unpack(el))
        elseif Ass_Ops[tok.name] then
            local op = tok.name
            if not exp:is_lvalue() then
                syntax_error(ls, "expected lvalue")
            end
            ls:get()
            exp = Binary_Expr(ls, op, exp, parse_expr(ls))
        else
            return exp
        end
    end
end

parse_simpleexpr = function(ls)
    local tok = ls.token
    local name = tok.name

    if name == "fn" then
        return parse_function(ls)
    elseif name == "let" then
        return parse_let_with(ls)
    elseif name == "set" then
        return parse_set(ls)
    elseif name == "with" then
        return parse_let_with(ls, true)
    elseif name == "seq" then
        return parse_sequence(ls)
    elseif name == "quote" then
        return parse_quote(ls)
    elseif name == "unquote" then
        return parse_unquote(ls)
    elseif name == "enum" then
        return parse_enum(ls)
    elseif name == "if" then
        return parse_if(ls)
    elseif name == "match" then
        return parse_match(ls)
    elseif name == "do" then
        return parse_do(ls)
    elseif name == "loop" then
        return parse_loop(ls)
    elseif name == "for" then
        return parse_for(ls)
    elseif name == "break" then
        assert_check(ls, ls.lpstack:top(), "no loop to break")
        push_curline(ls)
        ls:get()
        return Break_Expr(ls)
    elseif name == "cycle" then
        assert_check(ls, ls.lpstack:top(), "no loop to cycle")
        push_curline(ls)
        ls:get()
        return Cycle_Expr(ls)
    elseif name == "redo" then
        assert_check(ls, ls.lpstack:top(), "no loop to redo")
        push_curline(ls)
        ls:get()
        return Redo_Expr(ls)
    elseif name == "result" then
        return parse_result(ls)
    elseif name == "return" then
        return parse_return(ls)
    elseif name == "yield" then
        return parse_yield(ls)
    elseif name == "clone" then
        return parse_object(ls)
    elseif name == "new" then
        return parse_new(ls)
    elseif name == "__FILE__" then
        push_curline(ls)
        ls:get()
        return Value_Expr(ls, ls.source)
    elseif name == "__LINE__" then
        push_curline(ls)
        ls:get()
        return Value_Expr(ls, ls.line_number)
    elseif name == "..." then
        assert_check(ls, ls.fnstack:top().vararg,
            "cannot use '...' outside a vararg function")
        push_curline(ls)
        ls:get()
        return Vararg_Expr(ls)
    else
        return parse_suffixedexpr(ls)
    end
end

local parse_condexpr = function(ls)
    local tok = ls.token

    local exp = parse_binexpr(ls)
    while true do
        if tok.name == "unless" then
            push_curline(ls)
            ls:get()
            local cond = parse_expr(ls)
            local fexpr
            if tok.name == "else" then
                ls:get()
                fexpr = parse_expr(ls)
            end
            exp = If_Expr(ls, Unary_Expr(nil, "not", cond), exp, fexpr)
        elseif tok.name == "when" then
            push_curline(ls)
            ls:get()
            local cond = parse_expr(ls)
            local fexpr
            if tok.name == "else" then
                ls:get()
                fexpr = parse_expr(ls)
            end
            exp = If_Expr(ls, cond, exp, fexpr)
        else
            return exp
        end
    end
end

parse_binexpr = function(ls, mp)
    local curr, tok = ls.ndstack, ls.token
    local len = #curr
    push_curline(ls)
    push_curline(ls)
    mp = mp or 1

    local lhs = parse_simpleexpr(ls)

    while true do
        local cur = tok.name

        local t = Binary_Ops[cur]
        if not cur or not t or t[1] < mp then break end

        local op, p1, p2 = cur, t[1], t[2]

        ls:get()
        local rhs = parse_binexpr(ls, p1 > p2 and p1 or p2)

        lhs = Binary_Expr(ls, op, lhs, rhs)
        push_curline(ls)
    end
    for i = 1, #curr - len do curr:pop() end
    return lhs
end

parse_expr = function(ls)
    return parse_condexpr(ls)
end

local parse = function(fname, reader)
    if not reader then
        local str = fname
        fname  = '[string \"' .. fname:match("[^\r\n]*"):sub(1, 63) .. '"]'
        reader = util.string_stream(str)
    end
    local ls = lexer.init(fname, reader)
    ls.ndstack, ls.fnstack, ls.lpstack = Stack(), Stack(), Stack()
    -- global scope
    ls.fnstack:push({ vararg = false })
    ls:get()

    local ast, tok = {}, ls.token
    if tok.name ~= "<eos>" then
        while true do
            local ex = parse_expr(ls)
            ast[#ast + 1] = ex
            if tok.name == "<eos>" then
                break
            elseif tok.name == ";;" or tok.name == "end" then
                ls:get()
                assert_tok(ls, "<eos>")
                break
            elseif tok.name == ";" then
                ls:get()
                if tok.name == "<eos>" then
                    break
                end
            end
        end
    end

    return ast
end
M.parse = parse

local build = function(ast)
    util.randomseed(os.clock() * os.time())

    local ms = new_scope(Scope, nil, true)
    ms.data = {}

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

    return concat(hdr, "\n") .. "\n" .. ms:build()
end
M.build = build

local loadstr = loadstring
M.load = function(str)
    return loadstr(build(parse(str)))
end

return M
