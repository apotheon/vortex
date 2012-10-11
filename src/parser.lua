local lexer   = require("lexer")
local util    = require("util")

-- t[1] > t[2] == right-associative, otherwise left-associative
local Binary_Ops = {
    -- arithmetic operators
    ["+"  ] = { 6,   6 },
    ["-"  ] = { 6,   6 },
    ["*"  ] = { 7,   7 },
    ["/"  ] = { 7,   7 },
    ["%"  ] = { 7,   7 },
    ["^"  ] = { 12, 11 },

    -- concat
    [".." ] = { 5,   4 },

    -- comparison
    ["==" ] = { 3,   3 },
    ["<"  ] = { 3,   3 },
    ["<=" ] = { 3,   3 },
    ["!=" ] = { 3,   3 },
    [">"  ] = { 3,   3 },
    [">=" ] = { 3,   3 },

    -- compound
    [":=" ] = { 9,   8 },
    ["+=" ] = { 9,   8 },
    ["-=" ] = { 9,   8 },
    ["*=" ] = { 9,   8 },
    ["/=" ] = { 9,   8 },

    -- and / or
    ["and"] = { 2,   2 },
    ["or" ] = { 1,   1 }
}

local Unary_Ops = {
    ["-"  ] = 10,
    ["not"] = 10,
    ["#"  ] = 10
}

local syntax_error = lexer.syntax_error
local case_class = util.case_class
local unique_sym = util.unique_sym
local hash_sym = util.hash_sym

local assert_tok = function(ls, tok, allow)
    local n = ls.token.name
    if n ~= tok and (allow == nil or n ~= allow) then
        syntax_error(ls, "unexpected symbol")
    end
end

local concat = table.concat

local Scope = util.Object:clone {
    __init = function(self, fs, indent)
        self.body, self.fstate, self.indent = {}, fs, indent
    end,

    push = function(self, stat)
        local body = self.body
        body[#body + 1] = stat
    end,

    build = function(self)
        local ind = self.indent * META.cgen.indent
        return (" "):rep(ind) .. concat(self.body, "\n" .. (" "):rep(ind))
    end
}

local Function_State = Scope:clone {
}

local gen_local = function(names, vals, rec)
    if not vals then
        return concat { "local ", names }
    elseif rec then
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

-- classes

local Call_Expr

local Symbol_Expr = util.Object:clone {
    name = "Symbol_Expr",

    __init = function(self, sym)
        self.symbol = sym
    end,

    generate = function(self)
        return self.symbol
    end
}

local Value_Expr = util.Object:clone {
    name = "Value_Expr",

    __init = function(self, val)
        self.value = val
    end,

    generate = function(self)
        return self.value
    end
}

local Block_Expr = util.Object:clone {
    name = "Block_Expr",

    __init = function(self, exprs)
        self.exprs = exprs
    end,

    -- XXX: works only for functions right now
    generate = function(self, si)
        local exprs = self.exprs
        local sym   = unique_sym("block")
        local len   = #exprs

        for i = 1, len - 1 do
            exprs[i]:generate(si)
        end

        local last = exprs[len]
        if not last.is_a then
            local exps = {}
            for i = 1, #last do
                exps[#exps + 1] = last[i]:generate(si)
            end
            si:push(gen_ret(table.concat(exps, ", ")))
        else
            si:push(gen_ret(exprs[len]:generate(si, true)))
        end
    end
}

local Binary_Expr = util.Object:clone {
    name = "Binary_Expr",

    __init = function(self, op, lhs, rhs)
        self.op, self.lhs, self.rhs = op, lhs, rhs
    end,

    generate = function(self, si, notemp)
        local lhs = self.lhs:generate(si)
        local rhs = self.rhs:generate(si)

        if notemp then
            return gen_binexpr(self.op, lhs, rhs)
        end

        local sym = unique_sym(self.op)

        si:push(gen_local(sym, gen_binexpr(self.op, lhs, rhs)))
        return sym
    end
}

local Unary_Expr = util.Object:clone {
    name = "Unary_Expr",

    __init = function(self, op, lhs)
        self.op, self.rhs = op, rhs
    end,

    generate = function(self, si)
        local rhs = self.rhs:generate(si)
        local sym = unique_sym(self.op)

        si:push(gen_local(sym, gen_unexpr(self.op, rhs)))
        return sym
    end
}

local Let_Expr = util.Object:clone {
    name = "Let_Expr",

    __init = function(self, rec, idents, assign)
        self.rec, self.idents, self.assign = rec, idents, assign
    end,

    generate = function(self, si)
        local syms = {}
        local len = #self.assign
        for i = 1, len do
            local nd = self.assign[i]
            syms[#syms + 1] = nd:generate(si, i == len and nd:is_a(Call_Expr))
        end

        local idents = gen_seq(self.idents)
        syms = gen_seq(syms)

        si:push(gen_local(idents, syms, self.rec))
        return idents
    end
}

local Function_Expr = util.Object:clone {
    name = "Function_Expr",

    __init = function(self, params, defaults, body)
        self.params, self.defaults, self.body = params, defaults, body
    end,

    generate = function(self, si)
        local fs = Function_State(si.fstate, si.indent + 1)

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
                tsc:push(gen_ass(name, defs[i]:generate(tsc, true)))

                local fsc = Scope(fs, fs.indent + 1)
                fsc:push(gen_ass(name, "select(" .. i .. ", ...)"))

                fs:push(gen_if(gen_binexpr("<", nd, i), tsc, fsc))
            end
        end

        -- avoid temps
        if self.body:is_a(Block_Expr) then
            self.body:generate(fs)
        else
            fs:push(gen_ret(self.body:generate(fs, true)))
        end
        return gen_fun(gen_seq(np), fs)
    end
}

local If_Expr = util.Object:clone {
    name = "If_Expr",

    __init = function(self, cond, tval, fval)
        self.cond, self.tval, self.fval = cond, tval, fval
    end,

    generate = function(self, si)
        local sym = unique_sym("if")

        local tsc = Scope(si.fstate, si.indent + 1)
        tsc:push(gen_ass(sym, self.tval:generate(tsc)))

        local fsc
        local fval = self.fval
        if fval then
            fsc = Scope(si.fstate, si.indent + 1)
            fsc:push(gen_ass(sym, fval:generate(fsc)))
        end

        si:push(gen_local(sym))
        si:push(gen_if(self.cond:generate(si), tsc, fsc))

        return sym
    end
}

local Sequence_Expr = util.Object:clone {
    name = "Sequence_Expr",

    __init = function(self, expr)
        self.expr = expr
    end,

    generate = function(self, si)
    end
}

Call_Expr = util.Object:clone {
    name = "Call_Expr",

    __init = function(self, expr, params)
        self.expr, self.params = expr, params
    end,

    generate = function(self, si, notemp)
        local syms = {}
        local len  = #self.params
        for i = 1, len do
            local param = self.params[i]
            syms[#syms + 1] = param:generate(si, param:is_a(Call_Expr))
        end
        syms = gen_seq(syms)

        if notemp then
            return gen_call(self.expr:generate(si), syms)
        end

        local sym = unique_sym("call")
        si:push(gen_local(sym, gen_call(self.expr:generate(si), syms)))
        return sym
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
    until tok.name == "," or tok.name == "}"

    if tok.name == "}" then
        ls:get()
        return Block_Expr(exprs)
    end

    -- we had an expression list at the end
    ls:get()

    local elist = { exprs[#exprs] }
    exprs[#exprs] = elist

    repeat
        elist[#elist + 1] = parse_expr(ls)
    until (tok.name ~= "," or tok.name == "}") or not ls:get()

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
    assert_tok(ls, "->")
    ls:get()

    return Sequence_Expr(parse_expr(ls))
end

local parse_if = function(ls)
    ls:get()
    local cond = parse_expr(ls)

    assert_tok(ls, "->")
    ls:get()

    local tval = parse_expr(ls)

    local tok = ls.token
    if tok.name == "else" then
        ls:get()
        assert_tok(ls, "->")
        ls:get()
        return If_Expr(cond, tval, parse_expr(ls))
    end

    return If_Expr(cond, tval, nil)
end

-- main expression parsing function
parse_expr = function(ls)
    local tok  = ls.token
    local name = tok.name

    if name == "fn" then
        return parse_function(ls)
    elseif name == "let" then
        ls:get()

        local recur = false
        if tok.name == "rec" then
            recur = true
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

        return Let_Expr(recur, ids, exprs)
    elseif name == "seq" then
        return parse_sequence(ls)
    elseif name == "if" then
        return parse_if(ls)
    elseif name == "{" then
        return parse_block(ls)
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
            ast[i]:generate(ms)
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
