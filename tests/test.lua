local __4x4k_rt = require("rt_init")
local __ucqk_rtfn = __4x4k_rt.__vx_select
local __mg7s_rtfn = __4x4k_rt.__vx_seq_create
local __e9e9_rtfn = __4x4k_rt.__vx_coro_create
local __qc3k_rtfn = __4x4k_rt.__vx_coro_yield
local __icyo_rtfn = __4x4k_rt.__vx_env_set
local __e9iw_rtfn = __4x4k_rt.__vx_def_env
__icyo_rtfn(1, __e9iw_rtfn)
local fact; fact = function(n)
    if (n == 0) then
        return 1
    else
        print("hai from fact")
        return (n * fact((n - 1)))
    end
end
local fact_tail; fact_tail = function(n, ...)
    local __ywqk__35d = __ucqk_rtfn('#', ...)
    local acc
    if (__ywqk__35d < 1) then
        acc = 1
    else
        acc = __ucqk_rtfn(1, ...)
    end
    if (n == 0) then
        return acc
    else
        local __7wq1_arg = (n - 1)
        return fact_tail(__7wq1_arg, (n * acc))
    end
end
local ack; ack = function(m, n)
    if (m == 0) then
        return (n + 1)
    else
        if (n == 0) then
            local __ewq1_arg = (m - 1)
            return ack(__ewq1_arg, 1)
        else
            local __7wak_arg = (m - 1)
            local __uoa5_arg = m
            return ack(__7wak_arg, ack(__uoa5_arg, (n - 1)))
        end
    end
end
local fun = function(a, b, c)
    return (a + (b * c))
end
print("hello world!")
print("regular factorial", fact(6))
print("tail recursive factorial", fact_tail(6))
print("ack(1, 2)", ack(1, 2))
print("fun", fun(5, 10, 15))
local defargs = function(a, b, ...)
    local __aoq9__35d = __ucqk_rtfn('#', ...)
    local c
    if (__aoq9__35d < 1) then
        c = 5
    else
        c = __ucqk_rtfn(1, ...)
    end
    local d
    if (__aoq9__35d < 2) then
        d = "hello"
    else
        d = __ucqk_rtfn(2, ...)
    end
    local e
    if (__aoq9__35d < 3) then
        e = (c * c)
    else
        e = __ucqk_rtfn(3, ...)
    end
    local __uweo_arg = a
    local __q5e1_arg = b
    local __71uw_arg = c
    local __ak7k_arg = d
    return print(__uweo_arg, __q5e1_arg, __71uw_arg, __ak7k_arg, e)
end
defargs(66)
defargs(66, 67)
defargs(66, 67, 169)
defargs(66, 67, 170, "world")
defargs(66, 67, 171, "world", 81)
defargs(66, 67, nil, "hai", nil)
local defvarargs = function(a, ...)
    local __aw71__35d = __ucqk_rtfn('#', ...)
    local b
    if (__aw71__35d < 1) then
        b = 5
    else
        b = __ucqk_rtfn(1, ...)
    end
    local c
    if (__aw71__35d < 2) then
        c = (b * (b + 1))
    else
        c = __ucqk_rtfn(2, ...)
    end
    local __ykuo_arg = a
    local __uk7c_arg = b
    print("regular", __ykuo_arg, __uk7c_arg, c)
    return print("varargs", __ucqk_rtfn(3, ...))
end
defvarargs(150)
defvarargs(150, 10)
defvarargs(150, 10, "something", "something else", 3.14)
local block1 = function(a)
    print("foo")
    local x = (a * a)
    return x
end
local block2 = function(a, b)
    print("hello")
    print("world")
    local __q1y9_ret = (a + b)
    local __aoik_ret = (a - b)
    local __a5eg_ret = (a * b)
    return __q1y9_ret, __aoik_ret, __a5eg_ret, (a / b)
end
local block_tail; block_tail = function()
    return block_tail()
end
print(block1(5))
print(block2(6, 7))
local __uoq9_block
do
    print("hi from a block")
    __uoq9_block = 5
end
local blk = __uoq9_block
print("block ret", blk)
local i = 0
while (i < 5) do
    local __ekq1_rhs = (i + 1)
    i = __ekq1_rhs
    print("hello from the loop", __ekq1_rhs)
end
while (i > 150) do
    local a = 150
end
local __ikic_rhs = (i + (i * 2))
i = __ikic_rhs
print(i)
print("tests/test.vx", 93)
local __y57w_arr = (25 * 3)
local tbl = {
    5, 10, 15, 20,
    __y57w_arr, 30, 35, 40,
    45, 50,
    sneaky_one = "hi!", another = 631,
    foo = "haaaaaaaa", some_assoc = "fun fun fun fun"
}
local __q5mc_arg = tbl
print(table["concat"](__q5mc_arg, ", "))
local __7k3w_arg = tbl[2]
print(__7k3w_arg, tbl["sneaky_one"])
local foo = function()
    print("foo")
    return 2
end
local __q93w_index = foo()
local __icic_rhs = (tbl[__q93w_index] + 15)
tbl[__q93w_index] = __icic_rhs
print(tbl[2])
local __mg71_rhs = (i + 10)
i = __mg71_rhs
local __momc_arg = __mg71_rhs
local __mca1_rhs = (i + 324)
i = __mca1_rhs
local __m5i5_arg = __mca1_rhs
local __mga9_arg = i
print(__momc_arg, __m5i5_arg, __mga9_arg, (i * 3))
local a, b, c, d, e, f = __mg7s_rtfn(__e9e9_rtfn(function()
    __qc3k_rtfn(5, 10, 15)
    __qc3k_rtfn("hello", 3.14)
    return __qc3k_rtfn(fact(6))
end))
local __7cio_arg = a
local __qgqk_arg = b
local __i5y5_arg = c
local __i5qo_arg = d
local __ekao_arg = e
print(__7cio_arg, __qgqk_arg, __i5y5_arg, __i5qo_arg, __ekao_arg, f)
local __awus_arg = _L
local __ukys_arg = _G
local __a939_arg = _R
print(__awus_arg, __ukys_arg, __a939_arg, _VERSION)