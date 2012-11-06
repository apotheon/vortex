--[[ Vortex 0.1 core runtime module

 Author: q66 <quaker66@gmail.com>
 The core runtime module for Vortex. Features wrapped functions from the core
 Lua lib that are used across the runtime and other basic things.
 
 It provides the namespace for all Vortex runtime to live; every other runtime
 module just appends to it.

 Available under the terms of the MIT license.
]]

local M = {
    -- core
    __vx_assert   = _G.assert,
    __vx_gcollect = _G.collectgarbage,
    __vx_error    = _G.error,
    __vx_getmt    = _G.getmetatable,
    __vx_setmt    = _G.setmetatable,
    __vx_pairs    = _G.pairs,
    __vx_ipairs   = _G.ipairs,
    __vx_load     = _G.load,
    __vx_loadstr  = _G.loadstring,
    __vx_next     = _G.next,
    __vx_pcall    = _G.pcall,
    __vx_xpcall   = _G.xpcall,
    __vx_print    = _G.print,
    __vx_raweq    = _G.rawequal,
    __vx_rawget   = _G.rawget,
    __vx_rawset   = _G.rawset,
    __vx_rawlen   = _G.rawlen,
    __vx_require  = _G.require,
    __vx_select   = _G.select,
    __vx_tonum    = _G.tonumber,
    __vx_tostr    = _G.tostring,
    __vx_type     = _G.type,
    __vx_luaver   = _G._VERSION,
    __vx_lua_g    = _G,

    -- coroutines
    __vx_coro_create  = _G.coroutine.create,
    __vx_coro_resume  = _G.coroutine.resume,
    __vx_coro_yield   = _G.coroutine.yield,
    __vx_coro_running = _G.coroutine.running,
    __vx_coro_status  = _G.coroutine.status,
    __vx_coro_wrap    = _G.coroutine.wrap
}

-- bitlib - bit32 for lua 5.2, otherwise bit for luajit
if _VERSION == "Lua 5.2" then
    M.__vx_band = _G.bit32.band
    M.__vx_bor  = _G.bit32.bor
    M.__vx_bxor = _G.bit32.bxor
    M.__vx_bnot = _G.bit32.bnot
    M.__vx_arsh = _G.bit32.arshift
    M.__vx_lsh  = _G.bit32.lshift
    M.__vx_rsh  = _G.bit32.rshift
    M.__vx_rol  = _G.bit32.lrotate
    M.__vx_ror  = _G.bit32.rrotate

    local band, bor, lsh, rsh
        = bit32.band, bit32.bor, bit32.lshift, bit32.rshift
    M.__vx_bswap = function(x)
        x = bor(lsh(band(x, 0x0000FFFF, 16), rsh(band(x, 0xFFFF0000), 16)))
        x = bor(lsh(band(x, 0x00FF00FF,  8), rsh(band(x, 0xFF00FF00),  8)))
        return x
    end
else
    local bit = require "bit"
    M.__vx_band  = bit.band
    M.__vx_bor   = bit.bor
    M.__vx_bxor  = bit.bxor
    M.__vx_bnot  = bit.bnot
    M.__vx_arsh  = bit.arshift
    M.__vx_lsh   = bit.lshift
    M.__vx_rsh   = bit.rshift
    M.__vx_rol   = bit.rol
    M.__vx_ror   = bit.ror
    M.__vx_bswap = bit.bswap
end

-- temporary
M.__vx_str_fmt = _G.string.format

return M