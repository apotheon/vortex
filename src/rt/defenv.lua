--[[ Vortex 0.1 default script environment

 Author: q66 <quaker66@gmail.com>
 Provides the default environment under which all Vortex scripts run.

 Available under the terms of the MIT license.
]]

local M = require("rt.core")

local env = {}
M.__vx_def_env = env

env.assert   = M.__vx_assert
env.error    = M.__vx_error
env.get_mt   = M.__vx_getmt
env.set_mt   = M.__vx_setmt
env.print    = M.__vx_print
env.select   = M.__vx_select
env.tostr    = M.__vx_tostr
env.tonum    = M.__vx_tonum
env.pcall    = M.__vx_pcall
env.xpcall   = M.__vx_xpcall
env.raw_eq   = M.__vx_raweq
env.raw_get  = M.__vx_rawget
env.raw_set  = M.__vx_rawset
env.raw_len  = M.__vx_rawlen
env.gcollect = M.__vx_gcollect
env.type     = M.__vx_type
env.pairs    = M.__vx_pairs
env.ipairs   = M.__vx_ipairs

env.unpack   = M.__vx_tbl_unpack

-- temporary until the standard library is in
env.table    = _G.table
env.string   = _G.string
env.debug    = _G.debug
env.math     = _G.math
env.os       = _G.os
env.io       = _G.io

env._L       = _G
env._G       = env
env._R       = M
env._VERSION = "Vortex 0.1"