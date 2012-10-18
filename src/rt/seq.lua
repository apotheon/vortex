--[[ Vortex 0.1 runtime module for sequences

 Author: q66 <quaker66@gmail.com>
 The sequence specific things for Vortex runtime. Appends to the default
 runtime namespace.

 Available under the terms of the MIT license.
]]

local M = require("rt.core")

local cs, cr, cy, ta, tu, sl = M.__vx_coro_status, M.__vx_coro_create,
                               M.__vx_coro_yield,  M.__vx_tbl_append,
                               M.__vx_tbl_unpack,  M.__vx_select
M.__vx_seq_create = function(coro)
    local rets = {}
    while cs(coro) ~= "dead" do
        ta(rets, select(2, cr(coro))
    end
    return tu(rets)
end