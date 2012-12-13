--[[ Vortex 0.1 runtime init module

 Author: q66 <quaker66@gmail.com>
 Initializes all the runtime modules.
]]

local M = require "rt.core"
require "rt.env"
require "rt.table"
require "rt.list"
require "rt.seq"
require "rt.object"
require "rt.parser"
require "rt.defenv"

return M