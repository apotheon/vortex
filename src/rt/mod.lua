--[[ Vortex 0.1 module loader

 Author: q66 <quaker66@gmail.com>
 Provides a "vortloader", a module handler. Vortex modules have their own
 path var, package.vxpath. The searcher has higher precedence compared to
 regular Lua searchers (it's second in the list). Lua 5.2 only at this point.

 Available under the terms of the MIT license.
]]

local M = require("rt.core")
local util = require("util")

local spath = package.searchpath
local io_open, io_close, load, vxpcall = io.open, io.close, load, util.vxpcall

local parse, build = M.__vx_parser.parse, M.__vx_parser.build
local ifstream = util.file_istream

-- Vortex is separate from Lua
package.vxpath = "./?.vx"

table.insert(package.searchers, 2, function(modname)
    local path, err = spath(modname, package.vxpath)
    if not path then return err end
    local file = io_open(path, "rb")
    if file then
        local st = ifstream(file)
        io_close(file)

        local stat, ret = vxpcall(parse, path, st)
        if not stat then return ret end
        stat, ret = vxpcall(build, ret)
        if not stat then return ret end

        local f, err = load(ret, "@" .. path)
        return f or err
    end
end)