--[[ Vortex 0.1 main program

 Author: q66 <quaker66@gmail.com>
 Available under the terms of the MIT license.
]]

package.path = package.path .. ";./src/?.lua;./src/?/init.lua"

local util   = require("util")
local parser = require("parser")

local stderr, exit = io.stderr, os.exit
local error_exit = function(msg)
    stderr:write(msg, "\n")
    stderr:flush()
    exit(1)
end

local vxpcall = util.vxpcall
local compile_all = function(args)
    for i = 1, #args do
        local  ifname = args[i]
        local  rs = io.open(ifname, "r")
        if not rs then
            io.stderr:write(ifname .. ": No such file or directory\n")
            return 1
        end

        local st = util.file_istream(rs)
        io.close(rs)

        local stat, ret = vxpcall(parser.parse, ifname, st)
        if not stat then error_exit(ret) end
        stat, ret = vxpcall(parser.build, ret)
        if not stat then error_exit(ret) end

        local  ofname
        local  has_ext = ifname:find("%.vx")
        if not has_ext then
            ofname = ifname .. ".lua"
        else
            ofname = ifname:gsub("%.vx", ".lua")
        end

        local  ws = io.open(ofname, "w")
        if not ws then
            io.stderr:write("Cannot open " .. ofname ..
                " for writing.\n")
            io.close(rs)
            return 1
        end
        ws:write(ret)
        io.close(ws)
    end
end

os.exit(compile_all(arg) or 0)
