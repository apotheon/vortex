--[[ Vortex 0.1 main program

 Author: q66 <quaker66@gmail.com>
 Available under the terms of the MIT license.
]]

META = {
    general = {
        version = 0.01
    },
    lexer = {
    },
    parser = {
    },
    cgen = {
        indent = 4
    }
}

local util   = require("util")
local parser = require("parser")

local help = function(args)
    print("Vortex compiler v" .. META.general.version)
    print("Usage:")
    print("    " .. args[-1] .. " " .. args[0] .. " [files.vx]")
end

local compile_all = function(args)
    for i = 1, #args do
        local  ifname = args[i]
        local  rs = io.open(ifname, "r")
        if not rs then
            io.stderr:write(ifname .. ": No such file or directory\n")
            return 1
        end

        local  ofname
        local  has_ext = ifname:find("%.vx")
        if not has_ext then
            ofname = ifname .. ".lua"
        else
            ofname = ifname:gsub("%.vx", ".lua")
        end

        local  ws = io.open(ofname, "w")
        if not ws then
            io.stderr:write("Cannot open " .. ofname .. " for writing.\n")
            io.close(rs)
            return 1
        end

        --print("Compiling " .. ifname .. " to " .. ofname .. "...")

        local ret = parser.parse(
            ifname, util.file_istream(rs), util.file_ostream(ws))

        io.close(rs)
        io.close(ws)

        if ret and ret ~= 0 then
            return ret
        end
    end
end

local main = function(args)
    if #args == 0 then
        help(args)
        return 0
    end
    return compile_all(args)
end

os.exit(main(arg))
