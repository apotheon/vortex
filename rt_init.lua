--[[ Vortex 0.1 runtime initialization module

 Author: q66 <quaker66@gmail.com>
 Available under the terms of the MIT license.
]]

package.path = package.path .. ";./src/?.lua;./src/?/init.lua"
return require "rt"