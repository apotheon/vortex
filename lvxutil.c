/* lvxutil.c - a collection of utilities for Vortex
 * Features signal handling, additional I/O utils or table utils
 *
 * Copyright (C) 2013 Daniel "q66" Kolesa <quaker66@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>

#include "lua.h"
#include "luaconf.h"
#include "lauxlib.h"

#if LUA_VERSION_NUM < 502
#define luaL_newlib(L, l) (lua_newtable(L), luaL_register(L, NULL, l))
#endif

/* SIGNAL HANDLING */

#define VX_SIG "vx_sig"

static const struct {
    const char *name;
    int signal;
} vx_signals[] = {
    { "SIGABRT", SIGABRT },
    { "SIGFPE" , SIGFPE  },
    { "SIGILL" , SIGILL  },
    { "SIGINT" , SIGINT  },
    { "SIGSEGV", SIGSEGV },
    { "SIGTERM", SIGTERM },
    { NULL, 0 }
};

static lua_State *GL = NULL;
static lua_Hook sigh = NULL;
static int sign = 0, hmask = 0, hcnt = 0;

static void vx_sighook(lua_State *L, lua_Debug *ar) {
    lua_sethook(L, sigh, hmask, hcnt);
    lua_pushstring(L, VX_SIG);
    lua_gettable(L, LUA_REGISTRYINDEX);
    lua_pushinteger(L, sign);
    lua_gettable(L, -2);
    lua_pushinteger(L, sign);
    lua_call(L, 1, 0);
    lua_pop(L, 1);
}

static void vx_sighandler(int sig) {
    sign = sig;
    sigh = lua_gethook(GL);
    hcnt = lua_gethookcount(GL);
    hmask = lua_gethookmask(GL);

    lua_sethook(GL, vx_sighook, LUA_MASKCALL | LUA_MASKRET | LUA_MASKCOUNT, 1);
}

static int vx_signal(lua_State *L) {
    int sig;
    switch (lua_type(L, 1)) {
        case LUA_TNUMBER:
            sig = lua_tointeger(L, 1);
            break;
        case LUA_TSTRING:
            lua_pushstring(L, VX_SIG);
            lua_gettable(L, LUA_REGISTRYINDEX);
            lua_pushvalue(L, 1);
            lua_gettable(L, -2);
            if (!lua_isnumber(L, -1)) {
                luaL_error(L, "invalid signal name: %s", lua_tostring(L, 1));
            }
            sig = lua_tointeger(L, -1);
            lua_pop(L, 2);
            break;
        default:
            luaL_checkint(L, 1);
            break;
    }

    if (lua_isnone(L, 2) || lua_isnil(L, 2)) {
        lua_pushstring(L, VX_SIG);
        lua_gettable(L, LUA_REGISTRYINDEX);
        lua_pushinteger(L, sig);
        lua_gettable(L, -2);
        lua_pushinteger(L, sig);
        lua_pushnil(L);
        lua_settable(L, -4);
        lua_remove(L, -2);
        signal(sig, SIG_DFL);
    } else {
        luaL_checktype(L, 2, LUA_TFUNCTION);
        lua_pushstring(L, VX_SIG);
        lua_gettable(L, LUA_REGISTRYINDEX);
        lua_pushinteger(L, sig);
        lua_pushvalue(L, 2);
        lua_settable(L, -3);
        lua_pop(L, 1);

        GL = L;
        lua_pushboolean(L, (signal(sig, vx_sighandler) == SIG_ERR) ? 0 : 1);
    }
    return 1;
}

static int vx_raise(lua_State *L) {
    switch (lua_type(L, 1)) {
        case LUA_TNUMBER:
            lua_pushinteger(L, raise(lua_tointeger(L, 1)));
            break;
        case LUA_TSTRING: {
            int ret;
            lua_pushstring(L, VX_SIG);
            lua_gettable(L, LUA_REGISTRYINDEX);
            lua_pushvalue(L, 1);
            lua_gettable(L, -2);
            if (!lua_isnumber(L, -1)) {
                luaL_error(L, "invalid signal name: %s", lua_tostring(L, 1));
            }
            ret = raise(lua_tointeger(L, -1));
            lua_pop(L, 2);
            lua_pushinteger(L, ret);
            break;
        }
        default:
            luaL_checkint(L, 1);
            break;
    }
    return 1;
}

/* IO EXTENSIONS */

#if defined(VX_POSIX)
#include <unistd.h>
static int vx_isatty(lua_State *L) {
    if (lua_gettop(L) == 0) {
        lua_pushboolean(L, isatty(0));
    } else {
        lua_pushboolean(L, isatty(fileno(
            *(FILE**)luaL_checkudata(L, 1, "FILE*"))));
    }
    return 1;
}
#elif defined(VX_WIN)
static int vx_isatty(lua_State *L) {
    if (lua_gettop(L) == 0) {
        lua_pushboolean(L, _isatty(_fileno(stdin)));
    } else {
        lua_pushboolean(L, _isatty(_fileno(
            *(FILE**)luaL_checkudata(L, 1, "FILE*"))));
    }
    return 1;
}
#else
static int vx_isatty(lua_State *L) {
    lua_pushboolean(L, 1);
    return 1;
}
#endif

/* READLINE */

#ifdef VX_READLINE
#include <readline/readline.h>
#include <readline/history.h>
static int vx_readline(lua_State *L) {
    char *rd = readline(luaL_optstring(L, 1, NULL));
    if (!rd) {
        return 0;
    } else {
        lua_pushstring(L, rd);
        free(rd);
        return 1;
    }
}
static int vx_add_history(lua_State *L) {
    add_history(luaL_checkstring(L, 1));
    return 0;
}
#else
#define VX_MAX_INPUT 512
static int vx_readline(lua_State *L) {
    char buf[VX_MAX_INPUT];
    const char *str = luaL_optstring(L, 1, NULL);
    fputs(str, stdout);
    fflush(stdout);
    if (!fgets(buf, VX_MAX_INPUT, stdin)) {
        return 0;
    } else {
        size_t len = strlen(buf);
        lua_pushlstring(L, buf, (buf[len - 1] == '\n') ? (len - 1) : len);
        return 1;
    }
}
static int vx_add_history(lua_State *L) {
    return 0;
}
#endif

static luaL_Reg lvxutil_lib[] = {
    { "signal"     , vx_signal      },
    { "raise"      , vx_raise       },
    { "isatty"     , vx_isatty      },
    { "readline"   , vx_readline    },
    { "add_history", vx_add_history },
    { NULL, NULL }
};

int luaopen_vxutil(lua_State *L) {
    int i = 0;
    luaL_newlib(L, lvxutil_lib);

    lua_pushstring(L, VX_SIG);
    lua_createtable(L, 0, 0);

    while (vx_signals[i].name) {
        lua_pushstring(L, vx_signals[i].name);
        lua_pushinteger(L, vx_signals[i].signal);
        lua_settable(L, -3);
        lua_pushstring(L, vx_signals[i].name);
        lua_pushinteger(L, vx_signals[i].signal);
        lua_settable(L, -5);
        ++i;
    }
    lua_settable(L, LUA_REGISTRYINDEX);

    return 1;
}