var keywords = {
    "and"   : "kw1",
    "as"    : "kw1",
    "asr"   : "kw1",
    "band"  : "kw1",
    "bnot"  : "kw1",
    "bor"   : "kw1",
    "break" : "kw1",
    "bsl"   : "kw1",
    "bsr"   : "kw1",
    "bxor"  : "kw1",
    "case"  : "kw1",
    "cfn"   : "kw1",
    "clone" : "kw1",
    "coro"  : "kw1",
    "cycle" : "kw1",
    "do"    : "kw1",
    "else"  : "kw1",
    "false" : "kw3",
    "fn"    : "kw1",
    "for"   : "kw1",
    "glob"  : "kw2",
    "goto"  : "kw1",
    "if"    : "kw1",
    "in"    : "kw2",
    "let"   : "kw1",
    "match" : "kw1",
    "module": "kw1",
    "new"   : "kw1",
    "nil"   : "kw3",
    "not"   : "kw1",
    "or"    : "kw1",
    "quote" : "kw1",
    "rec"   : "kw2",
    "return": "kw1",
    "seq"   : "kw1",
    "true"  : "kw3",
    "when"  : "kw1",
    "while" : "kw1",
    "yield" : "kw1",

    "assert": "kw2", "self": "kw2", "super": "kw2",

    "__FILE__": "kw3", "__LINE__": "kw3"
};

function process_syntax(text) {
    var newstr = "";
    var len = text.length;
    var i = 0;
    var nest = 0;
    var make_span = function(input, cl) {
        return "<span class=\"" + cl + "\">" + input + "</span>";
    };
    var is_alnum = function(c) {
        var ascii = c.charCodeAt(0);
        return ((ascii >= 48 && ascii <= 57)
             || (ascii >= 65 && ascii <= 90)
             || (ascii >= 97 && ascii <= 122));
    };
    var is_digit = function(c) {
        var ascii = c.charCodeAt(0);
        return (ascii >= 48 && ascii <= 57);
    };
    var is_idkw = function(c) {
        var ascii = c.charCodeAt(0);
        /* allow : here for method calls */
        return (c == "_" || c == "#" || c == "@" || c == ":"
            || (ascii >= 48 && ascii <= 57)
            || (ascii >= 65 && ascii <= 90)
            || (ascii >= 97 && ascii <= 122));
    };
    var read_num = function() {
        var sidx = i;
        ++i;
        while (is_alnum(text[i])) {
            ++i;
        }
        newstr += make_span(text.substring(sidx, i), "num");
        --i;
    };
    for (i = 0; i < len; ++i) {
        var curr = text[i];
        if (curr == "\r" || curr == "\n" || curr == " " ||
            curr == "\f" || curr == "\t" || curr == "\v") {
            newstr += curr;
        } else if (curr == "=") {
            ++i;
            if (text[i] != "=") {
                --i;
                newstr += make_span("=", "seq");
                continue;
            }
            newstr += make_span("==", "seq");
        } else if (curr == ">") {
            ++i;
            if (text[i] != "=") {
                --i;
                newstr += make_span(">", "seq");
                continue;
            }
            newstr += make_span(">=", "seq");
        } else if (curr == "<") {
            ++i;
            if (text[i] != "=") {
                --i;
                newstr += make_span("<", "seq");
                continue;
            }
            newstr += make_span("<=", "seq");
        } else if (curr == "!") {
            ++i;
            if (text[i] != "=") {
                --i;
                newstr += make_span("!", "seq");
                continue;
            }
            newstr += make_span("!=", "seq");
        } else if (curr == "+") {
            ++i;
            if (text[i] == "+") {
                ++i;
                if (text[i] == "=") {
                    newstr += make_span("++=", "seq");
                    continue;
                }
                --i;
                newstr += make_span("++", "seq");
            } else if (text[i] == "=") {
                newstr += make_span("+=", "seq");
            } else {
                --i;
                newstr += make_span("+", "seq");
            }
        } else if (curr == "-") {
            ++i;
            if (text[i] == ">") {
                newstr += make_span("->", "kw2");
            } else if (text[i] == "=") {
                newstr += make_span("-=", "seq");
            } else {
                --i;
                newstr += make_span("-", "seq");
            }
        } else if (curr == "*") {
            ++i;
            if (text[i] == "*") {
                ++i;
                if (text[i] == "=") {
                    newstr += make_span("**=", "seq");
                    continue;
                }
                --i;
                newstr += make_span("**", "seq");
            } else if (text[i] == "=") {
                newstr += make_span("*=", "seq");
            } else {
                --i;
                newstr += make_span("*", "seq");
            }
        } else if (curr == "%") {
            if (text[i + 1] != "=") {
                newstr += make_span(curr, "seq");
                continue;
            }
            ++i;
            newstr += make_span(curr + "=", "seq");
        } else if (curr == "/") {
            ++i;
            if (text[i] == "/") {
                newstr += "<span class=\"comment\">//";
                ++i;
                while (i < len && text[i] != "\n" && text[i] != "\r") {
                    newstr += text[i];
                    ++i;
                }
                newstr += "</span>";
                --i;
            } else if (text[i] == "*") {
                newstr += "<span class=\"comment\">/*";
                ++i;
                nest = 0;
                while (i < len) {
                    if (text[i] == "/") {
                        newstr += "/";
                        ++i;
                        if (text[i] == "*") {
                            newstr += "*";
                            ++nest;
                            ++i;
                        }
                    }
                    if (text[i] == "*") {
                        newstr += "*";
                        ++i;
                        if (text[i] == "/") {
                            newstr += "/";
                            --nest;
                            ++i;
                            if (nest < 0) {
                                --i;
                                break;
                            }
                        }
                    }
                    newstr += text[i];
                    ++i;
                }
                newstr += "</span>";
            } else if (text[i] == "=") {
                newstr += make_span("/=", "seq");
            } else {
                --i;
                newstr += make_span("/", "seq");
            }
        } else if (curr == ".") {
            ++i;
            if (text[i] == ".") {
                ++i;
                if (text[i] == ".") {
                    newstr += make_span("...", "seq");
                    continue;
                }
                --i;
                newstr += make_span("..", "seq");
                continue;
            } else if (is_digit(text[i])) {
                --i;
                read_num();
            } else {
                --i;
                newstr += ".";
            }
        } else if (is_digit(curr)) {
            read_num();
            continue;
        } else if (curr == ":") {
            if (text[i + 1] != ":") {
                newstr += make_span(":", "seq");
                continue;
            }
            ++i;
            newstr += make_span("::", "seq");
            continue;
        } else if (curr == "[") {
            if (text[i + 1] != "[") {
                newstr += "[";
                continue;
            }
            newstr += "<span class=\"str\">[[";
            i += 2;
            nest = 0;
            while (i < len) {
                if (text[i] == "[") {
                    newstr += "[";
                    ++i;
                    if (text[i] == "[") {
                        newstr += "[";
                        ++nest;
                        ++i;
                    }
                }
                if (text[i] == "]") {
                    newstr += "]";
                    ++i;
                    if (text[i] == "]") {
                        newstr += "]";
                        --nest;
                        ++i;
                        if (nest < 0) {
                            --i;
                            break;
                        }
                    }
                }
                newstr += text[i];
                ++i;
            }
            newstr += "</span>";
        } else if (curr == "\"" || curr == "'") {
            newstr += "<span class=\"str\">" + curr;
            ++i;
            while (i < len && text[i] != curr) {
                newstr += text[i];
                ++i;
            }
            if (i < len) {
                newstr += curr;
            }
            newstr += "</span>";
        } else {
            if (is_idkw(curr)) {
                var sidx = i;
                ++i;
                while (is_idkw(text[i])) {
                    ++i;
                }
                var str = text.substring(sidx, i);
                var cls = keywords[str];
                if (cls) {
                    newstr += make_span(str, cls);
                } else {
                    if (text[i] == "(") {
                        newstr += make_span(str, "call");
                    } else {
                        newstr += str;
                    }
                }
                --i;
            } else {
                if (curr == "@") {
                    newstr += make_span(str, "seq");
                } else {
                    newstr += curr;
                }
            }
        }
    }
    return newstr;
}

function prettify_all() {
    var ndl = document.getElementsByClassName("code");
    for (var i = 0; i < ndl.length; ++i) {
        var nd = ndl[i];
        var txt = nd.innerText;
        if (txt == undefined) {
            txt = nd.textContent;
        }
        nd.innerHTML = process_syntax(txt);
    }
}