-module(reftest_reflib_header).
-vsn("$Rev: 4210 $").

-compile([export_all]).

-include("test.hrl").

files() ->
[{header, "header.hrl",
%	      "-module(header).    \n"
	      "-record(person, {name,age,phone}).\n"
	      "-define(DEBUG, false).\n"
	      },
{module, "mod.erl",
	      "-module(mod). \n"
	      "-include(\"header.hrl\"). \n" 
	      "-export([run/1]). \n"
	      "run(Alma) -> \n"
	      "ok. \n"
	      }
].

test_file() ->
    File = ?Query:exec1(?File:find("header.hrl"), file_not_found),
    "header.hrl" = filename:basename(?File:path(File)),
    ok.

