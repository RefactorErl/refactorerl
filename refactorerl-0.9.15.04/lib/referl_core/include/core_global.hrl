%%% -*- coding: latin-1 -*-

%%% This file is part of RefactorErl.
%%%
%%% RefactorErl is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published
%%% by the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% RefactorErl is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% ===========================================================================
%%% Node data records

-record(file,           {type, path, eol, lastmod, hash}).
-record(form,           {type, tag, paren=default, pp=none, hash, form_length,
                         start_scalar, start_line}).
-record(clause,         {type, var, pp=none}).
-record(expr,           {type, role, value, pp=none}).
-record(typexp,         {type, tag}).
-record(lex,            {type, data}).
-record(token,          {type, text, prews="", postws="", scalar, linecol}).

-record(module,         {name}).
-record(record,         {name}).
-record(field,          {name}).
-record(spec,           {name, arity}).
-record(specparam,      {type, name, value}).
-record(specclause,     {}).
-record(specguard,      {value}).
-record(namedtype,      {name, arity, isopaque, isbuiltin}).
-record(namedtypeparam, {name, type}).
-record(namedtypebody,  {value}).

-record(func,           {name             :: atom(),
                         arity            :: integer(),
                         dirty  = int     :: no | int | ext,
                         type   = regular :: regular | anonymous,
                         opaque = false   :: false | module | name | arity}).
-record(variable,       {name}).
-record(env,            {name, value}).

-record(ets_tab,        {names}).
-record(pid,            {reg_names = [], mod, func, ary}).

