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
%%% Modules

%% Storage layer
-define(Graph,          refcore_graph).

%% Semantical layer
-define(ESG,            refcore_esg).
-define(Syn,            refcore_syntax).
-define(FileMan,        refcore_fileman_gen).
-define(PreProc,        refcore_preproc).
-define(Dataflow,       refanal_dataflow).
-define(FileCacheMan,   refcore_filecacheserver).
-define(Proc,           refanal_proc).

%%% ===========================================================================
%%% Graph node

-define(NODETAG, '$gn').
-define(IS_NODE(Node), element(1, Node) =:= ?NODETAG).

%%% ===========================================================================
%%% Refactoring changes

-define(RefChangeTab, filename:join(mnesia:system_info(directory), "ref_change")).

%%% ===========================================================================
%%% Development mode

%% For development, uncomment the first line and comment the second one.
%% For releases, uncomment the second line and comment the first one.
-define(development_mode, development_mode).
%-undef(development_mode).

%% On/off switch for dynamic analysis of ETS tables
%% Note that this setting adjusts the database scheme
% -define(ets_mode, true).

%%% ===========================================================================
%%% Debugging

-ifdef(development_mode).
    %% Prettyprints the value of a single expression with module/line information.
    -define(d2(Name, X), io:format("~4w ~s~n ~12s: ~p~n", [?LINE, atom_to_list(?MODULE), Name, X])).
    -define(d(X), ?d2(??X, X)).

    %% Prettyprints a list of expressions with module/line information.
    -define(ds(Xs), io:format(lists:flatten(["~p-~p", ["~n  ~p" || _ <- Xs], "~n"]),
                                [?LINE, ?MODULE] ++ Xs)).

    -define(autoreset_schema, true).
-else.
    -undef(ericsson).
    -define(d2(Name, X), no_debug_in_release_mode).
    -define(d(X),        no_debug_in_release_mode).
    -define(ds(Xs),      no_debug_in_release_mode).
    -define(autoreset_schema, false).
-endif.

%%% ===========================================================================
%%% Sort of compile-time configuration

-define(PosModTab, posmod_tab).

-define(dynfun_maybe_limit,      10).
-define(dynfun_complexity_limit, 30).

%% Note:
% http://www.erlang.org/download/otp_src_R15B.readme
% OTP-9649
%
% Otp versions prior to R15B allow the users to define only such implicit function
% with fun M:F/A syntax which module name is an atom.
% Consider the following Eshell output (Otp version : R14B03):
%1> M=lists.
%lists
%2> fun M:max/1.
%* 1: syntax error before: M
%2> fun lists:max/1.
% #Fun<lists.max.1>
%3> {M,max}([1,2]).
%2
% In higher Otp versions the tuple funs are deprecated..
% @see build.rules for details
% OtpVersionMacName =
%   case erlang:system_info(otp_release)<"R15B" of
%       true -> priorR15B;
%       false -> leastR15B
%   end.
-ifdef(priorR15B).
    -define(implicitFun(Module, Function, _Arity), {Module, Function}).
    -define(wxGraphicsContext_destroy(X), X).
-else.
    -define(implicitFun(Module, Function, Arity), fun Module:Function/Arity).
    -define(wxGraphicsContext_destroy(X), wxGraphicsContext:destroy(X)).
-endif.

%% Positioning modes which are available
-define(availablePosModes, [abs, rel]).

% internal globals
-define(syn_database_hash, syn_db_hash).
-define(sem_database_hash, sem_db_hash).
