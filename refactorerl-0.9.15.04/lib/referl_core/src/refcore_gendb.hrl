%%% ----------------------------------------------------------------------------
%%% Data types

%% Class table: stores the attribute names for every node class
-record(class, {name, attribs, links}).
%% Target table: stores the link target class name for every starting class,
%% link tag pair
-record(target, {from, tag, to}).

%%% ----------------------------------------------------------------------------
%%% Macro Shortcuts

%% #class records
-define(CLASS_SCHEMA, referl_class_schema).
%% #target records
-define(CLASS_TARGETS, referl_class_targets).
%% saving envs
%% enviromental nodes depend on the used graph
%% envs are stored in different files, based on the used graph name,
%% to avoid mixing graph- related  informations
-define(ENV_CONF_FILE, 
        begin
            ENV_DBMOD = get(dbmod),
            PostFix =
                case ENV_DBMOD of
                    %mnesia doesn't handle multi graphs.
                    refdb_mnesia ->
                        "mnesia";
                    undefined -> 
                        "";
                    _ when is_atom(ENV_DBMOD) ->
                        %need to call actual_graph/0 directly to avoid being in a deadlock.
                        atom_to_list(element(2,ENV_DBMOD:actual_graph()))
                end,
            filename:join([?MISC:data_dir(), 
                           "refactorerl.configuration_" ++ PostFix])
        end).
%% debug
-define(DEBUG(Deb, Tag), sys:handle_debug(Deb, {?MODULE, handle_dbg}, ?MODULE, Tag)).
%% sync operation
-define(SYNC_OP, '$sync_op').
%% async operation
-define(ASYNC_OP, '$async_op').
%% transform file for undo
-define(BEFORE_TRANSFORMATION_FILE, "__before_tarnsformation").

%%% ----------------------------------------------------------------------------
%%% Macros for the db implementations

%% next class fwd
-define(GET_NEXT_CLASS_FWD(Class, Tag),
        case ets:match_object(?CLASS_TARGETS, #target{from = Class, tag = Tag, to = '_'}) of
            [#target{to = NextCls}] -> {class, NextCls};
            _                       -> throw({bad_path, Class, Tag, fwd})
        end).
%% next class back
-define(GET_NEXT_CLASS_BACK(Class, Tag),
        case ets:match_object(?CLASS_TARGETS, #target{from = '_', tag = Tag, to = Class}) of
            [#target{from = NextCls}] -> {class, NextCls};
            _                         -> throw({bad_path, Class, Tag, back})
        end).
%% target for path
-define(GET_NEXT_CLASS(Dir, Class, Tag),
	case Dir of
            fwd -> ?GET_NEXT_CLASS_FWD(Class, Tag);
            back -> ?GET_NEXT_CLASS_BACK(Class, Tag)
        end).
%% calcualating fwd targets
-define(GET_NEXT_CLASSES_FWD(Class),
	[{Tag, TCl} || #target{tag = Tag, to = TCl}
		       <- ets:match_object(?CLASS_TARGETS,
					   #target{from = Class,
						   tag = '_',
						   to = '_'})]).
%% calculating back targets
-define(GET_NEXT_CLASSES_BACK(Class),
	[{Tag, FCl} || #target{from = FCl, tag = Tag}
		       <- ets:match_object(?CLASS_TARGETS,
					   #target{from = '_',
						   tag = '_',
						   to = Class})]).
%% calculating targets
-define(GET_NEXT_CLASSES(Dir, Class),
	case Dir of
	    fwd -> ?GET_NEXT_CLASSES_FWD(Class);
	    back -> ?GET_NEXT_CLASSES_BACK(Class)
	end).
%% class attributes
-define(GET_CLASS_ATTRS(Class),
	begin
	    [#class{attribs = Attrs}] =
		ets:match_object(?CLASS_SCHEMA, #class{name = Class,
						       attribs = '_',
						       links = '_'}),
	    Attrs
	end).
%% list of available classes
-define(GET_LIST_OF_CLASSES,
	ets:foldl(fun(#class{name = Cls}, Acc) -> [Cls | Acc] end, [], ?CLASS_SCHEMA)).
%% validate class
-define(IS_VALID_CLASS(Class),
	case length(ets:match_object(?CLASS_SCHEMA, #class{name=Class,
							   attribs = '_',
							   links = '_'})) of
	    0 -> exit({aborted, {not_exists, Class}});
	    _ -> ok
	end).
%% save environmental variables
-define(SAVE_ENVS,
	refcore_gendb:save_envs(?MODULE)).
%% restore environmental variables
-define(RESTORE_ENVS,
	refcore_gendb:restore_envs(?MODULE)).
%% handle path internally by gendb
-define(HANDLE_PATH_PLEASE(Node, Path),
	refcore_gendb:handle_path_here(Node, Path, ?MODULE)).
