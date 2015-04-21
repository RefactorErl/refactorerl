%% General
-define(IdFun, fun(X) -> X end).

%% Constants
-define(INFINITY, infinity).
-define(stbar_Notif_Timeout, infinity).
-define(stbar_ShownUp_Timeout, 2500).

-define(SEP, "\t").
-define(SEP_INT, hd(?SEP)).

-define(wxInvFileName, "wx_last_dependency_result").

%% Dependency graph
-define(INCH, 10). %% inch to pixel ratio 1 inch = x pixels
-define(G,     5). %% Number of groups and zoom levels
-define(C(X), referl_wx_logic:ceil(X)). %% ceil of a number
-define(F(X), referl_wx_logic:floor(X)). %% floor of a number
-define(NodeVsButtonRatio, 1.9). %% Float used to multiply node's radius to get
                                 %% a width and height for node buttons on Linux

%% Modules
-define(Logic,  referl_wx_logic).
-define(wxMain, referl_wx).
-define(wxCB,   referl_wx_codebrowser).
-define(wxFB,   referl_wx_filebrowser).
-define(wxIB,   referl_wx_invbox).

-define(ExecLCall(FunName, Args),                      referl_wx:execute_logic_ui_call(FunName, Args)).
-define(HandleRes(R, ErrRet),                          referl_wx:handle_result(R, ErrRet)).
-define(HandleRes(R), 					                       referl_wx:handle_result(R)).
-define(HandleRes(R, HandlerFun, HandlerArgs), 	       referl_wx:handle_result(R, HandlerFun, HandlerArgs)).
-define(HandleRes(R, HandlerFun, HandlerArgs, ErrRet), referl_wx:handle_result(R, HandlerFun, HandlerArgs, ErrRet)).

-define(SEQ(A,B),       ?Query:seq(A,B)).
-define(SEQ(QueryList), ?Query:seq(QueryList)).
-define(ANY(QueryList), ?Query:any(QueryList)).

-define(DBHASH, ?Logic:get_database_hash()).

%% Records
-record(q_info, %% query info 
	{
	  key, %% = {QString, File, Pos}  (if no file and pos then it is undefined)
             %%   where QString is a 'safe string' (no whitespaces between letters)
	  qstr, %% ~ safeq with whitespace
      users,
      result,
      hash
	 }).

-record(rq_info, %% query info 
	{
      reqid,
      user,
      qid, 
      qstr
	 }).

-record(deprec, %% dependency graph record
  {
      opts,
      hash,
      graph
   }).

%% Investigation
-record(inv,
   {
      name,   %% Id
      hash,   %% DB Hash
      users,  %% Users who are allowed to see this investigation
      invdata %% Investigation tree
   }).

-record(invnode, 
  {
      name,  %% Name of the node (user given)
      show,  %% Whether this invnode is shown or notS
      node,  %% A real graphnode or memo (if this invnode is a memo)
      text,  %% The text stored in this invnode
      label, %% The label of the edge belonging to this node
      memo,  %% If this invnode is a memo or not
      file_data, %% Filepath, offset position for queries (scalar) and start line (linecol)
      pos    %% Position on the display (given by wxWindow:getPosition)
   }).

%% Error handling

%% Give a standard "RefErl Wx Error Tuple": {error, ErrorText}
%% Handles RefErrors and {error, Error} tuples
-define(wxError(Error), case Error of {{_M, _T, _D}, ErrTxt}      -> {error, ErrTxt}; 
                                      {_,ErrTxt}                  -> {error, ErrTxt};
                                      ErrTxt when is_list(ErrTxt) -> {error, ErrTxt}; 
                                      _Err                        -> {error, "An unknown error occured"} end).
                                      %% ... or the stack trace: io_lib:format("~p",[Err])
-define(wxErr(StrErr), {error, StrErr}).
