@echo off

REM Defaults (also used to clear up variable values)

if not defined ERL SET ERL=erl


SET SERVER=yes
SET CLIENT=no
SET CPP_COMPILER=g++
SET NAME_TYPE=-sname
SET NAME=refactorerl@localhost
SET SRVNAME=refactorerl@localhost
SET BASE="%CD%"
SET ARGS=
SET YNAME=referl
SET YPATH=no_path
SET YPORT=8001
SET YLISTEN=0.0.0.0
SET BROWSER_ROOT=no_path
SET DBMOD=-dbmod refdb_mnesia
SET DBARGS=-dbargs []
SET COMPILE_CPP=true
SET COMPILE_BUFSRV=false
SET REFERL_DIR=data
SET YAWS189=false
SET POS=-pos default
SET SYNC=
SET IMAGES_DIR="%CD%"
SET RESTRICTED_MODE=false
SET ETS_LIMIT=32000
SET COMPILE_QT=false

REM Interpret arguments

:argloop
if x%1 == x goto endarg
if %1 == -erl goto erl
if %1 == -base goto base
if %1 == -wrangler goto wrangler
if %1 == -sname goto name
if %1 == -name  goto name
if %1 == -srvname goto srvname
if %1 == -server goto server
if %1 == -emacs goto emacs
if %1 == -g++ goto g++
if %1 == -build goto build
if %1 == -no_cpp goto no_cpp
if %1 == -client goto client
if %1 == -yaws_189 goto yaws_189
if %1 == -yaws goto yaws
if %1 == -yaws_path goto yaws_path
if %1 == -yaws_name goto yaws_name
if %1 == -yaws_port goto yaws_port
if %1 == -yaws_listen goto yaws_listen
if %1 == -nitrogen goto nitrogen
if %1 == -web2 goto web2
if %1 == -browser_root goto browser_root
if %1 == -db goto db
if %1 == -dbmod goto dbmod
if %1 == -dbargs goto dbargs
if %1 == -help goto help
if %1 == -dir goto dir
if %1 == -pos goto pos
if %1 == -synchronize goto synchronize
if %1 == -images_dir goto images_dir
if %1 == -restricted_mode goto restricted_mode
if %1 == -vim goto vim
if %1 == -qt_gui goto qt

shift
goto argloop

:qt
set COMPILE_QT=true
shift
goto argloop

:dir
shift
set REFERL_DIR=%1
shift
goto argloop

:erl
shift
set ERL=%1
shift
goto argloop

:base
shift
SET BASE=%1
shift
goto argloop

:wrangler
shift
SET ARGS=%ARGS% -pa %1
shift
goto argloop

:name
SET NAME_TYPE=%1
shift
SET NAME=%1
shift
goto argloop

:srvname
shift
SET SRVNAME=%1
shift
goto argloop

:server
set SERVER=yes
set CLIENT=server
shift
goto argloop

:emacs
set CLIENT=emacs
shift
goto argloop

:vim
set CLIENT=vim
shift
goto argloop

:g++
shift
set CPP_COMPILER=%1
shift
goto argloop

:build
set CLIENT=build
set SERVER=no
set NAME=build@localhost
shift
set TARGET=%1
shift
goto argloop

:no_cpp
set COMPILE_CPP=false
shift

:bufsrv
set COMPILE_BUFSRV=true
shift

:client
set SERVER=no
shift
goto argloop

:yaws_189
set YAWS189=true
shift
goto argloop

:yaws
set CLIENT=yaws
shift
goto argloop

:yaws_path
shift
set YPATH=%1
shift
goto argloop

:yaws_name
shift
set YNAME=%1
shift
goto argloop

:yaws_port
shift
set YPORT=%1
shift
goto argloop

:yaws_listen
shift
set YLISTEN=%1
shift
goto argloop

:nitrogen
set CLIENT=nitrogen
shift
goto argloop

:web2
set CLIENT=web2
shift
goto argloop

:browser_root
shift
set BROWSER_ROOT=%1
shift
goto argloop

:images_dir
shift
set IMAGES_DIR=%1
shift
goto argloop

:restricted_mode
set RESTRICTED_MODE=true
shift
goto argloop

:db
shift
if %1 == mnesia goto db_mnesia
if %1 == nif goto db_nif
if %1 == kcmini (goto db_kcmini) else (goto db_unknown)
shift
goto argloop

:db_mnesia
set DBMOD=-dbmod refdb_mnesia
set DBARGS=-dbargs []
goto argloop

:db_nif
set DBMOD=-dbmod refdb_nif
set DBARGS=-dbargs []
goto argloop

:db_kcmini
echo Currently Kyoto is an unsupported database mode only on Windows. Halting..
goto exit

:db_unknown
echo [WARNING] Unsupported database mode. Halting..
goto exit

:dbmod
shift
set DBMOD=-dbmod %1
shift
goto argloop

:dbargs
shift
set DBARGS=-dbargs %1
shift
goto argloop

:pos
shift
if %1 == abs set POS=-pos abs
if %1 == rel set POS=-pos rel
shift
goto argloop

:synchronize
shift
set SYNC=-s ri database_synchronization
shift
goto argloop

:help
echo Usage: referl [Option]...
echo Starts RefactorErl, using the current working directory as the data directory.

echo Recognised options:

echo   -build TARGET    Build TARGET (e.g. tool, doc, clean)
echo   -bufsrv          Bufferserver will be compiled (use with ''-build tool'')
echo   -igraph PATH     Path to the Igraph lib directory
echo   -yaws_189        Prepare for the Yaws 1.89 to use during compilation (use with '-build tool')
echo   -no_cpp          CPP code will not be compiled (use with '-build tool')

echo   -db [mnesia^|nif] Choose which database to use. Default is mnesia.
echo   -dir DIR         Sets the RefactorErl data directory
echo   -base PATH       Path to the RefactorErl base directory
echo   -pos POS         [abs^|rel] The positioning mode to use (default: abs)
echo   -erl PATH        Path to the Erlang executable to use
echo   -g++ PATH        Path of the g++ compiler to use
echo   -synchronize     Database synchronization
echo   -help            Print this help text

echo   -server          Start in server mode (no shell is started)
echo   -sname NAME      Short name of the Erlang node
echo   -name NAME       Full name of the Erlang node
echo   -srvname NAME    Name of the Erlang server node to connect
echo   -client          Start in client mode (no server is started)

echo   -emacs           Start as an Emacs client
echo   -vim             Start as a Vim client

echo   -nitrogen        Start with Nitrogen
echo   -web2            Start with web2
echo   -yaws_path PATH  Path to the Yaws ebin directory (need /ebin at the end)
echo   -yaws_name NAME  Set yaws server name
echo   -yaws_port PORT  Set yaws port
echo   -yaws_listen IP  Set yaws IP
echo   -browser_root    Set the file browser root dir
echo   -images_dir      Set root directory where generated Nitrogen images will be written
echo   -restricted_mode Set restricted mode on the web interface


goto exit
REM echo   -wrangler PATH   Path to a Wrangler installation

:endarg

REM Set extra arguments
if %CLIENT%==server   set ARGS=%ARGS% -noinput
if %CLIENT%==emacs    set ARGS=%ARGS% -noshell -s referl_emacs start %SRVNAME%
if %CLIENT%==vim      set ARGS=%ARGS% -noshell -s referl_vim start %SRVNAME%
if %CLIENT%==build    set ARGS=%ARGS% -noshell -run referl_gen_build start %TARGET%
if %CLIENT%==yaws     set ARGS=%ARGS% -run web_helper start_yaws from_script %YPATH% %YNAME% %YPORT% %YLISTEN%
if %CLIENT%==nitrogen set ARGS=%ARGS% -noshell -run referl_ui_nitrogen_core start_nitrogen from_script %YPATH% %YNAME% %YPORT% %YLISTEN% %BROWSER_ROOT% %IMAGES_DIR% %RESTRICTED_MODE%
if %CLIENT%==web2     set ARGS=%ARGS% -noshell -run referl_ui_web2 start from_script %YPATH% %YPORT% %YLISTEN% %RESTRICTED_MODE% %BROWSER_ROOT%

:start

SET ERL_LIBS=%BASE:"=%\lib

if %SERVER%==yes goto server
if %CLIENT%==build goto build
goto client

:server
"%ERL%" ^
  -smp ^
  %NAME_TYPE%  "%NAME%" ^
  -config "%BASE%\sys.config" ^
  -boot   "%BASE%\refactorerl" ^
  -env ERL_MAX_ETS_TABLES %ETS_LIMIT% ^
  -s reflib_ui_router set_ref_node server %NAME% ^
  %$SYNC% ^
  -mnesia dir \"%REFERL_DIR%\" ^
  +W "w" ^
  %DBMOD% %DBARGS% ^
  %POS% ^
  %ARGS%
goto exit


:build
"%ERL%" -make


"%ERL%" ^
  %NAME_TYPE% "%NAME%" ^
  -env ERL_MAX_ETS_TABLES %ETS_LIMIT% ^
  %DBMOD% %DBARGS% ^
  %ARGS%


:client
"%ERL%" ^
  %NAME_TYPE%  "%NAME%" ^
  -s reflib_ui_router set_ref_node %SRVNAME% ^
  -mnesia dir \"%REFERL_DIR%\" ^
  %ARGS%

:exit
