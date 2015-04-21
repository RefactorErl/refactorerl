" Vim global plugin for using RefactorErl
"
" Maintainer: Roland Király <kiralyroland@inf.elte.hu>
"             Gabor Olah <olikas.g@gmail.com>
"             Richard Szabo <szrqaai@inf.elte.hu>
"
" License:
"
" This file is part of RefactorErl.
"
" RefactorErl is free software: you can redistribute it and/or modify
" it under the terms of the GNU Lesser General Public License as published
" by the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
"
" RefactorErl is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU Lesser General Public License for more details.
"
" You should have received a copy of the GNU Lesser General Public License
" along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
"
" The Original Code is RefactorErl.
"
" The Initial Developer of the Original Code is Eötvös Loránd University.
" Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
" are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
" and Ericsson Hungary. All Rights Reserved.

" For usage see: 
" http://pnyf.inf.elte.hu/trac/refactorerl/wiki/VimInterface

if exists("g:loaded_referl")
    finish
endif

let g:loaded_referl = 1
let g:ref_vis_start = -1
let g:ref_vis_stop = -1

let s:save_cpo = &cpo
set cpo&vim

if !hasmapto('<Plug>RefactorErl')
  map <unique> <Leader>a <Plug>RefactorErl
endif

let s:ref_base_path = '/path/to/tool'
let s:refpath =  '/path/to/RefactorErl'
let s:ref_undo_mode = 'one_step' "one_step or selective_undo
let s:ref_datadir = '/path/to/data/dir'
let s:ref_name_type = 'name' "name or sname
let s:ref_server_name = 'refactorerl@localhost'
let s:ref_db_type = 'mnesia'

let s:refpath = s:refpath . ' '

if !exists('g:ref_autocommands_loaded')
	let g:ref_autocommands_loaded = 1
	augroup referl
		if s:ref_undo_mode == 'selective_undo'
			set ul=-1
			autocmd BufWritePost *.erl call s:fileSaved(expand("<afile>:p"))
			autocmd BufWritePost *.hrl call s:fileSaved(expand("<afile>:p"))

			autocmd InsertEnter *.erl call s:fileEdited(expand("<afile>:p"))
			autocmd InsertEnter *.hrl call s:fileEdited(expand("<afile>:p"))
			autocmd InsertLeave *.erl call s:fileEdited(expand("<afile>:p"))
			autocmd InsertLeave *.hrl call s:fileEdited(expand("<afile>:p"))

			autocmd BufRead *.erl call s:fileAdded(expand("<afile>:p"))
			autocmd BufRead *.hrl call s:fileAdded(expand("<afile>:p"))

			autocmd BufEnter *.hrl call s:loadMenu(expand("%:p"))
			autocmd BufEnter *.erl call s:loadMenu(expand("%:p"))

			autocmd BufDelete *.erl call s:fileDropped(expand("<afile>:p"))
			autocmd BufDelete *.hrl call s:fileDropped(expand("<afile>:p"))

			autocmd VimLeavePre *.erl call s:stopPlugin()
			autocmd VimLeavePre *.hrl call s:stopPlugin()
		endif
	augroup END

	let s:scriptfile=expand("<sfile>") 
	setlocal autoread
    execute "pyfile ".fnameescape(fnamemodify(s:scriptfile, ":h")."/referl.py") 
	python << EOF
import vim, time

global customUndo
global customMode
customMode=vim.eval('s:ref_undo_mode') == 'selective_undo'
if customMode:
	customUndo = CustomUndo(vim.eval('expand("%:p")'), vim.eval('s:refpath')) 
EOF
endif


noremenu  <silent> &RefactorErl.&Server.St&art\ server :call <SID>refsystem('start')<CR>
noremenu  <silent> &RefactorErl.&Server.St&op\ server :call <SID>refsystem('stop')<CR>

noremenu  <silent> &RefactorErl.F&iles.&Add\ file :call <SID>addfile()<CR>
noremenu  <silent> &RefactorErl.F&iles.&Drop\ file :call <SID>dropfile()<CR>
noremenu  <silent> &RefactorErl.F&iles.&Show\ database :call <SID>show()<CR>
noremenu  <silent> &RefactorErl.F&iles.Draw\ &Graph :call <SID>drgph()<CR>

if s:ref_undo_mode == 'one_step'
	noremenu  <silent> &RefactorErl.&Undo\ (One\ step\ only) :call <SID>refundo()<CR>
elseif s:ref_undo_mode == 'selective_undo'
	noremenu  <silent> &RefactorErl.&Undo.last\ step :call <SID>customUndo(0)<CR>
	noremenu  <silent> &RefactorErl.&Undo.-Sep1- :

	noremenu  <silent> &RefactorErl.&Redo.last\ step :call <SID>customRedo(0)<CR>
	noremenu  <silent> &RefactorErl.&Redo.-Sep1- :
endif
noremenu  <silent> &RefactorErl.-Sep1- :

noremenu  <silent> &RefactorErl.&Semantic\ query :call <SID>paramsq()<CR>

noremenu  <silent> &RefactorErl.-Sep1- :

noremenu  <silent> &RefactorErl.&Function.Reorder\ function\ &parameter :call <SID>refact('reorder_funpar')<CR>
noremenu  <silent> &RefactorErl.&Function.Generate\ function\ &specification :call <SID>refact('genspec')<CR>

noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Eliminate\ function\ call :call <SID>refact('inline_fun')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Eliminate\ macro\ substitution :call <SID>refact('inline_mac')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Eliminate\ variable :call <SID>refact('elim_var')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Eliminate\ fun\ expression :call <SID>refactrange('expand_funexpr')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ function :call <SID>refactrange('extract_fun')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ import :call <SID>refactrange('introduce_import')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ record :call <SID>refactrange('introduce_rec')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ variable :call <SID>refactrange('merge')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ process :call <SID>refact('funapp_to_proc')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ function\ argument :call <SID>refactrange('gen')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ tuple :call <SID>refactrange('tuple_funpar')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Transform\ list\ comprehension :call <SID>refactrange('list_comp')<CR>

noremenu  <silent> &RefactorErl.&Move\ to\ another\ module.Move\ function :call <SID>refact('move_fun')<CR>
noremenu  <silent> &RefactorErl.&Move\ to\ another\ module.Move\ macro :call <SID>refact('move_mac')<CR>
noremenu  <silent> &RefactorErl.&Move\ to\ another\ module.Move\ record :call <SID>refact('move_rec')<CR>

noremenu  <silent> &RefactorErl.&Renaming.Universal\ renamer :call <SID>refact('rename')<CR>
noremenu  <silent> &RefactorErl.&Renaming.Rename\ function :call <SID>refact('rename_fun')<CR>
noremenu  <silent> &RefactorErl.&Renaming.Rename\ header :call <SID>refact('rename_header')<CR>
noremenu  <silent> &RefactorErl.&Renaming.Rename\ macro :call <SID>refact('rename_mac')<CR>
noremenu  <silent> &RefactorErl.&Renaming.Rename\ module :call <SID>refact('rename_mod')<CR>
noremenu  <silent> &RefactorErl.&Renaming.Rename\ record :call <SID>refact('rename_rec')<CR>
noremenu  <silent> &RefactorErl.&Renaming.Rename\ record\ field :call <SID>refact('rename_recfield')<CR>
noremenu  <silent> &RefactorErl.&Renaming.Rename\ variable :call <SID>refact('rename_var')<CR>

noremenu  <silent> &RefactorErl.&Upgrade.Upgrade\ regexp\ interface :call <SID>refact('upgrade_regexp')<CR>

noremenu  <silent> &RefactorErl.-Sep1- :

noremenu  <silent> &RefactorErl.Help :call <SID>help()<CR>

command Refh call s:help()
command RefSta call s:refsystem('start')
command RefSto call s:refsystem('stop')
command Refa call s:addfile()
command Refd call s:dropfile()
command Refls call s:show()
command Refgraph call s:drgph()
command Refundo call s:refundo()
command Refredo call s:refredo()
command Refsq call s:paramsq()
command Refrf call s:refact('rename_fun')
command Refrh call s:refact('rename_header')
command Refrc call s:refact('rename_mac')
command Refrm call s:refact('rename_mod')
command Refrrf call s:refact('rename_recfield')
command Refrrd call s:refact('rename_rec')
command Refrv call s:refact('rename_var')
command Refmf call s:refact('move_fun')
command Refmm call s:refact('move_mac')
command Refmr call s:refact('move_rec')
command Refef call s:refact('inline_fun')
command Refof call s:refact('reorder_funpar')
command Refgs call s:refact('genspec')
command Refem call s:refact('inline_mac')
command Refev call s:refact('elim_var')
command Refeu call s:refactrange('expand_funexpr')
command Refif call s:refactrange('extract_fun')
command Refii call s:refactrange('introduce_import')
command Refir call s:refactrange('introduce_rec')
command Refiv call s:refactrange('merge')
command Reffp call s:refact('funapp_to_proc')
command Refia call s:refactrange('gen')
command Reftf call s:refactrange('tuple_funpar')
command Reftl call s:refactrange('list_comp')
command Refuir call s:refact('upgrade_regexp')


let s:winop = 0

function s:help()
    if s:winop == 1
        execute ":confirm close"
    endif
    let s:winop = 1
    botright new
    set buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
    call setline(2,'See the manual at http://pnyf.inf.elte.hu/trac/refactorerl/wiki/VimInterface')
	call setline(3, 'More information about the project: http://plc.inf.elte.hu/erlang')
    1
endfunction

" Universal refactoring function calling ri:transform/2.
" Param: refactoring is the name of the refactoring accepted by ri:transform.
function s:refact(refactoring)
    let cbn = expand("%:p")
    let pos = line2byte( line( "." )) + col( "." ) - 1
	call s:sendBufsrvCmd("begin-update")
	call s:sendBufsrvCmd('refac-type "' . a:refactoring . '"')
    let stcmd = s:refpath." ri transform " . a:refactoring . " \"[{file, \\\"" . cbn . "\\\"}, {position, " . pos . "}]\""
    execute '!' . stcmd
	call s:sendBufsrvCmd("end-update")
	autocmd! referl BufDelete *.erl
	autocmd! referl BufDelete *.hrl
	python << EOF
import vim
global customUndo
global customMode

if customMode:
	oldFilename = vim.eval("cbn")
	if oldFilename in customUndo.renames.keys():
		newFilename = customUndo.renames[oldFilename]
		vim.command(":edit " + newFilename)
		vim.command("bw! " + oldFilename)
		del customUndo.renames[oldFilename]
EOF
	call s:handleChanges()
	autocmd referl BufDelete *.erl call s:fileDropped(expand("<afile>:p"))
	autocmd referl BufDelete *.hrl call s:fileDropped(expand("<afile>:p"))
    1
endfunction

" Universal refactoring function based on range calling ri:transform/2.
" Param: refactoring is the name of the refactoring accepted by ri:transform.
function s:refactrange(refactoring)
    let cbn = expand("%:p")
    let posstr = "{posrange, {". g:ref_vis_start .",". g:ref_vis_stop ."}}"
	call s:sendBufsrvCmd("begin-update")
	call s:sendBufsrvCmd("refac-type " . a:refactoring)
    let stcmd = s:refpath." ri transform " . a:refactoring . " \"[{file, \\\"" . cbn . "\\\"}, ".posstr."]\""
    execute '!' . stcmd
    let g:ref_vis_start = -1
    let g:ref_vis_stop = -1
	call s:sendBufsrvCmd("end-update")
	call s:handleChanges()
	"call s:refreshFileWindow( bufnr( "%" ), bufname( bufnr( "%" ) ) )
    1
endfunction

" Semantic query handler
" TODO position fix
function s:paramsq()
    let cbf = expand("%:p")
    let querystr = input("Type the query: ")
    let pos = line2byte( line( "." )) + col( "." ) - 1

    let shellcmd = s:refpath . " ri q \\\"" . cbf . "\\\" \"{position, " . pos . "}\" \\\"" . querystr. "\\\" \"[linenum,no_check]\""
    let bnum = bufnr('referl')
    if bufexists(bnum)
        execute ':buffer '.bnum
        execute ':bd'
    endif
    execute ':botright  new '.'referl'
    set buftype=nofile noswapfile nowrap
    call setline(1, 'Referl Command:  ' . querystr)
    call append(line('$'), substitute(getline(2), '.', '=', 'g'))
    silent execute '$read !'. shellcmd
    1
endfunction

function s:refundo()
if s:ref_undo_mode == 'one_step'
    let stcmd = s:refpath." ri undo"
    execute '!'.stcmd
    1
elseif s:ref_undo_mode == 'selective_undo'
	call s:customUndo(0)
endif
endfunction

function s:refredo()
if s:ref_undo_mode == 'one_step'
    echo "No redo in one step mode!"
	"let stcmd = s:refpath." ri undo"
    "execute '!'.stcmd
    "1
elseif s:ref_undo_mode == 'selective_undo'
	call s:customRedo(0)
endif
endfunction

function SavePos()
    if mode()!~#"^[vV\<C-v>]"
        let g:ref_vis_start = line2byte( line( "'<" )) + col( "'<" ) - 1
        let g:ref_vis_stop = line2byte( line( "'>" )) + col( "'>" ) - 1
    endif
    1
endfunction

noremap <silent> <F2> :call SavePos()<CR>

function s:refsystem(fun)
    let stcmd = s:refpath.a:fun
    if a:fun == 'start'
	    let stcmd = stcmd.' '.s:ref_db_type
    endif
    execute '!'.stcmd
    1
endfunction


function s:addfile()
    let stcmd = s:refpath." ri add \"".expand("%:p")."\""
    execute '!'.stcmd
	if s:ref_undo_mode == 'selective_undo'
		call s:loadMenu(expand("%:p"))
	endif
    1
endfunction

function s:dropfile()
    let stcmd = s:refpath." ri drop \"".expand("%:p")."\""
    execute '!'.stcmd
	if s:ref_undo_mode == 'selective_undo'
		call s:loadMenu(expand("%:p"))
	endif
    1
endfunction


function s:bfnpath(bfname)
    let lastSlash = strridx(a:bfname, '/')
    return strpart(a:bfname, 0, lastSlash+1)
endfunction

function s:shname(bfname)
    let lastSlash = strridx(a:bfname, '/')
    let shrtname = strpart(a:bfname, lastSlash+1, strlen(a:bfname))
    let dotp = strridx(shrtname, '.')
    return strpart(shrtname, 0, dotp)
endfunction

function s:refreshFileWindow(currentbuf, filename)
	"exe 'bw '.a:currentbuf
    exe ':edit! '.a:filename
endfunction

function s:show()
    let bnum = bufnr('referl')
    if bufexists(bnum)
        execute ':buffer '.bnum
        execute ':bd'
    endif
    execute ':botright  new '.'referl'
    set buftype=nofile noswapfile nowrap
    let shellcmd = s:refpath." ri ls "
    call setline(1, 'Referl Command:  ' ." Show database")
    call append(line('$'), substitute(getline(2), '.', '=', 'g'))
    silent execute '$read !'. shellcmd
    1
endfunction

function s:drgph()
    let file = input("Type the file name here: ")
    let stcmd = s:refpath." ri graph \"".file."\""
    execute '!'.stcmd
    echo stcmd
    1
endfunction

command! -complete=shellcmd -nargs=+ DoShellCmd call s:refAutoCommand(<q-args>)
function s:refAutoCommand(command)
    "let mainbuf = bufnr("$")
    let shellcmd = s:refpath.pr
    for word in split(shellcmd)
    if word[0] =~ '\v[%#<]'
        let word = expand(word)
    endif
    let word = shellescape(word, 1)
    call add(words, word)
    endfor
    let shellcmd = join(words)
    let bnum = bufnr('referl')
    if bufexists(bnum)
        execute ':buffer '.bnum
        execute ':bd'
    endif
    execute ':botright  new '.'referl'
    set buftype=nofile noswapfile nowap
    call setline(1, 'Referl Command: ' . pr)
    call append(line('$'), substitute(getline(2), '.', '=', 'g'))
    silent execute '$read !'. shellcmd
    1
endfunction

" custom undo operations

function s:customUndo(n)
	let filename = expand("%:p")
	call s:sendBufsrvCmd("undo " . "\"" . filename . "\"" . " " . a:n)
	autocmd! referl BufDelete *.erl
	autocmd! referl BufDelete *.hrl
	call s:handleChanges()
	autocmd referl BufDelete *.erl call s:fileDropped(expand("<afile>:p"))
	autocmd referl BufDelete *.hrl call s:fileDropped(expand("<afile>:p"))
	echo "Undo"
endfunction

function s:customRedo(n)
	let filename = expand("%:p")
	call s:sendBufsrvCmd("redo " . "\"" . filename . "\"" . " " . a:n)
	autocmd! referl BufDelete *.erl
	autocmd! referl BufDelete *.hrl
	call s:handleChanges()
	autocmd referl BufDelete *.erl call s:fileDropped(expand("<afile>:p"))
	autocmd referl BufDelete *.hrl call s:fileDropped(expand("<afile>:p"))
	echo "Redo"
endfunction

function s:sendBufsrvCmd(cmd)
	let str = a:cmd
	python << EOF
import vim
global customUndo
global customMode
if customMode:
	customUndo.sendCommand(vim.eval('str'))
EOF
endfunction

function s:fileEdited(filename)
	python << EOF
global customUndo
if (vim.eval('a:filename') in customUndo.buffers.keys()):
	if vim.eval('&modified'):
		vim.command(":silent :w! " + vim.eval("a:filename") + "~")
		customUndo.sendCommand('load-edited "' + vim.eval('a:filename') + '~" "' + vim.eval('a:filename') + '"')
		customUndo.readAnswer()
		customUndo.loadMenu(vim.eval('a:filename'))
EOF
endfunction


function s:fileSaved(filename)
	python << EOF
global customUndo
if (vim.eval('a:filename') in customUndo.buffers.keys()):
	customUndo.sendCommand('load-edited "' + vim.eval('a:filename') + '" "' + vim.eval('a:filename') + '"')
	customUndo.sendRefCommand(['add', '"' + vim.eval('a:filename') + '"'])
	customUndo.readAnswer()
	customUndo.loadMenu(vim.eval('a:filename'))

EOF
endfunction

function s:handleChanges()
	python << EOF
global customUndo
	
global customMode
if customMode:
	customUndo.readAnswer()
	customUndo.loadMenu(vim.eval("expand('%:p')"))
EOF
endfunction

function s:loadMenu(filename)
	python << EOF
global customUndo
import vim
filename = vim.eval("a:filename")
customUndo.loadMenu(filename)
EOF
endfunction

function s:fileAdded(filename)
	setlocal autoread
	python << EOF
global customUndo
import vim
filename = vim.eval("a:filename")
customUndo.checkBuffer(filename)
EOF
endfunction

function s:fileDropped(filename)
let filename = a:filename	
	python << EOF
global customUndo
import vim

filename = vim.eval("a:filename")
customUndo.closeBuffer(filename)
EOF
endfunction

function s:stopPlugin()
	python << EOF
global customUndo
customUndo.stopPlugin()
EOF

endfunction


" Avoid installing twice or when in unsupported Vim version.
if exists('g:loaded_file_line') || (v:version < 700)
	finish
endif





let &cpo = s:save_cpo
