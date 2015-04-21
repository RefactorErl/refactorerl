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

%%% ============================================================================
%%% Module Informations

%%% @doc Efficient matrix implementation to be used for clustering.
%%% Attribute matrices are usually quite sparse, entity distance matrices
%%% are not; this representation should work reasonably well with both.
%%%
%%% The module also provides an attribute matrix representation.
%%% An attribute matrix contains information about the relationships
%%% between the entities of the files in the database.
%%% These matrices can store information about functions, or about modules.
%%%
%%% It stores the values in the following format:
%%% {{Ent1, Ent2}, Value}
%%%
%%% Ent1 and Ent2 are either a module or a function, depending on the type of
%%% the attribute matrix. Value is a #matrix_item record with 3 fields:
%%% "func", "rec" and "macro". It represents, that Ent1 calls Ent2 "func" times,
%%% they use "rec" common records and "macro" common macros.
%%% There are getter and setter functions for this data type, so other modules
%%% can use these values without having to "manually" recover the desired
%%% information.

%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Laszlo Budai <budail@caesar.elte.hu>

-module(cl_matrix).
-vsn("$Rev: 10531 $").

-include_lib("referl_cluster/include/cluster.hrl").



%%% ============================================================================
%%% Imports/Exports

% Proplists
-import(proplists, [get_value/2, get_value/3]).

% Matrix
-export([new/3, new/4, delete/1, clone/1, pack/1, unpack/1,
         is_matrix/1, close/1]).
% Manage rows and columns
-export([add_col/2, add_col/3, add_row/2, add_row/3,
         insert_new_row/3, % depricated
         del_col/2, del_row/2,
         swap_row/3, swap_col/3, move_row/4, move_col/4,
         reorder_rows/2, reorder_cols/2]).
% Get and set values
-export([cols/1, rows/1,
         get/3, get2/3, get_row/2, get_col/2,
         get_row_non_def/2, get_col_non_def/2,
         get_row_non_def_ids/2, get_col_non_def_ids/2]).
% Transform matrix
-export([transform/2, transform2/2,
         fold_col/4, fold_row/4, fold_col2/5, fold_row2/5]).
% Conversions
-export([to_list/1, from_list/2, to_record/1, from_record/1]).
% Dump
-export([dump/1, dump/2, dump/4]).

% Getters for matrix_item
-export([get_from_dets/4, get_from_ets/4, get_from_value/2,
         get_default/0, itemize/1, set_ets/5, change_value/3]).

% Attribute matrix
-export([load_attribute_matrix/1, get_entities/1, unique_fun_name/1]).

%%% ============================================================================
%%% Types

-record(matrix, {rows, cols, default, entity, table}).
-record(matrix_item, {func, rec, macro}).

%%% @type row() = term(). Row labels.

%%% @type col() = term(). Column labels.

%%% @type val() = term(). Matrix element values.

%%% @type matrix(). The representation of a matrix is not defined.

%%% @type entity() = term(). Entities of the matrix.

%%% @type matrix_item() = #matrix_item{}. An item of the attribute matrix.



%%% ============================================================================
%%% Matrix

%% @spec new([row()], [col()], val()) -> #matrix{}
%%
%% @doc Creates a new matrix with `Rows' as row labels, `Cols' as column
%% labels and `Default' as the default value of uninitialised elements.
new(Rows, Cols, Default) ->
    #matrix{rows=Rows, cols=Cols, default=Default, entity=module,
            table=ets:new(table,[])}.
new(Rows, Cols, Default, Entities) ->
    #matrix{rows=Rows, cols=Cols, default=Default, entity=Entities,
            table=ets:new(table,[])}.

%% @spec delete(matrix()) -> ok
%%
%% @doc Deletes the matrix.
delete(#matrix{table=Tab}) ->
    ets:delete(Tab).


%% @spec clone(matrix()) -> matrix()
%%
%% @doc Creates a copy of a matrix.
%% If modification is made in the original or in the clone, it will not affect
%% the other one.
clone(#matrix{table=Table} = Matrix) ->
    EtsTable = ets:new(clone_matrix,[]),
    dets:to_ets(Table, EtsTable),
    etsClone = ?MISC:ets_clone(Table),
    {_, detsClone} = dets:open_file(clone_dets_matrix, [{type,set}]),
    dets:from_ets(detsClone, etsClone),
    Matrix#matrix{table=detsClone}.


%% @spec pack(Matrix::matrix()) -> PackedMatrix::matrix()
%% @doc Create a new `matrix' object which contains all information from
%%      the original `Matrix' as values.
%%      Don't use the returned `PackedMatrix' object as the normal
%%      `matrix' objects! In this case a run time error may occur.
%%      If you have a packed `matrix' object you need to unpack with the
%%      {@link unpack/1} function before use it.
%% @see unpack/1
pack(Matrix=#matrix{}) ->
    to_record(Matrix).

%% @spec unpack(PackedMatrix::matrix()) -> Matrix::matrix()
%% @doc Create a normal `matrix' object from the given packed
%%      `Packedmatrix' object.
%% @see pack/1
unpack(Matrix=#matrix{}) ->
    from_record(Matrix).

close(Matrix) ->
    ets:delete(Matrix#matrix.table).

%% @spec is_matrix(Term::term()) -> boolean()
%% @doc  Return `true' if `Term' is a `matrix()' otherwise `false'.
is_matrix(#matrix{}) -> true;
is_matrix(_)         -> false.


%%% ============================================================================
%%% Manage rows and columns

%% @spec add_col(col(), matrix()) -> matrix()
%%
%% @doc Adds a new column label `Col' to the matrix. Returns the updated
%% matrix.
add_col(Col, Matrix=#matrix{cols=Cols}) ->
    case lists:member(Col, Cols) of
        true ->
            erlang:error({existing_column, Col});
        false ->
            Matrix#matrix{cols=Cols++[Col]}
    end.

%% @spec add_row(row(), matrix()) -> matrix()
%%
%% @doc Adds a new row label `Row' to the matrix. Returns the updated
%% matrix.
add_row(Row, Matrix=#matrix{rows=Rows}) ->
    case lists:member(Row, Rows) of
        true ->
            erlang:error({existing_row, Row});
        false ->
            Matrix#matrix{rows=Rows++[Row]}
    end.


%% @spec add_col(ColLabel::row(),
%%               ColValues::[ {RowLabel::col(), CellValue::val()} ],
%%               Matrix::matrix()) -> NewMatrix::matrix()
%% @doc Add a new column labeled with `ColLabel' to the `Matrix' matrix. The
%%      pairs in the `ColValues' specify the initial values in the column.
%%      Return the updated matrix.
add_col(ColLabel, ColValues, Matrix=#matrix{}) ->
    M2 = add_col(ColLabel, Matrix),
    #matrix{table=Tab} = M2,
    lists:foreach(
        fun ({Row, Value}) -> ets:insert(Tab, {{Row, ColLabel}, Value}) end,
        ColValues),
    M2.

%% @spec add_row(RowLabel::row(),
%%               RowValues::[{ColLabel::col(), CellValue::val()}],
%%               Matrix::matrix()) -> NewMatrix::matrix()
%% @doc Add a new row labeled with `RowLabel' to the `Matrix' matrix. The
%%      pairs in the `RowValues' specify the initial values in the row.
%%      Return the updated matrix.
add_row(RowLabel, RowValues, Matrix=#matrix{}) ->
    M2 = add_row(RowLabel, Matrix),
    #matrix{table=Tab} = M2,
    lists:foreach(
        fun ({Col, Value}) -> ets:insert(Tab, {{RowLabel, Col}, Value}) end,
        RowValues),
    M2.

%% @spec insert_new_row(row(), [{col(), val()}], matrix()) -> matrix()
%%
%% @doc Inserts a new row with its elements into the matrix, and returns the
%% updated matrix.
%% @deprecated Please use the function {@link add_row/3} instead.
insert_new_row(RowLabel, RowValues, Matrix) ->
    % M2 = add_row(RowLabel, Matrix),
    % #matrix{table=Tab} = M2,
    % lists:foreach(fun ({Col, Value}) ->
                    % ets:insert(Tab, {{RowLabel, Col}, Value})
                  % end, RowValues),
    % M2.
    add_row(RowLabel, RowValues, Matrix).


%% @spec del_col(col(), matrix()) -> matrix()
%%
%% @doc Deletes column `Col' from the matrix, returns the updated matrix.
del_col(Col, Matrix=#matrix{cols=Cols, rows=Rows, table=Tab}) ->
    lists:foreach(
      fun(Row) -> ets:delete(Tab, {Row, Col}) end,
      Rows),
    Matrix#matrix{cols=Cols -- [Col]}.

%% @spec del_row(row(), matrix()) -> matrix()
%%
%% @doc Deletes row `Row' from the matrix, returns the updated matrix.
del_row(Row, Matrix=#matrix{cols=Cols, rows=Rows, table=Tab}) ->
    lists:foreach(
      fun(Col) -> ets:delete(Tab, {Row, Col}) end,
      Cols),
    Matrix#matrix{rows=Rows -- [Row]}.


%% @spec swap_row(Row1::row(), Row2::row(), Matrix::matrix()) ->
%%           NewMatrix::matrix()
%% @doc Swap `Row1' and `Row2' rows in the matrix.
swap_row(Row1, Row2, M=#matrix{rows=Rows}) ->
    M#matrix{rows=?MISC:list_swap(Row1, Row2, Rows)}.

%% @spec swap_col(Col1::col(), Col2::col(), Matrix::matrix()) ->
%%           NewMatrix::matrix()
%% @doc Swap `Col1' and `Col2' columns in the matrix.
swap_col(Col1, Col2, M=#matrix{cols=Cols}) ->
    M#matrix{cols=?MISC:list_swap(Col1, Col2, Cols)}.


%% @spec move_row(SrcRow::row(), DstRow::row(), Mode, Matrix::matrix()) ->
%%           NewMatrix::matrix()
%%      Mode = before | after
%% @doc Move the `SrcRow' before/after the `DstRow' in the matrix.
move_row(SrcRow, DstRow, Mode, M=#matrix{rows=Rows}) ->
    M#matrix{rows=?MISC:list_move(SrcRow, DstRow, Mode, Rows)}.

%% @spec move_col(SrcCol::row(), DstCol::row(), Mode, Matrix::matrix()) ->
%%           NewMatrix::matrix()
%%      Mode = before | after
%% @doc Move the `SrcCol' before/after the `DstCol' in the matrix.
move_col(SrcCol, DstCol, Mode, M=#matrix{cols=Cols}) ->
    M#matrix{cols=?MISC:list_move(SrcCol, DstCol, Mode, Cols)}.


%% @spec reorder_rows(Rows::[row()], Matrix::matrix()) ->
%%           NewMatrix::matrix()
%% @doc Reorder the rows of the `Matrix' by the permutation of rows
%%      given in `Rows'.
reorder_rows(Rows, M=#matrix{rows=Rows0}) when is_list(Rows) ->
    case []==(Rows0--Rows) andalso []==(Rows--Rows0) of
        true -> ok;
        _    -> throw(?LocalError(invalid_permutation, [Rows0, Rows]))
    end,
    M#matrix{rows=Rows}.

%% @spec reorder_cols(Cols::[col()], Matrix::matrix()) ->
%%           NewMatrix::matrix()
%% @doc Reorder the columns of the `Matrix' by the permutation of columns
%%      given in `Cols'.
reorder_cols(Cols, M=#matrix{cols=Cols0}) when is_list(Cols) ->
    case []==(Cols0--Cols) andalso []==(Cols--Cols0) of
        true -> ok;
        _    -> throw(?LocalError(invalid_permutation, [Cols0, Cols]))
    end,
    M#matrix{cols=Cols}.



%%% ============================================================================
%%% Get and set values

%% @spec cols(Matrix::matrix()) -> [col()]
%%
%% @doc Returns the column labels of the matrix.
cols(#matrix{cols=Cols}) ->
    Cols.

%% @spec rows(Matrix::matrix()) -> [row()]
%%
%% @doc Returns the row labels of the matrix.
rows(#matrix{rows=Rows}) ->
    Rows.

%% @spec get(row(), col(), Matrix::matrix()) -> val()
%%
%% @doc Returns the element at position (`Row', `Col') in an ets table.
get(Row, Col, #matrix{table=Tab, default=Def}) ->
    case ets:lookup(Tab, {Row, Col}) of
        []         -> Def;
        [{_, Val}] -> Val
    end.

%% @spec getFromDets(row(), col(), Matrix::#matrix{}) -> val()
%%
%% @doc Returns the element at position (`Row', `Col') in a dets table.
getFromDets(Row, Col, #matrix{table=Tab, default=Def}) ->
    case dets:lookup(Tab, {Row, Col}) of
        []         -> Def;
        [{_, Val}] -> Val
    end.

%% @spec get2(RowLabel::row(), ColumnLabel::col(), Matrix::matrix()) ->
%%           {default, DefaultValue::val()} | {value, Value::val()}
%% @doc Returns the element at position (`Row', `Col').
get2(Row, Col, #matrix{table=Tab, default=Def}) ->
    case ets:lookup(Tab, {Row, Col}) of
        []         -> {default, Def};
        [{_, Val}] -> {value,   Val}
    end.

%% @spec get_row(row(), matrix()) -> [{col(), val()}]
%%
%% @doc Returns the specified row of the matrix.
get_row(Row, Mtr) ->
    fold_row(fun (C, V, L) -> [{C, V} | L] end, [], Row, Mtr).

%% @spec get_col(col(), matrix()) -> [{row(), val()}]
%%
%% @doc Returns the specified col of the matrix.
get_col(Col, Mtr) ->
    fold_col(fun (R, V, L) -> [{R, V} | L] end, [], Col, Mtr).


%% @spec get_row_non_def(RowId::row(), Matrix::matrix()) ->
%%           [{ColId::col(), CellValue::val()}]
%% @doc Returns the specified column of the matrix without the default values.
get_row_non_def(Row, #matrix{table=TableID}) ->
    Row1 = if
        is_tuple(Row) -> {Row};
        true -> Row
    end,
    ets:select(TableID, [{ {{'$1','$2'},'$3'}, [{'==',Row1,'$1'}],
                           [{{ '$2','$3' }}] }]).

%% @spec get_col_non_def(ColId::col(), Matrix::matrix()) ->
%%           [{RowId::row(), CellValue::val()}]
%% @doc Returns the specified row of the matrix without the default values.
get_col_non_def(Col, #matrix{table=TableID}) ->
    Col1 = if
        is_tuple(Col) -> {Col};
        true -> Col
    end,
    ets:select(TableID, [{ {{'$1','$2'},'$3'}, [{'==',Col1,'$2'}],
                           [{{ '$1','$3' }}] }]).


%% @spec get_row_non_def_ids(RowId::row(), Matrix::matrix()) ->
%%           [{ColId::col(), CellValue::val()}]
%% @doc Returns the specified column of the matrix without the default values.
get_row_non_def_ids(Row, #matrix{table=TableID}) ->
    Row1 = if
        is_tuple(Row) -> {Row};
        true -> Row
    end,
    ets:select(TableID, [{ {{'$1','$2'},'_'}, [{'==',Row1,'$1'}], ['$2'] }]).

%% @spec get_col_non_def_ids(ColId::col(), Matrix::matrix()) ->
%%           [{RowId::row(), CellValue::val()}]
%% @doc Returns the specified row of the matrix without the default values.
get_col_non_def_ids(Col, #matrix{table=TableID}) ->
    Col1 = if
        is_tuple(Col) -> {Col};
        true -> Col
    end,
    ets:select(TableID, [{ {{'$1','$2'},'_'}, [{'==',Col1,'$2'}], ['$1'] }]).

%% @spec get_entities(Matrix::matrix()) -> entity()
%%
%% @doc Returns the type of entities the matrix uses.
get_entities(#matrix{entity=E}) -> E.

%%% ============================================================================
%%% Transform matrix

%% @spec transform((val()) -> val(), matrix()) -> matrix()
%%
%% @doc Applies `Fun' to every element of the matrix and stores the result
%% in its place.
transform(Fun, Matrix=#matrix{default=Def, table=Tab}) ->
    transform(Fun, ets:first(Tab), Tab),
    Matrix#matrix{default=Fun(Def)}.

transform(_Fun, '$end_of_table', _Tab) ->
    ok;
transform(Fun, Key, Tab) ->
    [{_, Val}] = ets:lookup(Tab, Key),
    ets:insert(Tab, {Key, Fun(Val)}),
    transform(Fun, ets:next(Tab, Key), Tab).


%% @spec transform2((col_key(), row_key(),val()) -> val(), matrix()) -> matrix()
%%
%% @doc Applies `Fun' to every element of the matrix and stores the result
%% in its place.
transform2(Fun, Matrix=#matrix{default=Def, table=Tab}) ->
    transform2(Fun, ets:first(Tab), Tab),
    Matrix#matrix{default=Fun(default,default,Def)}.

transform2(_Fun, '$end_of_table', _Tab) ->
    ok;
transform2(Fun, Key, Tab) ->
    [{_, Val}] = ets:lookup(Tab, Key),
    {RowKey, ColKey} = Key,
    ets:insert(Tab, {Key, Fun(RowKey,ColKey,Val)}),
    transform2(Fun, ets:next(Tab, Key), Tab).


%% @spec fold_col(FunType, term(), col(), matrix()) -> term()
%%       FunType = (row(), val(), Acc::term()) -> term()
%%
%% @doc Usual fold operation on a given column. No ordering guarantees.
fold_col(Fun, Acc0, Col, M=#matrix{rows=Rows}) ->
    lists:foldl(
      fun(Row, Acc) -> Fun(Row, get(Row, Col, M), Acc) end,
      Acc0, Rows).

%% @spec fold_row(FunType, term(), row(), matrix()) -> term()
%%       FunType = (col(), val(), Acc::term()) -> term()
%%
%% @doc Usual fold operation on a given row. No ordering guarantees.
fold_row(Fun, Acc0, Row, M=#matrix{cols=Cols}) ->
    lists:foldl(
      fun(Col, Acc) -> Fun(Col, get(Row, Col, M), Acc) end,
      Acc0, Cols).


%% @spec fold_col2(FunType, term(), col(), col(), matrix()) -> term()
%%       FunType = (row(), Val1::val(), Val2::val(), Acc::term()) -> term()
%%
%% @doc Usual fold operation on two columns. No ordering guarantees.
fold_col2(Fun, Acc0, Col1, Col2, M=#matrix{rows=Rows}) ->
    lists:foldl(
      fun(Row, Acc) ->
              Fun(Row, get(Row, Col1, M), get(Row, Col2, M), Acc)
      end, Acc0, Rows).

%% @spec fold_row2(FunType, term(), row(), row(), matrix()) -> term()
%%       FunType = (col(), Val1::val(), Val2::val(), Acc::term()) -> term()
%%
%% @doc Usual fold operation on two rows. No ordering guarantees.
fold_row2(Fun, Acc0, Row1, Row2, M=#matrix{cols=Cols}) ->
    lists:foldl(
      fun(Col, Acc) ->
              Fun(Col, get(M, Row1, Col), get(M, Row2, Col), Acc)
      end, Acc0, Cols).



%%% ============================================================================
%%% Conversion

%% @spec to_list(Matrix::matrix()) ->
%%           [{{Row::row(), Col::col()}, Value::term()}]
%% @doc  Fetch `Matrix' contain into a list.
to_list(#matrix{table=TabID}) ->
    EtsTable = ets:new(clone_matrix,[]),
%    ets:to_ets(TabID, EtsTable),
    ets:tab2list(EtsTable).


%% @spec from_list(List::[{{Row::row(), Col::col()}, Value::term()}],
%%               Default::term()) -> matrix()
%% @doc  Create a matrix from `List'. Every element in the `List' contain the
%%       description of a cell of the matrix. This is a row-column index pair
%%       and the value of current cell.
from_list(List, Default) ->
    % Calculate row and column identifications lists
    {Rows1, Cols1} = lists:foldl(
        fun({{Row,Col}, _Value}, {Rs,Cs}) -> {[Row|Rs],[Col|Cs]} end,
        {[],[]},
        List),
    % Create matrix
    lists:foldl(
        fun({{Row,Col}, Value}, Matrix=#matrix{}) ->
            set_ets(Row, Col, Value, Matrix, all)
        end,
        new(lists:usort(Rows1), lists:usort(Cols1), Default),
        List).


%% @spec to_record(Matrix::matrix()) -> RecordMatrix::matrix()
%% @doc  Create a record from `Matrix'. This record contains all information
%%       about `Matrix' and all the values as well. There are no other required
%%       resources like ETS tables, etc. It contains everything as values.
%%
%%       `RecordMatrix' is not a usable matrix with matrix operations. It
%%       can be used to store the whole matrix in a single term for example
%%       when you want to save the matrix into a file. Before the use you need
%%       to convert it back with the {@link from_record/1} function.
to_record(M=#matrix{}) ->
    M#matrix{table=to_list(M)}.

%% @spec from_record(RecordMatrix::matrix()) -> Matrix::matrix()
%% @doc  Create a matrix from the `RecordMatrix' that was created previously
%%       from a matrix by the {@link to_record/1} function.
%% @see to_record/1
from_record(M=#matrix{rows=Rows, cols=Cols, default=Default, table=TabList}) ->
    #matrix{rows=Rows1,cols=Cols1,table=TabID1} = from_list(TabList,Default),
    case []==(Rows1--Rows) andalso []==(Cols1--Cols) of
        true -> M#matrix{table=TabID1};
        _    -> throw(?LocalError(bad_rowcols, [Rows1--Rows, Cols1--Cols]))
    end.



%%% ============================================================================
%%% Dump

%% @spec dump(matrix()) -> ok
%% @doc Prints the contents of a matrix to the standard output.
%% @see dump/2
%% @see dump/4
dump(M=#matrix{}) ->
    dump(standard_io, M).

%% @spec dump(matrix(), io_device()) -> ok
%% @doc Prints the contents of a matrix to a given device.
%% @see dump/4
dump(Dev, M=#matrix{}) ->
    dump(normal, Dev, M, fun(Id) -> Id end).

%% @spec dump(FileFormat, matrix(), io_device(), Id2NameFun) -> ok
%%       FileFormat = normal | csv
%%       Id2NameFun = ((term()) -> term())
%% @doc Prints the contents of a matrix to a given device in a specified file
%%      format.
%%
%% Allowed file formats:
%% <ul>
%%   <li>`normal': write the column headers in a list in the first line.
%%     The second line contain a list with 2-tuples. The first element of the
%%     tuples is the row header and the second element is a list with the
%%     row values in same order as the cols in the first line.</li>
%%   <li>`list': write a list with 2-tuple elements. The first element of every
%%     tuple is a 2-tuple which contain the row and column label. The second
%%     element is the value of the corresponding cell.</li>
%%   <li>`csv': (Comma Separated Values) Wtite a table. The rows are in
%%     separated lines. The values in a row are separated with a semicolon(;).
%%     The first line contains the column headers (started with an empty cell).
%%     The following lines contain the rows started with the row header.</li>
%% </ul>
%%
%% When the column and row headers need to write the `Id2NameFun(Header)' will
%% be written instead the original headers. In default the `Id2NameFun' is the
%% `fun(Id) -> Id end' identical function.
dump(normal, Dev, M=#matrix{}, Id2NameFun) when is_function(Id2NameFun) ->
    io:format(Dev, "~p~n~p~n",
               [lists:map(Id2NameFun,cols(M)),
                [{Id2NameFun(R),lists:reverse(
                      fold_row(fun(_,V,L)-> [V|L] end, [], R, M))} ||
                    R <- rows(M)]]);
% List: simple dump the to_list/1 result
dump(list, Dev, M=#matrix{}, Id2NameFun) when is_function(Id2NameFun) ->
    io:format(Dev, "~p\n",
        [lists:map(fun({{R,C},V}) -> {{Id2NameFun(R),Id2NameFun(C)},V} end,
                  to_list(M))]);
% CSV (Comma Separated Values)
dump(csv, Dev, M=#matrix{}, Id2NameFun) when is_function(Id2NameFun) ->
    % Get row and column identifiers
    Rows=rows(M),
    Cols=cols(M),
    % Write column headers
    lists:foreach(fun(C) -> io:format(Dev, ";~p", [Id2NameFun(C)]) end, Cols),
    io:format(Dev, "\n", []),
    % Write rows
    lists:foreach(
        fun(R) ->
            io:format(Dev, "~p", [Id2NameFun(R)]),
            lists:foreach(
                fun(C) ->
                    case get2(R,C,M) of
                        {default, _} -> io:format(Dev, ";",   []);
                        {value,   V} -> io:format(Dev, ";~p", [V])
                    end
                end,
                Cols),
            io:format(Dev, "\n", [])
        end,
        Rows).

%%% ============================================================================
%%% Attribute Matrices

%% @spec get_from_dets(row(), col(), Matrix::matrix(),field()) -> val()
%%
%% @doc Returns the given attribute of the element
%%      at position (`Row', `Col') in a dets table.
get_from_dets(Row, Col,
             #matrix{table=Tab, default=#matrix_item{func=F,rec=R,macro=M}},
             Field) ->
    case dets:lookup(Tab, {Row, Col}) of
        []         -> case Field of
                          func -> F;
                          rec -> R;
                          macro -> M
                      end;
        [{_, #matrix_item{func=VF,rec=VR,macro=VM}}] ->
                      case Field of
                          func -> VF;
                          rec -> VR;
                          macro -> VM
                      end
    end.

%% @spec get_from_ets(row(), col(), Matrix::matrix(),field()) -> val()
%%
%% @doc Returns the first attribute of the element
%%      at position (`Row', `Col') in an ets table.
get_from_ets(Row, Col,
             #matrix{table=Tab, default=#matrix_item{func=F,rec=R,macro=M}},
             Field) ->
    case ets:lookup(Tab, {Row, Col}) of
        []         -> case Field of
                          func -> F;
                          rec -> R;
                          macro -> M
                      end;
        [{_, #matrix_item{func=VF,rec=VR,macro=VM}}] ->
                      case Field of
                          func -> VF;
                          rec -> VR;
                          macro -> VM
                      end
    end.

%% @spec get_from_value(val(), field()) -> val()
%%
%% @doc Returns the given attribute of the given Matrix value.
get_from_value(#matrix_item{func=F,rec=R,macro=M}, Field) ->
    case Field of
        func -> F;
        rec -> R;
        macro -> M
    end.

%% @spec set_dets(row(), col(), val(), Matrix::matrix(),field()) -> matrix()
%%
%% @doc Updates the first attribute of the element at position
%%      (`Row', `Col') in a dets table, returns the updated matrix.
set_dets(Row, Col, Value, Matrix=#matrix{table=Tab}, Field) ->
    Item = getFromDets(Row, Col, Matrix),
    case Field of
        func -> dets:insert(Tab, {{Row,Col},
                           #matrix_item{func=Value,
                                        rec=Item#matrix_item.rec,
                                        macro=Item#matrix_item.macro}});
        rec -> dets:insert(Tab, {{Row,Col},
                          #matrix_item{func=Item#matrix_item.func,
                                       rec=Value,
                                       macro=Item#matrix_item.macro}});
        macro -> dets:insert(Tab, {{Row,Col},
                            #matrix_item{func=Item#matrix_item.func,
                                         rec=Item#matrix_item.rec,
                                         macro=Value}})
%        all -> dets:insert(Tab, {{Row, Col}, Value})
    end,
    Matrix.

%% @spec set_ets(row(), col(), val(), Matrix::matrix(), field()) -> matrix()
%%
%% @doc Updates the element at position (`Row', `Col') in an ets table
%%, returns the updated matrix.
set_ets(Row, Col, Value, Matrix=#matrix{table=Tab}, Field) ->
    Item = ets:lookup(Tab, {Row, Col}),
    It = case length(Item) of
        0 -> get_default();
        _ -> lists:nth(1,Item)
    end,

    case Field of
        func -> ets:insert(Tab, {{Row,Col}, change_value(It, func, Value)});
        rec -> ets:insert(Tab, {{Row,Col}, change_value(It, rec, Value)});
        macro -> ets:insert(Tab, {{Row,Col}, change_value(It, macro, Value)});
        all -> ets:insert(Tab, {{Row, Col}, Value})
    end,
    Matrix.

%% @spec change_value(#matrix_item{}, field(), val()) -> #matrix_item{}
%%
%% @doc Changes the given field in the given matrix item.
change_value(#matrix_item{func=F,rec=R,macro=M},Field, Val) ->
    case Field of
        func -> #matrix_item{func=Val, rec=R, macro=M};
        rec -> %?d({"VESZÉLY!!!",Val}),
        #matrix_item{func=F, rec=Val, macro=M};
        macro -> #matrix_item{func=F, rec=R, macro=Val}
    end.

%% @spec get_default() -> matrix_item()
%%
%% @doc Returns the default ({0,0,0}) value of an attribute matrix.
get_default() -> #matrix_item{func=0,rec=[],macro=[]}.

%% @spec itemize({fun(),rec(),macro()}) -> matrix_item()
%%
%% @doc Returns the matrix item form of the given tuple.
itemize({F,R,M}) ->
    #matrix_item{func=F,rec=R,macro=M}.


%% @specload_attribute_matrix(entity()) -> matrix()
%%
%% @doc Loads the attribute matrix from a dets table, and updates it,
%%      if necessary.
load_attribute_matrix(Entities) ->
    % Getting the hash value of the saved matrix
    HashValList =
        dets:lookup(element(2,dets:open_file(db_hash_attr,[{type,set}])),hash),
    LoadedTableHash = case HashValList of
        [] -> -1;
        _ -> element(2,lists:nth(1,HashValList))
    end,
    %LoadedTableHash = -1,
    % Getting the hash value of the current database
    ActualHash = ?MISC:database_hash(),

    % Opening the dets table, where the matrix is stored
    case Entities of
        module ->
            FixTable = element(2,dets:open_file(module_table, [{type,set}]));
        function ->
            FixTable = element(2,dets:open_file(function_table, [{type,set}]))
    end,

    case ActualHash of
        % If the hash value matches the value of the current database
        LoadedTableHash ->
           % Making a copy to an ets table, and closing the dets table
           ClusterTable = dets:to_ets(FixTable, ets:new(cluster_table,[])),
           dets:traverse(
               FixTable, fun(X) -> ets:insert(ClusterTable, X), continue end),
           EntityList = get_entity_list(Entities),
           dets:close(FixTable),
           #matrix{rows=EntityList,cols=EntityList,entity=Entities,
                   default=#matrix_item{func=0,rec=[],macro=[]},
                   table=ClusterTable};
        % If the value doesn't match...
        _ ->
           % Clearing the dets table
           dets:delete_all_objects(FixTable),
           % Updating
           WorkMatrix1 =
                   update_module_attribute_matrix(
                       #matrix{table=FixTable,
                               default=#matrix_item{func=0,rec=[],macro=[]}}),
           WorkMatrix2 =
                   update_function_attribute_matrix(
                       #matrix{table=FixTable,
                               default=#matrix_item{func=0,rec=[],macro=[]}}),
           WorkMatrix = case Entities of
				module -> WorkMatrix1;
				_ -> WorkMatrix2
		   end,
           % Copying to an ets table
           ClusterTable = dets:to_ets(FixTable, ets:new(cluster_table,[])),
           dets:traverse(
               FixTable, fun(X) -> ets:insert(ClusterTable, X), continue end),
           dets:insert(db_hash_attr,{hash, ActualHash}),
           dets:close(db_hash_attr),
           dets:close(FixTable),
           ReturnMatrix = #matrix{rows=WorkMatrix#matrix.rows,
                                  cols=WorkMatrix#matrix.cols,
                                  entity=Entities,
                                  table=ClusterTable,
                                  default=#matrix_item{func=0,rec=[],macro=[]}},
           ReturnMatrix
    end.


%% @spec get_entity_list(entity()) -> [mod_name()] | [fun_name()]
%%
%% @doc Returns the list of the given entities.
get_entity_list(Entities) ->
   case Entities of
      function -> FunList = ?Query:exec(?Query:seq([?File:all(), ?File:module(),
                                          ?Mod:locals()])),
                  lists:map(fun(FNode) ->
                      ?ClAttr:fun_to_fun_attr(FNode)
                  end, FunList);
      module -> ModList = ?Query:exec(?Query:seq([?File:all(), ?File:module()])),
                lists:map(fun(MNode) -> ?Mod:name(MNode) end, ModList)
   end.


%% @spec unique_fun_name(node()) -> atom()
%%
%% @doc Generates a unique name for the given function node, in the
%%      following format: mod_name : fun_name / fun_arity.
unique_fun_name(Func) ->
    List = [?Mod:name(lists:nth(1,?Query:exec(Func, ?Fun:module()))),
            ":",
            ?Fun:name(Func),
            "/",
            ?Fun:arity(Func)],
    list_to_atom(lists:concat(List)).


%% @spec update_function_attribute_matrix(dets_table()) -> dets_table()
%%
%% @doc Updates the dets table. It uses queries to determine the
%%      relationships between each entity. In this case, entities
%%      are functions.
update_function_attribute_matrix(Table) ->
    % Recovering function calls
    Modules = ?Query:exec(?Query:seq(?File:all(),?File:module())),

    Functions = lists:map(
        fun(Mod) -> ordsets:from_list(?Query:exec(Mod,?Mod:locals()))
    end, Modules),

    Entities = ordsets:to_list(ordsets:union(Functions)),
    EntityNames = lists:map(
        fun(Func)-> ?ClAttr:fun_to_fun_attr(Func) end, Entities),

    FunctionCalls = lists:map(
        fun(Func) -> {Func,
                      gb_sets:from_list(?Query:exec(Func, ?Fun:funcalls()))}
    end, Entities),

    ConvFunctions = lists:map(fun(FL)-> {FL,gb_sets:from_list([FL])} end, Entities),

    Table1 = Table#matrix{rows=EntityNames, cols=EntityNames},

    make_changes(func, ConvFunctions, FunctionCalls,
                 Table1, false, fun ?ClAttr:fun_to_fun_attr/1),

    % Recovering record info
    ConvertedFunRecList = recover_record_info(Entities, fun ?Form:func/0),
    make_changes(rec, ConvertedFunRecList, ConvertedFunRecList,
                 Table1, true, fun ?ClAttr:fun_to_fun_attr/1),

    %% Recovering macro info
    ConvertedFunMacroList = recover_macro_info(Entities, fun ?Form:func/0),
    make_changes(macro, ConvertedFunMacroList, ConvertedFunMacroList,
                 Table1, true, fun ?ClAttr:fun_to_fun_attr/1),

    Table1.
%% @spec update_module_attribute_matrix(dets_table()) -> dets_table()
%%
%% @doc Updates the dets table. It uses queries to determine the
%%      relationships between each entity. In this case, entities
%%      are modules.
update_module_attribute_matrix(Table) ->
    % Recovering function calls between modules
    Modules = ?Query:exec(?Query:seq(?File:all(),?File:module())),
    ModuleNames = lists:map(fun(Mod) -> ?Mod:name(Mod) end, Modules),

    Functions = lists:map(
        fun(Mod) -> {?Mod:name(Mod), ?Query:exec(Mod,?Mod:locals())}
        end, Modules),

    FunctionCalls = lists:map(
        fun({Mod, Funs}) -> {Mod, gb_sets:from_list(lists:foldr(
            fun(F, Acc) -> ?Query:exec(F,?Fun:funcalls()) ++ Acc end, [], Funs))}
    end, Functions),

    ConvFunctions = lists:map(fun({M, F}) -> {M, gb_sets:from_list(F)} end, Functions),

    Table1 = Table#matrix{rows=ModuleNames, cols=ModuleNames},

    make_changes(func, ConvFunctions, FunctionCalls, Table, false, fun(M) -> M end),

    % Recovering record usage
    ConvertedModRecList = recover_record_info(Modules, fun ?Form:module/0),
    make_changes(rec, ConvertedModRecList, ConvertedModRecList,
                 Table1, true, fun ?Mod:name/1),

    % Recovering macro usage
    ConvertedModMacroList = recover_macro_info(Modules, fun ?Form:module/0),
    make_changes(macro, ConvertedModMacroList, ConvertedModMacroList,
                 Table1, true, fun ?Mod:name/1),

    Table1.

%% @spec recover_record_info([ent()], fun(() -> [atom()])) -> [{ent(),[record()]}]
%%
%% @doc  Collects the information about common record usage. The input is
%%       a list of the entities used in the matrix (e.g.: list of modules)
%%       the output is a pair, which describes which records
%%       the entity ent() uses. Records are stored in a gb_sets structure.
recover_record_info(EntityList, EntityFunction) ->
    AllRecord = ?Query:exec(?Query:seq([?File:all(),?File:records()])),
    ConvRecList = lists:map(fun(Rec) -> {Rec,
       ?Query:exec(Rec,?Query:seq([?Rec:references(),
                       ?Expr:clause(),?Clause:form(),EntityFunction()]))}
    end, AllRecord),

    EntRecList = lists:map(fun(Ent) -> {Ent,
    lists:foldr(
        fun({Rec,RList}, Acc) -> case lists:member(Ent,RList) of
                true -> [Rec] ++ Acc;
                false -> Acc
            end
    end, [], ConvRecList)} end, EntityList),

    lists:map(
        fun({Ent,L}) -> {Ent, gb_sets:from_list(L)} end, EntRecList).

%% @spec recover_macro_info([ent()], fun(() -> [atom()])) -> [{ent(),[macro()]}]
%%
%% @doc  Collects the information about common macro usage. The input is
%%       a list of the entities used in the matrix (e.g.: list of modules)
%%       the output is a pair, which describes which macros
%%       the entity ent() uses. Macros are stored in a gb_sets structure.
recover_macro_info(EntityList, EntityFunction) ->
    AllMacro = ?Query:exec(?Query:seq([?File:all(),?File:macros()])),
    MacroOriginList = lists:map(fun(Macro) ->
        {?Macro:name(Macro),
	    case length(?Macro:refs(Macro)) of
			0 -> [];
			_ -> lists:map(
			fun(Node) ->
				case ?Syn:node_type(Node) of
					expr -> ?Query:exec(Node,
										?Query:seq([
											?Expr:clause(),
											?Clause:form(),
											EntityFunction()]));
					clause -> ?Query:exec(Node,
										?Query:seq([
											?Clause:form(),
											EntityFunction()]));
					form -> ?Query:exec(Node, EntityFunction())
				end
			end, lists:last(?Macro:refs(Macro)))
		end }
	end, AllMacro),

	% Creating {Entity, [Macro]} pairs
    EntMacroList = lists:map(fun(Ent) -> {Ent,
        lists:foldr(
            fun({Macro, ConvList}, Acc) ->
                case lists:member(Ent, ConvList) of
                        true -> [Macro] ++ Acc;
                        false -> Acc
                end
    end, [], MacroOriginList)} end, EntityList),

    % Converting lists to gb_sets
    lists:map(
        fun({Ent,L}) -> {Ent, gb_sets:from_list(L)} end, EntMacroList).

%% @spec make_changes(val_type(), [{entity(), val()}], [{entity(), val()}],
%%                    Matrix::#matrix{}, boolean(), fun()) -> [{entity(), val()}]
%%
%% @doc Changes the given values in the given table. This function changes only
%%      one type of value, according to the lists.
make_changes(EntType, List1, List2, Table, Symmetric, NameFun) ->
    lists:map(
        fun({Ent1, EList1}) ->
            lists:map(
                fun({Ent2, EList2}) ->
                    case Ent1 =/= Ent2 of
                        true ->
                            Int = gb_sets:intersection(EList1, EList2),
                            case EntType of
                                func -> FinalValue = gb_sets:size(Int);
                                rec -> FinalValue = gb_sets:to_list(Int);
                                macro -> FinalValue = gb_sets:to_list(Int)
                            end,
                            case gb_sets:size(Int) > 0 of
                                true ->
          %                          ?d({"Updating: ", NameFun(Ent1), NameFun(Ent2), FinalValue, EntType}),
                                    set_dets(NameFun(Ent1),
                                                 NameFun(Ent2),
                                                 FinalValue,
                                                 Table, EntType),
                                    case Symmetric of
                                        true -> set_dets(NameFun(Ent2),
                                                 NameFun(Ent1),
                                                 FinalValue,
                                                 Table, EntType);
                                        false -> ok
                                    end;
                                false -> ok
                            end;
                        false ->
                            ok
                    end
                end, List1)
        end, List2).


