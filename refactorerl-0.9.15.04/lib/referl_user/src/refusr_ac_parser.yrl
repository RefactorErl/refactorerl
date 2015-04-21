Expect 40.

Nonterminals semantic_query first_query_element query_seq
                query_seq_body query_element selector variable_bind
                variable_match get_variable main_set_operation
                set_operation set_operation_operand stat iteration
                closure filter filter_expression exp100 exp200 exp300
                exp400 exp_max exp_query comparator exp_seq list item
                items interv .

Terminals       '.' '..' '[' ']' '{' '}' '(' ')' '+' ':' '?' '/?' '->' '|'
                atom int string variable filter_set_op
                'not' 'and' 'or' 'like' 'in' set_op
                '/=' '==' '<' '=<' '>' '>=' '=:=' '=/=' '~'.

Rootsymbol      semantic_query.

semantic_query -> first_query_element                   : '$1'.
semantic_query -> first_query_element query_seq         : '$1' ++ '$2'.
semantic_query -> first_query_element query_seq stat    : '$1' ++ '$2' ++ '$3'.
semantic_query -> main_set_operation                    : ['$1'].
semantic_query -> '?'                                   : [{complete, {help, initial_selectors}}].

first_query_element -> atom                         : [{complete, {initial_selector, value_of('$1')}}].
first_query_element -> '(' main_set_operation ')'   : ['$2'].

main_set_operation -> first_query_element set_op semantic_query             : {complete, {initial_selector, {set_op, {value_of('$2'), '$1', '$3'}}}}.
main_set_operation -> first_query_element query_seq set_op semantic_query   : {complete, {initial_selector, {set_op, {value_of('$3'), '$1' ++ '$2', '$4'}}}}.
main_set_operation -> first_query_element set_op                            : {incomplete, {initial_selector, {set_op, {value_of('$2'), '$1'}}}}.
main_set_operation -> first_query_element query_seq set_op                  : {incomplete, {initial_selector, {set_op, {value_of('$3'), '$1' ++ '$2'}}}}.

set_operation -> set_operation_operand set_op set_operation_operand : {complete, {set_op, {value_of('$2'), '$1', '$3'}}}.
set_operation -> set_operation_operand set_op set_operation         : {complete, {set_op, {value_of('$2'), '$1', ['$3']}}}.
set_operation -> set_operation_operand set_op                       : {incomplete, {set_op, {value_of('$2'), '$1'}}}.

set_operation_operand -> atom                               : [{complete, {initial_selector, value_of('$1')}}].
set_operation_operand -> query_seq                          : '$1'.
set_operation_operand -> atom query_seq                     : [{complete, {initial_selector, value_of('$1')}} | '$2'].

variable_bind -> '->' variable               : [{complete, {variable_bind, value_of('$2')}}].
variable_bind -> '->'                        : [{incomplete, {variable_bind, '->'}}].

variable_match -> '?' variable               : [{complete, {variable_match, '?', value_of('$2')}}].
variable_match -> '/?' variable              : [{complete, {variable_match, '/?', value_of('$2')}}].
variable_match -> '?'                        : [{incomplete, {variable_match, '?'}}].
variable_match -> '/?'                       : [{incomplete, {variable_match, '/?'}}].

query_seq -> query_seq_body                  : '$1'.

query_seq_body -> query_element                             : '$1'.
query_seq_body -> query_seq_body query_element              : '$1' ++ '$2'.

%% breaks closures: 
%% query_element -> '(' query_seq_body ')'                  : '$2'.
query_element -> selector                                   : ['$1'].
query_element -> iteration                                  : ['$1'].
query_element -> closure                                    : ['$1'].
query_element -> filter                                     : ['$1'].
query_element -> variable_match                             : '$1'.
query_element -> variable_bind                              : '$1'.
query_element -> get_variable                               : '$1'.
query_element -> '(' set_operation ')'                      : ['$2'].
query_element -> '(' atom query_seq ')'                     : [{incomplete, {set_op, [{complete, {initial_selector, value_of('$2')}} | '$3']}}].
%% the following breaks funs[not (header)] :
%%query_element -> '(' atom ')'                               : [{incomplete, {set_op, [{complete, {initial_selector, value_of('$2')}}]}}].
query_element -> '(' set_operation                          : [{incomplete, element(2, '$2')}].
%query_element -> '(' set_operation_operand                  : [].
query_element -> '('                                        : [{incomplete, {set_op, '('}}].

selector -> '.' atom           : {complete, {selector, value_of('$2')}}.
selector -> '.' '?'            : {complete, {help, queries}}.
selector -> '.'                : {incomplete, {'query', '.'}}.

get_variable -> '.' variable   : [{variable, value_of('$2')}].    

stat -> ':' atom               : [{complete, {statistics, value_of('$2')}}].
stat -> ':' '?'                : [{complete, {help, statistics}}].
stat -> ':'                    : [{incomplete, {statistics, '|'}}].

closure -> '.' '(' set_operation  ')' '+'    : {complete, {closure, {seq, ['$3']}, {mult, infinite}}}.
closure -> '.' '(' set_operation  ')' int    : {complete, {closure, {seq, ['$2']}, {mult, value_of('$5')}}}.
closure -> '.' '(' atom           ')' '+'    : {complete, {closure, {seq, [{selector, value_of('$3')}]}, {mult, infinite}}}.
closure -> '.' '(' atom           ')' int    : {complete, {closure, {seq, [{selector, value_of('$3')}]}, {mult, value_of('$5')}}}.
%% allows nested closures, like .(.(calls)+)+ :
closure -> '.' '(' query_seq      ')' '+'    : {complete, {closure, {seq, '$3'}, {mult, infinite}}}.
closure -> '.' '(' query_seq      ')' int    : {complete, {closure, {seq, '$3'}, {mult, value_of('$5')}}}.
closure -> '.' '(' atom query_seq ')' '+'    : {complete, {closure, {seq, [{selector, value_of('$3')} | '$4']}, {mult, infinite}}}.
closure -> '.' '(' atom query_seq ')' int    : {complete, {closure, {seq, [{selector, value_of('$3')} | '$4']}, {mult, value_of('$6')}}}.

iteration -> '.' '{' atom           '}' int  : {complete, {iteration, {seq, [{selector, value_of('$3')}]}, {mult, value_of('$5')}}}.
%% allows nested iterations:
iteration -> '.' '{' query_seq      '}' int  : {complete, {iteration, {seq, '$3'}, {mult, value_of('$5')}}}.
iteration -> '.' '{' atom query_seq '}' int  : {complete, {iteration, {seq, [{selector, value_of('$3')} | '$4']}, {mult, value_of('$6')}}}.
iteration -> '.' '{' set_operation  '}' int  : {complete, {iteration, {seq, ['$3']}, {mult, value_of('$5')}}}.
iteration -> '.' '{' atom           '}'      : {incomplete, {iteration, element(1, '$4')}}.
iteration -> '.' '{' query_seq      '}'      : {incomplete, {iteration, element(1, '$4')}}.
iteration -> '.' '{' atom query_seq '}'      : {incomplete, {iteration, element(1, '$5')}}.
iteration -> '.' '{' set_operation  '}'      : {incomplete, {iteration, element(1, '$4')}}.
iteration -> '.' '{' set_operation           : {incomplete, {iteration, ['$3']}}.
iteration -> '.' '{' set_operation_operand   : {incomplete, {iteration, ['$3']}}.
iteration -> '.' '{'                         : {incomplete, {iteration, element(1, '$2')}}.

filter -> '[' filter_expression ']'     : {complete, {filter, '$2'}}.
filter -> '[' '?' ']'                   : {complete, {help, filters}}.
filter -> '[' filter_expression         : {incomplete, {filter, '$2'}}.
filter -> '['                           : {incomplete, {filter, '['}}.
filter -> '[' ']'                       : {incomplete, {filter, '[]'}}.

comparator -> '=='      : element(1, '$1').
comparator -> '/='      : element(1, '$1').
comparator -> '<'       : element(1, '$1').
comparator -> '=<'      : element(1, '$1').
comparator -> '>'       : element(1, '$1').
comparator -> '>='      : element(1, '$1').
comparator -> '=:='     : element(1, '$1').
comparator -> '=/='     : element(1, '$1').
comparator -> '~'       : element(1, '$1').

filter_expression  -> exp100        : '$1'.

%%Disjunction
exp100  -> exp200 'or' exp100       : {complete,   {'or', '$1', '$3'}}.
exp100  -> exp200 'or'              : {incomplete, {'or', '$1'}}.
exp100  -> exp200                   : '$1'.
%%Conjunction
exp200  -> exp300 'and' exp200      : {complete,   {'and', '$1', '$3'}}.
exp200  -> exp300 'and'             : {incomplete, {'and', '$1'}}.
exp200  -> exp300                   : '$1'.
%%Comparison
exp300  -> exp400 comparator exp400 : {complete,   {'$2', '$1', '$3'}}.
exp300  -> exp400 comparator        : {incomplete, {comparator, '$1'}}.
exp300  -> exp400 'like' exp400     : {complete,   {'like', '$1', '$3'}}.
exp300  -> exp400 'like'            : {incomplete, {'like', '$1'}}.

exp300 -> exp_max 'in' exp_max      : {complete,{'in', '$1', '$3'}}.
exp300 -> exp_max 'in' exp_query    : {complete,{'in', '$1', '$3'}}.
exp300 -> exp_max 'in'              : {incomplete, {'in', '$1'}}.

%exp300 -> atom filter_set_op ... - Single atoms handled by the rule below should be initial selectors
exp300 -> exp_max filter_set_op exp_max : {complete, {set_op, {value_of('$2'), prop_to_initsel('$1'), prop_to_initsel('$3')}}}.
exp300 -> exp_max filter_set_op exp_query : {complete, {set_op, {value_of('$2'), prop_to_initsel('$1'), prop_to_initsel('$3')}}}.
exp300 -> atom query_seq filter_set_op exp_max : {complete, {set_op, {value_of('$3'), [{complete, {initial_selector, value_of('$1')}} | '$2'], prop_to_initsel('$4')}}}.
exp300 -> exp_max filter_set_op         : {incomplete, {set_op, {value_of('$2'), prop_to_initsel('$1')}}}.

exp300  -> exp400                   : '$1'.

%%Negation
exp400  -> 'not' exp_max            : {complete,   {'not', '$2'}}.
exp400  -> 'not'                    : {incomplete, {'not'}}.
exp400  -> exp_max                  : '$1'.
%%Parenthesis
exp_max -> '(' exp100 ')'           : '$2'.
%exp_max -> '(' exp100              : [].
%exp_max -> '('                     : [].
%%Embedded queries
exp_max -> exp_seq                  : {complete, {seq, '$1'}}.
%exp_max -> '(' exp_query ')'              : '$2'.
%%Simple values
exp_max -> list                     : '$1'.
exp_max -> atom                     : value_of('$1').
exp_max -> int                      : value_of('$1').
exp_max -> string                   : value_of('$1').
exp_max -> variable                 : {variable, value_of('$1')}.

exp_seq -> query_seq set_op exp_seq     : [{complete, {set_op, {value_of('$2'), '$1', '$3'}}}].
exp_seq -> query_seq set_op             : [{incomplete, {set_op, {value_of('$2'), '$1'}}}].
exp_seq -> query_seq                    : '$1'.

exp_query -> atom query_seq                 : [{complete, {initial_selector, value_of('$1')}} | '$2'].

list -> '|''|'             : {complete,{cons, []}}.
list -> '|' items '|'      : {complete, {cons, '$2'}}.
list -> '|' interv '|'     : '$2'.

list -> '|'              : {incomplete, {cons, '|'}}.
list -> '|' items        : {incomplete, {cons,['$2']}}.
list -> '|' interv       : {incomplete, element(2, '$2')}.

items -> item                       : ['$1'].                                
items -> item 'and' items           : ['$1'] ++ '$3'.
interv -> int '..' int :
    {complete,{cons,
        case value_of('$1') > value_of('$3') of
            true -> [];
            _ -> lists:seq(value_of('$1'), value_of('$3'))
        end
    }}.
interv -> int '..'                  : {incomplete,{cons, value_of('$1')}}.
item -> atom                        : value_of('$1').                    
item -> int                         : value_of('$1').
item -> string                      : value_of('$1').

Erlang code.
value_of(Token) ->
    element(3, Token).

prop_to_initsel(Exp) when is_atom(Exp) ->
    [{complete, {initial_selector, Exp}}];
prop_to_initsel(Exp) ->
    Exp.
