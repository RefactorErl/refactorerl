Definitions.

Whitespace = [\n\t\s]
Lower      = [a-z]
Upper      = [A-Z]
Digit      = [0-9]

Rules.

{Digit}+ :
    {token, {int, TokenLine, list_to_integer(TokenChars)}}.

(except|minus|\-\-|subtract) :
    {token, {set_op, TokenLine, 'minus'}}.

(union|U) :
    {token, {set_op, TokenLine, 'union'}}.

(intersect|intersection|I) :
    {token, {set_op, TokenLine, 'intersect'}}.

(any_in|all_in) :
    Atom = list_to_atom(TokenChars),
    {token, {filter_set_op, TokenLine, Atom}}.

orelse|\; :
    {token, {'or', TokenLine}}.
    
andalso|\, :
    {token, {'and', TokenLine}}.
    
\.\. :
    {token, {'..', TokenLine}}.
    
= :
    {token, {'==', TokenLine}}.

({Lower}|@)({Lower}|{Upper}|{Digit}|_|@)* :
    Atom = list_to_atom(TokenChars),
    {token, case reserved_word(Atom) of
                true  -> {Atom, TokenLine};
                false -> {atom, TokenLine, Atom}
            end}.
'[^']*' :
    Atom = list_to_atom(lists:sublist(TokenChars, 2, TokenLen-2)),
    {token, {atom, TokenLine, {quoted, Atom}}}.

\"[^\"]*\" :
    String = lists:sublist(TokenChars, 2, TokenLen-2),
    {token, {string, TokenLine, String}}.

{Upper}({Lower}|{Upper}|{Digit}|_)* :
    {token, {variable, TokenLine, TokenChars}}.

[\[\]\.\+\{\}\(\)\:\?\|] :
    {token, {list_to_atom(TokenChars), TokenLine}}.

/\? : 
    {token, {list_to_atom(TokenChars), TokenLine}}.

\-\> : 
    {token, {list_to_atom(TokenChars), TokenLine}}.


(/=|==|\>=|=\<|\<|\>|=\:=|=/=|~) :
    {token, {list_to_atom(TokenChars), TokenLine}}.

{Whitespace}+ :
    skip_token.

Erlang code.

reserved_word('and') -> true;
reserved_word('or') -> true;
reserved_word('not') -> true;
reserved_word('in') -> true;
reserved_word('like') -> true;
reserved_word(_) -> false.
