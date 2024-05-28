:- module(rowFunctions,   [extractValueFromRowElement/2, applyFuncToArgs/4,
                            applyToRow/2, compoundArgumentsNumber/2]).

% Predicate to extract a value from a row result in sparql_query/3
extractValueFromRowElement(RowElement, Value) :-
    RowElement =.. [_, Funct],
    (Funct = type(_, X), ! ;
    Funct = lang(_, X)), !,
    atom_string(X, Value).

% Base case: when all arguments are iterated
applyFuncToArgs(_, _, 0, []).
% Iterating over the arguments of a compound term, applying the function, and storing the results in a list
applyFuncToArgs(Compound, Func, N, Results) :-
    % Accessing the N-th argument of the compound term
    arg(N, Compound, Arg),
    % Decrementing N for the next argument
    NextN is N - 1,
    % Recursively calling applyFuncToArgs with the next argument index and result list
    applyFuncToArgs(Compound, Func, NextN, RestResults),
    % Applying the function Func to the argument Arg and storing the result
    call(Func, Arg, Result),
    % Appending the current result to the rest of the results
    append(RestResults, [Result], Results),!.

applyToRow(Row, Results):-
    compoundArgumentsNumber(Row, N),
    applyFuncToArgs(Row, extractValueFromRowElement, N, Results).

% Predicate to determine the number of arguments of a compound term
compoundArgumentsNumber(Compound, N) :-
    compound(Compound), % Check if Compound is a compound term
    compoundArgumentsNumber_(Compound, 0, N).

% Helper predicate to recursively count the number of arguments
compoundArgumentsNumber_(Compound, Acc, N) :-
    compound_name_arguments(Compound, _, Args),
    length(Args, Len),
    N is Acc + Len.