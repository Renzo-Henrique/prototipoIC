:- use_module(library(statistics)).
:- use_module('sparql_client.pl').
:- use_module('sparqlFunctions.pl').
:- use_module('queries.pl').
:- use_module('rowFunctions.pl').
:- use_module('../farmacinha.pl').


meu_predicado(PredicateString) :-
    % Use read_term_from_atom/3 para converter a string em um termo Prolog
    read_term_from_atom(PredicateString, Predicate, []),
    % Use call/1 para executar o predicado
    call(Predicate).

main(PredicateString) :-
    % Marque o tempo de início
    statistics(runtime, [Start|_]),
    
    % Chame o predicado que você quer medir
    meu_predicado(PredicateString),
    
    % Marque o tempo de fim
    statistics(runtime, [Stop|_]),
    
    % Calcule o tempo de execução em milissegundos
    Runtime is Stop - Start,
    
    % Imprima o tempo de execução
    format('Tempo de execução: ~3f segundos.~n', [Runtime]), !.
