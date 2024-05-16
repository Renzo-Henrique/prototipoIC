:- use_module(library(statistics)).
:- use_module('sparql_client.pl').
:- use_module('sparqlFunctions.pl').
:- use_module('queries.pl').
:- use_module('rowFunctions.pl').
:- use_module('auxFunctions.pl').


/**
 * meuPredicado(+PredicadoString)
 *
 * Predicado que executa um predicado em formato String com respectivos argumentos
 *
 * @param PredicadoString String contendo a chamada do predicado
 * 
 * @example meuPredicado("includeConditionSubstring('?drugIdentifier', 'DB00001', Condition)").
 */
meuPredicado(PredicadoString) :-
    % Use read_term_from_atom/3 para converter a string em um termo Prolog
    read_term_from_atom(PredicadoString, Predicado, []),
    % Use call/1 para executar o predicado
    call(Predicado).

/**
 * main(+PredicadoString)
 *
 * Executa uma consulta de predicado e imprime o tempo de execução em milissegundos
 *
 * @param PredicadoString String contendo a chamada do predicado
 * 
 * @example main("includeConditionSubstring('?drugIdentifier', 'DB00001', Condition)").
 */
main(PredicadoString) :-
    % Marque o tempo de início
    statistics(runtime, [Start|_]),
    
    % Chame o predicado que você quer medir
    meuPredicado(PredicadoString),
    
    % Marque o tempo de fim
    statistics(runtime, [Stop|_]),
    
    % Calcule o tempo de execução em milissegundos
    Runtime is Stop - Start,
    
    % Imprima o tempo de execução
    format('Tempo de execução: ~6f milissegundos.~n', [Runtime]), !.
