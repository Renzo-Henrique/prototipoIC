:- module(sparqlFunctions,   [  rowResultado/2, rowfiltroQuery/3,
                                resultadoListado/2, resultadoListado/4,
                                resultadoFiltro/4]).
:- use_module('sparql_client.pl').
:- use_module('queries.pl').
:- use_module('rowFunctions.pl').

% Predicado que executa uma query de entrada.
executeSparqlQuery(QueryString, Resultado) :-
    sparql_query(   QueryString,
                    Resultado,
                    [host('drugbank.bio2rdf.org'), path('/sparql/'), cert_verify_hook(cert_accept_any)]
    ).



%------------------------------
% Resultado da query
rowResultado(QueryPredicado, Resultado):-
    call(QueryPredicado, QueryString),
    executeSparqlQuery(QueryString, Resultado).

%------------------------------
% Resultado com filtragem DENTRO da query
rowfiltroQuery(QueryPredicado, Condicao, Resultado):-
    call(QueryPredicado, StringQuery),
    includeFilterQuery(StringQuery, ResultadoQuery, Condicao),
    executeSparqlQuery(ResultadoQuery, Resultado).

%------------------------------
% Resultado com filtragem FORA da query
% Chave é o valor a ser encontrado
% Index é onde ele se encontra na lista que é resultado da query
resultadoFiltro(QueryPredicado, Chave, Index, Lista):-
    resultadoListado(QueryPredicado, Lista),
    nth0(Index, Lista, Objetivo, _),
    Chave == Objetivo.

/**
 * resultadoListado(QueryPredicado, Lista).
 * Executa a query e retorna o resultado em uma lista.
 * Caso queira os elementos separadamente, defina cada um em uma lista
 * OBS: Verifique a quantidade de resultados de cada query
 *
 * @example 
 *   ?- resultadoListado(queryProduct, [DrugIdentifier, ProductName, ProductIdentifier]).
 *
*/
resultadoListado(QueryPredicado, Lista):-
    rowResultado(QueryPredicado, Row),
    applyToRow(Row, Lista).


/**
 * resultadoListado(QueryPredicado, Chave, Valor, Lista).
 * Executa a query e retorna o resultado em uma lista.
 * Caso queira os elementos separadamente, defina cada um em uma lista
 * OBS: Verifique a quantidade de resultados de cada query
 * OBS: Chave deve ser uma variável de consulta válida
 *
 * @example 
 *
 *   ?- resultadoListado(queryProduct, '?drugIdentifier', 'DB00026', [DrugIdentifier, ProductName, ProductIdentifier]).
*/
resultadoListado(QueryPredicado, Chave, Valor, Lista):-
    includeConditionSubstring(Chave, Valor, Condicao),
    rowfiltroQuery(QueryPredicado, Condicao, Resultado),
    applyToRow(Resultado, Lista).
 


/** <examples>

?- filtro(queryDrugCategory, 'DB00001', 0, List).

?- resultadoValoresSeparados(queryFoodInteraction, DrugIdentifier, FoodInteraction).
?- resultadoValoresSeparados(queryInteraction, DrugIdentifier, InteractionIDs, Description).

?- resultadoSeparado(queryFoodInteraction, DrugIdentifier, FoodInteraction).
?- resultadoSeparado(queryInteraction,DrugIdentifier, InteractionIDs, Description).

?- resultadoListado(queryDrugCategory, Lista).
?- resultadoListado(queryDrugClassification, Lista).
?- resultadoListado(queryDrugInformation, Lista).
?- resultadoListado(queryFoodInteraction, Lista).
?- resultadoListado(queryInteraction, Lista).
?- resultadoListado(queryProduct, Lista).

?- rowfiltroQuery(queryInteraction, " CONTAINS(?interactionIDs, 'DB00026') ", Resultado).
?- resultadoListado(queryInteraction, " CONTAINS(?interactionIDs, 'DB00026') ", Lista).

?- resultadoListado(queryProduct, '?drugIdentifier', 'DB00945', Lista).
*/