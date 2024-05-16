:- module(sparqlFunctions,   [  rowResultado/2, rowfiltroQuery/3,
                        resultadoSeparadoFiltrado/5, resultadoSeparadoFiltrado/6,
                        resultadoSeparado/3, resultadoSeparado/4,
                        resultadoValoresSeparados/3, resultadoValoresSeparados/4,
                        resultadoListado/2, resultadoListado/4,
                        filtroResultado/4]).

/*:- use_module('modules/sparql_client.pl').
:- use_module('modules/queries.pl').
:- use_module('modules/rowFunctions.pl').*/
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
% Resultado predicados com argumentos = 2
resultadoSeparado(QueryPredicado, A, B):-
    rowResultado(QueryPredicado, Resultado),
    Resultado = row(A, B).

%------------------------------
% Resultado predicados com argumentos = 3
resultadoSeparado(QueryPredicado, A,B,C):-
    rowResultado(QueryPredicado, Resultado),
    Resultado = row(A, B, C).

%------------------------------
% Resultado no formato string com argumentos = 2
resultadoValoresSeparados(QueryPredicado, A,B):-
    rowResultado(QueryPredicado,Resultado),
    Resultado = row(Index1, Index2),
    extractValueFromRowElement(Index1, A),
    extractValueFromRowElement(Index2, B).

%------------------------------
% Resultado no formato string com argumentos = 3
resultadoValoresSeparados(QueryPredicado, A,B,C):-
    rowResultado(QueryPredicado, Resultado),
    Resultado = row(Valor1, Valor2, Valor3),
    extractValueFromRowElement(Valor1, A),
    extractValueFromRowElement(Valor2, B),
    extractValueFromRowElement(Valor3, C).


%------------------------------
% Resultado filtrado na query no formato string com argumentos = 5
resultadoSeparadoFiltrado(QueryPredicado, Chave, Valor, A,B):-
    includeConditionSubstring(Chave, Valor, Condicao),
    rowfiltroQuery(QueryPredicado, Condicao, Resultado),
    Resultado = row(Index1, Index2),
    extractValueFromRowElement(Index1, A),
    extractValueFromRowElement(Index2, B).

%------------------------------
% Resultado filtrado na query no formato string com argumentos = 6
resultadoSeparadoFiltrado(QueryPredicado, Chave, Valor, A,B,C):-
    includeConditionSubstring(Chave, Valor, Condicao),
    rowfiltroQuery(QueryPredicado, Condicao, Resultado),
    Resultado = row(Valor1, Valor2, Valor3),
    extractValueFromRowElement(Valor1, A),
    extractValueFromRowElement(Valor2, B),
    extractValueFromRowElement(Valor3, C).

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
 
%------------------------------
% Resultado filtrado por uma chave e onde 
% ela se encontra na lista que é resultado da query
filtroResultado(QueryPredicado, Chave, Index, Lista):-
    resultadoListado(QueryPredicado, Lista),
    nth0(Index, Lista, Objetivo, _),
    Chave == Objetivo.

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