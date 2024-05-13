:- use_module('modules/sparql_client.pl').
:- use_module('modules/queries.pl').
:- use_module('modules/rowFunctions.pl').
:- debug.

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
rowfiltroQuery(QueryPredicado, Condicao, Lista):-
    call(QueryPredicado, StringQuery),
    includeFilterQuery(StringQuery, ResultadoQuery, Condicao),
    executeSparqlQuery(ResultadoQuery, Lista).

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
% Resultado em lista
resultadoListado(QueryPredicado, Lista):-
    rowResultado(QueryPredicado, Row),
    applyToRow(Row, Lista).

%------------------------------
% Resultado em lista com condicao
resultadoListado(QueryPredicado, Condicao, Lista):-
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

?- filtroQuery(queryInteraction, " CONTAINS(?interactionIDs, 'DB00026') ", Lista).
*/