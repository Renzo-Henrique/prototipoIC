:- use_module('modules/sparql_client.pl').
:- use_module('modules/queries.pl').
:- use_module('modules/rowFunctions.pl').


% Predicado que executa uma query de entrada.
executeSparqlQuery(QueryString, Resultado) :-
    sparql_query(   QueryString,
                    Resultado,
                    [host('drugbank.bio2rdf.org'), path('/sparql/'), cert_verify_hook(cert_accept_any)]
    ).


%------------------------------
%------------------------------
% Resultados com callback -> Melhora na modularização
rowResultado(QueryPredicado, Resultado):-
    call(QueryPredicado, QueryString),
    executeSparqlQuery(QueryString, Resultado).

%------------------------------
% Resultado predicados com argumentos = 2
resultadoSeparado(QueryPredicado, A, B):-
    rowResultado(QueryPredicado, Resultado),
    Resultado = row(A, B).

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

% Resultado no formato string com argumentos = 3
resultadoValoresSeparados(QueryPredicado, A,B,C):-
    rowResultado(QueryPredicado, Resultado),
    Resultado = row(Valor1, Valor2, Valor3),
    extractValueFromRowElement(Valor1, A),
    extractValueFromRowElement(Valor2, B),
    extractValueFromRowElement(Valor3, C).

% Resultado in a lista
resultadoListado(QueryPredicado, Lista):-
    rowResultado(QueryPredicado, Row),
    applyToRow(Row, Lista).

% Resultadoado filtrado por uma chave e onde 
% ela se encontra na listaa que é resultado da query
filtro(QueryPredicado, Chave, Index, Lista):-
    resultadoListado(QueryPredicado, Lista),
    nth0(Index, Lista, Objetivo, _),
    Chave == Objetivo.

/*
*   Exemplos de queries conhecidas com respectivas informações
*
queryDrugCategory               ,DrugIdentifier, DrugCategory

queryDrugClassification         ,DrugIdentifier, DrugClassification

queryDrugInformation            ,DrugIdentifier, ActivePrinciple, Indication

queryFoodInteraction            ,DrugIdentifier, FoodInteraction

queryInteraction                ,DrugIdentifier, InteractionIDs, Description

queryProduct                    ,DrugIdentifier, ProductName, ProductIdentifier

*/

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
*/


/*
TODO: perguntas

?- filtro(queryDrugCategory, 'DB00001', 0, List).
List = ['DB00001', 'Antithrombins'] ;
List = ['DB00001', 'Fibrinolytic Agents'] ;
false.

Isso deve ser consertado? Não retornar falso quando não achar mais resultados

*/