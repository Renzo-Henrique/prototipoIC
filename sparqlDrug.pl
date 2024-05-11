:- use_module('modules/sparql_client.pl').
:- use_module('modules/queries.pl').
:- use_module('modules/rowFunctions.pl').


% Predicado que executa uma query de entrada.
executeSparqlQuery(Resultado, Query) :-
    sparql_query(   Query,
                    Resultado,
                    [host('drugbank.bio2rdf.org'), path('/sparql/'), cert_verify_hook(cert_accept_any)]
    ).


%------------------------------
%------------------------------
% Resultados com callback -> Melhora na modularização

%------------------------------
% With callback for modularization
rowResult(Result, QueryPredicate):-
    call(QueryPredicate, Query),
    executeSparqlQuery(Result, Query).

%------------------------------
% Result with predicates with arity = 2
resultSeparated(A,B, QueryPredicate):-
    rowResult(Result, QueryPredicate),
    Result = row(A, B).

% Result with predicates with arity = 3
resultSeparated(A,B,C, QueryPredicate):-
    rowResult(Result, QueryPredicate),
    Result = row(A, B, C).

%------------------------------
% Result values without any kind of type with predicates with arity = 2
resultSeparatedValues(A,B, QueryPredicate):-
    rowResult(Result, QueryPredicate),
    Result = row(Index1, Index2),
    extractValueFromRowElement(Index1, A),
    extractValueFromRowElement(Index2, B).

% Result values without any kind of type with predicates with arity = 3
resultSeparatedValues(A,B,C, QueryPredicate):-
    rowResult(Result, QueryPredicate),
    Result = row(Index1, Index2, Index3),
    extractValueFromRowElement(Index1, A),
    extractValueFromRowElement(Index2, B),
    extractValueFromRowElement(Index3, C).

% Result in a list
resultListed(List, QueryPredicate):-
    rowResult(Row, QueryPredicate),
    applyToRow(Row, List).

% Resultado filtrado por uma chave e onde 
% ela se encontra na lista que é resultado da query
filter(Key, Index, List, QueryPredicate):-
    resultListed(List, QueryPredicate),
    nth0(Index, List, Target, _),
    Key == Target.

/** callback examples
 * 
?- rowResult(Result, queryDrugInformation).
?- resultSeparated(DrugIdentifier, ActivePrinciple, Indication, queryDrugInformation).
?- resultSeparatedValues(DrugIdentifier, ActivePrinciple, Indication, queryDrugInformation).
?- filter('DB00001', 0, List, queryDrugInformation).

?- rowResult(Result, queryDrugCategory).
?- resultSeparated(DrugIdentifier, DrugCategory, queryDrugCategory).
?- resultSeparatedValues(DrugIdentifier, DrugCategory, queryDrugCategory).
?- filter('DB00001', 0, List, queryDrugCategory).

?- rowResult(Result, queryDrugClassification).
?- resultSeparated(DrugIdentifier, DrugClassification, queryDrugClassification).

?- rowResult(Result, queryFoodInteraction).
?- resultSeparated(DrugIdentifier, FoodInteraction, queryFoodInteraction).

?- rowResult(Result, queryInteraction).
?- resultSeparated(DrugIdentifier, InteractionIDs, Description, queryInteraction).

?- rowResult(Result, queryProduct).
?- resultSeparated(DrugIdentifier, ProductName, ProductIdentifier, queryProduct).

*/
