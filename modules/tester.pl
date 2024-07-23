:- use_module('sparqlFunctions.pl').
:- use_module('sparql_client.pl').
:- use_module('queries.pl').
:- use_module('rowFunctions.pl').
:- use_module('auxFunctions.pl').

% Módulo de testes
:- use_module(library(plunit)).


:- begin_tests(sparqlFunctionsQueries).

test(testeDrugCategory) :-
    resultadoListado(queryDrugCategory, Lista),
    length(Lista, Tam),
    assertion(Tam > 0), garbage_collect(), !.

test(testeDrugClassification) :-
    resultadoListado(queryDrugClassification, Lista), 
    length(Lista, Tam),
    assertion(Tam > 0), garbage_collect(), !.

test(testeDrugInformation) :-
    resultadoListado(queryDrugInformation, Lista), 
    length(Lista, Tam),
    assertion(Tam > 0), garbage_collect(), !.

test(testeFoodInteraction) :-
    resultadoListado(queryFoodInteraction, Lista), 
    length(Lista, Tam),
    assertion(Tam > 0), garbage_collect(), !.

test(testeInteraction) :-
    resultadoListado(queryInteraction, Lista), 
    length(Lista, Tam),
    assertion(Tam > 0), garbage_collect(), !.

test(testeProduct) :-
    resultadoListado(queryProduct, Lista), 
    length(Lista, Tam),
    assertion(Tam > 0), garbage_collect(), !.

test(testeProductInteraction) :-
    resultadoListado(queryProductInteraction, Lista), 
    length(Lista, Tam),
    assertion(Tam > 0), garbage_collect(), !.

:- end_tests(sparqlFunctionsQueries).

:- begin_tests(sparqlFunctionsTests).

%%%%%%%%%%%%%
% Testes dependentes da saída da query:

test(testPredicado1) :-
    rowfiltroQuery(queryInteraction, " CONTAINS(?interactionIDs, \"DB00026\") ", Resultado),
    assertion(Resultado = row(literal(type('http://www.w3.org/2001/XMLSchema#string', 'DB00005')), literal(type('http://www.w3.org/2001/XMLSchema#string', 'DB00005_DB00026')), literal(lang(en, 'DDI between Etanercept and Anakinra - Anti-TNF Agents may enhance the adverse/toxic effect of Anakinra. An increased risk of serious infection during concomitant use has been reported.')))),
    !.
test(testPredicado2) :-
    resultadoListado(queryProduct, "?drugIdentifier", "DB00945", Lista),
    assertion(Lista = ["DB00945", "Entrophen 10 650 mg Enteric-Coated Tablet", "0377fffd0546225a918b5a674c1c1a09"]),
    !.

test(testPredicado3) :-
    resultadoFiltro(queryProduct, "DB00945", 0, Lista),
    assertion(Lista = ["DB00945", "Entrophen 10 650 mg Enteric-Coated Tablet", "0377fffd0546225a918b5a674c1c1a09"]),
    !.

:- end_tests(sparqlFunctionsTests).

runSparqlFunctionsTodosTestes:-
    runSparqlFunctionsPredicadosTestes,
    runSparqlFunctionsQueriesTestes.

runSparqlFunctionsPredicadosTestes:-
    run_tests(sparqlFunctionsTests).

runSparqlFunctionsQueriesTestes:-
    run_tests(sparqlFunctionsQueries).

/** <examples>

?- resultadoListado(queryDrugCategory, Lista).
?- resultadoListado(queryDrugClassification, Lista).
?- resultadoListado(queryDrugInformation, Lista).
?- resultadoListado(queryFoodInteraction, Lista).
?- resultadoListado(queryInteraction, Lista).
?- resultadoListado(queryProduct, Lista).
?- resultadoListado(queryProductInteraction, Lista).

?- rowfiltroQuery(queryInteraction, " CONTAINS(?interactionIDs, "DB00026") ", Resultado).
?- resultadoListado(queryProduct, "?drugIdentifier", "DB00945", Lista).
*/

