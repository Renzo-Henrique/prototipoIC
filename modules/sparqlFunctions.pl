:- module(sparqlFunctions,   [  rowResultado/2, rowfiltroQuery/3,
                                resultadoListado/2, resultadoListado/4,
                                resultadoFiltro/4, executeSparqlQuery/2]).
:- use_module('sparql_client.pl').
:- use_module('auxFunctions.pl').
:- use_module('rowFunctions.pl').


/**
 * executeSparqlQuery(+QueryString, -Resultado)
 *
 * Predicado que realiza uma consulta SPARQL especificada por `QueryString`, uma String, e retorna o resultado na variável `Resultado`.
 *
 * @param QueryString String contendo a consulta SPARQL a ser executada.
 * @param Resultado Variável que será unificada com o resultado da consulta.
 *
 * @note Lembre-se de que a consulta SPARQL deve ser válida para que este predicado retorne um resultado útil.
 * @note Olhe queries.pl em caso de dúvida em queries
 * 
 * @formato O resultado é uma estrutura complexa contendo informações obtidas pela consulta SPARQL. Resultado = row(A,B,C, ...) 
 * sendo cada elemento no formato literal(compound(Type, Value))
 */
executeSparqlQuery(QueryString, Resultado) :-
    sparql_query(   QueryString,
                    Resultado,
                    [host('drugbank.bio2rdf.org'), path('/sparql/'), cert_verify_hook(cert_accept_any)]
    ).



/**
 * rowResultado(+QueryPredicado, -Resultado)
 *
 * Predicado que realiza uma consulta SPARQL especificada por `QueryPredicado`, um predicado, e retorna o resultado na variável `Resultado`.
 *
 * @param QueryPredicado Predicado que representa a consulta SPARQL a ser executada.
 * @param Resultado Variável que será unificada com o resultado da consulta SPARQL.
 * @note Olhe queries.pl em caso de dúvida em queries
 * 
 * @formato O resultado é uma estrutura complexa contendo informações obtidas pela consulta SPARQL. Resultado = row(A,B,C, ...) 
 * sendo cada elemento no formato literal(compound(Type, Value)) 
 */
rowResultado(QueryPredicado, Resultado):-
    call(QueryPredicado, QueryString),
    executeSparqlQuery(QueryString, Resultado).

/**
 * rowfiltroQuery(+QueryPredicado, +Condicao, -Resultado)
 *
 * Predicado que realiza uma consulta SPARQL especificada por `QueryPredicado`, um predicado, incluindo uma `Condicao` para filtragem DENTRO da query ]
 * e retorna o resultado na variável `Resultado`.
 * 
 *
 * @param QueryPredicado Predicado que representa a consulta SPARQL a ser executada.
 * @param Condição Variável que será unificada com o resultado da consulta SPARQL.
 * @param Resultado Variável que será unificada com o resultado da consulta SPARQL.
 *
 * 
 * @example  Condicao = "CONTAINS(?productNameA,\"Angiomax 250 mg vial\")"
 * @example  Condicao = "CONTAINS(?drugIdentifier,\"DB00006\")"
 * @example  rowfiltroQuery(queryDrugInformation, "CONTAINS(?drugIdentifier,\"DB00006\")", Resultado)
 * 
 * 
 * @formato O resultado é uma estrutura complexa contendo informações obtidas pela consulta SPARQL. Resultado = row(A,B,C, ...) 
 * sendo cada elemento no formato literal(compound(Type, Value))
 */
rowfiltroQuery(QueryPredicado, Condicao, Resultado):-
    call(QueryPredicado, StringQuery),
    incluirFiltroQuery(StringQuery, ResultadoQuery, Condicao),
    executeSparqlQuery(ResultadoQuery, Resultado).

/**
 * resultadoFiltro(+QueryPredicado, +Chave, +Indice, -Lista)
 *
 * Predicado que realiza uma consulta SPARQL especificada por `QueryPredicado`, um predicado, incluindo uma `Condicao` para filtragem DENTRO da query ]
 * e retorna o resultado na variável `Resultado`.
 * 
 *
 * @param QueryPredicado Predicado que representa a consulta SPARQL a ser executada.
 * @param Chave é o valor a ser filtrado
 * @param Indice é o índice em que a chave se encontra no resultado de uma query.
 * @param Resultado Variável que será unificada com o resultado da consulta SPARQL.
 * 
 * @note Para saber qual o índice a ser usado em uma chave verifique as variáveis de consulta da query em sparql, exemplo, "?drugIdentifier"
 * 
 * @example  resultadoFiltro(queryDrugInformation, "DB00006", 0, Lista)
 * 
 * @formato O resultado é uma estrutura complexa contendo informações obtidas pela consulta SPARQL. Resultado = row(A,B,C, ...) 
 * sendo cada elemento no formato literal(compound(Type, Value))
 */
resultadoFiltro(QueryPredicado, Chave, Indice, Lista):-
    resultadoListado(QueryPredicado, Lista),
    nth0(Indice, Lista, Objetivo, _),
    Chave == Objetivo.

/**
 * resultadoListado(+QueryPredicado, -Lista).
 * Executa a query e retorna o resultado em uma lista.
 * Caso queira os elementos separadamente, defina cada um em uma lista
 * OBS: Verifique a quantidade de resultados de cada query
 *
 * @example 
 *   ?- resultadoListado(queryProduct, [DrugIdentifier, ProductName, ProductIdentifier]).
 *   ?- resultadoListado(queryProduct, Lista).
 * @formato Lista é uma lista contendo cada valor resultante da query
*/
resultadoListado(QueryPredicado, Lista):-
    rowResultado(QueryPredicado, Row),
    applyToRow(Row, Lista).


/**
 * resultadoListado(+QueryPredicado, +Chave, +Valor, -Lista).
 * Executa a query e retorna o resultado em uma lista.
 * Caso queira os elementos separadamente, defina cada um em uma lista
 * OBS: Verifique a quantidade de resultados de cada query
 * OBS: Chave deve ser uma variável de consulta válida
 *
 * @example 
 *
 *   ?- resultadoListado(queryProduct, "?drugIdentifier", "DB00026", [DrugIdentifier, ProductName, ProductIdentifier]).
*/
resultadoListado(QueryPredicado, Chave, Valor, Lista):-
    incluirCondicaoString(Chave, Valor, Condicao),
    rowfiltroQuery(QueryPredicado, Condicao, Resultado),
    applyToRow(Resultado, Lista).
 