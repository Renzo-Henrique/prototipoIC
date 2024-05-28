%% Definition of predicates with corresponding query for modularization purposes.

:- module(queries,   [  incluirFiltroQuery/3, incluirCondicaoString/3, finalStringQuery/2,
                        queryDrugCategory/1, queryDrugClassification/1,
                        queryDrugInformation/1, queryFoodInteraction/1,
                        queryInteraction/1, queryProduct/1,
                        queryProductInteraction/1]).

/**
 * queryDrugCategory(-Query)
 *
 * Predicado que retorna uma consulta SPARQL válida em formato de string.
 *
 * @param Query String contendo a consulta SPARQL válida a ser retornada.
 *
 * @note possui as seguintes variáveis de consulta: 
 * ?drugIdentifier ?drugCategory 
 *
 */
queryDrugCategory(Query):- 
        Query ="
        PREFIX biovoc: <http://bio2rdf.org/bio2rdf_vocabulary:>
        PREFIX dbvoc: <http://bio2rdf.org/drugbank_vocabulary:>

        SELECT distinct  ?drugIdentifier ?drugCategory 
        WHERE {
        ?sub a dbvoc:Drug ;
                biovoc:identifier ?drugIdentifier;
                dbvoc:category/dcterms:title ?drugCategory.

        } ORDER BY ?drugIdentifier ".

/**
 * queryDrugClassification(-Query)
 *
 * Predicado que retorna uma consulta SPARQL válida em formato de string.
 *
 * @param Query String contendo a consulta SPARQL válida a ser retornada.
 *
 * @note possui as seguintes variáveis de consulta: 
 * ?drugIdentifier ?drugClassification 
 *
 */
queryDrugClassification(Query):- 
        Query ="
        PREFIX biovoc: <http://bio2rdf.org/bio2rdf_vocabulary:>
        PREFIX dbvoc: <http://bio2rdf.org/drugbank_vocabulary:>
        PREFIX dcterms: <http://purl.org/dc/terms/>

        SELECT distinct  ?drugIdentifier ?drugClassification
        WHERE {
        ?sub a dbvoc:Drug ;
                biovoc:identifier ?drugIdentifier;
                dbvoc:drug-classification-category/dcterms:title ?drugClassification.

        } ORDER BY ?drugIdentifier".

/**
 * queryDrugInformation(-Query)
 *
 * Predicado que retorna uma consulta SPARQL válida em formato de string.
 *
 * @param Query String contendo a consulta SPARQL válida a ser retornada.
 *
 * @note possui as seguintes variáveis de consulta: 
 * ?drugIdentifier ?activePrinciple ?indication 
 *
 */
queryDrugInformation(Query):-
        Query =  "
        PREFIX biovoc: <http://bio2rdf.org/bio2rdf_vocabulary:>
        PREFIX dbvoc: <http://bio2rdf.org/drugbank_vocabulary:>
        PREFIX dcterms: <http://purl.org/dc/terms/>

        SELECT distinct  ?drugIdentifier ?activePrinciple ?indication 
        WHERE {
        ?sub a dbvoc:Drug ;
                biovoc:identifier ?drugIdentifier;
                dcterms:title ?activePrinciple;
                dbvoc:indication/dcterms:description ?indication.

        } ORDER BY ?drugIdentifier".

/**
 * queryFoodInteraction(-Query)
 *
 * Predicado que retorna uma consulta SPARQL válida em formato de string.
 *
 * @param Query String contendo a consulta SPARQL válida a ser retornada.
 *
 * @note possui as seguintes variáveis de consulta: 
 * ?drugIdentifier ?foodInteraction
 *
 */
queryFoodInteraction(Query):- 
        Query ="
        PREFIX biovoc: <http://bio2rdf.org/bio2rdf_vocabulary:>
        PREFIX dbvoc: <http://bio2rdf.org/drugbank_vocabulary:>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

        SELECT distinct  ?drugIdentifier ?foodInteraction
        WHERE {
        ?sub a dbvoc:Drug ;
                biovoc:identifier ?drugIdentifier;
                dbvoc:food-interaction/rdf:value ?foodInteraction.
        } ORDER BY ?drugIdentifier".

/**
 * queryInteraction(-Query)
 *
 * Predicado que retorna uma consulta SPARQL válida em formato de string.
 *
 * @param Query String contendo a consulta SPARQL válida a ser retornada.
 *
 * @note possui as seguintes variáveis de consulta: 
 * ?drugIdentifier ?interactionIDs  ?interactionDescription
 *
 */
queryInteraction(Query):- 
        Query ="
        PREFIX biovoc: <http://bio2rdf.org/bio2rdf_vocabulary:>
        PREFIX dbvoc: <http://bio2rdf.org/drugbank_vocabulary:>
        PREFIX dcterms: <http://purl.org/dc/terms/>
        

        SELECT distinct  ?drugIdentifier ?interactionIDs  ?interactionDescription
        WHERE {
        ?sub a dbvoc:Drug;
                dbvoc:ddi-interactor-in ?interactionLink;
                biovoc:identifier ?drugIdentifier.
        
        ?interactionLink dcterms:title ?interactionDescription;
                biovoc:identifier ?interactionIDs.
        } ORDER BY ?drugIdentifier".

/**
 * queryProduct(-Query)
 *
 * Predicado que retorna uma consulta SPARQL válida em formato de string.
 *
 * @param Query String contendo a consulta SPARQL válida a ser retornada.
 *
 * @note possui as seguintes variáveis de consulta: 
 * ?drugIdentifier ?productName ?productIdentifier 
 *
 */
queryProduct(Query):- 
        Query ="
        PREFIX biovoc: <http://bio2rdf.org/bio2rdf_vocabulary:>
        PREFIX dbvoc: <http://bio2rdf.org/drugbank_vocabulary:>
        PREFIX dcterms: <http://purl.org/dc/terms/>

        SELECT distinct ?drugIdentifier ?productName ?productIdentifier 
        WHERE {
        ?sub a dbvoc:Drug;
                biovoc:identifier ?drugIdentifier;
                dbvoc:product ?productLink.

        ?productLink dcterms:title ?productName;
                biovoc:identifier ?productIdentifier
        
        } ORDER BY ?drugIdentifier".

/**
 * queryProductInteraction(-Query)
 *
 * Predicado que retorna uma consulta SPARQL válida em formato de string.
 *
 * @param Query String contendo a consulta SPARQL válida a ser retornada.
 *
 * @note possui as seguintes variáveis de consulta: 
 * ?drugIdentifier ?productName ?productIdentifier ?interactionIDs  ?interactionDescription
 *
 */
queryProductInteraction(Query):-  
        Query ="
        PREFIX biovoc: <http://bio2rdf.org/bio2rdf_vocabulary:>
        PREFIX dbvoc: <http://bio2rdf.org/drugbank_vocabulary:>
        PREFIX dcterms: <http://purl.org/dc/terms/>

        SELECT distinct  ?drugIdentifier ?productName ?productIdentifier ?interactionIDs  ?interactionDescription
        WHERE {
        ?d1 a dbvoc:Drug;
                dbvoc:ddi-interactor-in ?interactionLink;
                biovoc:identifier ?drugIdentifier;
                dbvoc:product ?productLink.

        ?interactionLink dcterms:title ?interactionDescription;
        biovoc:identifier ?interactionIDs.
        ?productLink dcterms:title ?productName;
                biovoc:identifier ?productIdentifier
     	
        } ORDER BY ?drugIdentifier".

% Predicate to include a condicao to filter in a query
% Condicao must be able match the argument Query
% example: ' CONTAINS(?interactionIDs, "DB00005")' for Query in queryInteraction
% example: ' CONTAINS(?drugIdentifier, "DB00001")' for all the queries made.
incluirFiltroQuery(QueryString, QueryResultado, Condicao):-
        removerUltimaOcorrencia(QueryString, '}', Removido),
        incluirFiltro(Condicao, FiltroString),
        padraofinalStringQuery(FinalStr),
        string_concat(Removido, FiltroString, R1),
        string_concat(R1, FinalStr, QueryResultado), !.

/**
 * indiceOcorrencia(+Lista, +Elemento, -Indice).
 *
 * Obtem o índice que o elemento se encontra na lista
 *
 * @param Lista, lista a ser verificada
 * @param Elemento, elemento a ser encontrado na lista
 * @param Indice, índice de onde o elemento se encontra na lista
 *
 * @note Este predicado é recursivo.
 * @note Elemento na cabeça da lista é caso base, retornando 0, caso não encontre ele procura no restante da lista.
 */
indiceOcorrencia([Elemento|_], Elemento, 0).

indiceOcorrencia([_|Resto], Elemento, Indice) :-
        indiceOcorrencia(Resto, Elemento, IndiceAnterior),
        Indice is IndiceAnterior + 1.

/**
 * ultimaOcorrenciaRecursiva(+String, +Caractere, -Posicao).
 *
 * Obtem o índice da última ocorrência do elemento na string
 *
 * @param Lista, lista a ser verificada
 * @param Elemento, elemento a ser encontrado na lista
 * @param Indice, índice de onde o elemento se encontra na lista
 *
 * @note Este predicado é recursivo.
 * @note Elemento na cabeça da lista é caso base, retornando 0, caso não encontre ele procura no restante da lista.
 */
ultimaOcorrenciaRecursiva(String, Caractere, Posicao) :-
        string_chars(String, Chars), % Converte string em lista de caracteres
        findall(Indice, indiceOcorrencia(Chars, Caractere, Indice), Posicaos), % Encontra todas as ocorrencias do caractere
        max_list(Posicaos, Posicao), % Obtem maior índice
        Posicao >= 0.
    

/**
 * removerUltimaOcorrencia(+String, +Caractere, -Resultado)
 *
 * Predicado que obtem a substring até última ocorrência de um caractere em uma string de entrada.
 *
 * @param String A string de entrada da qual deseja-se remover a última ocorrência do caractere.
 * @param Caractere O caractere a ser removido da string.
 * @param Resultado Variável que será unificada com a string resultante após a remoção da última ocorrência do caractere.
 *
 * @note Este predicado é determinístico.
 */
removerUltimaOcorrencia(String, Caractere, Resultado) :-
        ultimaOcorrenciaRecursiva(String, Caractere, UltimaOcorrencia),
        sub_string(String, 0, UltimaOcorrencia, _, Resultado). % Extrai a substring antes do último caractere
            

/**
 * removerPrimeiraOcorrencia(+String, +Caractere, -Resultado)
 *
 * Predicado que remove a primeira ocorrência de um caractere em uma string de entrada.
 *
 * @param String A string de entrada da qual deseja-se remover a primeira ocorrência do caractere.
 * @param Caractere O caractere a ser removido da string.
 * @param Resultado Variável que será unificada com a string resultante após a remoção da primeira ocorrência do caractere.
 *
 * @note Este predicado é determinístico.
 */
removerPrimeiraOcorrencia(String, Caractere, Resultado) :-
        sub_atom(String, Antes, _, _, Caractere), % Acha o índice da primeira ocorrencia
        sub_atom(String, 0, Antes, _, Resultado), !. % Obtem a substring até a ocorrencia do caractere


/**
 * padraofinalStringQuery(-Str)
 *
 * Predicado que obtem uma String no padrão final esperado para uma consulta SPARQL.
 *
 * @param Str A string a ser retornada
 *
 */
padraofinalStringQuery(Str):-
        Str = "} ORDER BY ?drugIdentifier".

/**
 * finalStringQuery(-Str, +Var)
 *
 * Predicado que obtem uma String no padrão final esperado para uma consulta SPARQL, utiliza Var para definir a variável de consulta.
 *
 * @param Str A string a ser retornada
 * @param Var A string contendo a variável de consulta a ser utilizada
 * 
 */
finalStringQuery(Str, Var):-
        string_concat("} ORDER BY ", Var, Str).

/**
 * incluirFiltroQuery(+Condicao, -Filtro)
 *
 * Predicado que obtem o Filtro após inserir uma condicional
 *
 * @param Condicao uma condição a ser inserida dentro do FILTER da query
 * @param Filtro É o resultado a ser obtido
 * 
 */
incluirFiltro(Condicao, Filtro) :-
        string_concat(" FILTER(", Condicao, Temp),
        string_concat(Temp, ")\n", Filtro).

/**
 * incluirCondicaoString(+Chave, +Valor, -Condicao)
 *
 * Predicado que obtem a condicional de conter substring
 *
 * @param Chave Variável de consulta a ser analisada
 * @param Valor Valor que a variável de consulta deve ter
 * @param Condicao, condicional de conter substring referente a chave e valor
 * 
 */
incluirCondicaoString(Chave, Valor, Condicao):-
        string_concat(" CONTAINS(", Chave, Temp1),
        string_concat(Temp1, ", \"", Temp2),
        string_concat(Temp2, Valor, Temp3),
        string_concat(Temp3, "\") ", Condicao).



/** <examples>

?- incluirCondicaoString("?interactionIDs", 'DB00026', Condicao).
*/