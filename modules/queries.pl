%% Definition of predicates with corresponding query for modularization purposes.

:- module(queries,   [  queryDrugCategory/1, queryDrugClassification/1,
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

