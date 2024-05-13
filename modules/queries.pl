%% Definition of predicates with corresponding query for modularization purposes.

:- module(queries,   [  callBackFilterQuery/3,
                        queryDrugCategory/1, queryDrugClassification/1,
                        queryDrugInformation/1, queryFoodInteraction/1,
                        queryInteraction/1, queryProduct/1]).

queryDrugCategory(Query):- 
        Query ="
        PREFIX biovoc: <http://bio2rdf.org/bio2rdf_vocabulary:>
        PREFIX dbvoc: <http://bio2rdf.org/drugbank_vocabulary:>

        SELECT distinct  ?drugIdentifier ?drugCategory 
        WHERE {
        ?sub a dbvoc:Drug ;
                biovoc:identifier ?drugIdentifier;
                dbvoc:category/dcterms:title ?drugCategory.

        } ORDER BY ?drugIdentifier LIMIT 1000 ".
            
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

        } ORDER BY ?drugIdentifier LIMIT 1000".

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
        } ORDER BY ?drugIdentifier LIMIT 1000".

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
        
        } ORDER BY ?drugIdentifier LIMIT 1000".

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
        
        } ORDER BY ?drugIdentifier LIMIT 1000".

% callBackFilterQuery(queryInteraction, " CONTAINS(?drugIdentifier, 'DB00026') ", QueryResulted).
callBackFilterQuery(QueryPredicate, Condition, QueryResulted):-
        call(QueryPredicate, StringQuery),
        includeFilterQuery(StringQuery, QueryResulted, Condition).


includeFilterQuery(Query, QueryResulted, Condition):-
        removeAfterCharacter(Query, "}", Removed),
        includeFilter(Condition, FilterString),
        finalOfStringQuery(FinalStr),
        string_concat(Removed, FilterString, R1),
        string_concat(R1, FinalStr, QueryResulted), !.

removeAfterCharacter(String, Character, Result) :-
        sub_atom(String, Before, _, _, Character), % Find the position of the character
        sub_atom(String, 0, Before, _, Result), !. % Extract the substring before the character

/*concatQuery(Query, Str, QueryResulted):-
        string_concat([Query, Str], QueryResulted).*/

finalOfStringQuery(Str):-
        Str = "} ORDER BY ?drugIdentifier".

% includeFilter(" CONTAINS(?drugIdentifier, 'DB00026') ", Filter).
% includeFilterQuery(" CONTAINS(?drugIdentifier, 'DB00026') ",Filter).
includeFilter(Condition, Filter) :-
        (   nonvar(Condition)
        ->  string_concat(" FILTER(", Condition, Temp),
        string_concat(Temp, ")\n", Filter)
        ;   % Condition is not instantiated
        throw(error(instantiation_error, includeFilter/2))
        ).

% ?- filtroQuery(queryInteraction, , Lista).

% callBackFilterQuery(queryInteraction, ' CONTAINS(?interactionIDs, "DB00005")', QueryResulted).

% filtroQuery(queryInteraction, " CONTAINS(?interactionIDs, \"DB00005\")", Lista).


