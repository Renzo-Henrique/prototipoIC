%% Definition of predicates with corresponding query for modularization purposes.

:- module(queries,   [  includeFilterQuery/3, includeConditionSubstring/3,
                        queryDrugCategory/1, queryDrugClassification/1,
                        queryDrugInformation/1, queryFoodInteraction/1,
                        queryInteraction/1, queryProduct/1,
                        queryProductInteraction/1]).

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
        } ORDER BY ?drugIdentifier".

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

% Predicate to include a condition to filter in a query
% Condition must be able match the argument Query
% example: ' CONTAINS(?interactionIDs, "DB00005")' for Query in queryInteraction
% example: ' CONTAINS(?drugIdentifier, "DB00001")' for all the queries made.
includeFilterQuery(Query, QueryResulted, Condition):-
        removeAfterLastCharacter(Query, '}', Removed),
        includeFilter(Condition, FilterString),
        finalOfStringQuery(FinalStr),
        string_concat(Removed, FilterString, R1),
        string_concat(R1, FinalStr, QueryResulted), !.

% Base case: If the element is found at the head of the list, return 0.
occurrenceIndex([Element|_], Element, 0).

% Recursive case: If the element is not found at the head, search in the rest of the list.
occurrenceIndex([_|Tail], Element, Index) :-
        occurrenceIndex(Tail, Element, PreviousIndex),
        Index is PreviousIndex + 1.

% Predicate to find the last ocurrence of 'Character'.
lastOccurrenceRecursive(String, Character, Position) :-
        string_chars(String, Chars), % Convert string to list of characters
        findall(Index, occurrenceIndex(Chars, Character, Index), Positions), % Find all occurrences of the character
        max_list(Positions, Position), % Select the maximum position
        Position >= 0. % Ensure Position is non-negative
    

% Remove the substring after the last ocurrence of 'Character' of a 'String'
removeAfterLastCharacter(String, Character, Result) :-
        lastOccurrenceRecursive(String, Character, LastOcurrence),
        sub_string(String, 0, LastOcurrence, _, Result). % Extrair a substring antes do último caractere
            

% Remove the substring after the first ocurrence of 'Character' of a 'String'
removeAfterCharacter(String, Character, Result) :-
        sub_atom(String, Before, _, _, Character), % Find the position of the character
        sub_atom(String, 0, Before, _, Result), !. % Extract the substring before the character


% String normally found in the last characters of Queries
finalOfStringQuery(Str):-
        Str = "} ORDER BY ?drugIdentifier".

% Put the condition in the Filter sintax of sparql
includeFilter(Condition, Filter) :-
        string_concat(" FILTER(", Condition, Temp),
        string_concat(Temp, ")\n", Filter).

% Include key and value into a verifier of Substring in sparql
includeConditionSubstring(Key, Value, Condition):-
        string_concat(" CONTAINS(", Key, Temp1),
        string_concat(Temp1, ", \'", Temp2),
        string_concat(Temp2, Value, Temp3),
        string_concat(Temp3, "\') ", Condition).




% includeConditionSubstring("?interactionIDs", 'DB00026', Condition).

/** <examples>

?- includeConditionSubstring("?interactionIDs", 'DB00026', Condition).
*/