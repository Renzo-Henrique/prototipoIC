%% Definition of predicates with corresponding query for modularization purposes.

:- module(queries,   [  includeFilterQuery/3,
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
        {}
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

includeFilterQuery(Query, QueryResulted, Condition):-
        removeAfterLastCharacter(Query, "}", Removed),
        includeFilter(Condition, FilterString),
        finalOfStringQuery(FinalStr),
        string_concat(Removed, FilterString, R1),
        string_concat(R1, FinalStr, QueryResulted), !.



lastOccurrence([], _, Index, Index) :- !.
lastOccurrence([H|T], Character, Index, Position) :-
    (H == Character ->
        NewIndex is Index + 1,
        lastOccurrence(T, Character, NewIndex, NewPosition),
        Position is NewPosition
    ;
        NewIndex is Index + 1,
        lastOccurrence(T, Character, NewIndex, Position)
    ).

/*lastOcurrence([], _, _,_).
lastOccurrence([H|T], Character, I, Position) :-
        ( (H == Character, write(H), Position = I); true),
        lastOccurrence(T, Character, I+1, Position).*/

% string_chars("Olá {} sou } ponto", Chars).
% lastOccurrenceRecursive("Olá {} sou } ponto", '}', Result).
lastOccurrenceRecursive(String, Character, Position) :-
        string_chars(String, Chars), % Convert string to list of characters
        lastOccurrence(Chars, Character, 0 , Position).
    

% incluir erro ao não achar caractere ( LastOcurrence = -1)
removeAfterLastCharacter(String, Character, Result) :-
        lastOccurrenceRecursive(String, Character, LastOcurrence),
        sub_string(String, 0, LastOcurrence, _, Result). % Extrair a substring antes do último caractere
            

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
        string_concat(" FILTER(", Condition, Temp),
        string_concat(Temp, ")\n", Filter).

% ?- filtroQuery(queryInteraction, , Lista).

% includeFilterQuery(queryInteraction, ' CONTAINS(?interactionIDs, "DB00005")', QueryResulted).

% filtroQuery(queryInteraction, " CONTAINS(?interactionIDs, \"DB00005\")", Lista).

% removeAfterLastCharacter("Olá {} sou } ponto", '}', Result).

% Base case: Reversing an empty list results in an empty list
reverse_iterate([], []).

% Recursive case: Reversing a non-empty list is appending the last element to the reversed rest of the list
reverse_iterate([Head|Tail], Reversed) :-
    reverse_iterate(Tail, ReversedTail),  % Recursively reverse the tail of the list
    append(ReversedTail, [Head], Reversed).  % Append the head to the reversed tail



