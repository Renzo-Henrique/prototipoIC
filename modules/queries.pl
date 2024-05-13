%% Definition of predicates with corresponding query for modularization purposes.

:- module(queries,   [  queryDrugCategory/1, queryDrugClassification/1,
                        queryDrugInformation/1, queryFoodInteraction/1,
                        queryInteraction/1, queryProduct/1]).

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