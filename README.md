# prototipoIC
Protótipos da IC

/*
*   Exemplos de queries feitas com respectivas informações
*
queryDrugCategory               ,DrugIdentifier, DrugCategory

queryDrugClassification         ,DrugIdentifier, DrugClassification

queryDrugInformation            ,DrugIdentifier, ActivePrinciple, Indication

queryFoodInteraction            ,DrugIdentifier, FoodInteraction

queryInteraction                ,DrugIdentifier, InteractionIDs, Description

queryProduct                    ,DrugIdentifier, ProductName, ProductIdentifier

*/

/*
TODO: perguntas

?- filtro(queryDrugCategory, 'DB00001', 0, List).
List = ['DB00001', 'Antithrombins'] ;
List = ['DB00001', 'Fibrinolytic Agents'] ;
false.

Isso deve ser consertado? Não retornar falso quando não achar mais resultados

*/
