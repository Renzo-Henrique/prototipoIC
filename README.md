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

queryProductInteraction         ,DrugIdentifier, ProductName, ProductIdentifier, InteractionIDs, InteractionDescription

*/

/*
TODO: perguntas

Utilização de assert diminui pesquisas com backtracking, mas é viável? Pode travar?

Unificar queries que são usadas juntas pra diminuir custo da pesquisa: queryInteraction e queryProduct resultando em queryProductInteraction.
Custa mais memória, mas é mais rápida


*/
