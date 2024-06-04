# PrototipoIC
Protótipo da IC feito em prolog para verificar interações entre medicamentos comprados por uma pessoa. Essas interações serão válidadas usando dados reais do endpoint drugbank.bio2rdf.org/sparql/

# Motivo da modularização
* farmacinha.pl será o programa principal, portanto se encontra apenas predicados relacionados a interações e a 'farmacinha'.
* auxFunctions.pl são predicados para modularizar outros predicados.
* rowFunctions.pl são predicados que facilitam o uso do compound row, que é o compound usado para demonstrar os resultados das queries
* queries.pl são predicados que auxiliam em modificações dentro das queries além de queries prontas, definidas no próximo tópico
* sparql_client_.pl é a biblioteca padrão, mas modificada para aceitar qualquer certificado dos sites consultados pelo sparql, apenas incluido: url_option(cert_verify_hook(cert_accept_any)) ao invés de url_option(cert_verify_hook).
* sparqlFunctions.pl são predicados que facilitam nas chamadas das consultas em sparql
* timeTester.pl é um testador de tempo para predicados, para verificar eficiência em tempo

## Exemplos de queries feitas com respectivas informações
queryDrugCategory               ,DrugIdentifier, DrugCategory

queryDrugClassification         ,DrugIdentifier, DrugClassification

queryDrugInformation            ,DrugIdentifier, ActivePrinciple, Indication

queryFoodInteraction            ,DrugIdentifier, FoodInteraction

queryInteraction                ,DrugIdentifier, InteractionIDs, Description

queryProduct                    ,DrugIdentifier, ProductName, ProductIdentifier

queryProductInteraction         ,DrugIdentifier, ProductName, ProductIdentifier, InteractionIDs, InteractionDescription

*/


# Algumas dúvidas atuais referente ao código

interacaoProdutosDiferentes_substring_Dynamic/3
        % Como é uma pesquisa por substring é mais válido fazer direto no sparql do que tentar dar match com assert?

