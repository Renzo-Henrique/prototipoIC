# PrototipoIC


# Motivo da modularização


## Exemplos de queries feitas com respectivas informações
queryDrugCategory               ,DrugIdentifier, DrugCategory

queryDrugClassification         ,DrugIdentifier, DrugClassification

queryDrugInformation            ,DrugIdentifier, ActivePrinciple, Indication

queryFoodInteraction            ,DrugIdentifier, FoodInteraction

queryInteraction                ,DrugIdentifier, InteractionIDs, Description

queryProduct                    ,DrugIdentifier, ProductName, ProductIdentifier

queryProductInteraction         ,DrugIdentifier, ProductName, ProductIdentifier, InteractionIDs, InteractionDescription

*/

# Novo readme:

# Prototipo IC
Esse protótipo foi produzido para o projeto PIBIC UFES 2023 e com o título Desenvolvimento de Sistema para Gestão de Medicações Prescritas, do EDITAL FAPES Nº 09/2023 - PIBICES 2023

## Descrição
Esse protótipo é apenas um Backend feito para verificar interações entre medicamentos receitados e medicamentos na farmacinha caseira. Nota-se que essas interações serão validadas usando dados reais do endpoint drugbank.bio2rdf.org/sparql/. A implementação foi realizada majoritariamente em sparql e prolog, cujas funções são consultas ao endpoint para obtenção de dados e manipulação de informações obtidas, respectivamente.

### Detalhes de implementação
* farmacinha.pl é o programa principal, portanto se encontra apenas predicados relacionados a interações e a 'farmacinha caseira'.
* auxFunctions.pl são predicados para modularizar outros predicados.
* rowFunctions.pl são predicados que facilitam o uso do compound row, que é o compound usado para demonstrar os resultados das queries sparql
* queries.pl são predicados que auxiliam em modificações dentro das queries além de queries prontas, definidas no próximo tópico
* sparql_client_.pl é a biblioteca padrão sparql_client.pl -- SPARQL client library, mas modificada para aceitar qualquer certificado dos sites consultados pelo sparql, apenas incluido: url_option(cert_verify_hook(cert_accept_any)) ao invés de url_option(cert_verify_hook). OBS: Utilizado somente para fazer a consulta no endpoint mencionado
* sparqlFunctions.pl são predicados que facilitam as chamadas das consultas em sparql e manipulação de resultados

## Instalação
### Instalação do swi-prolog
### Instalação do protótipo
## Uso
Para uso desse protótipo faz-se necessário entender apenas como é utilizado farmacinha.pl

## Como modificar?
### sparqlFunctions.pl
### queries.pl

## Agradecimentos

## Glossário
### Farmacinha Caseira
Nome dado aos medicamentos que uma pessoa possui na residência, sejam receitados ou não.
