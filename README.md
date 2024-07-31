# Prototipo IC
Esse protótipo foi produzido para o projeto PIBIC UFES 2023 e com o título Desenvolvimento de Sistema para Gestão de Medicações Prescritas, do EDITAL FAPES Nº 09/2023 - PIBICES 2023

## Descrição
Esse protótipo é apenas um Backend feito para verificar interações entre medicamentos receitados e medicamentos na farmacinha caseira. Nota-se que essas interações são validadas usando dados reais do endpoint drugbank.bio2rdf.org/sparql/. A implementação foi realizada majoritariamente em sparql e prolog, cujas funções são consultas ao endpoint para obtenção de dados e manipulação de informações obtidas, respectivamente.

### Detalhes de implementação
* `farmacinha.pl` é o programa principal, portanto se encontra apenas predicados relacionados a interações entre medicamentos e a 'farmacinha caseira'.
* `auxFunctions.pl` são predicados para modularizar outros predicados.
* `rowFunctions.pl` são predicados que facilitam o uso do compound row, que é o compound usado para demonstrar os resultados das queries sparql
* `queries.pl` são predicados que auxiliam em modificações dentro das queries além de queries prontas, definidas no próximo tópico
* `sparql_client_.pl` é a biblioteca padrão sparql_client.pl -- SPARQL client library, mas modificada para aceitar qualquer certificado dos sites consultados pelo sparql, apenas incluido: url_option(cert_verify_hook(cert_accept_any)) ao invés de url_option(cert_verify_hook). OBS: Utilizado somente para fazer a consulta no endpoint mencionado
* `sparqlFunctions.pl` são predicados que facilitam as chamadas das consultas em sparql e manipulação de resultados
* `tester.pl` é um arquivo contendo módulos de teste para garantir versionamento do projeto

## Instalação
### Instalação do swi-prolog
Siga as instruções no site oficial do swi-prolog para [download](https://www.swi-prolog.org/download/devel)

### Instalação do protótipo
Baixe o código do [repositório](https://github.com/Renzo-Henrique/prototipoIC) e extraia para o diretório de sua escolha.

## Uso
Execute o aplicativo swi-prolog. No canto superior esquerdo clique em "File" e após isso "Consult ..." logo abaixo. Após isso selecione `farmacinha.pl` para ser consultado e já está disponível para usar o protótipo.

Para uso desse protótipo faz-se necessário entender apenas os predicados para inclusão de dados (axiomas):
* receita(Paciente, ListaMedicamentos)
* medicamentoComprado(Paciente,NomeProduto, CodigoProduto)

Além dos alertas de interação medicamentosa:
* alertaInteracaoFarmacinha(Paciente, ProdutoA, CodigoA, ProdutoB, CodigoB, -Descricao, ListaA, ListaB)
* alertaInteracao(Paciente, ProdutoA, CodigoA, ProdutoB, CodigoB, -Descricao, ListaA, ListaB)

Para uso de exemplos verifique a tag `<examples>` no final do arquivo `farmacinha.pl`.

## Como modificar?
O diretório `./modules` possui todos os arquivos necessários para adaptação do código para o seu problema em específico, em especial `sparqlFunctions.pl` possui os predicados que facilitam a manipulação das informações obtidas da consulta sparql.

### `queries.pl`
Existem atualmente os seguintes predicados em sparql query
| Predicado de querie | Variáveis de Consulta |
|-------------|-------------|
| queryDrugCategory               | DrugIdentifier, DrugCategory |
| queryDrugClassification         | DrugIdentifier, DrugClassification |
| queryDrugInformation            | DrugIdentifier, ActivePrinciple, Indication |
| queryFoodInteraction            | DrugIdentifier, FoodInteraction |
| queryInteraction                | DrugIdentifier, InteractionIDs, Description |
| queryProduct                    | DrugIdentifier, ProductName, ProductIdentifier |
| queryProductInteraction         | DrugIdentifier, ProductName, ProductIdentifier, InteractionIDs, InteractionDescription |

Podem ser implementadas queries adicionais em formato de predicado se forem válidos para o endpoint, para isso indico o uso de [yasgui](https://yasgui.triply.cc/#) com o endpoint do drugbank citado. Após isso inclua a query como o predicado em queries.pl no arquivo `./modules/queries.pl` e na seguinte formatação:
```
myQuery(Query):- 
        Query = "Inclua sua query no formato string".
```

Também faça a exportação dela no trecho de código, substitua `myQuery` pelo nome da sua query :
```
:- module(queries,   [  incluirFiltroQuery/3, incluirCondicaoString/3, finalStringQuery/2,
                        queryDrugCategory/1, queryDrugClassification/1,
                        queryDrugInformation/1, queryFoodInteraction/1,
                        queryInteraction/1, queryProduct/1,
                        queryProductInteraction/1,

                        myQuery/1]).
```
OBS: Evite uso de aspas duplas dentro da query


## Glossário
### Farmacinha Caseira
Nome dado ao conjunto de medicamentos, receitados ou não, que uma pessoa possui na residência.


# Erros
Fazer a pesquisa por substring funcionar em nível de sparqlFunctions, farmacinha n deve saber como isso é implementado.
