aler:- use_module("modules/queries.pl").
:- use_module("modules/sparqlFunctions.pl").
:- use_module("modules/auxFunctions.pl").
:- use_module(library(pldoc)).

/*
* Predicados que podem ser usados frequentemente
* ------------------------------------------
* resultadoListado(queryDrugCategory,[IdPrimario, DrugCategory])
* 
* resultadoListado(queryDrugClassification,[IdPrimario, DrugClassification])
* 
* resultadoListado(queryDrugInformation,[IdPrimario, ActivePrinciple, Indication])
* 
* resultadoListado(queryFoodInteraction,[IdPrimario, FoodInteraction])
* 
* resultadoListado(queryInteraction,[IdPrimario, InteractionIDs, Description])
* 
* resultadoListado(queryProduct,[IdPrimario, ProductName, ProductIdentifier])
*
*/


/* --------------------------------
 * --------------------------------
 * Inferências baseadas nas informações disponíveis
 * 
*/


/**
 * listaIdPrimarioProduto(NomeProduto, CodigoProduto, -Lista)
 *
 * Predicado que retorna uma lista de identificadores primários associados a um produto.
 *
 * @param NomeProduto O nome do produto para o qual deseja-se obter os identificadores primários.
 * @param CodigoProduto O código do produto para o qual deseja-se obter os identificadores primários.
 * @param Lista Variável que será unificada com a lista de identificadores primários associados ao produto.
 *
 * @note Este predicado é não determinístico devido ao uso de bagof/3 para obter múltiplas soluções.
 * @note Caso o produto não possua identificadores primários associados, a Lista será unificada com uma lista vazia.
 */
listaIdPrimarioProduto(NomeProduto, CodigoProduto, Lista):-
    bagof(IdPrimario, resultadoListado(queryProduct, "?productIdentifier", CodigoProduto,  [IdPrimario, NomeProduto, CodigoProduto]), Temp ),
    sort(Temp, Lista).


/**
 * interacaoProdutosSimilares( +ProdutoA, +CodigoA, +ProdutoB, +CodigoB, -Descricao)
 *
 * Predicado que verifica se produtos são similares (Possuem os mesmos Princípios Ativos)
 *
 * @param Produto_ O nome do produto para o qual deseja-se obter os identificadores primários.
 * @param Codigo_ O código do produto para o qual deseja-se obter os identificadores primários.
 *
 */
interacaoProdutosSimilares( ProdutoA, CodigoA, ProdutoB, CodigoB, ListaIdA, ListaIdB):-
    listaIdPrimarioProduto(ProdutoA, CodigoA, ListaIdA),
    listaIdPrimarioProduto(ProdutoB, CodigoB, ListaIdB),
    CodigoA \== CodigoB,
    ListaIdA == ListaIdB.

/**
 * interacaoProdutoSimplificada(+IdPrimarioA, +IdPrimarioB, -Descricao)
 *
 * Predicado que verifica se produtos são similares (Possuem os mesmos Princípios Ativos)
 *
 * @param Produto_ O nome do produto para o qual deseja-se obter os identificadores primários.
 * @param Codigo_ O código do produto para o qual deseja-se obter os identificadores primários.
 *
 */
interacaoSimplificada(IdPrimarioA, IdPrimarioB, Descricao):-
    resultadoListado(queryInteraction, [IdPrimarioA, InteractionIDs, Descricao]),
    IdPrimarioA \== IdPrimarioB,
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

/**
 * interacaoProdutosDiferentes(ProdutoA, CodigoA, ProdutoB, CodigoB, -Descricao)
 *
 * Predicado que verifica a descrição da interação entre diferentes produtos
 *
 * @param Produto_ O nome do produto para o qual deseja-se obter os identificadores primários.
 * @param Codigo_ O código do produto para o qual deseja-se obter os identificadores primários.
 * @param Descricao É a descrição da interação entre os princípios ativos dos produtos
 *
 */
interacaoProdutosDiferentes(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    resultadoListado(queryProduct, [IdPrimarioA, ProdutoA, CodigoA]),
    resultadoListado(queryProduct, [IdPrimarioB, ProdutoB, CodigoB]),
    IdPrimarioA \== IdPrimarioB,
    resultadoListado(queryInteraction, [IdPrimarioA, InteractionIDs, Descricao]),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

/**
 * interacaoProdutosDiferentes_A_Especifico(+ProdutoA, CodigoA, ProdutoB, CodigoB, -Descricao)
 *
 * Predicado que verifica a descrição da interação entre diferentes produtos
 *
 * @param Produto_ O nome do produto para o qual deseja-se obter os identificadores primários.
 * @param Codigo_ O código do produto para o qual deseja-se obter os identificadores primários.
 * @param Descricao É a descrição da interação entre os princípios ativos dos produtos
 *
 */
interacaoProdutosDiferentes_A_Especifico(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    resultadoListado(queryProduct, "?productName", ProdutoA, [IdPrimarioA, ProdutoA, CodigoA]),
    resultadoListado(queryProduct, [IdPrimarioB, ProdutoB, CodigoB]),
    IdPrimarioA \== IdPrimarioB,
    resultadoListado(queryInteraction, "?drugIdentifier", IdPrimarioA, [IdPrimarioA, InteractionIDs, Descricao]),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

/**
 * interacaoProdutosDiferentes_AB_Especifico(+ProdutoA, CodigoA, +ProdutoB, CodigoB, -Descricao)
 *
 * Predicado que verifica a descrição da interação entre diferentes produtos
 *
 * @param Produto_ O nome do produto para o qual deseja-se obter os identificadores primários.
 * @param Codigo_ O código do produto para o qual deseja-se obter os identificadores primários.
 * @param Descricao É a descrição da interação entre os princípios ativos dos produtos
 *
 */
interacaoProdutosDiferentes_AB_Especifico(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    resultadoListado(queryProduct, "?productName", ProdutoA, [IdPrimarioA, ProdutoA, CodigoA]),
    resultadoListado(queryProduct, "?productName", ProdutoB, [IdPrimarioB, ProdutoB, CodigoB]),
    IdPrimarioA \== IdPrimarioB,
    resultadoListado(queryInteraction, "?drugIdentifier", IdPrimarioA, [IdPrimarioA, InteractionIDs, Descricao]),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).


/**
 * interacaoProdutosDiferentes__substring(+ChaveA, +ValorA, -Resultado)
 *
 * Predicado que verifica a descrição da interação entre diferentes produtos
 *
 * @param Chave_ O nome da variável de consulta utilizada para pesquisa
 * @param Valor_ Valor que precisa ser encontrado na variável de consulta
 * @param Resultado é o resultado da busca
 *
 */
interacaoProdutosDiferentes_substring(ChaveA, ValorA, Resultado):-
    (   
        resultadoListado(queryProduct, ChaveA, ValorA, [IdPrimarioA, ProdutoA, CodigoA])
    ),
    (   
        resultadoListado(queryInteraction, "?drugIdentifier", IdPrimarioA, [IdPrimarioA, InteractionIDs, Descricao]),
        stringAfterChar(InteractionIDs, '_', IdPrimarioB)
    ),
    IdPrimarioA \== IdPrimarioB,
    (   
        resultadoListado(queryProduct, "?drugIdentifier", IdPrimarioB, [IdPrimarioB, ProdutoB, CodigoB])
    ),
    Resultado = [[IdPrimarioA, ProdutoA, CodigoA],[IdPrimarioB, ProdutoB, CodigoB], Descricao ].

/**
 * interacaoProdutosDiferentes__substring(+ChaveA, +ValorA, +ChaveB, +ValorB, -Resultado)
 *
 * Predicado que verifica a descrição da interação entre diferentes produtos
 *
 * @param Chave_ O nome da variável de consulta utilizada para pesquisa
 * @param Valor_ Valor que precisa ser encontrado na variável de consulta
 * @param Resultado é o resultado da busca
 *
 */
interacaoProdutosDiferentes_substring(ChaveA, ValorA, ChaveB, ValorB, Resultado):-
    (   
        resultadoListado(queryProduct, ChaveA, ValorA, [IdPrimarioA, ProdutoA, CodigoA])
    ),

    (   
        resultadoListado(queryProduct, ChaveB, ValorB, [IdPrimarioB, ProdutoB, CodigoB])
    ),
    IdPrimarioA \== IdPrimarioB,
    (   
        resultadoListado(queryInteraction, "?drugIdentifier", IdPrimarioA, [IdPrimarioA, InteractionIDs, Descricao]), 
        sub_string(InteractionIDs, _, _, _, IdPrimarioB)
        
    ),
    Resultado = [[IdPrimarioA, ProdutoA, CodigoA],[IdPrimarioB, ProdutoB, CodigoB], Descricao ].

/**
 * interacaoProdutos_productInteraction(+ProdutoA, CodigoA, ProdutoB, CodigoB, -Descricao)
 *
 * Predicado que verifica a descrição da interação entre diferentes produtos, só que com a queryProductInteraction
 *
 * @param Produto_ O nome do produto para o qual deseja-se obter os identificadores primários.
 * @param Codigo_ O código do produto para o qual deseja-se obter os identificadores primários.
 * @param Descricao É a descrição da interação entre os princípios ativos dos produtos
 *
 */ %TODO: problema, vários resultados iguais por causa do assert interacaoIdPrimario
interacaoProdutos_productInteraction(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    
    resultadoListado(queryProductInteraction, "?productName", ProdutoA, [IdPrimarioA, ProdutoA, CodigoA, InteractionIDs, Descricao ]),
    resultadoListado(queryProduct, [IdPrimarioB, ProdutoB, CodigoB]),
    IdPrimarioA \== IdPrimarioB,
    sub_string(InteractionIDs, _, _, _, IdPrimarioB), interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao).


/*------------------------
 *------------------------
 * Simulação da farmacinha
 * 
*/


/*
* receita(Paciente, ListaMedicamentos).
*
* Esse predicado simula a receita de um médico, um médico coloca para um paciente uma lista 
* contendo com cada elemento tendo as informações sobre como fazer uma medicação específica: (NomeProduto, CodigoProduto, Posologia) 
* Logo, ListaRecomendacoes possui cada elemento sendo uma lista que possui os elementos da forma:
* (NomeProduto, CodigoProduto, Posologia)
*
* ---------------
* @note Nota-se que receita é algo mais complexo e essa é uma simplificação
* ---------------
*/
receita("José",[["Entrophen 10 650 mg Enteric-Coated Tablet","0377fffd0546225a918b5a674c1c1a09", "Consumir de 8 em 8 horas"],
                ["Teste simples", "Codigo", "Ingerir com água"],
                ["Evite atividades intensas"],
                ["Refludan 50 mg vial", "172d9ce8065ac96f019189b12bde6767", "--"]
                ] ).

/*
* receitaMedicamento(Paciente, NomeProduto, CodigoProduto, Posologia )
*
* Obtem medicamento específico para um paciente
* @param Paciente O Nome do paciente
* @param NomeProduto O nome do produto relacionado ao paciente
* @param CodigoProduto O código do produto relacionado paciente
* @param Posologia É a descrição de como o medicamento deve ser utilizado
*/
receitaMedicamento(Paciente, NomeProduto, CodigoProduto, Posologia):-
    receita(Paciente, ListaMedicamentos),
    elementosInternos(ListaMedicamentos, NomeProduto, CodigoProduto, Posologia).

/*
* medicamentoComprado(Paciente,NomeProduto, CodigoProduto)
*
* Medicamento que uma pessoa comprou e possui na "farmacinha"
* @param Paciente O Nome do paciente
* @param NomeProduto O nome do produto relacionado ao paciente
* @param CodigoProduto O código do produto relacionado paciente
*/
medicamentoComprado("José","Angiomax 250 mg vial", "050312783d93f8e97fbe03456bf168c9").
medicamentoComprado("Maria", "Pulmozyme 1 mg/ml Solution 2.5ml Plastic Container", "062c64e7cdc2435ce743297119614312").
medicamentoComprado("Maria", "Lufyllin-GG 200-200 mg tablet", "3b316e3524ae739666b2c595ece5d0f8").

/* 
*  alertaInteracaoFarmacinha(Paciente, ProdutoA, CodigoA, ProdutoB, CodigoB, -Descricao, ListaA, ListaB)
*
* Predicado que verifica interações entre medicamentos comprados por tal Paciente
* 
* @param Paciente É o paciente a ser analisado
* @param Produto_ Nome do produto a ser analisado
* @param Codigo_ Código do produto a ser analisado
* @param Descricao É a descrição da interação entre os dois, se houver
* @param Lista_ Lista de IdPrimarios do produto a ser analisado
* 
* 
*/
alertaInteracaoFarmacinha(Paciente, ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao, ListaA, ListaB):-
    CodigoA \== CodigoB,
    medicamentoComprado(Paciente, ProdutoA, CodigoA),
    medicamentoComprado(Paciente, ProdutoB, CodigoB),
    listaIdPrimarioProduto(ProdutoA, CodigoA, ListaA),
    listaIdPrimarioProduto(ProdutoB, CodigoB, ListaB),
    CodigoA \== CodigoB,
    (
        % Produtos iguais
        ListaA == ListaB, stringMesmoPrincipioAtivo(Descricao) ;

        % Necessário callback para cada elemento da lista
        processar(ListaA, ListaB, Descricao, interacaoSimplificada)
    ).

/* 
*  alertaInteracao(Paciente, ProdutoA, CodigoA, ProdutoB, CodigoB, -Descricao, ListaA, ListaB)
*
* Predicado que verifica interações entre medicamento comprado e um receitado de tal paciente
* 
* @param Paciente É o paciente a ser analisado
* @param Produto_ Nome do produto a ser analisado
* @param Codigo_ Código do produto a ser analisado
* @param Descricao É a descrição da interação entre os dois, se houver
* @param Lista_ IdPrimarios do produto a ser analisado
* 
*
* 
*/
alertaInteracao(Paciente, ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao, ListaA, ListaB):-
    CodigoA \== CodigoB,
    receitaMedicamento(Paciente, ProdutoA, CodigoA, _),
    medicamentoComprado(Paciente, ProdutoB, CodigoB),
    listaIdPrimarioProduto(ProdutoA, CodigoA, ListaA),
    listaIdPrimarioProduto(ProdutoB, CodigoB, ListaB),
    (
        % Produtos iguais
        ListaA == ListaB, stringMesmoPrincipioAtivo(Descricao) ;

        % Necessário callback para cada elemento da lista
        processar(ListaA, ListaB, Descricao, interacaoSimplificada)
    ).


/** <examples>

?-interacaoProdutos_productInteraction("Entrophen 10 650 mg Enteric-Coated Tablet","0377fffd0546225a918b5a674c1c1a09", P, C, Descricao).
?-interacaoProdutos_productInteraction("Entrophen 10 650 mg Enteric-Coated Tablet","0377fffd0546225a918b5a674c1c1a09", "Angiomax 250 mg vial", "050312783d93f8e97fbe03456bf168c9", Descricao).
?-interacaoProdutos_productInteraction("Entrophen 10 650 mg Enteric-Coated Tablet","0377fffd0546225a918b5a674c1c1a09", "Angiomax 250 mg vial", Codigo, "DDI between Bivalirudin and Acetylsalicylic acid - May enhance the anticoagulant effect of Anticoagulants.").

?- interacaoProdutosDiferentes("Pulmozyme 1 mg/ml Solution 2.5ml Plastic Container", "062c64e7cdc2435ce743297119614312", "Lufyllin-GG 200-200 mg tablet", "3b316e3524ae739666b2c595ece5d0f8", Descricao).

?- interacaoSimplificada("DB00005", "DB00026", Descricao).

?- interacaoProdutosDiferentes_substring("?productName", "Entrophen", "?productName","Angiomax", Descricao).
?- interacaoProdutosDiferentes_substring("?productName", "Enbrel", Resultado).
?- interacaoProdutosDiferentes_substring("?productIdentifier", "0377fffd0546225a", Resultado).

?- alertaInteracaoFarmacinha("Maria", ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao, ListaA, ListaB).
?- alertaInteracao(Paciente, ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao, ListaA, ListaB).
*/





