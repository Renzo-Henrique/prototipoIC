:- use_module("modules/queries.pl").
:- use_module("modules/sparqlFunctions.pl").
:- use_module("modules/auxFunctions.pl").

/*
* Nomeclaturas adotadas para simplificar nome das variáveis:
* ------------------------------------------
* IdPrimario significa Id do princípio ativo 
*
*/

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

% Predicados dinâmicos que auxiliam consultas verificadas frequentemente
% TODO:: Usar com cuidado

:- dynamic produtoIdPrimario/3.
% produtoIdPrimario(IdPrimario, NomeProduto, CodigoProduto ).

:- dynamic interacaoIdPrimario/3.
% interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao).

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
    bagof(IdPrimario, resultadoListado(queryProduct, [IdPrimario, NomeProduto, CodigoProduto]), Temp ),
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
 * interacaoProdutosDiferentes_AB_Especifico(+ProdutoA, CodigoA, +ProdutoB, CodigoB, -Descricao)
 *
 * Predicado que verifica a descrição da interação entre diferentes produtos
 *
 * @param Produto_ O nome do produto para o qual deseja-se obter os identificadores primários.
 * @param Codigo_ O código do produto para o qual deseja-se obter os identificadores primários.
 * @param Descricao É a descrição da interação entre os princípios ativos dos produtos
 *
 * @note Utiliza dynamic para facilitar pesquisas futuras
 */
interacaoProdutosDiferentes_AB_Especifico_dynamic(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    (   
        %TODO:: MUDAR OS ARGUM
        produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA) ;
        resultadoListado(queryProduct, "?productName", ProdutoA, [IdPrimarioA, ProdutoA, CodigoA]), 
        assert(produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA))
    ), % Corte para evitar backtracking caso o assert já tenha sido feito

    (   
        produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB) ;
        resultadoListado(queryProduct, "?productName", ProdutoB, [IdPrimarioB, ProdutoB, CodigoB]), 
        assert(produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB))
    ),!, % Corte para evitar backtracking caso o assert já tenha sido feito
    IdPrimarioA \== IdPrimarioB,
    (   
        interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao),sub_string(InteractionIDs, _, _, _, IdPrimarioB) ;

        interacaoIdPrimario(IdPrimarioB, InteractionIDs, Descricao),sub_string(InteractionIDs, _, _, _, IdPrimarioA) ;

        resultadoListado(queryInteraction, "?drugIdentifier", IdPrimarioA, [IdPrimarioA, InteractionIDs, Descricao]), 
        assert(interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao)),
        sub_string(InteractionIDs, _, _, _, IdPrimarioB),assert(interacaoIdPrimario(IdPrimarioB, InteractionIDs, Descricao))
    ), !.


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
    (produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA), interacaoIdPrimario(IdPrimarioA, InteractionIDs, _), !  ;
        resultadoListado(queryProductInteraction, "?productName", ProdutoA, [IdPrimarioA, ProdutoA, CodigoA, InteractionIDs, Descricao ]),
        assert(produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA)), assert(interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao))
        
    ),
    (produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB) ;
    resultadoListado(queryProduct, [IdPrimarioB, ProdutoB, CodigoB]), assert(produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB))
    ),
    IdPrimarioA \== IdPrimarioB,
    sub_string(InteractionIDs, _, _, _, IdPrimarioB), interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao).

retractDynamic():-
    retractall(produtoIdPrimario(_, _, _)),
    retractall(interacaoIdPrimario(_, _, _)).


/*------------------------
 *------------------------
 * Simulação da farmacinha
 * 
*/

/**
 * elementosInternos(+Lista, -X, -Y, -Z)
 *
 * Predicado que busca elementos internos de sublistas em uma lista.
 *
 * @param Lista A lista a ser percorrida em busca de sublistas contendo os elementos X, Y e Z.
 * @param X Variável que será unificada com o primeiro elemento das sublistas encontradas.
 * @param Y Variável que será unificada com o segundo elemento das sublistas encontradas.
 * @param Z Variável que será unificada com o terceiro elemento das sublistas encontradas.
 *
 * Este predicado é não determinístico e faz uso de backtracking.
 * @note Se uma sublista contendo os elementos X, Y e Z for encontrada, o predicado unifica as variáveis X, Y e Z e retorna true. Caso contrário, o predicado faz backtracking para encontrar outras sublistas que correspondam aos critérios.
 */
elementosInternos([H|T], X, Y, Z) :-
    H = [X, Y, Z] ;
    elementosInternos(T, X, Y, Z).

/*
* Esse é o que mais se aproxima do que um médico faz
* ListaRecomendacoes possui cada elemento sendo uma lista que possui os seguintes elementos:
* (NomeProduto, CodigoProduto, Posologia).
*
* receita(Paciente, ListaMedicamentos).
* ---------------
* Nota-se que receita é algo mais complexo
* ---------------
*/
receita("José",[["Entrophen 10 650 mg Enteric-Coated Tablet","0377fffd0546225a918b5a674c1c1a09", "Consumir de 8 em 8 horas"],
                ["Teste simples", "Codigo", "Ingerir com água"],
                ["Evite atividades intensas"],
                ["Refludan 50 mg vial", "172d9ce8065ac96f019189b12bde6767", "--"]
                ] ).

% Medicamento específico para paciente
% receitaMedicamento(Paciente, NomeProduto, CodigoProduto, Posologia ).
receitaMedicamento(Paciente, NomeProduto, CodigoProduto, Posologia):-
    receita(Paciente, ListaMedicamentos),
    elementosInternos(ListaMedicamentos, NomeProduto, CodigoProduto, Posologia).


/*------------------------
 * Em uso
 * medicamentoEmUso(Paciente, Produto,CodigoProduto).
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
* @param Lista_ IdPrimarios do produto a ser analisado
* 
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
*  alertaInteracaoFarmacinha(Paciente, ProdutoA, CodigoA, ProdutoB, CodigoB, -Descricao, ListaA, ListaB)
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
        processar(ListaA, ListaB, Descricao, interacaoProdutoSimplificada)
    ).


/** <examples>

?- interacaoProdutosDiferentes_AB_Especifico_dynamic("Entrophen 10 650 mg Enteric-Coated Tablet","0377fffd0546225a918b5a674c1c1a09", "Angiomax 250 mg vial", "050312783d93f8e97fbe03456bf168c9", Descricao).

?-interacaoProdutos_productInteraction("Entrophen 10 650 mg Enteric-Coated Tablet","0377fffd0546225a918b5a674c1c1a09", P, C, Descricao).

?-interacaoProdutos_productInteraction("Entrophen 10 650 mg Enteric-Coated Tablet","0377fffd0546225a918b5a674c1c1a09", "Angiomax 250 mg vial", "050312783d93f8e97fbe03456bf168c9", Descricao).

?-interacaoProdutos_productInteraction("Entrophen 10 650 mg Enteric-Coated Tablet","0377fffd0546225a918b5a674c1c1a09", "Angiomax 250 mg vial", Codigo, "DDI between Bivalirudin and Acetylsalicylic acid - May enhance the anticoagulant effect of Anticoagulants.").

?- interacaoProdutosDiferentes("Pulmozyme 1 mg/ml Solution 2.5ml Plastic Container", "062c64e7cdc2435ce743297119614312", "Lufyllin-GG 200-200 mg tablet", "3b316e3524ae739666b2c595ece5d0f8", Descricao).

?- interacaoSimplificada("DB00005", "DB00026", Descricao).
*/





