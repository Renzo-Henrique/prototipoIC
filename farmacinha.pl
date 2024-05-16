/*:- module(farmacinha,   
                [  interacaoProdutosDiferentes_v1/5, interacaoProdutosDiferentes_v2/5, interacaoProdutosDiferentes_Especificos_1/5]).
*/
:- use_module('modules/queries.pl').
:- use_module('modules/sparqlFunctions.pl').


%
% Nomeclaturas adotadas para simplificar nome das variáveis:
% ------------------------------------------
% IdPrimario significa Id do princípio ativo 
%
%

%
% Predicados que podem ser usados frequentemente
% ------------------------------------------
% resultadoValoresSeparados(queryDrugCategory,IdPrimario, DrugCategory)
% 
% resultadoValoresSeparados(queryDrugClassification,IdPrimario, DrugClassification)
% 
% resultadoValoresSeparados(queryDrugInformation,IdPrimario, ActivePrinciple, Indication)
% 
% resultadoValoresSeparados(queryFoodInteraction,IdPrimario, FoodInteraction)
% 
% resultadoValoresSeparados(queryInteraction,IdPrimario, InteractionIDs, Description)
% 
% resultadoValoresSeparados(queryProduct,IdPrimario, ProductName, ProductIdentifier)
%
%

% Predicados dinâmicos que auxiliam consultas verificadas frequentemente
% TODO:: Usar com cuidado

:- dynamic produtoIdPrimario/3.
% produtoIdPrimario(IdPrimario, NomeProduto, CodigoProduto ).

:- dynamic interacaoIdPrimario/3.
% interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao).
%:- dynamic produtoIdInteraction/5.
% produtoIdInteraction(IdPrimario, NomeProduto, CodigoProduto, InteractionIDs, Descricao).


/* --------------------------------
 * --------------------------------
 * Inferências baseadas nas informações disponíveis
 * 
*/

%------------------------
% Lista com IdPrimario que o produto possui (casos em que pode-se ter mais que um)
listaIdPrimarioProduto(NomeProduto, CodigoProduto, Lista):-
    bagof(IdPrimario, resultadoValoresSeparados(queryProduct, IdPrimario, NomeProduto, CodigoProduto), Temp ),
    sort(Temp, Lista),
    length(Lista, Tam),
    Tam > 1.


/*------------------------
 * Informações para saber sobre interações entre produtos com princípios ativos IGUAIS
 * 
*/
interacaoProdutosSimilares(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    listaIdPrimarioProduto(ProdutoA, CodigoA, ListaA),
    listaIdPrimarioProduto(ProdutoB, CodigoB, ListaB),
    ListaA == ListaB, Descricao = "Possuem mesmo principio ativo".

/*------------------------
 * Informações para saber sobre interações entre produtos com princípios ativos DIFERENTES
 * 
*/
interacaoProdutosDiferentes(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    resultadoValoresSeparados(queryProduct, IdPrimarioA, ProdutoA, CodigoA),
    resultadoValoresSeparados(queryProduct, IdPrimarioB, ProdutoB, CodigoB),
    IdPrimarioA \== IdPrimarioB,
    resultadoValoresSeparados(queryInteraction, IdPrimarioA, InteractionIDs, Descricao),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

% Interacao com produtos especificos!!!!!
interacaoProdutosDiferentes_A_Especifico(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    resultadoSeparadoFiltrado(queryProduct, "?productName", ProdutoA, IdPrimarioA, ProdutoA, CodigoA),
    resultadoValoresSeparados(queryProduct, IdPrimarioB, ProdutoB, CodigoB),
    IdPrimarioA \== IdPrimarioB,
    resultadoSeparadoFiltrado(queryInteraction, '?drugIdentifier', IdPrimarioA, IdPrimarioA, InteractionIDs, Descricao),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

interacaoProdutosDiferentes_AB_Especifico(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    resultadoSeparadoFiltrado(queryProduct, '?productName', ProdutoA, IdPrimarioA, ProdutoA, CodigoA),
    resultadoSeparadoFiltrado(queryProduct, '?productName', ProdutoB, IdPrimarioB, ProdutoB, CodigoB),
    IdPrimarioA \== IdPrimarioB,
    resultadoSeparadoFiltrado(queryInteraction, '?drugIdentifier', IdPrimarioA, IdPrimarioA, InteractionIDs, Descricao),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).


%  Uso de dynamic para acelerar a verificação das queries
interacaoProdutosDiferentes_A_Especifico_dynamic(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    (produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA) ;
    resultadoSeparadoFiltrado(queryProduct, '?productName', ProdutoA, IdPrimarioA, ProdutoA, CodigoA), assert(produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA))
    ),

    (produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB) ;
    resultadoValoresSeparados(queryProduct, IdPrimarioB, ProdutoB, CodigoB), assert(produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB))
    ),

    IdPrimarioA \== IdPrimarioB,
    resultadoSeparadoFiltrado(queryInteraction, '?drugIdentifier', IdPrimarioA, IdPrimarioA, InteractionIDs, Descricao),
    
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

interacaoProdutosDiferentes_AB_Especifico_dynamic(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    (produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA) ;
    resultadoSeparadoFiltrado(queryProduct, '?productName', ProdutoA, IdPrimarioA, ProdutoA, CodigoA), assert(produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA))
    ),

    (produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB) ;
    resultadoSeparadoFiltrado(queryProduct, '?productName', ProdutoB, IdPrimarioB, ProdutoB, CodigoB), assert(produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB))
    ),

    IdPrimarioA \== IdPrimarioB,
    resultadoSeparadoFiltrado(queryInteraction, '?drugIdentifier', IdPrimarioA, IdPrimarioA, InteractionIDs, Descricao),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).


% TODO: Problema, leia README
interacaoProdutos_productInteraction(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    (produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA), interacaoIdPrimario(IdPrimarioA, InteractionIDs, _)  ;
        resultadoListado(queryProductInteraction, '?productName', ProdutoA, ListaInf),
        ListaInf = [IdPrimarioA, ProdutoA, CodigoA, InteractionIDs, Descricao ],
        assert(produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA)),
        assert(interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao))
    ),
    (produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB) ;
    resultadoValoresSeparados(queryProduct, IdPrimarioB, ProdutoB, CodigoB), assert(produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB))
    ),
    IdPrimarioA \== IdPrimarioB,
    sub_string(InteractionIDs, _, _, _, IdPrimarioB),
    distinct([ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao], (  produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA),
                    interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao), 
                    produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB)) ).
/*
(produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA), interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao)  ;
        resultadoListado(queryProductInteraction, '?productName', ProdutoA, ListaInf),
        ListaInf = [IdPrimarioA, ProdutoA, CodigoA, InteractionIDs, Descricao ],
        assert(produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA)),
        assert(interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao))
    ),
    (produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB) ;
    resultadoValoresSeparados(queryProduct, IdPrimarioB, ProdutoB, CodigoB), assert(produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB))
    ),
    IdPrimarioA \== IdPrimarioB,
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).
*/
% [DrugIdentifier, ProductName, ProductIdentifier, InteractionIDs, InteractionDescription]

retractDynamic():-
    retractall(produtoIdPrimario(_, _, _)),
    retractall(interacaoIdPrimario(_, _, _)).

% interacaoProdutosDiferentes_Especificos_dynamic_v1('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09',Produto, Codigo, Descricao).
% interacaoProdutosDiferentes_Especificos_dynamic_v2('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09',Produto, Codigo, Descricao).
% interacaoProdutosDiferentes_Especificos_dynamic_v3('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09',Produto, Codigo, Descricao).
% interacaoProdutos_productInteraction('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09',Produto, Codigo, Descricao).
/*------------------------
 *------------------------
 * Simulação da farmacinha
 * 
*/
elementos_internos([H|T], X, Y, Z) :-
    H = [X, Y, Z] ;
    elementos_internos(T, X, Y, Z).

% Esse é o que mais se aproxima do que um médico faz
% ListaRecomendacoes possui cada elemento sendo uma lista que possui os seguintes elementos:
% (NomeProduto, CodigoProduto, Posologia).
%
% receita(Paciente, ListaMedicamentos).
% ---------------
% Nota-se que receita é algo mais complexo
% ---------------
receita("José",[["Entrophen 10 650 mg Enteric-Coated Tablet","0377fffd0546225a918b5a674c1c1a09", "Consumir de 8 em 8 horas"],
                ["Teste simples", "Codigo", "Ingerir com água"],
                ["Refludan 50 mg vial", "172d9ce8065ac96f019189b12bde6767", "--"]
                ] ).

% Medicamento específico para paciente
% receitaMedicamento(Paciente, NomeProduto, CodigoProduto, Posologia ).
receitaMedicamento(Paciente, NomeProduto, CodigoProduto, Posologia):-
    receita(Paciente, ListaMedicamentos),
    elementos_internos(ListaMedicamentos, NomeProduto, CodigoProduto, Posologia).


/*------------------------
 * Em uso
 * medicamentoEmUso(Paciente, Produto,CodigoProduto).
*/
medicamentoEmUso("João","Refludan", "02240996", "Canada").
medicamentoEmUso("João","Aspirin", "0498-0114", "US").

medicamentoEmUso("José","Aspirin", "0498-0114", "US").


/** <examples>

?- interacaoProdutosDiferentes_v1('Enbrel 25 mg kit', CodigoA, 'Kineret 1 Box = 7 Syringes, 4.69ml Box', CodigoB, Descricao).
?- interacaoProdutosDiferentes_AB_Especifico_dynamic('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09', 'Angiomax 250 mg vial', '050312783d93f8e97fbe03456bf168c9', Descricao).

interacaoProdutos_productInteraction('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09', 'Angiomax 250 mg vial', '050312783d93f8e97fbe03456bf168c9', Descricao).

interacaoProdutos_productInteraction('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09', 'Angiomax 250 mg vial', Codigo, 'DDI between Bivalirudin and Acetylsalicylic acid - May enhance the anticoagulant effect of Anticoagulants.').
interacaoProdutos_productInteraction('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09', 'Angiomax 250 mg vial', Codigo, Des).

*/
