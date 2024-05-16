/*:- module(farmacinha,   
                [  interacaoProdutosDiferentes_v1/5, interacaoProdutosDiferentes_v2/5, interacaoProdutosDiferentes_Especificos_1/5]).
*/
:- use_module('modules/queries.pl').
:- use_module('modules/sparqlFunctions.pl').
:- use_module('modules/auxFunctions.pl').

%
% Nomeclaturas adotadas para simplificar nome das variáveis:
% ------------------------------------------
% IdPrimario significa Id do princípio ativo 
%
%

%
% Predicados que podem ser usados frequentemente
% ------------------------------------------
% resultadoListado(queryDrugCategory,[IdPrimario, DrugCategory])
% 
% resultadoListado(queryDrugClassification,[IdPrimario, DrugClassification])
% 
% resultadoListado(queryDrugInformation,[IdPrimario, ActivePrinciple, Indication])
% 
% resultadoListado(queryFoodInteraction,[IdPrimario, FoodInteraction])
% 
% resultadoListado(queryInteraction,[IdPrimario, InteractionIDs, Description])
% 
% resultadoListado(queryProduct,[IdPrimario, ProductName, ProductIdentifier])
%
%

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


%------------------------
% Lista com IdPrimario que o produto possui (casos em que pode-se ter mais que um)
listaIdPrimarioProduto(NomeProduto, CodigoProduto, Lista):-
    bagof(IdPrimario, resultadoListado(queryProduct, [IdPrimario, NomeProduto, CodigoProduto]), Temp ),
    sort(Temp, Lista).
    %length(Lista, Tam),
    %Tam > 1.


/*------------------------
 * Informações para saber sobre interações entre produtos com princípios ativos IGUAIS
 * 
*/
interacaoProdutosSimilares( ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    listaIdPrimarioProduto(ProdutoA, CodigoA, ListaIdA),
    listaIdPrimarioProduto(ProdutoB, CodigoB, ListaIdB),
    ListaIdA == ListaIdB, stringMesmoPrincipioAtivo(Descricao).

/*------------------------
 * Informações para saber sobre interações entre produtos com princípios ativos DIFERENTES
 *  
*/
interacaoProdutoSimplificada(IdPrimarioA, IdPrimarioB, Descricao):-
    IdPrimarioA \== IdPrimarioB,
    resultadoListado(queryInteraction, [IdPrimarioA, InteractionIDs, Descricao]),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

interacaoProdutosDiferentes(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    resultadoListado(queryProduct, [IdPrimarioA, ProdutoA, CodigoA]),
    resultadoListado(queryProduct, [IdPrimarioB, ProdutoB, CodigoB]),
    IdPrimarioA \== IdPrimarioB,
    resultadoListado(queryInteraction, [IdPrimarioA, InteractionIDs, Descricao]),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

% Interacao com produtos especificos!!!!!
interacaoProdutosDiferentes_A_Especifico(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    resultadoListado(queryProduct, "?productName", ProdutoA, [IdPrimarioA, ProdutoA, CodigoA]),
    resultadoListado(queryProduct, [IdPrimarioB, ProdutoB, CodigoB]),
    IdPrimarioA \== IdPrimarioB,
    resultadoListado(queryInteraction, '?drugIdentifier', IdPrimarioA, [IdPrimarioA, InteractionIDs, Descricao]),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

interacaoProdutosDiferentes_AB_Especifico(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    resultadoListado(queryProduct, '?productName', ProdutoA, [IdPrimarioA, ProdutoA, CodigoA]),
    resultadoListado(queryProduct, '?productName', ProdutoB, [IdPrimarioB, ProdutoB, CodigoB]),
    IdPrimarioA \== IdPrimarioB,
    resultadoListado(queryInteraction, '?drugIdentifier', IdPrimarioA, [IdPrimarioA, InteractionIDs, Descricao]),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).


%  Uso de dynamic para acelerar a verificação das queries
interacaoProdutosDiferentes_A_Especifico_dynamic(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    (produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA) ;
    resultadoListado(queryProduct, '?productName', ProdutoA, [IdPrimarioA, ProdutoA, CodigoA]), assert(produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA))
    ),

    (produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB) ;
    resultadoListado(queryProduct, [IdPrimarioB, ProdutoB, CodigoB]), assert(produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB))
    ),

    IdPrimarioA \== IdPrimarioB,
    resultadoListado(queryInteraction, '?drugIdentifier', IdPrimarioA, [IdPrimarioA, InteractionIDs, Descricao]),
    
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

interacaoProdutosDiferentes_AB_Especifico_dynamic(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    (produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA) ;
    resultadoListado(queryProduct, '?productName', ProdutoA, [IdPrimarioA, ProdutoA, CodigoA]), assert(produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA))
    ),

    (produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB) ;
    resultadoListado(queryProduct, '?productName', ProdutoB, [IdPrimarioB, ProdutoB, CodigoB]), assert(produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB))
    ),
    IdPrimarioA \== IdPrimarioB, 
    resultadoListado(queryInteraction, '?drugIdentifier', IdPrimarioA, [IdPrimarioA, InteractionIDs, Descricao]),
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).


% TODO: Problema, Ao usar assert em interactionID está tendo muitas cópias de resultados
interacaoProdutos_productInteraction(ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao):-
    (produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA), interacaoIdPrimario(IdPrimarioA, InteractionIDs, _)  ;
        resultadoListado(queryProductInteraction, '?productName', ProdutoA, [IdPrimarioA, ProdutoA, CodigoA, InteractionIDs, Descricao ]),
        assert(produtoIdPrimario(IdPrimarioA, ProdutoA, CodigoA)),
        assert(interacaoIdPrimario(IdPrimarioA, InteractionIDs, Descricao))
    ),
    (produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB) ;
    resultadoListado(queryProduct, [IdPrimarioB, ProdutoB, CodigoB]), assert(produtoIdPrimario(IdPrimarioB, ProdutoB, CodigoB))
    ),
    IdPrimarioA \== IdPrimarioB,
    sub_string(InteractionIDs, _, _, _, IdPrimarioB).

retractDynamic():-
    retractall(produtoIdPrimario(_, _, _)),
    retractall(interacaoIdPrimario(_, _, _)).


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
receita("José",[['Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09', "Consumir de 8 em 8 horas"],
                ["Teste simples", "Codigo", "Ingerir com água"],
                ['Evite atividades intensas'],
                ['Refludan 50 mg vial', '172d9ce8065ac96f019189b12bde6767', "--"]
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
medicamentoComprado("José",'Angiomax 250 mg vial', '050312783d93f8e97fbe03456bf168c9').
medicamentoComprado("Maria", 'Pulmozyme 1 mg/ml Solution 2.5ml Plastic Container', '062c64e7cdc2435ce743297119614312').
medicamentoComprado("Maria", 'Lufyllin-GG 200-200 mg tablet', '3b316e3524ae739666b2c595ece5d0f8').

alertaInteracaoFarmacinha(Paciente, ProdutoA, CodigoA, ProdutoB, CodigoB, Descricao, ListaA, ListaB):-
    CodigoA \== CodigoB,
    medicamentoComprado(Paciente, ProdutoA, CodigoA),
    medicamentoComprado(Paciente, ProdutoB, CodigoB),
    listaIdPrimarioProduto(ProdutoA, CodigoA, ListaA),
    listaIdPrimarioProduto(ProdutoB, CodigoB, ListaB),
    (
        % Produtos iguais
        ListaA == ListaB, stringMesmoPrincipioAtivo(Descricao) ;

        % Necessário callback para cada elemento da lista
        processar(ListaA, ListaB, Descricao, interacaoProdutoSimplificada)
    ).
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

?- interacaoProdutosDiferentes_v1('Enbrel 25 mg kit', CodigoA, 'Kineret 1 Box = 7 Syringes, 4.69ml Box', CodigoB, Descricao).
?- interacaoProdutosDiferentes_AB_Especifico_dynamic('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09', 'Angiomax 250 mg vial', '050312783d93f8e97fbe03456bf168c9', Descricao).

?-interacaoProdutos_productInteraction('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09', 'Angiomax 250 mg vial', '050312783d93f8e97fbe03456bf168c9', Descricao).

?-interacaoProdutos_productInteraction('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09', 'Angiomax 250 mg vial', '050312783d93f8e97fbe03456bf168c9', Descricao).

?-interacaoProdutos_productInteraction('Entrophen 10 650 mg Enteric-Coated Tablet','0377fffd0546225a918b5a674c1c1a09', 'Angiomax 250 mg vial', Codigo, 'DDI between Bivalirudin and Acetylsalicylic acid - May enhance the anticoagulant effect of Anticoagulants.').

?- interacaoProdutosDiferentes('Pulmozyme 1 mg/ml Solution 2.5ml Plastic Container', '062c64e7cdc2435ce743297119614312', 'Lufyllin-GG 200-200 mg tablet', '3b316e3524ae739666b2c595ece5d0f8', Descricao).

*/



