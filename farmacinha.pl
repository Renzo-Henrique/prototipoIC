:- use_module('queries.pl').
:- use_module('modules/sparqlFunctions.pl').


/* --------------------------------
 * --------------------------------
 * Inferências baseadas nas informações disponíveis
 * 
*/

/*------------------------
 * Simplificando a busca com código do produto
 * 
*/
medicamentoProdutoSimplificado(DrugbankPrimaryId,	PrincipioAtivo,
       							Nome, 				Codigo, 	
       							Country,		DonoDoRegistro):-
    
    	medicamentoProduto(	DrugbankPrimaryId,		PrincipioAtivo,
						Nome,	Codigo ,_,	
                       	Country, DonoDoRegistro,_,_,_,_), Country == "US"
    		;
    	medicamentoProduto(	DrugbankPrimaryId,		PrincipioAtivo,
						Nome,	_ ,Codigo,	
                       	Country, DonoDoRegistro,_,_,_,_), Country == "Canada".


/*------------------------
 * Mesmo princípio ativo
 *
*/
produtoMesmoPrincipioAtivo(	ProdutoA,				CodigoA, CountryA,
                    		ProdutoB,				CodigoB, CountryB,
                           	PrincipioAtivo):-
    	medicamentoProdutoSimplificado(Id, PrincipioAtivo, ProdutoA, CodigoA, CountryA, _),
    	medicamentoProdutoSimplificado(Id, PrincipioAtivo, ProdutoB, CodigoB, CountryB, _).
    	

		
    						

/*------------------------
 * Informações para saber sobre interações entre produtos
 * 
 * OBS: Se a descrição for "Mesmo principio ativo" quer dizer que a interação
 * não está no banco de dados diretamente, mas deve-se atentar para overdose
 * do princípio ativo.
*/

/*------------------------
 * Informações para saber sobre interações entre produtos com princípios ativos DIFERENTES
 * 
*/
interacaoProdutosDiferentes(	ProdutoA,				CodigoA,	CountryA,
                    ProdutoB,				CodigoB,	CountryB,
                  	Descricao
                    ):-
    	medicamentoProdutoSimplificado(IdA, PrincipioAtivoA, ProdutoA,	CodigoA,	CountryA,	_),
    	medicamentoProdutoSimplificado(IdB, PrincipioAtivoB, ProdutoB,	CodigoB,  	CountryB, 	_),
    	interacaoMedicamentosa(IdA, PrincipioAtivoA,
                               IdB, PrincipioAtivoB, Descricao),
    	IdA \== IdB.



/*------------------------
 * Informações para saber sobre interações entre produtos com princípios ativos IGUAIS
 * 
*/
interacaoProdutosSimilares(	ProdutoA,				CodigoA,	CountryA,
                    ProdutoB,				CodigoB,	CountryB,
                  	Descricao
                    ):-
    	produtoMesmoPrincipioAtivo(ProdutoA,CodigoA, CountryA,ProdutoB,CodigoB, CountryB,_),
    	CodigoA \== CodigoB, Descricao = "Mesmo principio ativo".

/*------------------------
 * Informações para saber sobre interações entre produtos IGUAIS
 * 
*/
interacaoProdutosIguais(	ProdutoA,				CodigoA,	CountryA,
                    ProdutoB,				CodigoB,	CountryB,
                  	Descricao
                    ):-
    	produtoMesmoPrincipioAtivo(ProdutoA,CodigoA, CountryA,ProdutoB,CodigoB, CountryB,_),
    	CodigoA == CodigoB, Descricao = "Mesmo produto".

/*------------------------
 * Informações para saber sobre interações entre produtos, 
 * englobando os 3 casos anteriores
 * 
*/
interacaoProdutos(	ProdutoA,				CodigoA,	CountryA,
                    ProdutoB,				CodigoB,	CountryB,
                  	Descricao
                    ):-
    	interacaoProdutosDiferentes(ProdutoA,CodigoA,CountryA,ProdutoB,CodigoB,CountryB,Descricao);
    	interacaoProdutosSimilares(	ProdutoA,CodigoA,CountryA,ProdutoB,CodigoB,CountryB,Descricao);
    	interacaoProdutosIguais(	ProdutoA,CodigoA,CountryA,ProdutoB,CodigoB,CountryB,Descricao).

    	
/*------------------------
 *------------------------
 * Simulação da farmacinha
 * 
*/

/*------------------------
 * Receita
 * 
 * receita(Paciente, Produto, CodigoProduto).
*/


/*------------------------
 * Em uso
 * medicamentoEmUso(Paciente, Produto,CodigoProduto).
*/

/*------------------------
 * Comprado
 * medicamentoComprado(Paciente, Produto,CodigoProduto).
*/


/*------------------------
 *------------------------
 * Alertas desejados no sistema
 * 
*/

/*------------------------
 * Similaridade de produto receitado com algum comprado
 * 
 * Possuem o mesmo principio ativo
 * 
*/
alertaSimilaridade(Paciente, ProdutoReceitado,CodigoReceitado,CountryReceitado, Produto,Codigo,Country):-
    receita(Paciente, ProdutoReceitado, CodigoReceitado, CountryReceitado),
    medicamentoComprado(Paciente, Produto, Codigo, Country),	
    (
    interacaoProdutosSimilares(ProdutoReceitado, CodigoReceitado, CountryReceitado, Produto, Codigo, Country,_) ;
    interacaoProdutosIguais(ProdutoReceitado, CodigoReceitado, CountryReceitado, Produto, Codigo, Country,_)
    ).

/*------------------------
 * Interação descrita no drugbank como ruim entre princípios ativos que possuem uma interação
 * 
 * Entre um medicamento em uso e um receitado
 * 
*/
alertaInteracaoReceita(Paciente, ProdutoA,CodigoA,CountryA, ProdutoB,CodigoB,CountryB, Descricao):-
    receita(Paciente, ProdutoA, CodigoA, CountryA),
    medicamentoEmUso(Paciente, ProdutoB, CodigoB, CountryB),
    (   
    interacaoProdutosDiferentes(ProdutoA, CodigoA, CountryA, ProdutoB, CodigoB, CountryB, Descricao);
    interacaoProdutosSimilares(ProdutoA, CodigoA, CountryA, ProdutoB, CodigoB, CountryB, Descricao);
    interacaoProdutosIguais(ProdutoA, CodigoA, CountryA, ProdutoB, CodigoB, CountryB, Descricao)
    ).

/*------------------------
 * Interação descrita no drugbank como ruim entre princípios ativos que possuem uma 
 * 
 * Entre um medicamento em uso e outro medicamento em uso
 * 
*/
alertaInteracaoEmUso(Paciente, ProdutoA,CodigoA,CountryA, ProdutoB,CodigoB,CountryB, Descricao):-
    medicamentoEmUso(Paciente, ProdutoA, CodigoA, CountryA),
    medicamentoEmUso(Paciente, ProdutoB, CodigoB, CountryB),
    (   
    interacaoProdutosDiferentes(ProdutoA, CodigoA, CountryA, ProdutoB, CodigoB, CountryB, Descricao);
    interacaoProdutosSimilares(ProdutoA, CodigoA, CountryA, ProdutoB, CodigoB, CountryB, Descricao);
    interacaoProdutosIguais(ProdutoA, CodigoA, CountryA, ProdutoB, CodigoB, CountryB, Descricao)
    ), CodigoA \== CodigoB.


/* <examples>
?- medicamentoPrincipioAtivo(DrugbankId,PrincipioAtivo).
?- recomendacaoAlimenticia("DB00001",Recomendacao).
?- recomendacaoAlimenticia("DB00945",Recomendacao).
?- interacaoMedicamentosa("DB00001", "Lepirudin", InterageId, InterageNome, Descricao).
?- interacaoMedicamentosa("DB00001", "Lepirudin", "DB00945", InterageNome, Descricao).
?- interacaoMedicamentosa("DB00001", "Lepirudin", "DB22945", InterageNome, Descricao).
?- interacaoProdutos("Aspirin","Honeywell Safety Products USA, Inc",
					"Refludan","Bayer",Descricao).
?- produtoMesmoPrincipioAtivo(	"217", "Merck Frosst Canada &amp; Cie, Merck Frosst Canada &amp; Co.", "Canada",
								"Aspirin", "Honeywell Safety Products USA, Inc", "US", PrincipioAtivo).
?- interacaoProdutos(	ProdutoA,"00666645",CountryA,ProdutoB,CodigoB,	CountryB,Descricao).

?- alertaSimilaridade("José", ProdutoReceitado,CodigoReceitado,CountryReceitado, Produto,Codigo,Country)
?- alertaInteracaoReceita(Paciente, ProdutoA,CodigoA,CountryA, ProdutoB,CodigoB,CountryB, Descricao)
?- alertaInteracaoEmUso("João", ProdutoA,CodigoA,CountryA, ProdutoB,CodigoB,CountryB, Descricao)
*/