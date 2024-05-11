
/*--
 * Essa seria uma busca com as informações mais importantes vistas
 * até agora para um princípio ativo
 * 
medicamentos_drugbank(DrugBankPrimaryId,		DrugbankId, 
                      PrincipioAtivo, 			CasNumber,
                      Sinonimo,					Descricao,
                      Indicacao,				Categoria,
                      Produto,					RecomendacaoComida,
                      InteracaoMedicamentoId,	InteracaoMedicamentoNome, 
                      InteracaoMedicamentoDescricao).

 * ------------------------
 * ------------------------
*/



/*------------------------
 * Informações que descrevem um medicamento
 * 
medicamento(	DrugBankPrimaryId, 	PrincipioAtivo, 	
				CasNumber, 			Descricao,			
                Indicacao)

 
*/
%% OBS: Várias linhas de indicação e descrição, cada uma referente a uma
%		informação diferente, separadas por &#13
%%
medicamento("DB00001","Lepirudin","138068-37-8",
			"Lepirudin is a recombinant hirudin formed by 65 amino acids that acts as a highly specific and direct thrombin inhibitor.[L41539,L41569] Natural hirudin is an endogenous anticoagulant found in _Hirudo medicinalis_ leeches.[L41539] Lepirudin is produced in yeast cells and is identical to natural hirudin except for the absence of sulfate on the tyrosine residue at position 63 and the substitution of leucine for isoleucine at position 1 (N-terminal end).[A246609] &#13;
&#13;
Lepirudin is used as an anticoagulant in patients with heparin-induced thrombocytopenia (HIT), an immune reaction associated with a high risk of thromboembolic complications.[A3, L41539] HIT is caused by the expression of immunoglobulin G (IgG) antibodies that bind to the complex formed by heparin and platelet factor 4. This activates endothelial cells and platelets and enhances the formation of thrombi.[A246609] Bayer ceased the production of lepirudin (Refludan) effective May 31, 2012.[L41574]</description>
  "			,
            
             "Lepirudin is indicated for anticoagulation in adult patients with acute coronary syndromes (ACS) such as unstable angina and acute myocardial infarction without ST elevation. In patients with ACS, lepirudin is intended for use with [aspirin].[L41539] Lepirudin is also indicated for anticoagulation in patients with heparin-induced thrombocytopenia (HIT) and associated thromboembolic disease in order to prevent further thromboembolic complications.[L41539]"
            ).

% Só pra esse existem mais de 10 indicações que vão de detalhadas para simples
medicamento("DB00945","Acetylsalicylic acid","50-78-2",
			"Also known as _Aspirin_, acetylsalicylic acid (ASA) is a commonly used drug for the treatment of pain and fever due to various causes. Acetylsalicylic acid has both anti-inflammatory and antipyretic effects.  This drug also inhibits platelet aggregation and is used in the prevention of blood clots stroke, and myocardial infarction (MI) [FDA label].  &#13;
&#13;
Interestingly, the results of various studies have demonstrated that long-term use of acetylsalicylic acid may decrease the risk of various cancers, including colorectal, esophageal, breast, lung, prostate, liver and skin cancer [A177325]. Aspirin is classified as a _non-selective cyclooxygenase (COX) inhibitor_ [A32682, A177268] and is available in many doses and forms, including chewable tablets, suppositories, extended release formulations, and others [L5968]. &#13;
&#13;
Acetylsalicylic acid is a very common cause of accidental poisoning in young children. It should be kept out of reach from young children, toddlers, and infants [FDA label].",
            
            "**Pain, fever, and inflammation**&#13;
&#13;
Acetylsalicylic acid (ASA), in the regular tablet form (immediate-release), is indicated to relieve pain, fever, and inflammation associated with many conditions, including the flu, the common cold, neck and back pain, dysmenorrhea, headache, tooth pain, sprains, fractures, myositis, neuralgia, synovitis, arthritis, bursitis, burns, and various injuries. It is also used for symptomatic pain relief after surgical and dental procedures [FDA label]. &#13;
&#13;
The _extra strength_ formulation of acetylsalicylic acid is also indicated for the management migraine pain with photophobia (sensitivity to light) and phonophobia (sensitivity to sound)[FDA label].&#13;
&#13;
**Other indications**&#13;
&#13;
ASA is also indicated for various other purposes, due to its ability to inhibit platelet aggregation. These include: &#13;
&#13;
Reducing the risk of cardiovascular death in suspected cases of myocardial infarction (MI) [FDA label]. &#13;
&#13;
Reducing the risk of a first non-fatal myocardial infarction in patients, and for reducing the risk of morbidity and mortality in cases of unstable angina and in those who have had a prior myocardial infarction [FDA label].&#13;
&#13;
For reducing the risk of transient ischemic attacks (TIA) and to prevent atherothrombotic cerebral infarction (in conjunction with other treatments) [FDA label].&#13;
&#13;
For the prevention of thromboembolism after hip replacement surgery [FDA label]. &#13;
&#13;
For decreasing platelet to platelet adhesion following carotid endarterectomy, aiding in the prevention of transient ischemic attacks (TIA) [FDA label].&#13;
&#13;
Used for patients undergoing hemodialysis with a silicone rubber arteriovenous cannula inserted to prevent thrombosis at the insertion site [FDA Label]. &#13;
&#13;
**Important note regarding use of the extended-release formulation [F4405]**&#13;
&#13;
In the setting of acute myocardial infarction, or before percutaneous interventions, the extended-release form of acetylsalicylic acid should not be used. Use immediate-release formulations in scenarios requiring rapid onset of action [Label, F4405]. The extended-release form is taken to decrease the incidence of mortality and myocardial infarction (MI) for individuals diagnosed with chronic coronary artery disease (CAD), including patients with previous myocardial infarction (MI) or unstable angina or with chronic stable angina. Additionally, the extended-release form is used to decrease the risk of death and recurrent episodes of stroke in patients with a history of stroke or TIA [F4405].&#13;
&#13;
&#13;"
            ).

/*------------------------
 * MedicamentoId e seu principioAtivo
 * 
medicamentoPrincipioAtivo(	DrugBankPrimaryId, 	PrincipioAtivo)

 
*/

medicamentoPrincipioAtivo(	"DB00001", 	"Lepirudin").
medicamentoPrincipioAtivo(	"DB00945", 	"Acetylsalicylic acid").

/*------------------------
 * Id secundarios
 * 
medicamentoId(	DrugBankPrimaryId,	DrugbankId)

 
*/
medicamentoId("DB00001","BTD00024").
medicamentoId("DB00001","BIOD00024").

medicamentoId("DB00945","APRD00264").
medicamentoId("DB00945","EXPT00475").
/*------------------------
 * Categorias de um medicamento
 * 
medicamentoCategoria(	DrugBankPrimaryId,	Categoria, MeshId)

 
*/
medicamentoCategoria("DB00001", 
                     "Amino Acids, Peptides, and Proteins","D000602").
medicamentoCategoria("DB00001", 
                     "Anticoagulants","D000925").
medicamentoCategoria("DB00001", 
                     "Antithrombin Proteins","D058833").
medicamentoCategoria("DB00001", 
                     "Antithrombins","D000991").

medicamentoCategoria("DB00945", 
                     "Acids, Carbocyclic","D000146").
medicamentoCategoria("DB00945", 
                     "Agents causing angioedema","").
medicamentoCategoria("DB00945", 
                     "Analgesics","D000700").
medicamentoCategoria("DB00945", 
                     "Anti-Inflammatory Agents, Non-Steroidal","D000894").
/*------------------------
 * Informações para saber sobre um nome sinônimo do medicamento
 * 
medicamentoSinonimo( 	DrubankPrimaryId,	LinguaSinonimo,
						Coder, 				Sinonimo)


*/
medicamentoSinonimo("DB00001", "english", "", "Lepirudin recombinant").
medicamentoSinonimo("DB00001", "english", "", "Hirudin variant-1").
medicamentoSinonimo("DB00001", "english", "inn", "Lepirudin").
medicamentoSinonimo("DB00001", "english", "", "Desulfatohirudin").


medicamentoSinonimo("DB00945", "english", 
                    "", "2-Acetoxybenzenecarboxylic acid").
medicamentoSinonimo("DB00945", "english", 
                    "", "Acetylsalicylate").
medicamentoSinonimo("DB00945", "spanish", 
                    "inn", "ácido acetilsalicílico").

/*------------------------
 * Informações para saber sobre produtos de um PrincipioAtivo 
 *	

medicamentoProduto(	DrugbankPrimaryId,		PrincipioAtivo,
					Nome,					NationalDrugCode,
            		DrugProductDatabase,	Country,
                    DonoDoRegistro,			FormaDosagem,
                    ViaDeAdministracao,		Strenght,
                    Generico
            ).
            
%% Observação: 
%% NationalDrugCode:- NDC
%% DrugProductDatabse:- dpd
%% Labeller:- DonoDoRegistro

*/

/*------------------------
 * Principais informações a respeito de um produto
*/
medicamentoProduto(	"DB00001",				"Lepirudin",
					"Refludan",				"50419-150",
            		"",						"US",
                    "Bayer",				"Powder",
                    "Intravenous",		"50 mg/1mL",
                    false
            ).
medicamentoProduto(	"DB00001",				"Lepirudin",
					"Refludan",				"",
            		"02240996",				"Canada",
                    "Bayer",				"Powder, for solution",
                    "Intravenous",		"50 mg / vial",
                    false
            ).


medicamentoProduto(	"DB00945",				"Acetylsalicylic acid",
					"217",					"",
            		"00666645",				"Canada",
                    "Merck Frosst Canada &amp; Cie, Merck Frosst Canada &amp; Co.",				
                   	"Tablet",	
                   	"Oral",		"",
                    false
            ).
medicamentoProduto(	"DB00945",				"Acetylsalicylic acid",
					"Aspirin",				"0498-0114",
            		"",						"US",
                    "Honeywell Safety Products USA, Inc",				
                   	"Tablet",	
                   	"Oral",					"325 mg/1",
                    false
            ).


/*------------------------
 * Informações para saber sobre recomendações com alimentos
 *

recomendacaoAlimenticia(	DrugbankPrimaryId
							RecomendacaoComida)
					
*/

recomendacaoAlimenticia(	"DB00001",
                           "Avoid herbs and supplements with anticoagulant/antiplatelet activity. Examples include chamomile, garlic, ginger, ginkgo and ginseng."
                        ).

recomendacaoAlimenticia(	"DB00945",
                           "Avoid alcohol. Alcohol increases the risk of gastrointestinal bleeding."
                        ).
recomendacaoAlimenticia(	"DB00945",
                           "Avoid herbs and supplements with anticoagulant/antiplatelet activity. Examples include garlic, ginger, bilberry, danshen, piracetam, and ginkgo biloba."
                        ).
recomendacaoAlimenticia(	"DB00945",
                           "Take after a meal. This reduces irritating gastrointestinal effects."
                        ).
recomendacaoAlimenticia(	"DB00945",
                           "Take with a full glass of water."
                        ).

/*------------------------
 * Informações para saber sobre interações com medicamentos
 *

interacaoMedicamentosa(		DrugbankPrimaryId,		NomeComum,
							InteracaoMedicamentoId, InteracaoMedicamentoNome,
                            InteracaoMedicamentoDescricao)
					
*/

interacaoMedicamentosa(		"DB00001",		"Lepirudin",
							"DB06605", 		"Apixaban",
                            "Apixaban may increase the anticoagulant activities of Lepirudin."
                       ).
interacaoMedicamentosa(		"DB00001",		"Lepirudin",
							"DB06695", 		"Dabigatran etexilate",
                            "Dabigatran etexilate may increase the anticoagulant activities of Lepirudin."
                       ).
interacaoMedicamentosa(		"DB00001",		"Lepirudin",
							"DB01254", 		"Dasatinib",
                            "The risk or severity of bleeding and hemorrhage can be increased when Dasatinib is combined with Lepirudin."
                       ).
interacaoMedicamentosa(		"DB00001",		"Lepirudin",
							"DB01609", 		"Deferasirox",
                            "The risk or severity of gastrointestinal bleeding can be increased when Lepirudin is combined with Deferasirox."
                       ).
interacaoMedicamentosa(		"DB00001",		"Lepirudin",
							"DB00945", 		"Acetylsalicylic acid",
                            "Acetylsalicylic acid may increase the anticoagulant activities of Lepirudin."
                       ).



interacaoMedicamentosa(		"DB00945",		"Acetylsalicylic acid",
							"DB06605", 		"Apixaban",
                            "The risk or severity of adverse effects can be increased when Acetylsalicylic acid is combined with Apixaban."
                       ).
interacaoMedicamentosa(		"DB00945",		"Acetylsalicylic acid",
							"DB01254", 		"Dasatinib",
                            "Dasatinib may increase the anticoagulant activities of Acetylsalicylic acid."
                       ).
interacaoMedicamentosa(		"DB00945",		"Acetylsalicylic acid",
							"DB00001", 		"Lepirudin",
                            "Acetylsalicylic acid may increase the anticoagulant activities of Lepirudin."
                       ).


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
 * receita(Paciente, Produto,CodigoProduto,CountryProduto).
*/

receita("José","Refludan", "50419-150", "US").

receita("Maria","Aspirin", "0498-0114", "US").

receita("João","Aspirin", "0498-0114", "US").

/*------------------------
 * Em uso
 * medicamentoEmUso(Paciente, Produto,CodigoProduto,CountryProduto).
*/
medicamentoEmUso("João","Refludan", "02240996", "Canada").
medicamentoEmUso("João","Aspirin", "0498-0114", "US").

medicamentoEmUso("José","Aspirin", "0498-0114", "US").

/*------------------------
 * Comprado
 * medicamentoComprado(Paciente, Produto,CodigoProduto,CountryProduto).
*/
medicamentoComprado("José","Refludan", "50419-150", "US").
medicamentoComprado("José","Refludan", "02240996", "Canada").
medicamentoComprado("José","217", "00666645", "Canada").
medicamentoComprado("José","Aspirin", "0498-0114", "US").

medicamentoComprado("Maria","Aspirin", "0498-0114", "US").

medicamentoComprado("João","Aspirin", "0498-0114", "US").


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

