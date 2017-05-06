# User Control ShowDelphi Edition

Esta Versao do componente Deriva da Versão da Comunidade Show Delphi com "Fork" no dia 05/05/2017

Versões testadas até o momento:

Delphi 10.2 Tokyo.
  

Adicionado PRIMARY KEY as Seguintes Tabelas:

   1 - UCTABUSERS
   
   2 - UCTABMESSAGES
   
   
Adicionado Novas Tabelas:

   1 - UCDEPARTMENT     Lotação do Usuário.
   
   2 - UCEMPRESA        Empresa     
   
   
   3 - UCUSERTYPE       Futuro definir se e administardor ou outros para da acesso ao cadstro de usuario e
						liberar apenas direitos que ele possua para ususrio que cadastra.
						
						
Adicionado Opções na Tela de Manutenção de Usuario:

Pesquisa por Nome ou Login.
 obs. Quando se tem poucos usaurios fica facil manter e navegar pelas teclas de navegação, mas quando se tem mais de 1000 usuários,  como eu tenho, fica muito dificil navegar ate encontar o usuario para edição.
 ![alt text](https://github.com/maurilima/usercontrol-sd/blob/master/TelaCadastro.PNG)
 
 
Testado e Intsalado No Delphi Tokyo Manual.

O UCSWInstall esta dando um erro, acho que devido ao D. Tokyo, que na compilação tenta carregar as depencias.

Fiz Uam pequena Modificao para adicionar o D. Tkyo na Relação. Mas não estudei mas a fundo o erro.
   

README ORIGINAL do Componente.

Mantido todos os Autores  e outros.
-- --------------------------------------------------------------------------------------------------
Uma versão dos Componentes User Control mantido pela Comunidade Show Delphi.
A comunidade tem o objetivo de manter esta biblioteca de componentes
compatíveis com as novas versões do Delphi, realizando melhorias, sempre 
que possível.

Fórum oficial: http://showdelphi.com.br/forum/forum/duvidas-e-problemas-relacionados-ao-usercontrol-showdelphi-edition/

O componente tem suporte do Delphi 7 até o Delphi 10.1 Berlin.

Versões testadas até o momento:

Delphi 7

Delphi 2010

Delphi XE 3

Delphi XE 6

Delphi XE 7

Delphi XE 8

Delphi 10 Seattle

Delphi 10.1 Berlin

Obs: Quem estiver utilizando a versão Starter do Berlin, precisa realizar a instalação manualmente,
pois o delphi não gera as bpls se você utilizar o instalador.
Lembramos também que o Berlin Starter, por padrão, não tem componentes de conexão instalados, logo
recomendamos a instalação do Zeos que é Open Source, assim será possível de utilizar o UserControl. 

Conectors Suportados até o momento:
ADOConnector, IBXConnector, DBXConnector, FireDACConnector, ZeosConnector, DataSnapConnector.

O RestConnector é experimental.
O Midas Connector é somente até o Delphi XE 8.

Há ainda outros connectors na pasta "conectors para testar", no entando estes connector não foram
testados pela nossa equipe.
Lista dos connectors para teste:
AboluteBase, AstaConn, BDEConn, DBISAMConn, FIBConn, IBOConn, MDOConn, MyDACConn,
NexusDBConn, ODACConn, UIBConn, UniDACConn.

O componente UserControl é compatível com a compilação em 64 bits.

Para ver uma descrição completa dos componentes acesse: http://showdelphi.com.br/descricao-dos-componentes-usercontrol/

Visite a comunidade: http://showdelphi.com.br




