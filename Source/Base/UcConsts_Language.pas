{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usu�rios }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{
Autor da vers�o Original: Rodrigo Alves Cordeiro

Colaboradores da vers�o original
Alexandre Oliveira Campioni - alexandre.rural@netsite.com.br
Bernard Grandmougin
Carlos Guerra
Daniel Wszelaki
Everton Ramos [BS2 Internet]
Francisco Due�as - fduenas@flashmail.com
Germ�n H. Cravero
Luciano Almeida Pimenta [ClubeDelphi.net]
Luiz Benevenuto - luiz@siffra.com
Luiz Fernando Severnini
Peter van Mierlo
Rodolfo Ferezin Moreira - rodolfo.fm@bol.com.br
Rodrigo Palhano (WertherOO)
Ronald Marconi
Sergiy Sekela (Dr.Web)
Stefan Nawrath
Vicente Barros Leonel [ Fknyght ]

*******************************************************************************}
{ Vers�o ShowDelphi Edition                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015   Giovani Da Cruz                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{ Voc� pode obter a �ltima vers�o desse arquivo na pagina do projeto           }
{ User Control ShowDelphi Edition                                              }
{ Componentes localizado em http://infussolucoes.github.io/usercontrol-sd/     }
{                                                                              }
{ Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la  }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{ Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{ Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto }
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{                                                                              }
{ Comunidade Show Delphi - www.showdelphi.com.br                               }
{                                                                              }
{ Giovani Da Cruz  -  giovani@infus.inf.br  -  www.infus.inf.br                }
{                                                                              }
{ ****************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 01/07/2015: Giovani Da Cruz
  |*  - Cria��o e distribui��o da Primeira Versao ShowDelphi
  ******************************************************************************* }
unit UcConsts_Language;

interface

Uses SysUtils;

Type
  TUCLanguage = (ucPortuguesBr, ucEnglish, ucSpanish, ucFrench);


ResourceString   
	rsSobreTitulo = 'UserControl ShowDelphi Edition VCL'; 
	rsSobreDescricao = 'UserControl ShowDelphi E. http://showdelphi.com.br/' + #13#10 +	
                     'Componentes Controle de Usu�rios' + #13#10 +                        
                     'Lesser General Public License version 2.0';						
	rsSobreLicencaStatus = 'LGPLv2';
	
	RsENoSplashServices = 'Unable to get Borland Splash Services';

Const
  MaxArray = 237; //  227;

  UC_PTBR: Array [0 .. MaxArray, 0 .. 1] of string = (('Const_Contr_TitleLabel','Sele��o de Componentes do Form. :'),
    ('Const_Contr_GroupLabel', 'Grupo :'),
    ('Const_Contr_CompDispLabel', 'Componentes Dispon�veis :'),
    ('Const_Contr_CompSelLabel', 'Componentes Selecionados :'),
    ('Const_Contr_BtOK', '&OK'), ('Const_Contr_BTCancel', '&Cancelar'),
    ('Const_Contr_DescCol', 'Descri��o'), ('Const_Contr_BtSellAllHint', 'Selecionar Todos'),
    ('Const_Contr_BtSelHint', 'Selecionar'),
    ('Const_Contr_BtUnSelHint', 'Desmarcar'),
    ('Const_Contr_BtUnSelAllHint',  'Desmarcar Todos'),
    ('Const_Msgs_BtNew', '&Nova Mensagem'),
    ('Const_Msgs_BtReplay', '&Responder'),
    ('Const_Msgs_BtForward','E&ncaminhar'),
    ('Const_Msgs_BtDelete', '&Excluir'),
    ('Const_Msgs_BtClose', '&Fechar'),
    ('Const_Msgs_WindowCaption', 'Mensagens do Sistema'),
    ('Const_Msgs_ColFrom', 'Remetente'),
    ('Const_Msgs_ColSubject', 'Assunto'),
    ('Const_Msgs_ColDate', 'Data'),
    ('Const_Msgs_PromptDelete', 'Confirma excluir as mensagens selecionadas ?'),
    ('Const_Msgs_PromptDelete_WindowCaption', 'Apagar mensagens'),
    ('Const_Msgs_NoMessagesSelected', 'N�o existem mensagens selecionadas'),
    ('Const_Msgs_NoMessagesSelected_WindowCaption', 'Informa��o'),
    ('Const_MsgRec_BtClose', '&Fechar'),
    ('Const_MsgRec_WindowCaption','Mensagem'),
    ('Const_MsgRec_Title', 'Mensagem Recebida'),
    ('Const_MsgRec_LabelFrom', 'De :'),
    ('Const_MsgRec_LabelDate', 'Data'),
    ('Const_MsgRec_LabelSubject', 'Assunto'),
    ('Const_MsgRec_LabelMessage', 'Mensagem'),
    ('Const_MsgSend_BtSend', '&Enviar'),
    ('Const_MsgSend_BtCancel', '&Cancelar'),
    ('Const_MsgSend_WindowCaption', 'Mensagem'),
    ('Const_MsgSend_Title', 'Enviar Nova Mensagem'),
    ('Const_MsgSend_GroupTo', 'Para'),
    ('Const_MsgSend_RadioUser', 'Usu�rio :'),
    ('Const_MsgSend_RadioAll', 'Todos'),
    ('Const_MsgSend_GroupMessage', 'Mensagem'),
    ('Const_MsgSend_LabelSubject', 'Assunto'),
    ('Const_MsgSend_LabelMessageText', 'Texto da mensagem'),
    ('MsgExceptConnection', 'N�o informado o Connection, Transaction ou Database do componente %s'),
    ('MsgExceptTransaction', 'N�o informado o Transaction do componente %s'),
    ('MsgExceptDatabase', 'N�o informado o Database do componente %s'),
    ('MsgExceptPropriedade', 'Favor informar a propriedade %s'),
    ('MsgExceptUserMngMenu', 'Informe na propriedade UsersForm.MenuItem ou UsersForm.Action o Item respons�vel pelo controle de usu�rios'),
    ('MsgExceptUserProfile', 'Informe na propriedade UsersProfile.MenuItem ou UsersProfile.Action o Item respons�vel pelo controle de Perfil de usu�rios'),
    ('MsgExceptChagePassMenu', 'Informe na propriedade ChangePasswordForm.MenuItem or .Action o Item que permite ao usu�rio alterar sua senha'),
    ('MsgExceptAppID', 'Na propriedade ApplicationID informe um nome para identificar a aplica��o na tabela de permiss�es'),
    ('MsgExceptUsersTable', 'Na propriedade TableUsers informe o nome da tabela que ser� criada para armazenar os dados dos usu�rios'),
    ('MsgExceptRightsTable', 'Na propriedade TableRights informe o nome da tabela que ser� criada para armazenar as permiss�es dos usu�rios'),
    ('MsgExceptConnector', 'Propriedade DataConnector n�o definida!'),
    ('Const_Men_AutoLogonError', 'Falha de Auto Logon!' + #13 + #10 +   'Informe um usu�rio e senha v�lidos.'),
    ('Const_Men_SenhaDesabitada', 'Retirada senha do Login %s'),
    ('Const_Men_SenhaAlterada', 'Senha alterada com sucesso!'),
    ('Const_Men_MsgInicial', 'ATEN��O Login Inicial:' + #13 + #10 + #13 + #10 + 'Usu�rio : :user' + #13 +  #10 + 'Senha : :password' + #13 + #10 + #13 + #10 + 'Defina as permiss�es para este usu�rio.'),
    ('Const_Men_MaxTentativas', '%d Tentativas de login inv�lido. Por motivos de seguran�a o ' + #13 + #10 + 'sistema ser� fechado.'),
    ('Const_Men_LoginInvalido', 'Usu�rio ou Senha inv�lidos!'),
    ('Const_Men_UsuarioExiste', 'O Usu�rio "%s" j� est� cadastrado no sistema !!'),
    ('Const_Men_PasswordExpired', 'Aten��o, sua senha expirou, favor troca-la'),
    ('Const_Log_BtCancelar', '&Cancelar'),
    ('Const_Log_BtOK', '&OK'),
    ('Const_Log_LabelSenha', 'Senha :'),
    ('Const_Log_LabelUsuario', 'Usu�rio :'), ('Const_Log_WindowCaption', 'Login'),
    ('Const_Log_LbEsqueciSenha', 'Esqueci a senha'),
    ('Const_Log_MsgMailSend', 'A senha foi enviada para o seu email.'),
    ('Const_Log_LabelTentativa', 'Tentativa : '),
    ('Const_Log_LabelTentativas', 'M�ximo de Tentativas : '),
    ('Const_LogC_WindowCaption', 'Seguran�a'),
    ('Const_LogC_LabelDescricao', 'Log do Sistema'),
    ('Const_LogC_LabelUsuario', 'Usu�rio :'), ('Const_LogC_LabelData', 'Data :'),
    ('Const_LogC_LabelNivel', 'N�vel m�nimo :'),
    ('Const_LogC_ColunaAppID', 'AppID'),
    ('Const_LogC_ColunaNivel', 'N�vel'),
    ('Const_LogC_ColunaMensagem', 'Mensagem'),
    ('Const_LogC_ColunaUsuario', 'Usu�rio'),
    ('Const_LogC_ColunaData', 'Data'),
    ('Const_LogC_BtFiltro', '&Aplicar Filtro'),
    ('Const_LogC_BtExcluir', '&Excluir Log'),
    ('Const_LogC_BtFechar', '&Fechar'),
    ('Const_LogC_ConfirmaExcluir', 'Confirma excluir todos os registros de log selecionados ?'),
    ('Const_LogC_ConfirmaDelete_WindowCaption', 'Confirma exclus�o'),
    ('Const_LogC_Todos', 'Todos'),
    ('Const_LogC_Low', 'Baixo'),
    ('Const_LogC_Normal', 'Normal'),
    ('Const_LogC_High', 'Alto'),
    ('Const_LogC_Critic', 'Cr�tico'),
    ('Const_LogC_ExcluirEfectuada', 'Exclus�o de log�s do sistema : Usu�rio = "%s" | Data = %s a %s | N�vel <= %s'),
    ('Const_Cad_WindowCaption', 'Seguran�a'),
    ('Const_Cad_LabelDescricao', 'Cadastro de Usu�rios'), ('Const_Cad_ColunaNome', 'Nome'),
    ('Const_Cad_ColunaLogin', 'Login'),
    ('Const_Cad_ColunaEmail', 'Email'),
    ('Const_Cad_BtAdicionar', '&Adicionar'),
    ('Const_Cad_BtAlterar', 'A&lterar'),
    ('Const_Cad_BtExcluir', '&Excluir'),
    ('Const_Cad_BtPermissoes', 'A&cessos'),
    ('Const_Cad_BtSenha', '&Senha'),
    ('Const_Cad_BtFechar', '&Fechar'),
    ('Const_Cad_ConfirmaExcluir', 'Confirma excluir o usu�rio "%s" ?'),
    ('Const_Cad_ConfirmaDelete_WindowCaption', 'Excluir usu�rio'),
    ('Const_Prof_WindowCaption', 'Seguran�a'),
    ('Const_Prof_LabelDescricao', 'Perfil de Usu�rios'),
    ('Const_Prof_ColunaNome', 'Perfil'),
    ('Const_Prof_BtAdicionar', '&Adicionar'),
    ('Const_Prof_BtAlterar', 'A&lterar'), ('Const_Prof_BtExcluir', '&Excluir'),
    ('Const_Prof_BtPermissoes', 'A&cessos'), ('Const_Prof_BtSenha', '&Senha'),
    ('Const_Prof_BtFechar', '&Fechar'), ('Const_Prof_ConfirmaExcluir',
    'Existem usu�rios com o perfil "%s". Confirma excluir?'),
    ('Const_Prof_ConfirmaDelete_WindowCaption', 'Delete profile'),
    ('Const_Inc_WindowCaption', 'Cadastro de Usu�rios'),
    ('Const_Inc_LabelAdicionar', 'Adicionar Usu�rio'),
    ('Const_Inc_LabelAlterar', 'Alterar Usu�rio'),
    ('Const_Inc_LabelNome', 'Nome :'), ('Const_Inc_LabelLogin', 'Login :'),
    ('Const_Inc_LabelEmail', 'Email :'), ('Const_Inc_LabelPerfil', 'Perfil :'),
    ('Const_Inc_CheckPrivilegiado', 'Usu�rio privilegiado'),
    ('Const_Inc_BtGravar', '&Gravar'), ('Const_Inc_BtCancelar', 'Cancelar'),
    ('Const_Inc_CheckEspira', 'Senha do usu�rio n�o expira'),
    ('Const_Inc_Dia', 'Dias'), ('Const_Inc_ExpiraEm', 'Expira em'),
    ('Const_PInc_WindowCaption', 'Perfil de Usu�rios'),
    ('Const_PInc_LabelAdicionar', 'Adicionar Perfil'),
    ('Const_PInc_LabelAlterar', 'Alterar Perfil'),
    ('Const_PInc_LabelNome', 'Descri��o :'), ('Const_PInc_BtGravar', '&Gravar'),
    ('Const_PInc_BtCancelar', 'Cancelar'), ('Const_Perm_WindowCaption',
    'Seguran�a'), ('Const_Perm_LabelUsuario', 'Permiss�es do Usu�rio :'),
    ('Const_Perm_LabelPerfil', 'Permiss�es do Perfil :'),
    ('Const_Perm_PageMenu', 'Itens do Menu'), ('Const_Perm_PageActions',
    'A��es'), ('Const_Perm_PageControls', 'Controles'),
    ('Const_Perm_BtLibera', '&Liberar'), ('Const_Perm_BtBloqueia', '&Bloquear'),
    ('Const_Perm_BtGravar', '&Gravar'), ('Const_Perm_BtCancelar', '&Cancelar'),
    ('Const_Troc_WindowCaption', 'Seguran�a'), ('Const_Troc_LabelDescricao',
    'Trocar Senha'), ('Const_Troc_LabelSenhaAtual', 'Senha Atual :'),
    ('Const_Troc_LabelNovaSenha', 'Nova Senha :'), ('Const_Troc_LabelConfirma',
    'Confirma��o :'), ('Const_Troc_BtGravar', '&Gravar'),
    ('Const_Troc_BtCancelar', 'Cancelar'), ('Const_ErrPass_SenhaAtualInvalida',
    'Senha Atual n�o confere!'), ('Const_ErrPass_ErroNovaSenha',
    'Os campos: Nova Senha e Confirma��o devem ser iguais.'),
    ('Const_ErrPass_NovaIgualAtual', 'Nova senha igual a senha atual'),
    ('Const_ErrPass_SenhaObrigatoria', 'A Senha � obrigat�ria'),
    ('Const_ErrPass_SenhaMinima',
    'A senha deve conter no m�nimo %d caracteres'),
    ('Const_ErrPass_SenhaInvalida', 'Proibido utilizar senhas obvias!'),
    ('Const_ErrPass_ForcaTrocaSenha', 'Mudan�a de senha obrigat�ria'),
    ('Const_DefPass_WindowCaption', 'Definir senha do usu�rio : "%s"'),
    ('Const_DefPass_LabelSenha', 'Senha :'), ('Const_TableUsers_FieldUserID',
    'UCIdUser'), ('Const_TableUsers_FieldUserName', 'UCUserName'),
    ('Const_TableUsers_FieldLogin', 'UCLogin'),
    ('Const_TableUsers_FieldPassword', 'UCPassword'),
    ('Const_TableUsers_FieldEmail', 'UCEmail'),
    ('Const_TableUsers_FieldPrivileged', 'UCPrivileged'),
    ('Const_TableUsers_FieldTypeRec', 'UCTypeRec'),
    ('Const_TableUsers_FieldProfile', 'UCProfile'),
    ('Const_TableUsers_FieldKey', 'UCKey'), ('Const_TableUsers_TableName',
    'UCTabUsers'), ('Const_TableUsers_FieldDateExpired', 'UCPASSEXPIRED'),
    ('Const_TableUser_FieldUserExpired', 'UCUserExpired'),
    ('Const_TableUser_FieldUserDaysSun', 'UCUserDaysSun'),
    ('Const_TableUser_FieldUserInative', 'UCInative'),
    ('Const_TableRights_FieldUserID', 'UCIdUser'),
    ('Const_TableRights_FieldModule', 'UCModule'),
    ('Const_TableRights_FieldComponentName', 'UCCompName'),
    ('Const_TableRights_FieldFormName', 'UCFormName'),
    ('Const_TableRights_FieldKey', 'UCKey'), ('Const_TableRights_TableName', 'UCTabRights'),
    ('Const_TableUsersLogged_FieldLogonID', 'UCIdLogon'),
    ('Const_TableUsersLogged_FieldUserID', 'UCIdUser'),
    ('Const_TableUsersLogged_FieldApplicationID', 'UCApplicationId'),
    ('Const_TableUsersLogged_FieldMachineName', 'UCMachineName'),
    ('Const_TableUsersLogged_FieldData', 'UCData'),
    ('Const_TableUsersLogged_TableName', 'UCTabUsersLogged'),
    ('Const_TableUser_FieldUserDepartment','UCUserDepartment'),  // Lotacao - Mauri
    ('Const_TableUser_FieldUserEmpresa','UCUserEmpresa'),  // Empresa - Mauri 26/01/2017
    ('Const_TableUser_FieldUserType','UCUserType'),  // Tipo Usuario  - Mauri
    ('Const_TableDepartment_TableName','UCDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldIDDepartment','UcIDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldNameDepartment','UcNameDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldStatusDepartment','UcStatusDepartment'),  // Lotacao Mauri
    ('Const_TableEmpresa_TableName','UCEmpresa'),  // Empresa Mauri 26/01/2017
    ('Const_TableEmpresa_FieldIDEmpresa','UcIDEmpresa'),  // Empresa Mauri 26/01/2017
    ('Const_TableEmpresa_FieldNameEmpresa','UcNameEmpresa'),  // Empresa Mauri 26/01/2017
    ('Const_Evento_Insert', 'Inserido'),
    ('Const_Evento_Delete', 'Apagado'),
    ('Const_Evento_Edit', 'Editado'),
    ('Const_Evento_NewRecord', 'Novo registro'),
    ('Const_Hist_All', 'Todos'),
    ('Const_Msg_LimpHistorico', 'Excluir todo o conte�do do hist�rico ?'),
    ('Const_Msg_MensConfirma', 'Confirma��o'),
    ('Const_Msg_LogEmptyHistory','Usu�rio %s apagou todo o hist�rico as %s'),
    ('Const_LabelDescricao', 'Hist�rico de tabelas'),
    ('Const_LabelUser', 'Usu�rio'),
    ('Const_LabelForm', 'Formul�rio'),
    ('Const_LabelEvento', 'Evento'),
    ('Const_LabelTabela', 'Tabela'), ('Const_LabelDataEvento', 'Data'),
    ('Const_LabelHoraEvento', 'Hora'), ('Const_Msg_NewRecord',
    '%s inseriu um novo registro'), ('Const_Hist_TableName', 'UCTABHistory'),
    ('Const_Hist_FieldApplicationID', 'ApplicationID'),
    ('Const_Hist_FieldUserID', 'UserID'), ('Const_Hist_FieldEventDate',
    'EventDate'), ('Const_Hist_FieldEventTime', 'EventTime'),
    ('Const_Hist_FieldForm', 'Form'), ('Const_Hist_FieldCaptionForm',
    'FormCaption'), ('Const_Hist_FieldEvent', 'Event'),
    ('Const_Hist_FieldObs', 'Obs'), ('Const_Hist_FieldTableName', 'tName'),
    ('Const_Hist_MsgExceptPropr', 'Favor informar a propriedade %s'),
    ('Const_Hist_BtnFiltro', '&Aplicar Filtro'), ('Const_Hist_BtnExcluir',
    '&Excluir Hist�rico'), ('Const_Hist_BtnFechar', '&Fechar'),

    ('Const_UserLogged_BtnMsg', '&Mensagem'), ('Const_UserLogged_Refresh',
    '&Atualizar'), ('Const_UserLogged_LabelDescricao', 'Usu�rios Logados'),
    ('Const_UserLogged_LabelCaption', 'Usu�rios Logados no sistema'),
    ('Const_CadColuna_Computer', 'Computador'), ('Const_CadColuna_Data',
    'Data'), ('Const_UserLogged_InputCaption', 'Mensagem'),
    ('Const_UserLogged_InputText', 'Digite sua mensagem'),
    ('Const_UserLogged_MsgSystem', 'Mensagem do sistema'),
    ('Const_Men_LoginInativo', 'Aten��o, seu login esta inativo'),
    ('Const_Inc_LabelStatus', 'Status'), ('Const_Inc_StatusActive', 'Ativo'),
    ('Const_Inc_StatusDisabled', 'Inativo')
    );

  { ------------------------------------------------------------------------------ }

Const
  UC_ENGLISH: Array [0 .. MaxArray, 0 .. 1] of string =
    (('Const_Contr_TitleLabel', 'Team of Components of the Form. :'),
    ('Const_Contr_GroupLabel', 'Group:'), ('Const_Contr_CompDispLabel',
    'Available components:'), ('Const_Contr_CompSelLabel',
    'Selected components:'), ('Const_Contr_BtOK', '&OK'),
    ('Const_Contr_BTCancel', '&Cancel'), ('Const_Contr_DescCol', 'Description'),
    ('Const_Contr_BtSellAllHint', 'Select All'), ('Const_Contr_BtSelHint',
    'Select'), ('Const_Contr_BtUnSelHint', 'Unselect'),
    ('Const_Contr_BtUnSelAllHint', 'Unselect All'),
    ('Const_Msgs_BtNew', '&New Message'), ('Const_Msgs_BtReplay', '&Replay'),
    ('Const_Msgs_BtForward', 'F&orward'), ('Const_Msgs_BtDelete', '&Delete'),
    ('Const_Msgs_BtClose', '&Close'), ('Const_Msgs_WindowCaption',
    'Messages of the System'), ('Const_Msgs_ColFrom', 'From'),
    ('Const_Msgs_ColSubject', 'Subject'), ('Const_Msgs_ColDate', 'Date'),
    ('Const_Msgs_PromptDelete',
    'It confirms exclusion of the selected messages?'),
    ('Const_Msgs_PromptDelete_WindowCaption', 'Delete messages'),
    ('Const_Msgs_NoMessagesSelected', 'No Messages selected'),
    ('Const_Msgs_NoMessagesSelected_WindowCaption', 'Information'),
    ('Const_MsgRec_BtClose', '&Close'), ('Const_MsgRec_WindowCaption',
    'Message'), ('Const_MsgRec_Title', 'Received message'),
    ('Const_MsgRec_LabelFrom', 'From:'), ('Const_MsgRec_LabelDate', 'Date'),
    ('Const_MsgRec_LabelSubject', 'Subject'), ('Const_MsgRec_LabelMessage',
    'Message'), ('Const_MsgSend_BtSend', '&Send'), ('Const_MsgSend_BtCancel',
    '&Cancel'), ('Const_MsgSend_WindowCaption', 'Message'),
    ('Const_MsgSend_Title', 'Send New Message'), ('Const_MsgSend_GroupTo',
    'To'), ('Const_MsgSend_RadioUser', 'User:'), ('Const_MsgSend_RadioAll',
    'All'), ('Const_MsgSend_GroupMessage', 'Message'),
    ('Const_MsgSend_LabelSubject', 'Subject'),
    ('Const_MsgSend_LabelMessageText', 'Message text'),
    ('MsgExceptConnection',
    'Done not informed the Connection, Transaction or Database component %s'),
    ('MsgExceptTransaction', 'Done not informed the Transaction component %s'),
    ('MsgExceptDatabase', 'Done not informed the Database do component %s'),
    ('MsgExceptPropriedade', 'Inform the property %s'),
    ('MsgExceptUserMngMenu',
    'Inform in the property UsersForm.MenuItem or UsersForm.Action the item responsible for the users control'),
    ('MsgExceptUserProfile',
    'Inform in the property UsersProfile.MenuItem or UsersProfile.Action the Item  responsible for the control of users Profile '),
    ('MsgExceptChagePassMenu',
    'Inform in the property ChangePasswordForm.MenuItem or .Action the Item that allows to a user to alter his password'),
    ('MsgExceptAppID',
    'In the property "ApplicationID" inform a name to identify the application in the chart of permissions'),
    ('MsgExceptUsersTable',
    'In the property "TableUsers" inform the name of the chart that will be created to store the data of the users '),
    ('MsgExceptRightsTable',
    'In the property "TableRights" inform the name of the chart that will be created to store the permissions of the users '),
    ('MsgExceptConnector', 'The property DataConnector not defined!'),
    ('Const_Men_AutoLogonError', 'Fault of Car Logon !' + #13 + #10 +
    'Inform a valid user and password.'), ('Const_Men_SenhaDesabitada',
    'Retired password of the Login %s'), ('Const_Men_SenhaAlterada',
    'Password altered with success!'), ('Const_Men_MsgInicial',
    'ATTENTION, Inicial Login :' + #13 + #10 + #13 + #10 + 'User: :user' + #13 +
    #10 + 'Password : :password ' + #13 + #10 + #13 + #10 +
    'Define the permissions for this user.'), ('Const_Men_MaxTentativas',
    '%d Attempts of login invalid. By reasons of security the system will be closed.'),
    ('Const_Men_LoginInvalido', 'User invalids or password !'),
    ('Const_Men_UsuarioExiste',
    'The User "%s" is already set up in the system !!'),
    ('Const_Men_PasswordExpired',
    'Attention, his sign died, favor exchanges it '),
    ('Const_Log_BtCancelar', '&Cancel'), ('Const_Log_BtOK', '&OK'),
    ('Const_Log_LabelSenha', 'Password :'), ('Const_Log_LabelUsuario',
    'User :'), ('Const_Log_WindowCaption', 'Login'),
    ('Const_Log_LbEsqueciSenha', 'I forgot the password'),
    ('Const_Log_MsgMailSend', 'The password was sent for his email .'),
    ('Const_Log_LabelTentativa', 'Attempt : '), ('Const_Log_LabelTentativas',
    'Max of Attempts: '), ('Const_LogC_WindowCaption', 'Security'),
    ('Const_LogC_LabelDescricao', 'Log of system'), ('Const_LogC_LabelUsuario',
    'User :'), ('Const_LogC_LabelData', 'Date :'), ('Const_LogC_LabelNivel',
    'Least level:'), ('Const_LogC_ColunaAppID', 'AppID'),
    ('Const_LogC_ColunaNivel', 'Level '), ('Const_LogC_ColunaMensagem',
    'Message'), ('Const_LogC_ColunaUsuario', 'User'),
    ('Const_LogC_ColunaData', 'Date'), ('Const_LogC_BtFiltro', '&Apply Filter'),
    ('Const_LogC_BtExcluir', '&Erase Log'), ('Const_LogC_BtFechar', '&Close'),
    ('Const_LogC_ConfirmaExcluir',
    'It confirms to exclude all the registers of log selected ?'),
    ('Const_LogC_ConfirmaDelete_WindowCaption', 'Delete confirmation'),
    ('Const_LogC_Todos', 'All'), ('Const_LogC_Low', 'Low'),
    ('Const_LogC_Normal', 'Normal'), ('Const_LogC_High', 'High'),
    ('Const_LogC_Critic', 'Critic'), ('Const_LogC_ExcluirEfectuada',
    'Deletion of system log done: User = "%s" | Date = %s a %s | Level <= %s'),
    ('Const_Cad_WindowCaption', 'Security'), ('Const_Cad_LabelDescricao',
    'Users register '), ('Const_Cad_ColunaNome', 'Name'),
    ('Const_Cad_ColunaLogin', 'Login'), ('Const_Cad_ColunaEmail', 'Email'),
    ('Const_Cad_BtAdicionar', '&Add'), ('Const_Cad_BtAlterar', 'A&lter'),
    ('Const_Cad_BtExcluir', '&Erase'), ('Const_Cad_BtPermissoes', 'A&ccesses'),
    ('Const_Cad_BtSenha', '&Password'), ('Const_Cad_BtFechar', '&Close'),
    ('Const_Cad_ConfirmaExcluir', 'Confirm erase the user "%s" ?'),
    ('Const_Cad_ConfirmaDelete_WindowCaption', 'Delete user'),
    ('Const_Prof_WindowCaption', 'Security'), ('Const_Prof_LabelDescricao',
    'Users profile '), ('Const_Prof_ColunaNome', 'Profile'),
    ('Const_Prof_BtAdicionar', '&Add'), ('Const_Prof_BtAlterar', 'A&lter'),
    ('Const_Prof_BtExcluir', '&Delete'), ('Const_Prof_BtPermissoes',
    'A&ccesses'), ('Const_Prof_BtSenha', '&Password'),
    ('Const_Prof_BtFechar', '&Close'), ('Const_Prof_ConfirmaExcluir',
    'There are users with the profile  "%s". Confirm erase ?'),
    ('Const_Prof_ConfirmaDelete_WindowCaption', 'Delete profile'),
    ('Const_Inc_WindowCaption', 'Users register '), ('Const_Inc_LabelAdicionar',
    'Add User'), ('Const_Inc_LabelAlterar', 'Change User'),
    ('Const_Inc_LabelNome', 'Name :'), ('Const_Inc_LabelLogin', 'Login :'),
    ('Const_Inc_LabelEmail', 'Email :'), ('Const_Inc_LabelPerfil', 'Profile :'),
    ('Const_Inc_CheckPrivilegiado', 'Privileged user '),
    ('Const_Inc_BtGravar', '&Save'), ('Const_Inc_BtCancelar', 'Cancel'),
    ('Const_Inc_CheckEspira', 'Password do not expired'),
    ('Const_Inc_Dia', 'Day'), ('Const_Inc_ExpiraEm', 'Expired in'),
    ('Const_PInc_WindowCaption', 'Profile the Users'),
    ('Const_PInc_LabelAdicionar', 'Add Profile'), ('Const_PInc_LabelAlterar',
    'Change Profile '), ('Const_PInc_LabelNome', 'Description :'),
    ('Const_PInc_BtGravar', '&Save'), ('Const_PInc_BtCancelar', 'Cancel'),
    ('Const_Perm_WindowCaption', 'Security'), ('Const_Perm_LabelUsuario',
    'Permissions of the User :'), ('Const_Perm_LabelPerfil',
    'Permissions of the Profile  :'), ('Const_Perm_PageMenu',
    'Items of the Menu'), ('Const_Perm_PageActions', 'Actions'),
    ('Const_Perm_PageControls', 'Controls'), ('Const_Perm_BtLibera',
    '&Release'), ('Const_Perm_BtBloqueia', '&Block'),
    ('Const_Perm_BtGravar', '&Save'), ('Const_Perm_BtCancelar', '&Cancel'),
    ('Const_Troc_WindowCaption', 'Security'), ('Const_Troc_LabelDescricao',
    'Change Password'), ('Const_Troc_LabelSenhaAtual', 'Password :'),
    ('Const_Troc_LabelNovaSenha', 'New Password :'),
    ('Const_Troc_LabelConfirma', 'Confirmation :'),
    ('Const_Troc_BtGravar', '&Save'), ('Const_Troc_BtCancelar', 'Cancel'),
    ('Const_ErrPass_SenhaAtualInvalida', 'Current password does not tally!'),
    ('Const_ErrPass_ErroNovaSenha',
    'The Field: New Password and Confirmation must be the same.'),
    ('Const_ErrPass_NovaIgualAtual', 'New equal password to current password '),
    ('Const_ErrPass_SenhaObrigatoria', 'The password is compulsory '),
    ('Const_ErrPass_SenhaMinima',
    'The password must contain at least %d characters '),
    ('Const_ErrPass_SenhaInvalida',
    'When to use password was prohibited you obviate !'),
    ('Const_ErrPass_ForcaTrocaSenha', 'Compulsory change password'),
    ('Const_DefPass_WindowCaption', 'Define Password of the user  : "%s"'),
    ('Const_DefPass_LabelSenha', 'Password :'), ('Const_TableUsers_FieldUserID',
    'UCIdUser'), ('Const_TableUsers_FieldUserName', 'UCUserName'),
    ('Const_TableUsers_FieldLogin', 'UCLogin'),
    ('Const_TableUsers_FieldPassword', 'UCPassword'),
    ('Const_TableUsers_FieldEmail', 'UCEmail'),
    ('Const_TableUsers_FieldPrivileged', 'UCPrivileged'),
    ('Const_TableUsers_FieldTypeRec', 'UCTypeRec'),
    ('Const_TableUsers_FieldProfile', 'UCProfile'),
    ('Const_TableUsers_FieldKey', 'UCKey'), ('Const_TableUsers_TableName',
    'UCTabUsers'), ('Const_TableUsers_FieldDateExpired', 'UCPassExpired'),
    ('Const_TableUser_FieldUserExpired', 'UCUserExpired'),
    ('Const_TableUser_FieldUserDaysSun', 'UCUserDaysSun'),
    ('Const_TableUser_FieldUserInative', 'UCInative'),
    ('Const_TableRights_FieldUserID', 'UCIdUser'),
    ('Const_TableRights_FieldModule', 'UCModule'),
    ('Const_TableRights_FieldComponentName', 'UCCompName'),
    ('Const_TableRights_FieldFormName', 'UCFormName'),
    ('Const_TableRights_FieldKey', 'UCKey'), ('Const_TableRights_TableName',
    'UCTabRights'), ('Const_TableUsersLogged_FieldLogonID', 'UCIdLogon'),
    ('Const_TableUsersLogged_FieldUserID', 'UCIdUser'),
    ('Const_TableUsersLogged_FieldApplicationID', 'UCApplicationId'),
    ('Const_TableUsersLogged_FieldMachineName', 'UCMachineName'),
    ('Const_TableUsersLogged_FieldData', 'UCData'),
    ('Const_TableUsersLogged_TableName', 'UCTabUsersLogged'),
    ('Const_TableUser_FieldUserDepartment','UCUserDepartment'),  // Lotacao - Mauri
    ('Const_TableUser_FieldUserEmpresa','UCUserEmpresa'),  // Empresa - Mauri 26/01/2017
    ('Const_TableUser_FieldUserType','UCUserType'),  // Tipo Usuario  - Mauri
    ('Const_TableDepartment_TableName','UCDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldIDDepartment','UcIDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldNameDepartment','UcNameDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldStatusDepartment','UcStatusDepartment'),  // Lotacao Mauri
    ('Const_TableEmpresa_TableName','UCEmpresa'),  // Empresa Mauri 26/01/2017
    ('Const_TableEmpresa_FieldIDEmpresa','UcIDEmpresa'),  // Empresa Mauri 26/01/2017
    ('Const_TableEmpresa_FieldNameEmpresa','UcNameEmpresa'),  // Empresa Mauri 26/01/2017
    ('Const_Evento_Insert', 'Insert'), ('Const_Evento_Delete', 'Delete'),
    ('Const_Evento_Edit', 'Edit'), ('Const_Evento_NewRecord', 'New record'),
    ('Const_Hist_All', 'All'), ('Const_Msg_LimpHistorico',
    'Clean table history ?'), ('Const_Msg_MensConfirma', 'Confirm'),
    ('Const_Msg_LogEmptyHistory', 'User %s erase table history in %s'),
    ('Const_LabelDescricao', 'History of Tables'), ('Const_LabelUser', 'User'),
    ('Const_LabelForm', 'Form'), ('Const_LabelEvento', 'Event'),
    ('Const_LabelTabela', 'Table'), ('Const_LabelDataEvento', 'Date'),
    ('Const_LabelHoraEvento', 'Time'), ('Const_Msg_NewRecord',
    '%s insert new record'),

    ('Const_Hist_TableName', 'UCTABHistory'), ('Const_Hist_FieldApplicationID',
    'ApplicationID'), ('Const_Hist_FieldUserID', 'UserID'),
    ('Const_Hist_FieldEventDate', 'EventDate'), ('Const_Hist_FieldEventTime',
    'EventTime'), ('Const_Hist_FieldForm', 'Form'),
    ('Const_Hist_FieldCaptionForm', 'FormCaption'),
    ('Const_Hist_FieldEvent', 'Event'), ('Const_Hist_FieldObs', 'Obs'),
    ('Const_Hist_FieldTableName', 'tName'), ('Const_Hist_MsgExceptPropr',
    'Inform the property %s'), ('Const_Hist_BtnFiltro', '&Apply Filter'),
    ('Const_Hist_BtnExcluir', '&Erase History'), ('Const_Hist_BtnFechar',
    '&Close'),

    ('Const_UserLogged_BtnMsg', '&Message'), ('Const_UserLogged_Refresh',
    '&Refresh'), ('Const_UserLogged_LabelDescricao', 'Active users'),
    ('Const_UserLogged_LabelCaption', 'Active users in the System'),
    ('Const_CadColuna_Computer', 'Computer'), ('Const_CadColuna_Data', 'Date'),
    ('Const_UserLogged_InputCaption', 'Message'), ('Const_UserLogged_InputText',
    'Input your message'), ('Const_UserLogged_MsgSystem', 'Message of system'),
    ('Const_Men_LoginInativo', 'Inactive login'), ('Const_Inc_LabelStatus',
    'Status'), ('Const_Inc_StatusActive', 'Active'),
    ('Const_Inc_StatusDisabled', 'Disabled')

    );

  { ------------------------------------------------------------------------------ }

Const
  UC_SPANISH: Array [0 .. MaxArray, 0 .. 1] of string =
    (('Const_Contr_TitleLabel', 'Seleccionar Componentes del Formulario:'),
    ('Const_Contr_GroupLabel', 'Grupo:'), ('Const_Contr_CompDispLabel',
    'Componentes Disponibles:'), ('Const_Contr_CompSelLabel',
    'Componentes Seleccionados:'), ('Const_Contr_BtOK', '&Aceptar'),
    ('Const_Contr_BTCancel', '&Cancelar'), ('Const_Contr_DescCol',
    'Descripci�n'), ('Const_Contr_BtSellAllHint', 'Seleccionar Todo'),
    ('Const_Contr_BtSelHint', 'Seleccionar'), ('Const_Contr_BtUnSelHint',
    'Deseleccionar'), ('Const_Contr_BtUnSelAllHint', 'Deseleccionar Todo'),
    ('Const_Msgs_BtNew', '&Nuevo Mensaje'), ('Const_Msgs_BtReplay',
    '&Responder'), ('Const_Msgs_BtForward', '&Reenviar'),
    ('Const_Msgs_BtDelete', '&Borrar'), ('Const_Msgs_BtClose', '&Cerrar'),
    ('Const_Msgs_WindowCaption', 'Mensajes de Sistema'),
    ('Const_Msgs_ColFrom', 'Remitente'), ('Const_Msgs_ColSubject', 'Asunto'),
    ('Const_Msgs_ColDate', 'Fecha'), ('Const_Msgs_PromptDelete',
    '�Est� seguro de eliminar los mensajes seleccionados?'),
    ('Const_Msgs_PromptDelete_WindowCaption', 'Eliminar mensajes'),
    ('Const_Msgs_NoMessagesSelected', '!Ning�n mensaje seleccionado�'),
    ('Const_Msgs_NoMessagesSelected_WindowCaption', 'Informaci�n'),
    ('Const_MsgRec_BtClose', '&Cerrar'), ('Const_MsgRec_WindowCaption',
    'Mensaje'), ('Const_MsgRec_Title', 'Mensaje Recibido'),
    ('Const_MsgRec_LabelFrom', 'De:'), ('Const_MsgRec_LabelDate', 'Fecha'),
    ('Const_MsgRec_LabelSubject', 'Asunto'), ('Const_MsgRec_LabelMessage',
    'Mensaje'), ('Const_MsgSend_BtSend', '&Enviar'), ('Const_MsgSend_BtCancel',
    '&Cancelar'), ('Const_MsgSend_WindowCaption', 'Mensaje'),
    ('Const_MsgSend_Title', 'Enviar Nuevo Mensaje'),
    ('Const_MsgSend_GroupTo', 'Para:'), ('Const_MsgSend_RadioUser', 'Usuario:'),
    ('Const_MsgSend_RadioAll', 'Todos'), ('Const_MsgSend_GroupMessage',
    'Mensaje'), ('Const_MsgSend_LabelSubject', 'Asunto'),
    ('Const_MsgSend_LabelMessageText', 'Texto del mensaje'),
    ('MsgExceptConnection',
    '�Valor No V�lido para la propiedad Connection del componente %s!'),
    ('MsgExceptTransaction',
    '�Valor No V�lido para la propiedad Transaction del componente %s!'),
    ('MsgExceptDatabase',
    '�Valor No V�lido para la propiedad Database del componente %s!'),
    ('MsgExceptPropriedade', 'Favor informar a propriedade %s'),
    ('MsgExceptUserMngMenu',
    'Ingrese en la propiedad UsersForm.MenuItem o UsersForm.Action la opci�n del menu para abrir el Control de Usuarios'),
    ('MsgExceptUserProfile',
    'Ingrese en la propiedad UsersProfile.MenuItem o UsersProfile.Action la opci�n del menu para abrir el Perfil de Usuarios'),
    ('MsgExceptChagePassMenu',
    'Ingrese en la propiedad ChangePasswordForm.MenuItem o .Action la opci�n del men� que permite al usuario cambiar su Contrase�a'),
    ('MsgExceptAppID',
    'La propiedad ApplicationID requiere el nombre v�lido de una tabla para el registro de los Permisos de Usuario'),
    ('MsgExceptUsersTable',
    'La propiedad UserTable requiere el nombre v�lido de una tabla para registrar/seleccionar los datos de los usuarios'),
    ('MsgExceptRightsTable',
    'La propiead RightTable requiere el nombre v�lido de una tabla para registrar/seleccionar los permisos de los usuarios'),
    ('MsgExceptConnector', 'Propriedade DataConnector n�o definida!'),
    ('Const_Men_AutoLogonError', 'Error de Ingreso Autom�tico!' + #13 + #10 +
    'Especifique un Usuario y Contrase�a V�lidos.'),
    ('Const_Men_SenhaDesabitada', 'Contrase�a vac�a para el Usuario %s'),
    ('Const_Men_SenhaAlterada', '�Se ha cambiado la Contrase�a con �xito!'),
    ('Const_Men_MsgInicial', 'ATENCION! Conecci�n Inicial:' + #13 + #10 + #13 +
    #10 + 'Usuario : :user' + #13 + #10 + 'Contrase�a : :password' + #13 + #10 +
    #13 + #10 + 'Defina permisos para este usuario'),
    ('Const_Men_MaxTentativas', '%d Intentos de conecci�n inv�lidos !'),
    ('Const_Men_LoginInvalido', 'Usuario y/o Contrase�a Incorrectos!'),
    ('Const_Men_UsuarioExiste',
    'O Usu�rio "%s" j� est� cadastrado no sistema !!'),
    ('Const_Men_PasswordExpired', 'Aten��o, sua senha expirou, favor troca-la'),
    ('Const_Log_BtCancelar', '&Cancelar'), ('Const_Log_BtOK', 'Aceptar'),
    ('Const_Log_LabelSenha', 'Contrase�a:'), ('Const_Log_LabelUsuario',
    'Usuario: '), ('Const_Log_WindowCaption', 'Conecci�n'),
    ('Const_Log_LbEsqueciSenha', 'Olvid� mi Contrase�a'),
    ('Const_Log_MsgMailSend', 'La contrase�a fue enviada a su correo.'),
    ('Const_Log_LabelTentativa', 'Tentativa : '), ('Const_Log_LabelTentativas',
    'M�ximo de Tentativas : '), ('Const_LogC_WindowCaption', 'Seguridad'),
    ('Const_LogC_LabelDescricao', 'Visor de Eventos'),
    ('Const_LogC_LabelUsuario', 'Usuario:'), ('Const_LogC_ColunaAppID',
    'AppID'), ('Const_LogC_LabelData', 'Fecha:'), ('Const_LogC_LabelNivel',
    'Nivel M�nimo: '), ('Const_LogC_ColunaNivel', 'Nivel'),
    ('Const_LogC_ColunaMensagem', 'Mensaje'), ('Const_LogC_ColunaUsuario',
    'Usuario'), ('Const_LogC_ColunaData', 'Fecha'),
    ('Const_LogC_BtFiltro', '&Aplicar Filtro'), ('Const_LogC_BtExcluir',
    '&Borrar Bit�cora'), ('Const_LogC_BtFechar', '&Cerrar'),
    ('Const_LogC_ConfirmaExcluir',
    '�Est� seguro de Eliminar todos todos los registros de Bit�cora seleccionados?'),
    ('Const_LogC_ConfirmaDelete_WindowCaption', 'Confirmaci�n'),
    ('Const_LogC_Todos', 'Todos'), ('Const_LogC_Low', 'Bajo'),
    ('Const_LogC_Normal', 'Normal'), ('Const_LogC_High', 'Alto'),
    ('Const_LogC_Critic', 'Cr�tico'), ('Const_LogC_ExcluirEfectuada',
    'Borrado de registros de bit�cora realizado: Usuario = "%s" | Fecha = %s a %s | Nivel <= %s'),
    ('Const_Cad_WindowCaption', 'Seguridad'), ('Const_Cad_LabelDescricao',
    'Administraci�n de Usuarios'), ('Const_Cad_ColunaNome', 'Nombre'),
    ('Const_Cad_ColunaLogin', 'Usuario'), ('Const_Cad_ColunaEmail', 'Correo'),
    ('Const_Cad_BtAdicionar', '&Nuevo'), ('Const_Cad_BtAlterar', '&Editar'),
    ('Const_Cad_BtExcluir', 'E&liminar'), ('Const_Cad_BtPermissoes',
    '&Accesos'), ('Const_Cad_BtSenha', 'C&ontrase�a'),
    ('Const_Cad_BtFechar', '&Cerrar'), ('Const_Cad_ConfirmaExcluir',
    '�Est� seguro de Eliminar al Usuario "%s"?'),
    ('Const_Cad_ConfirmaDelete_WindowCaption', 'Eliminar usuario'),
    ('Const_Prof_WindowCaption', 'Seguridad'), ('Const_Prof_LabelDescricao',
    'Perfil de Usuario'), ('Const_Prof_ColunaNome', 'Perfil'),
    ('Const_Prof_BtAdicionar', '&Nuevo'), ('Const_Prof_BtAlterar', '&Editar'),
    ('Const_Prof_BtExcluir', 'E&liminar'), ('Const_Prof_BtPermissoes',
    '&Accesos'), ('Const_Prof_BtSenha', 'C&ontrase�a'),
    ('Const_Prof_BtFechar', '&Cerrar'), ('Const_Prof_ConfirmaExcluir',
    'Existe(n) usuario(s) con el Perfil "%s". �Est� seguro de eliminar el perfil?'),
    ('Const_Prof_ConfirmaDelete_WindowCaption', 'Eliminar perfil'),
    ('Const_Inc_WindowCaption', 'Administraci�n de Usuarios'),
    ('Const_Inc_LabelAdicionar', 'Nuevo Usuario'), ('Const_Inc_LabelAlterar',
    'Editar Usuario'), ('Const_Inc_LabelNome', 'Nombre:'),
    ('Const_Inc_LabelLogin', 'Usuario: '), ('Const_Inc_LabelEmail', 'Correo: '),
    ('Const_Inc_LabelPerfil', 'Perfil: '), ('Const_Inc_CheckPrivilegiado',
    'Usuario Privilegiado'), ('Const_Inc_BtGravar', '&Guardar'),
    ('Const_Inc_BtCancelar', 'Cancelar'), ('Const_Inc_CheckEspira',
    'Senha do usu�rio n�o expira'), ('Const_Inc_Dia', 'Dias'),
    ('Const_Inc_ExpiraEm', 'Expira em'), ('Const_PInc_WindowCaption',
    'Perfiles de Usuarios'), ('Const_PInc_LabelAdicionar', 'Nuevo Perfil'),
    ('Const_PInc_LabelAlterar', 'Editar Perfil'),
    ('Const_PInc_LabelNome', 'Descripci�n: '), ('Const_PInc_BtGravar',
    '&Guardar'), ('Const_PInc_BtCancelar', 'Cancelar'),
    ('Const_Perm_WindowCaption', 'Seguridad'), ('Const_Perm_LabelUsuario',
    'Permisos de Usuario : '), ('Const_Perm_LabelPerfil',
    'Permisos del Perfil : '), ('Const_Perm_PageMenu', 'Elementos del Men�'),
    ('Const_Perm_PageActions', 'Acciones'), ('Const_Perm_PageControls',
    'Controles'), ('Const_Perm_BtLibera', '&Permitir'),
    ('Const_Perm_BtBloqueia', '&Bloquear'), ('Const_Perm_BtGravar', '&Guardar'),
    ('Const_Perm_BtCancelar', '&Cancelar'), ('Const_Troc_WindowCaption',
    'Seguridad'), ('Const_Troc_LabelDescricao', 'Cambiar Contrase�a'),
    ('Const_Troc_LabelSenhaAtual', 'Contrase�a Actual:'),
    ('Const_Troc_LabelNovaSenha', 'Nueva Contrase�a:'),
    ('Const_Troc_LabelConfirma', 'Confirme Contrase�a:'),
    ('Const_Troc_BtGravar', '&Guardar'), ('Const_Troc_BtCancelar', 'Cancelar'),
    ('Const_ErrPass_SenhaAtualInvalida', '�Contrase�a Actual Incorrecta!'),
    ('Const_ErrPass_ErroNovaSenha',
    'Los campos Contrase�a Nueva y Confirme Contrase�a deben ser iguales'),
    ('Const_ErrPass_NovaIgualAtual',
    'Nueva Contrase�a y Contrase�a Actual deben ser diferentes'),
    ('Const_ErrPass_SenhaObrigatoria', '�La Contrase�a es obligatoria!'),
    ('Const_ErrPass_SenhaMinima',
    'La Contrase�a debe tener un m�nimo de %d caracteres'),
    ('Const_ErrPass_SenhaInvalida',
    '�Prohibido utilizar contrase�as NO Seguras!'),
    ('Const_ErrPass_ForcaTrocaSenha', 'Mudan�a de senha obrigat�ria'),
    ('Const_DefPass_WindowCaption', 'Ingrese Contrase�a de Usuario: "%s"'),
    ('Const_DefPass_LabelSenha', 'Contrase�a: '),
    ('Const_TableUsers_FieldUserID', 'UCIdUser'),
    ('Const_TableUsers_FieldUserName', 'UCUserName'),
    ('Const_TableUsers_FieldLogin', 'UCLogin'),
    ('Const_TableUsers_FieldPassword', 'UCPassword'),
    ('Const_TableUsers_FieldEmail', 'UCEmail'),
    ('Const_TableUsers_FieldPrivileged', 'UCPrivileged'),
    ('Const_TableUsers_FieldTypeRec', 'UCTypeRec'),
    ('Const_TableUsers_FieldProfile', 'UCProfile'),
    ('Const_TableUsers_FieldKey', 'UCKey'), ('Const_TableUsers_TableName',
    'UCTabUsers'), ('Const_TableUsers_FieldDateExpired', 'UCPASSEXPIRED'),
    ('Const_TableUser_FieldUserExpired', 'UCUserExpired'),
    ('Const_TableUser_FieldUserDaysSun', 'UCUserDaysSun'),
    ('Const_TableUser_FieldUserInative', 'UCInative'),
    ('Const_TableUser_FieldUserDepartment','UCUserDepartment'),  // Lotacao - Mauri
    ('Const_TableUser_FieldUserEmpresa','UCUserEmpresa'),  // Empresa - Mauri 26/01/2017

    ('Const_TableUser_FieldUserType','UCUserType'),  // Tipo Usuario - Mauri
    ('Const_TableDepartment_TableName','UCDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldIDDepartment','UcIDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldNameDepartment','UcNameDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldStatusDepartment','UcStatusDepartment'),  // Lotacao Mauri
    ('Const_TableEmpresa_TableName','UCEmpresa'),  //  Empresa Mauri 26/01/2017
    ('Const_TableEmpresa_FieldIDEmpresa','UcIDEmpresa'),  // Empresa Mauri 26/01/2017
    ('Const_TableEmpresa_FieldNameEmpresa','UcNameEmpresa'),  // Empresa Mauri 26/01/2017

    ('Const_TableRights_FieldUserID', 'UCIdUser'),
    ('Const_TableRights_FieldModule', 'UCModule'),
    ('Const_TableRights_FieldComponentName', 'UCCompName'),
    ('Const_TableRights_FieldFormName', 'UCFormName'),
    ('Const_TableRights_FieldKey', 'UCKey'), ('Const_TableRights_TableName',
    'UCTabRights'), ('Const_TableUsersLogged_FieldLogonID', 'UCIdLogon'),
    ('Const_TableUsersLogged_FieldUserID', 'UCIdUser'),
    ('Const_TableUsersLogged_FieldApplicationID', 'UCApplicationId'),
    ('Const_TableUsersLogged_FieldMachineName', 'UCMachineName'),
    ('Const_TableUsersLogged_FieldData', 'UCData'),
    ('Const_TableUsersLogged_TableName', 'UCTabUsersLogged'),
    ('Const_Evento_Insert', 'Nuevo'), ('Const_Evento_Delete', 'Eliminar'),
    ('Const_Evento_Edit', 'Editar'), ('Const_Evento_NewRecord',
    'Nuevo registro'), ('Const_Hist_All', 'Todos'), ('Const_Msg_LimpHistorico',
    'Excluir el contenido entero del hist�rico ?'), ('Const_Msg_MensConfirma',
    'Confirme'), ('Const_Msg_LogEmptyHistory',
    'El usuario %s borra la historia de mesa en %s '),
    ('Const_LabelDescricao', 'Hist�rico de tabelas'),
    ('Const_LabelUser', 'Usuario'), ('Const_LabelForm', 'Formul�rio'),
    ('Const_LabelEvento', 'Evento'), ('Const_LabelTabela', 'Tabela'),
    ('Const_LabelDataEvento', 'Data'), ('Const_LabelHoraEvento', 'Hora'),
    ('Const_Msg_NewRecord', '%s Inserte el nuevo registro'),
    ('Const_Hist_TableName', 'UCTABHistory'), ('Const_Hist_FieldApplicationID',
    'ApplicationID'), ('Const_Hist_FieldUserID', 'UserID'),
    ('Const_Hist_FieldEventDate', 'EventDate'), ('Const_Hist_FieldEventTime',
    'EventTime'), ('Const_Hist_FieldForm', 'Form'),
    ('Const_Hist_FieldCaptionForm', 'FormCaption'),
    ('Const_Hist_FieldEvent', 'Event'), ('Const_Hist_FieldObs', 'Obs'),
    ('Const_Hist_FieldTableName', 'tName'), ('Const_Hist_MsgExceptPropr',
    'Por favor informe la propiedad %s'), ('Const_Hist_BtnFiltro',
    '&Aplicar Filtro'), ('Const_Hist_BtnExcluir', '&Borrar Hist�rico'),
    ('Const_Hist_BtnFechar', '&Cerrar'),

    ('Const_UserLogged_BtnMsg', '&Mensaje'), ('Const_UserLogged_Refresh',
    '&Restaure'), ('Const_UserLogged_LabelDescricao', 'Usuarios activos'),
    ('Const_UserLogged_LabelCaption', 'Usuarios activos en el sistema'),
    ('Const_CadColuna_Computer', 'Computadora'), ('Const_CadColuna_Data',
    'Data'), ('Const_UserLogged_InputCaption', 'Mensaje'),
    ('Const_UserLogged_InputText', 'Entre su mensaje'),
    ('Const_UserLogged_MsgSystem', 'Mensaje del sistema'),
    ('Const_Men_LoginInativo', 'Aten��o, seu login esta inativo'),
    ('Const_Inc_LabelStatus', 'Status'), ('Const_Inc_StatusActive', 'Ativo'),
    ('Const_Inc_StatusDisabled', 'Inativo')

    );

  { ------------------------------------------------------------------------------ }

Const
  UC_FRENCH: Array [0 .. MaxArray, 0 .. 1] of string =
    (('Const_Contr_TitleLabel', 'L''�quipe de Composantes de la Forme  :'),
    ('Const_Contr_GroupLabel', 'Groupe :'), ('Const_Contr_CompDispLabel',
    'Composantes disponibles :'), ('Const_Contr_CompSelLabel',
    'Composantes choisies:'), ('Const_Contr_BtOK', '&OK'),
    ('Const_Contr_BTCancel', '&Annuler'), ('Const_Contr_DescCol',
    'Description'), // nao
    ('Const_Contr_BtSellAllHint', 'Choisissez Tous'),
    ('Const_Contr_BtSelHint', 'Choisir'), ('Const_Contr_BtUnSelHint',
    'No Choisir'), ('Const_Contr_BtUnSelAllHint', 'Non choisissez Tous'),
    ('Const_Msgs_BtNew', '&Nouveau Message'), ('Const_Msgs_BtReplay', '&Rejeu'),
    ('Const_Msgs_BtForward', 'Env&oyer'), ('Const_Msgs_BtDelete', '&Effacer'),
    ('Const_Msgs_BtClose', '&Pr�s'), ('Const_Msgs_WindowCaption',
    'Messages du Syst�me'), ('Const_Msgs_ColFrom', 'De'),
    ('Const_Msgs_ColSubject', 'Sujet'), ('Const_Msgs_ColDate', 'Date'), // nao
    ('Const_Msgs_PromptDelete',
    'Il confirme l''exclusion des messages choisis ?'),
    ('Const_Msgs_PromptDelete_WindowCaption', 'Effacez des messages'),
    ('Const_Msgs_NoMessagesSelected', 'Aucun Message choisi'),
    ('Const_Msgs_NoMessagesSelected_WindowCaption', 'Renseignements'),
    ('Const_MsgRec_BtClose', '&Pr�s'), ('Const_MsgRec_WindowCaption',
    'Message'), ('Const_MsgRec_Title', 'Message re�u'),
    ('Const_MsgRec_LabelFrom', 'De:'), ('Const_MsgRec_LabelDate', 'Date'),
    ('Const_MsgRec_LabelSubject', 'Sujet'), ('Const_MsgRec_LabelMessage',
    'Message'), ('Const_MsgSend_BtSend', '&Envoyer'),
    ('Const_MsgSend_BtCancel', '&Annuler'), ('Const_MsgSend_WindowCaption',
    'Message'), ('Const_MsgSend_Title', 'Envoyez le Nouveau Message'),
    ('Const_MsgSend_GroupTo', '�'), ('Const_MsgSend_RadioUser', 'Utilisateur:'),
    ('Const_MsgSend_RadioAll', 'Tous'), ('Const_MsgSend_GroupMessage',
    'Message'), ('Const_MsgSend_LabelSubject', 'Sujet'),
    ('Const_MsgSend_LabelMessageText', 'Texte de message'),
    ('MsgExceptConnection',
    'Fait non inform� la Connexion, la Transaction ou la composante de Base de donn�es %s'),
    ('MsgExceptTransaction',
    'Fait non inform� la composante Transactionnelle  %s'),
    ('MsgExceptDatabase',
    'Fait non inform� la Base de donn�es font la composante %s'),
    ('MsgExceptPropriedade', 'Informez la propri�t� %s'),
    ('MsgExceptUserMngMenu',
    'Informez la propri�t� UsersForm.MenuItem ou UsersForm.Action l''article responsable du contr�le d''utilisateurs '),
    ('MsgExceptUserProfile',
    'Informez la propri�t� UsersProfile.MenuItem ou UsersProfile.Action l''Article responsable du contr�le de Profil d''utilisateurs'),
    ('MsgExceptChagePassMenu',
    'Informez la propri�t� ChangePasswordForm.MenuItem ou .Action l''Article qui permet � un utilisateur de changer son mot de passe'),
    ('MsgExceptAppID',
    'Dans la propri�t� "ApplicationID" informent un nom pour identifier l''application dans le graphique de permissions'),
    ('MsgExceptUsersTable',
    'Dans la propri�t� "TableUsers" informent le nom du graphique qui sera cr�� pour conserver les donn�es des utilisateurs'),
    ('MsgExceptRightsTable',
    'Dans la propri�t� "TableRights" informent le nom du graphique qui sera cr�� pour conserver les permissions des utilisateurs'),
    ('MsgExceptConnector', 'La propri�t� "DataConnector" non d�fini!'),
    ('Const_Men_AutoLogonError', 'Faute de D�but de transaction Automobile!' +
    #13 + #10 + 'Informez un utilisateur valide et un mot de passe.'),
    ('Const_Men_SenhaDesabitada',
    'Mot de passe retrait� de l''Ouverture de session %s'),
    ('Const_Men_SenhaAlterada', 'Le mot de passe changeait avec le succ�s!'),
    ('Const_Men_MsgInicial', 'ATTENTION, Ouverture de session d''Inicial :' +
    #13 + #10 + #13 + #10 + 'Utilisateur : :user + '#13 + #10 +
    'Mot de passe : :password ' + #13 + #10 + #13 + #10 +
    'D�finissez les permissions pour cet utilisateur.'),
    ('Const_Men_MaxTentativas',
    '%d Essais d''infirme d''ouverture de session. Par les raisons de s�curit� le syst�me sera ferm�.'),
    ('Const_Men_LoginInvalido',
    'L''utilisateur devient infirme ou le mot de passe!'),
    ('Const_Men_UsuarioExiste',
    'L''Utilisateur "%s" est d�j� remis sur pied dans le syst�me !!'),
    ('Const_Men_PasswordExpired',
    'L''attention, son signe est mort, la faveur l''�change  '),
    ('Const_Log_BtCancelar', '&Annuler'), ('Const_Log_BtOK', '&OK'),
    ('Const_Log_LabelSenha', 'Passe :'), ('Const_Log_LabelUsuario',
    'Utilisateur :'), ('Const_Log_WindowCaption', 'Ouverture de session'),
    ('Const_Log_LbEsqueciSenha', 'J''ai oubli� le mot de passe'),
    ('Const_Log_MsgMailSend',
    'le mot de passe a �t� envoy� pour son courrier �lectronique .'),
    ('Const_Log_LabelTentativa', 'Essais : '), ('Const_Log_LabelTentativas',
    'Max d''Essais : '), ('Const_LogC_WindowCaption', 's�curit�'),
    ('Const_LogC_LabelDescricao', 'Rondin de syst�me'),
    ('Const_LogC_LabelUsuario', 'Utilisateur :'),
    ('Const_LogC_LabelData', 'Date :'), ('Const_LogC_LabelNivel',
    'La moindre partie de niveau:'), ('Const_LogC_ColunaAppID', 'AppID'),
    ('Const_LogC_ColunaNivel', 'Niveau '), ('Const_LogC_ColunaMensagem',
    'Message'), ('Const_LogC_ColunaUsuario', 'Utilisateur'),
    ('Const_LogC_ColunaData', 'Date'), ('Const_LogC_BtFiltro',
    '&Appliquez le Filtre'), ('Const_LogC_BtExcluir', '&Effacez le Rondin'),
    ('Const_LogC_BtFechar', '&Pr�s'), ('Const_LogC_ConfirmaExcluir',
    'Il confirme pour exclure tous les registres de rondin choisi?'),
    ('Const_LogC_ConfirmaDelete_WindowCaption', 'Effacez la confirmation '),
    ('Const_LogC_Todos', 'Tous'), ('Const_LogC_Low', 'Bas'),
    ('Const_LogC_Normal', 'Normal'), ('Const_LogC_High', 'Haut'),
    ('Const_LogC_Critic', 'Critique'), ('Const_LogC_ExcluirEfectuada',
    'L''effacement de rondin de syst�me fait : Utilisateur = "%s" | Date = %s a %s | Niveau <= %s'),
    ('Const_Cad_WindowCaption', 'S�curit�'), ('Const_Cad_LabelDescricao',
    'Les utilisateurs s''inscrivent'), ('Const_Cad_ColunaNome', 'Nom'),
    ('Const_Cad_ColunaLogin', 'Utilisateur'), ('Const_Cad_ColunaEmail',
    'Courrier �lectronique'), ('Const_Cad_BtAdicionar', '&Ajouter'),
    ('Const_Cad_BtAlterar', 'Ch&anger'), ('Const_Cad_BtExcluir', '&Effacer'),
    ('Const_Cad_BtPermissoes', 'A&pproches'), ('Const_Cad_BtSenha', '&Passe'),
    ('Const_Cad_BtFechar', '&Pr�s'), ('Const_Cad_ConfirmaExcluir',
    'Confirmez effacent l''utilisateur "%s" ?'),
    ('Const_Cad_ConfirmaDelete_WindowCaption', 'Effacez l''utilisateur '),
    ('Const_Prof_WindowCaption', 'S�curit�'), ('Const_Prof_LabelDescricao',
    'Les utilisateurs dressent le portrait'), ('Const_Prof_ColunaNome',
    'Profile'), ('Const_Prof_BtAdicionar', '&Ajouter'),
    ('Const_Prof_BtAlterar', '&Ajouter'), ('Const_Prof_BtExcluir', '&Effacer'),
    ('Const_Prof_BtPermissoes', 'A&pproches'), ('Const_Prof_BtSenha', '&Passe'),
    ('Const_Prof_BtFechar', '&Pr�s'), ('Const_Prof_ConfirmaExcluir',
    'Il y a des utilisateurs avec le profil "%s". Confirmez effacent ?'),
    ('Const_Prof_ConfirmaDelete_WindowCaption', 'Effacez le profil'),
    ('Const_Inc_WindowCaption', 'Les utilisateurs s''inscrivent '),
    ('Const_Inc_LabelAdicionar', 'Ajouter utilisateurs'),
    ('Const_Inc_LabelAlterar', 'Changer utilisateurs'),
    ('Const_Inc_LabelNome', 'Nom :'), ('Const_Inc_LabelLogin', 'Login :'),
    ('Const_Inc_LabelEmail', 'Courrier �lectronique :'),
    ('Const_Inc_LabelPerfil', 'Profil :'), ('Const_Inc_CheckPrivilegiado',
    'Utilisateur privil�gi�'), ('Const_Inc_BtGravar', '&Sauver'),
    ('Const_Inc_BtCancelar', 'Annuler'), ('Const_Inc_CheckEspira',
    'Le mot de passe ne fait pas expir�'), ('Const_Inc_Dia', 'Jour'),
    ('Const_Inc_ExpiraEm', 'Expir� dans'), ('Const_PInc_WindowCaption',
    'Dressez le portrait des Utilisateurs'), ('Const_PInc_LabelAdicionar',
    'Ajouter Profil'), ('Const_PInc_LabelAlterar', 'Ajouter Profil '),
    ('Const_PInc_LabelNome', 'Description :'), ('Const_PInc_BtGravar',
    '&Sauver'), ('Const_PInc_BtCancelar', 'Annuler'),
    ('Const_Perm_WindowCaption', 'S�curit�'), ('Const_Perm_LabelUsuario',
    'Permissions de l''Utilisateur :'), ('Const_Perm_LabelPerfil',
    'Permissions de l'' Profil :'), ('Const_Perm_PageMenu', 'Articles du Menu'),
    ('Const_Perm_PageActions', 'Actions'), ('Const_Perm_PageControls',
    'Controls'), ('Const_Perm_BtLibera', '&Lib�ration'),
    ('Const_Perm_BtBloqueia', '&Bloc'), ('Const_Perm_BtGravar', '&Sauver'),
    ('Const_Perm_BtCancelar', '&Annuler'), ('Const_Troc_WindowCaption',
    'S�curit�'), ('Const_Troc_LabelDescricao', 'Mot de passe de Changement '),
    ('Const_Troc_LabelSenhaAtual', 'Passe :'), ('Const_Troc_LabelNovaSenha',
    'Nouveau Mot de passe :'), ('Const_Troc_LabelConfirma', 'Confirmation :'),
    ('Const_Troc_BtGravar', '&Sauver'), ('Const_Troc_BtCancelar', 'Annuler'),
    ('Const_ErrPass_SenhaAtualInvalida',
    'Le mot de passe actuel ne correspond pas !'),
    ('Const_ErrPass_ErroNovaSenha',
    'Le Champ : le Nouveau Mot de passe et la Confirmation doivent �tre le m�me.'),
    ('Const_ErrPass_NovaIgualAtual',
    'Nouveau mot de passe �gal au mot de passe actuel'),
    ('Const_ErrPass_SenhaObrigatoria', 'Le mot de passe est obligatoire'),
    ('Const_ErrPass_SenhaMinima',
    'Le mot de passe doit contenir au moins %d des caract�res '),
    ('Const_ErrPass_SenhaInvalida',
    'Quand utiliser le mot de passe a �t� interdit vous obviez !'),
    ('Const_ErrPass_ForcaTrocaSenha', 'Mot de passe de changement obligatoire'),
    ('Const_DefPass_WindowCaption',
    'D�finissez le Mot de passe de l''utilisateur : "%s"'),
    ('Const_DefPass_LabelSenha', 'Passe :'), ('Const_TableUsers_FieldUserID',
    'UCIdUser'), ('Const_TableUsers_FieldUserName', 'UCUserName'),
    ('Const_TableUsers_FieldLogin', 'UCLogin'),
    ('Const_TableUsers_FieldPassword', 'UCPassword'),
    ('Const_TableUsers_FieldEmail', 'UCEmail'),
    ('Const_TableUsers_FieldPrivileged', 'UCPrivileged'),
    ('Const_TableUsers_FieldTypeRec', 'UCTypeRec'),
    ('Const_TableUsers_FieldProfile', 'UCProfile'),
    ('Const_TableUsers_FieldKey', 'UCKey'), ('Const_TableUsers_TableName',
    'UCTabUsers'), ('Const_TableUsers_FieldDateExpired', 'UCPassExpired'),
    ('Const_TableUser_FieldUserExpired', 'UCUserExpired'),
    ('Const_TableUser_FieldUserDaysSun', 'UCUserDaysSun'),
    ('Const_TableUser_FieldUserInative', 'UCInative'),
    ('Const_TableRights_FieldUserID', 'UCIdUser'),
    ('Const_TableRights_FieldModule', 'UCModule'),
    ('Const_TableRights_FieldComponentName', 'UCCompName'),
    ('Const_TableRights_FieldFormName', 'UCFormName'),
    ('Const_TableRights_FieldKey', 'UCKey'), ('Const_TableRights_TableName',
    'UCTabRights'), ('Const_TableUsersLogged_FieldLogonID', 'UCIdLogon'),
    ('Const_TableUsersLogged_FieldUserID', 'UCIdUser'),
    ('Const_TableUsersLogged_FieldApplicationID', 'UCApplicationId'),
    ('Const_TableUsersLogged_FieldMachineName', 'UCMachineName'),
    ('Const_TableUsersLogged_FieldData', 'UCData'),
    ('Const_TableUsersLogged_TableName', 'UCTabUsersLogged'),
    ('Const_TableUser_FieldUserType','UCUserType'),  // Tipo Usuario - Mauri
    ('Const_TableUser_FieldUserDepartment','UCTabUserDepartment'),  // Lotacao - Mauri
    ('Const_TableUser_FieldUserEmpresa','UCTabUserEmpresa'),  // Empresa Mauri 26/01/2017
    ('Const_TableDepartment_TableName','UCDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldIDDepartment','UcIDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldNameDepartment','UcNameDepartment'),  // Lotacao Mauri
    ('Const_TableDepartment_FieldStatusDepartment','UcStatusDepartment'),  // Lotacao Mauri
    ('Const_TableEmpresa_TableName','UCEmpresa'),  // Empresa Mauri 26/01/2017
    ('Const_TableEmpresa_FieldIDEmpresa','UcIDEmpresa'),  // Empresa Mauri 26/01/2017
    ('Const_TableEmpresa_FieldNameEmpresa','UcNameDepartment'),  // Empresa Mauri 26/01/2017

    ('Const_Evento_Insert', 'Insertion'), ('Const_Evento_Delete', 'Effacer'),
    ('Const_Evento_Edit', 'R�viser'), ('Const_Evento_NewRecord',
    'Nouveau record '), ('Const_Hist_All', 'Tous'), ('Const_Msg_LimpHistorico',
    'Histoire de table propre?'), ('Const_Msg_MensConfirma', 'Confirmer'),
    ('Const_Msg_LogEmptyHistory',
    'L''utilisateur %s efface l''histoire de table dans %s '),
    ('Const_LabelDescricao', 'Histoire de Tables'),
    ('Const_LabelUser', 'Utilisateur'), ('Const_LabelForm', 'Forme'),
    ('Const_LabelEvento', '�v�nement'), ('Const_LabelTabela', 'Table'),
    ('Const_LabelDataEvento', 'Date'), ('Const_LabelHoraEvento', 'Temps'),
    ('Const_Msg_NewRecord', '%s ins�rez le nouveau record'),
    ('Const_Hist_TableName', 'UCTABHistory'), ('Const_Hist_FieldApplicationID',
    'ApplicationID'), ('Const_Hist_FieldUserID', 'UserID'),
    ('Const_Hist_FieldEventDate', 'EventDate'), ('Const_Hist_FieldEventTime',
    'EventTime'), ('Const_Hist_FieldForm', 'Form'),
    ('Const_Hist_FieldCaptionForm', 'FormCaption'),
    ('Const_Hist_FieldEvent', 'Event'), ('Const_Hist_FieldObs', 'Obs'),
    ('Const_Hist_FieldTableName', 'tName'), ('Const_Hist_MsgExceptPropr',
    'Informez la propri�t� %s'), ('Const_Hist_BtnFiltro',
    '&Appliquez le Filtre'), ('Const_Hist_BtnExcluir', '&Effacez l''Histoire'),
    ('Const_Hist_BtnFechar', '&Pr�s'),

    ('Const_UserLogged_BtnMsg', '&R�g�n�rez'), ('Const_UserLogged_Refresh',
    '&Refresh'), ('Const_UserLogged_LabelDescricao', 'Utilisateurs actifs'),
    ('Const_UserLogged_LabelCaption', 'Utilisateurs actifs dans le syst�me'),
    ('Const_CadColuna_Computer', 'Ordinateur'), ('Const_CadColuna_Data',
    'Date'), ('Const_UserLogged_InputCaption', 'Message'),
    ('Const_UserLogged_InputText', 'Entrez votre message'),
    ('Const_UserLogged_MsgSystem', 'Message de syst�me'),
    ('Const_Men_LoginInativo', 'Aten��o, seu login esta inativo'),
    ('Const_Inc_LabelStatus', 'Status'), ('Const_Inc_StatusActive', 'Ativo'),
    ('Const_Inc_StatusDisabled', 'Inativo'));

Function RetornaLingua(Lingua: TUCLanguage; Prop: String): String;

implementation

Function RetornaLingua(Lingua: TUCLanguage; Prop: String): String;
Var
  I: Integer;
Begin
  Result := '';
  For I := 0 to MaxArray do
  Begin
    Case Lingua of
      ucPortuguesBr:
        If UpperCase(UC_PTBR[I, 0]) = UpperCase(Prop) then
          Result := UC_PTBR[I, 1];
      ucEnglish:
        If UpperCase(UC_ENGLISH[I, 0]) = UpperCase(Prop) then
          Result := UC_ENGLISH[I, 1];
      ucSpanish:
        if UpperCase(UC_SPANISH[I, 0]) = UpperCase(Prop) then
          Result := UC_SPANISH[I, 1];
      ucFrench:
        if UpperCase(UC_FRENCH[I, 0]) = UpperCase(Prop) then
          Result := UC_FRENCH[I, 1];
    else
      If UpperCase(UC_PTBR[I, 0]) = UpperCase(Prop) then
        Result := UC_PTBR[I, 1];
    End;
  End;
End;

end.
