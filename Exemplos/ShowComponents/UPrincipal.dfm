object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Visualizando os componentes Instalados'
  ClientHeight = 299
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object UserControl1: TUserControl
    ApplicationID = 'ProjetoNovo'
    LogControl.TableLog = 'UCLog'
    Language = ucPortuguesBr
    EncryptKey = 0
    Login.InitialLogin.User = 'admin'
    Login.InitialLogin.Email = 'usercontrol@usercontrol.net'
    Login.InitialLogin.Password = '123mudar'
    Login.MaxLoginAttempts = 0
    ExtraRights = <>
    TableUsers.FieldUserID = 'UCIdUser'
    TableUsers.FieldUserName = 'UCUserName'
    TableUsers.FieldLogin = 'UCLogin'
    TableUsers.FieldPassword = 'UCPassword'
    TableUsers.FieldEmail = 'UCEmail'
    TableUsers.FieldPrivileged = 'UCPrivileged'
    TableUsers.FieldTypeRec = 'UCTypeRec'
    TableUsers.FieldProfile = 'UCProfile'
    TableUsers.FieldKey = 'UCKey'
    TableUsers.FieldDateExpired = 'UCPASSEXPIRED'
    TableUsers.FieldUserExpired = 'UCUserExpired'
    TableUsers.FieldUserDaysSun = 'UCUserDaysSun'
    TableUsers.FieldUserInative = 'UCInative'
    TableUsers.TableName = 'UCTabUsers'
    TableRights.FieldUserID = 'UCIdUser'
    TableRights.FieldModule = 'UCModule'
    TableRights.FieldComponentName = 'UCCompName'
    TableRights.FieldFormName = 'UCFormName'
    TableRights.FieldKey = 'UCKey'
    TableRights.TableName = 'UCTabRights'
    TableUsersLogged.FieldLogonID = 'UCIdLogon'
    TableUsersLogged.FieldUserID = 'UCIdUser'
    TableUsersLogged.FieldApplicationID = 'UCApplicationId'
    TableUsersLogged.FieldMachineName = 'UCMachineName'
    TableUsersLogged.FieldData = 'UCData'
    TableUsersLogged.TableName = 'UCTabUsersLogged'
    Left = 112
    Top = 32
  end
  object UCSettings1: TUCSettings
    AppMessages.MsgsForm_BtNew = '&Nova Mensagem'
    AppMessages.MsgsForm_BtReplay = '&Responder'
    AppMessages.MsgsForm_BtForward = 'E&ncaminhar'
    AppMessages.MsgsForm_BtDelete = '&Excluir'
    AppMessages.MsgsForm_BtClose = '&Fechar'
    AppMessages.MsgsForm_WindowCaption = 'Mensagens do Sistema'
    AppMessages.MsgsForm_ColFrom = 'Remetente'
    AppMessages.MsgsForm_ColSubject = 'Assunto'
    AppMessages.MsgsForm_ColDate = 'Data'
    AppMessages.MsgsForm_PromptDelete = 'Confirma excluir as mensagens selecionadas ?'
    AppMessages.MsgsForm_PromptDelete_WindowCaption = 'Apagar mensagens'
    AppMessages.MsgsForm_NoMessagesSelected = 'N'#227'o existem mensagens selecionadas'
    AppMessages.MsgsForm_NoMessagesSelected_WindowCaption = 'Informa'#231#227'o'
    AppMessages.MsgRec_BtClose = '&Fechar'
    AppMessages.MsgRec_WindowCaption = 'Mensagem'
    AppMessages.MsgRec_Title = 'Mensagem Recebida'
    AppMessages.MsgRec_LabelFrom = 'De :'
    AppMessages.MsgRec_LabelDate = 'Data'
    AppMessages.MsgRec_LabelSubject = 'Assunto'
    AppMessages.MsgRec_LabelMessage = 'Mensagem'
    AppMessages.MsgSend_BtSend = '&Enviar'
    AppMessages.MsgSend_BtCancel = '&Cancelar'
    AppMessages.MsgSend_WindowCaption = 'Mensagem'
    AppMessages.MsgSend_Title = 'Enviar Nova Mensagem'
    AppMessages.MsgSend_GroupTo = 'Para'
    AppMessages.MsgSend_RadioUser = 'Usu'#225'rio :'
    AppMessages.MsgSend_RadioAll = 'Todos'
    AppMessages.MsgSend_GroupMessage = 'Mensagem'
    AppMessages.MsgSend_LabelSubject = 'Assunto'
    AppMessages.MsgSend_LabelMessageText = 'Texto da mensagem'
    CommonMessages.AutoLogonError = 'Falha de Auto Logon!'#13#10'Informe um usu'#225'rio e senha v'#225'lidos.'
    CommonMessages.ChangePasswordError.InvalidCurrentPassword = 'Senha Atual n'#227'o confere!'
    CommonMessages.ChangePasswordError.NewPasswordError = 'Os campos: Nova Senha e Confirma'#231#227'o devem ser iguais.'
    CommonMessages.ChangePasswordError.NewEqualCurrent = 'Nova senha igual a senha atual'
    CommonMessages.ChangePasswordError.PasswordRequired = 'A Senha '#233' obrigat'#243'ria'
    CommonMessages.ChangePasswordError.MinPasswordLength = 'A senha deve conter no m'#237'nimo %d caracteres'
    CommonMessages.ChangePasswordError.InvalidNewPassword = 'Proibido utilizar senhas obvias!'
    CommonMessages.InvalidLogin = 'Usu'#225'rio ou Senha inv'#225'lidos!'
    CommonMessages.InactiveLogin = 'Aten'#231#227'o, seu login esta inativo'
    CommonMessages.InitialMessage.Strings = (
      'ATEN'#199#195'O Login Inicial:'
      ''
      'Usu'#225'rio : :user'
      'Senha : :password'
      ''
      'Defina as permiss'#245'es para este usu'#225'rio.')
    CommonMessages.MaxLoginAttemptsError = 
      '%d Tentativas de login inv'#225'lido. Por motivos de seguran'#231'a o '#13#10'si' +
      'stema ser'#225' fechado.'
    CommonMessages.PasswordChanged = 'Senha alterada com sucesso!'
    CommonMessages.BlankPassword = 'Retirada senha do Login %s'
    CommonMessages.UsuarioExiste = 'O Usu'#225'rio "%s" j'#225' est'#225' cadastrado no sistema !!'
    CommonMessages.PasswordExpired = 'Aten'#231#227'o, sua senha expirou, favor troca-la'
    CommonMessages.ForcaTrocaSenha = 'Mudan'#231'a de senha obrigat'#243'ria'
    Login.WindowCaption = 'Login'
    Login.LabelUser = 'Usu'#225'rio :'
    Login.LabelPassword = 'Senha :'
    Login.BtOk = '&OK'
    Login.BtCancel = '&Cancelar'
    Login.LeftImage.Data = {
      07544269746D617016090000424D160900000000000036040000280000003200
      0000180000000100080000000000E00400000000000000000000000100000000
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000C0DCC000F0CAA6000020400000206000002080000020A0000020C0000020
      E00000400000004020000040400000406000004080000040A0000040C0000040
      E00000600000006020000060400000606000006080000060A0000060C0000060
      E00000800000008020000080400000806000008080000080A0000080C0000080
      E00000A0000000A0200000A0400000A0600000A0800000A0A00000A0C00000A0
      E00000C0000000C0200000C0400000C0600000C0800000C0A00000C0C00000C0
      E00000E0000000E0200000E0400000E0600000E0800000E0A00000E0C00000E0
      E00040000000400020004000400040006000400080004000A0004000C0004000
      E00040200000402020004020400040206000402080004020A0004020C0004020
      E00040400000404020004040400040406000404080004040A0004040C0004040
      E00040600000406020004060400040606000406080004060A0004060C0004060
      E00040800000408020004080400040806000408080004080A0004080C0004080
      E00040A0000040A0200040A0400040A0600040A0800040A0A00040A0C00040A0
      E00040C0000040C0200040C0400040C0600040C0800040C0A00040C0C00040C0
      E00040E0000040E0200040E0400040E0600040E0800040E0A00040E0C00040E0
      E00080000000800020008000400080006000800080008000A0008000C0008000
      E00080200000802020008020400080206000802080008020A0008020C0008020
      E00080400000804020008040400080406000804080008040A0008040C0008040
      E00080600000806020008060400080606000806080008060A0008060C0008060
      E00080800000808020008080400080806000808080008080A0008080C0008080
      E00080A0000080A0200080A0400080A0600080A0800080A0A00080A0C00080A0
      E00080C0000080C0200080C0400080C0600080C0800080C0A00080C0C00080C0
      E00080E0000080E0200080E0400080E0600080E0800080E0A00080E0C00080E0
      E000C0000000C0002000C0004000C0006000C0008000C000A000C000C000C000
      E000C0200000C0202000C0204000C0206000C0208000C020A000C020C000C020
      E000C0400000C0402000C0404000C0406000C0408000C040A000C040C000C040
      E000C0600000C0602000C0604000C0606000C0608000C060A000C060C000C060
      E000C0800000C0802000C0804000C0806000C0808000C080A000C080C000C080
      E000C0A00000C0A02000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0
      E000C0C00000C0C02000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0
      A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF00FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD525252525252525252525252
      5252FDFDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDAFB75E5E5E66666767676F6F6F6FB752FDFDFDFDFD0000FDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDAFBF6FBFBF
      BFBFBFBFBFBFBFBFBFB752FDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDAFB76F676F6FB7B7B7B7B7776F5EB752FD
      FDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDAFBF6FBFBFBFBFBFBFBFBFBFBFBFB752FDFDFDFDFD0000FDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDAFB76F676F6F6FB7B7
      B7B7776F5EB752FDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDAFBF6FBFBFBFBFBFBFBFBFBFBFBFBF52FDFDFDFDFD
      0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDAF
      B76767676F6F6FB7B7B7776F5EB752FDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDAFBF6FBFBFBFBFBFBFBFBFBFBF
      BFBF52FDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDAFB76767676F6F6F6FB7B7776F5EB752FDFDFDFDFD0000FDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDAFBF6FBFBF
      BFBFBFBFBFBFBFBFBFBF52FDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDAFBFB7AFAFB7B7B7B7BFBFBFBFB7B752FD
      FDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDAFB76F675E5E5E5E5E5E676FB7A5FDFDFDFDFDFD0000FDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDF7F79BFDFDFDFD
      FDFDA4F7A3FDFDFDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDF7F79BFDFDFDFDFDFDA407A3FDFDFDFDFDFDFD
      0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDF7F79BFDFDFDFDFDFDA407A3FDFDFDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDF7F7A4FDFDFDFDFDFDF707
      A3FDFDFDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDF707A49AFDFDFDFDA30707A3FDFDFDFDFDFDFD0000FDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDF7F6A4
      A49B9A9AA3A407079AFDFDFDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDF7F6F7F7A4A4F70707A3FDFDFDFD
      FDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDF7F6F6F6F6F607A3FDFDFDFDFDFDFDFDFD0000FDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDF7A3A3A3
      A3A3FDFDFDFDFDFDFDFDFDFD0000FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
      0000}
    Login.LabelTentativa = 'Tentativa : '
    Login.LabelTentativas = 'M'#225'ximo de Tentativas : '
    Log.WindowCaption = 'Seguran'#231'a'
    Log.LabelDescription = 'Log do Sistema'
    Log.LabelUser = 'Usu'#225'rio :'
    Log.LabelDate = 'Data :'
    Log.LabelLevel = 'N'#237'vel m'#237'nimo :'
    Log.ColAppID = 'AppID'
    Log.ColLevel = 'N'#237'vel'
    Log.ColMessage = 'Mensagem'
    Log.ColUser = 'Usu'#225'rio'
    Log.ColDate = 'Data'
    Log.BtFilter = '&Aplicar Filtro'
    Log.BtDelete = '&Excluir Log'
    Log.BtClose = '&Fechar'
    Log.PromptDelete = 'Confirma excluir todos os registros de log selecionados ?'
    Log.PromptDelete_WindowCaption = 'Confirma exclus'#227'o'
    Log.OptionUserAll = 'Todos'
    Log.OptionLevelLow = 'Baixo'
    Log.OptionLevelNormal = 'Normal'
    Log.OptionLevelHigh = 'Alto'
    Log.OptionLevelCritic = 'Cr'#237'tico'
    Log.DeletePerformed = 
      'Exclus'#227'o de log'#180's do sistema : Usu'#225'rio = "%s" | Data = %s a %s |' +
      ' N'#237'vel <= %s'
    UsersForm.WindowCaption = 'Seguran'#231'a'
    UsersForm.LabelDescription = 'Cadastro de Usu'#225'rios'
    UsersForm.ColName = 'Nome'
    UsersForm.ColLogin = 'Login'
    UsersForm.ColEmail = 'Email'
    UsersForm.BtAdd = '&Adicionar'
    UsersForm.BtChange = 'A&lterar'
    UsersForm.BtDelete = '&Excluir'
    UsersForm.PromptDelete = 'Confirma excluir o usu'#225'rio "%s" ?'
    UsersForm.PromptDelete_WindowCaption = 'Excluir usu'#225'rio'
    UsersForm.BtRights = 'A&cessos'
    UsersForm.BtPassword = '&Senha'
    UsersForm.BtClose = '&Fechar'
    AddChangeUser.WindowCaption = 'Cadastro de Usu'#225'rios'
    AddChangeUser.LabelAdd = 'Adicionar Usu'#225'rio'
    AddChangeUser.LabelChange = 'Alterar Usu'#225'rio'
    AddChangeUser.LabelName = 'Nome :'
    AddChangeUser.LabelLogin = 'Login :'
    AddChangeUser.LabelEmail = 'Email :'
    AddChangeUser.LabelPerfil = 'Perfil :'
    AddChangeUser.CheckPrivileged = 'Usu'#225'rio privilegiado'
    AddChangeUser.BtSave = '&Gravar'
    AddChangeUser.BtCancel = 'Cancelar'
    AddChangeUser.CheckExpira = 'Senha do usu'#225'rio n'#227'o expira'
    AddChangeUser.Day = 'Dias'
    AddChangeUser.ExpiredIn = 'Expira em'
    AddChangeUser.LabelStatus = 'Status'
    AddChangeUser.StatusActive = 'Ativo'
    AddChangeUser.StatusDisabled = 'Inativo'
    AddChangeProfile.WindowCaption = 'Perfil de Usu'#225'rios'
    AddChangeProfile.LabelAdd = 'Adicionar Perfil'
    AddChangeProfile.LabelChange = 'Alterar Perfil'
    AddChangeProfile.LabelName = 'Descri'#231#227'o :'
    AddChangeProfile.BtSave = '&Gravar'
    AddChangeProfile.BtCancel = 'Cancelar'
    UsersProfile.WindowCaption = 'Seguran'#231'a'
    UsersProfile.LabelDescription = 'Perfil de Usu'#225'rios'
    UsersProfile.ColProfile = 'Perfil'
    UsersProfile.BtAdd = '&Adicionar'
    UsersProfile.BtChange = 'A&lterar'
    UsersProfile.BtDelete = '&Excluir'
    UsersProfile.BtRights = 'A&cessos'
    UsersProfile.PromptDelete = 'Existem usu'#225'rios com o perfil "%s". Confirma excluir?'
    UsersProfile.PromptDelete_WindowCaption = 'Delete profile'
    UsersProfile.BtClose = '&Fechar'
    Rights.WindowCaption = 'Seguran'#231'a'
    Rights.LabelUser = 'Permiss'#245'es do Usu'#225'rio :'
    Rights.LabelProfile = 'Permiss'#245'es do Perfil :'
    Rights.PageMenu = 'Itens do Menu'
    Rights.PageActions = 'A'#231#245'es'
    Rights.PageControls = 'Controles'
    Rights.BtUnlock = '&Liberar'
    Rights.BtLock = '&Bloquear'
    Rights.BtSave = '&Gravar'
    Rights.BtCancel = '&Cancelar'
    ChangePassword.WindowCaption = 'Seguran'#231'a'
    ChangePassword.LabelDescription = 'Trocar Senha'
    ChangePassword.LabelCurrentPassword = 'Senha Atual :'
    ChangePassword.LabelNewPassword = 'Nova Senha :'
    ChangePassword.LabelConfirm = 'Confirma'#231#227'o :'
    ChangePassword.BtSave = '&Gravar'
    ChangePassword.BtCancel = 'Cancelar'
    ResetPassword.WindowCaption = 'Definir senha do usu'#225'rio : "%s"'
    ResetPassword.LabelPassword = 'Senha :'
    BancoDados = Firebird
    Language = ucPortuguesBr
    UsersLogged.BtnMessage = '&Mensagem'
    UsersLogged.BtnRefresh = '&Atualizar'
    UsersLogged.BtnClose = '&Fechar'
    UsersLogged.LabelDescricao = 'Usu'#225'rios Logados'
    UsersLogged.LabelCaption = 'Usu'#225'rios Logados no sistema'
    UsersLogged.ColName = 'Nome'
    UsersLogged.ColLogin = 'Login'
    UsersLogged.ColComputer = 'Computador'
    UsersLogged.ColData = 'Data'
    UsersLogged.InputCaption = 'Mensagem'
    UsersLogged.InputText = 'Digite sua mensagem'
    UsersLogged.MsgSystem = 'Mensagem do sistema'
    Left = 112
    Top = 96
  end
  object UCControls1: TUCControls
    Components = ''
    Left = 112
    Top = 160
  end
  object UCApplicationMessage1: TUCApplicationMessage
    Active = True
    Interval = 60000
    TableMessages = 'UCTABMESSAGES'
    Left = 112
    Top = 224
  end
  object UCIdle1: TUCIdle
    Timeout = 0
    Left = 208
    Top = 32
  end
  object MailUserControl1: TMailUserControl
    AuthType = alsmtpClientAuthPlain
    Porta = 25
    AdicionaUsuario.Ativo = True
    AdicionaUsuario.Titulo = 'Inclus'#227'o de usu'#225'rio'
    AdicionaUsuario.Mensagem.Strings = (
      
        '<html> <head> <title>Inclus'#227'o de Senha</title> <style type="text' +
        '/css"> <!-- body { '#9'margin-left: 0px; '#13#10'margin-top: 0px; '#9'margin' +
        '-right: 0px; '#9'margin-bottom: 0px; } --> </style></head>'#13#10'<body> ' +
        '<p>Aten'#231#227'o: <br>Senha criada com sucesso:</p>'#13#10'<table width="100' +
        '%" border="0" cellspacing="2" cellpadding="0"> '#13#10'<tr> '#13#10' <td wid' +
        'th="10%" align="right"><strong>Nome ..:&nbsp;</strong></td>'#13#10'<td' +
        '>:nome</td> '#13#10'</tr> '#13#10'<tr>  <td align="right"><strong>Login ..:&' +
        'nbsp;</strong></td>'#13#10'  <td>:login</td>'#13#10'</tr>'#13#10'  <tr> '#13#10'    <td ' +
        'align="right"><strong>Nova Senha ..:&nbsp;</strong></td>'#13#10'    <t' +
        'd>:senha</td>'#13#10'  </tr> '#13#10'<tr> '#13#10'<td align="right"><strong>Email ' +
        '..:&nbsp;</strong></td>'#13#10'<td>:email</td>'#13#10'</tr> '#13#10'<tr>'#13#10'<td alig' +
        'n="right"><strong>Perfil ..:&nbsp;</strong></td>'#13#10'<td>:perfil</t' +
        'd> '#13#10'</tr>'#13#10'</table>'#13#10'<p>Atenciosamente,</p>'#13#10'<p>Administrador d' +
        'o sistema</p></body></html>')
    AlteraUsuario.Ativo = True
    AlteraUsuario.Titulo = 'Altera'#231#227'o de usu'#225'rio'
    AlteraUsuario.Mensagem.Strings = (
      
        '<html> <head> <title>Altera'#231#227'o de Senha</title> <style type="tex' +
        't/css"> <!-- body { '#9'margin-left: 0px; '#13#10'margin-top: 0px; '#9'margi' +
        'n-right: 0px; '#9'margin-bottom: 0px; } --> </style></head>'#13#10'<body>' +
        ' <p>Aten'#231#227'o: <br> Voc'#234' solicitou uma altera'#231#227'o de senha do siste' +
        'ma, sua senha foi alterada para a senha abaixo:</p>'#13#10'<table widt' +
        'h="100%" border="0" cellspacing="2" cellpadding="0"> '#13#10'<tr> '#13#10' <' +
        'td width="10%" align="right"><strong>Nome ..:&nbsp;</strong></td' +
        '>'#13#10'<td>:nome</td> '#13#10'</tr> '#13#10'<tr>  <td align="right"><strong>Logi' +
        'n ..:&nbsp;</strong></td>'#13#10'  <td>:login</td>'#13#10'</tr>'#13#10'  <tr> '#13#10'  ' +
        '  <td align="right"><strong>Nova Senha ..:&nbsp;</strong></td>'#13#10 +
        '    <td>:senha</td>'#13#10'  </tr> '#13#10'<tr> '#13#10'<td align="right"><strong>' +
        'Email ..:&nbsp;</strong></td>'#13#10'<td>:email</td>'#13#10'</tr> '#13#10'<tr>'#13#10'<t' +
        'd align="right"><strong>Perfil ..:&nbsp;</strong></td>'#13#10'<td>:per' +
        'fil</td> '#13#10'</tr>'#13#10'</table>'#13#10'<p>Atenciosamente,</p>'#13#10'<p>Administr' +
        'ador do sistema</p></body></html>')
    EsqueceuSenha.Ativo = True
    EsqueceuSenha.Titulo = 'Altera'#231#227'o de Senha'
    EsqueceuSenha.Mensagem.Strings = (
      
        '<html> <head> <title>Altera'#231#227'o de Senha</title> <style type="tex' +
        't/css"> <!-- body { '#9'margin-left: 0px; '#13#10'margin-top: 0px; '#9'margi' +
        'n-right: 0px; '#9'margin-bottom: 0px; } --> </style></head>'#13#10'<body>' +
        ' <p>Aten'#231#227'o: <br> Voc'#234' solicitou um lembrete de senha do sistema' +
        ', sua senha foi alterada para a senha abaixo:</p>'#13#10'<table width=' +
        '"100%" border="0" cellspacing="2" cellpadding="0"> '#13#10'<tr> '#13#10' <td' +
        ' width="10%" align="right"><strong>Nome ..:&nbsp;</strong></td>'#13 +
        #10'<td>:nome</td> '#13#10'</tr> '#13#10'<tr>  <td align="right"><strong>Login ' +
        '..:&nbsp;</strong></td>'#13#10'  <td>:login</td>'#13#10'</tr>'#13#10'  <tr> '#13#10'    ' +
        '<td align="right"><strong>Nova Senha ..:&nbsp;</strong></td>'#13#10'  ' +
        '  <td>:senha</td>'#13#10'  </tr> '#13#10'<tr> '#13#10'<td align="right"><strong>Em' +
        'ail ..:&nbsp;</strong></td>'#13#10'<td>:email</td>'#13#10'</tr> '#13#10'<tr>'#13#10'<td ' +
        'align="right"><strong>Perfil ..:&nbsp;</strong></td>'#13#10'<td>:perfi' +
        'l</td> '#13#10'</tr>'#13#10'</table>'#13#10'<p>Atenciosamente,</p>'#13#10'<p>Administrad' +
        'or do sistema</p></body></html>')
    EsqueceuSenha.LabelLoginForm = 'Esqueci a senha'
    EsqueceuSenha.MensagemEmailEnviado = 'A senha foi enviada para o seu email.'
    SenhaForcada.Ativo = True
    SenhaForcada.Titulo = 'Troca de senha for'#231'ada'
    SenhaForcada.Mensagem.Strings = (
      
        '<html> <head> <title>Altera'#231#227'o de Senha For'#231'ada</title> <style t' +
        'ype="text/css"> <!-- body { '#9'margin-left: 0px; '#13#10'margin-top: 0px' +
        '; '#9'margin-right: 0px; '#9'margin-bottom: 0px; } --> </style></head>' +
        #13#10'<body> <p>Aten'#231#227'o: <br> Voc'#234' ou um administrador for'#231'ou a troc' +
        'a de sua senha do sistema, sua senha foi alterada para a senha a' +
        'baixo:</p>'#13#10'<table width="100%" border="0" cellspacing="2" cellp' +
        'adding="0"> '#13#10'<tr> '#13#10' <td width="10%" align="right"><strong>Nome' +
        ' ..:&nbsp;</strong></td>'#13#10'<td>:nome</td> '#13#10'</tr> '#13#10'<tr>  <td ali' +
        'gn="right"><strong>Login ..:&nbsp;</strong></td>'#13#10'  <td>:login</' +
        'td>'#13#10'</tr>'#13#10'  <tr> '#13#10'    <td align="right"><strong>Nova Senha ..' +
        ':&nbsp;</strong></td>'#13#10'    <td>:senha</td>'#13#10'  </tr> '#13#10'<tr> '#13#10'<td' +
        ' align="right"><strong>Email ..:&nbsp;</strong></td>'#13#10'<td>:email' +
        '</td>'#13#10'</tr> '#13#10'<tr>'#13#10'<td align="right"><strong>Perfil ..:&nbsp;<' +
        '/strong></td>'#13#10'<td>:perfil</td> '#13#10'</tr>'#13#10'</table>'#13#10'<p>Atenciosam' +
        'ente,</p>'#13#10'<p>Administrador do sistema</p></body></html>')
    SenhaTrocada.Ativo = True
    SenhaTrocada.Titulo = 'Altera'#231#227'o de senha'
    SenhaTrocada.Mensagem.Strings = (
      
        '<html> <head> <title>Altera'#231#227'o de Senha</title> <style type="tex' +
        't/css"> <!-- body { '#9'margin-left: 0px; '#13#10'margin-top: 0px; '#9'margi' +
        'n-right: 0px; '#9'margin-bottom: 0px; } --> </style></head>'#13#10'<body>' +
        ' <p>Aten'#231#227'o: <br> Voc'#234' alterou sua senha do sistema, sua senha f' +
        'oi alterada para a senha abaixo:</p>'#13#10'<table width="100%" border' +
        '="0" cellspacing="2" cellpadding="0"> '#13#10'<tr> '#13#10' <td width="10%" ' +
        'align="right"><strong>Nome ..:&nbsp;</strong></td>'#13#10'<td>:nome</t' +
        'd> '#13#10'</tr> '#13#10'<tr>  <td align="right"><strong>Login ..:&nbsp;</st' +
        'rong></td>'#13#10'  <td>:login</td>'#13#10'</tr>'#13#10'  <tr> '#13#10'    <td align="ri' +
        'ght"><strong>Nova Senha ..:&nbsp;</strong></td>'#13#10'    <td>:senha<' +
        '/td>'#13#10'  </tr> '#13#10'<tr> '#13#10'<td align="right"><strong>Email ..:&nbsp;' +
        '</strong></td>'#13#10'<td>:email</td>'#13#10'</tr> '#13#10'<tr>'#13#10'<td align="right"' +
        '><strong>Perfil ..:&nbsp;</strong></td>'#13#10'<td>:perfil</td> '#13#10'</tr' +
        '>'#13#10'</table>'#13#10'<p>Atenciosamente,</p>'#13#10'<p>Administrador do sistema' +
        '</p></body></html>')
    Left = 208
    Top = 96
  end
  object UCHist_DataSet1: TUCHist_DataSet
    Left = 304
    Top = 32
  end
  object UCControlHistorico1: TUCControlHistorico
    Options.SaveNewRecord = True
    Options.SaveDelete = True
    Options.SavePostInsert = True
    Options.SavePostEdit = True
    Options.TypeSavePostEdit = tpSaveAllFields
    TableHistory.TableName = 'UCTABHistory'
    TableHistory.FieldApplicationID = 'ApplicationID'
    TableHistory.FieldUserID = 'UserID'
    TableHistory.FieldEventDate = 'EventDate'
    TableHistory.FieldEventTime = 'EventTime'
    TableHistory.FieldForm = 'Form'
    TableHistory.FieldCaptionForm = 'FormCaption'
    TableHistory.FieldEvent = 'Event'
    TableHistory.FieldObs = 'Obs'
    TableHistory.FieldTableName = 'tName'
    HistoryMsg.Evento_Insert = 'Inserido'
    HistoryMsg.Evento_Delete = 'Apagado'
    HistoryMsg.Evento_Edit = 'Editado'
    HistoryMsg.Evento_NewRecord = 'Novo registro'
    HistoryMsg.Hist_All = 'Todos'
    HistoryMsg.Msg_LimpHistorico = 'Excluir todo o conte'#250'do do hist'#243'rico ?'
    HistoryMsg.Msg_MensConfirma = 'Confirma'#231#227'o'
    HistoryMsg.Msg_LogEmptyHistory = 'Usu'#225'rio %s apagou todo o hist'#243'rico as %s'
    HistoryMsg.LabelDescricao = 'Hist'#243'rico de tabelas'
    HistoryMsg.LabelUser = 'Usu'#225'rio'
    HistoryMsg.LabelForm = 'Formul'#225'rio'
    HistoryMsg.LabelEvento = 'Evento'
    HistoryMsg.LabelTabela = 'Tabela'
    HistoryMsg.LabelDataEvento = 'Data'
    HistoryMsg.LabelHoraEvento = 'Hora'
    HistoryMsg.Msg_NewRecord = '%s inseriu um novo registro'
    HistoryMsg.Hist_MsgExceptPropr = 'Favor informar a propriedade %s'
    HistoryMsg.Hist_BtnFiltro = '&Aplicar Filtro'
    HistoryMsg.Hist_BtnExcluir = '&Excluir Hist'#243'rico'
    HistoryMsg.Hist_BtnFechar = '&Fechar'
    Left = 304
    Top = 96
  end
  object UCFireDACConn1: TUCFireDACConn
    Left = 440
    Top = 32
  end
  object UCMidasConn1: TUCMidasConn
    BancoDados = bdFirebird
    Left = 440
    Top = 96
  end
  object UCIBXConn1: TUCIBXConn
    Left = 440
    Top = 160
  end
  object UCADOConn1: TUCADOConn
    Left = 440
    Top = 224
  end
end
