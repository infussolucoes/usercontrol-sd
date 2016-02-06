object FrmPrincipal: TFrmPrincipal
  Left = 408
  Top = 246
  Width = 627
  Height = 360
  Caption = 'Tutorial UserControl - www.showdelphi.com.br'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 40
    Top = 56
    object Cadastro1: TMenuItem
      Caption = 'Cadastro'
      object Bancos1: TMenuItem
        Caption = 'Bancos'
        OnClick = Bancos1Click
      end
      object Clientes1: TMenuItem
        Caption = 'Clientes'
        OnClick = Clientes1Click
      end
      object Cidades1: TMenuItem
        Caption = 'Cidades'
        OnClick = Cidades1Click
      end
      object Produtos1: TMenuItem
        Caption = 'Produtos'
        OnClick = Produtos1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Sair1: TMenuItem
        Caption = 'Sair'
        OnClick = Sair1Click
      end
    end
    object Seguranca1: TMenuItem
      Caption = 'Seguran'#231'a'
      object CadastrodeUsurios1: TMenuItem
        Caption = 'Cadastro de Usu'#225'rios'
      end
      object rocarSenha1: TMenuItem
        Caption = 'Trocar Senha'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Executarlogon1: TMenuItem
        Caption = 'Conectar com outro usu'#225'rio...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Mensagens1: TMenuItem
        Caption = 'Mensagens'
        OnClick = Mensagens1Click
      end
    end
    object estes1: TMenuItem
      Caption = 'Testes'
      object GerarLog1: TMenuItem
        Caption = 'Gerar Log'
        OnClick = GerarLog1Click
      end
    end
  end
  object ucMyControl: TUserControl
    AutoStart = True
    ApplicationID = 'NewProject'
    ControlRight.MainMenu = MainMenu1
    User.MenuItem = CadastrodeUsurios1
    UserPasswordChange.MenuItem = rocarSenha1
    UsersLogoff.MenuItem = Executarlogon1
    LogControl.TableLog = 'UCLog'
    Language = ucPortuguesBr
    EncryptKey = 0
    Login.InitialLogin.User = 'Admin'
    Login.InitialLogin.Email = 'teste@teste.com.br'
    Login.InitialLogin.Password = '#delphi'
    Login.MaxLoginAttempts = 0
    ExtraRights = <
      item
        FormName = 'FrmCliente'
        CompName = 'BtnIncluir'
        Caption = 'Incluir'
        GroupName = 'Tela de Clientes'
      end
      item
        FormName = 'FrmCliente'
        CompName = 'BtnAlterar'
        Caption = 'Alterar'
        GroupName = 'Tela de Clientes'
      end
      item
        FormName = 'FrmCliente'
        CompName = 'BtnExcluir'
        Caption = 'Excluir'
        GroupName = 'Tela de Clientes'
      end>
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
    DataConnector = UCIBXConn1
    Left = 200
    Top = 48
  end
  object UCIBXConn1: TUCIBXConn
    Connection = dmUC.IBDatabase1
    Transaction = dmUC.IBTransaction1
    Left = 200
    Top = 168
  end
  object UCApplicationMessage1: TUCApplicationMessage
    Active = True
    Interval = 60000
    TableMessages = 'UCTABMESSAGES'
    UserControl = ucMyControl
    Left = 344
    Top = 168
  end
end
