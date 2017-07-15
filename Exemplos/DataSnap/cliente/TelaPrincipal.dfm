object FrmPrincipal: TFrmPrincipal
  Left = 0
  Top = 0
  Caption = 'Tutorial UserControl - www.showdelphi.com.br'
  ClientHeight = 395
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 72
    Top = 72
    Width = 488
    Height = 33
    Alignment = taCenter
    Caption = 'CLIENTE DATASNAP USERCONTROL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 152
    Top = 168
    Width = 329
    Height = 33
    Alignment = taCenter
    Caption = 'SHOW DELPHI EDITION'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 208
    Top = 280
    Width = 226
    Height = 19
    Caption = 'Usu'#225'rio: Admin Senha: #delphi'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object MainMenu1: TMainMenu
    Left = 200
    Top = 152
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
    DataConnector = UCDataSnapConn1
    Left = 200
    Top = 48
  end
  object SQLConnection1: TSQLConnection
    ConnectionName = 'DataSnapCONNECTION'
    DriverName = 'DataSnap'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=DataSnap'
      'HostName=localhost'
      'port=211')
    AfterConnect = SQLConnection1AfterConnect
    BeforeDisconnect = SQLConnection1BeforeDisconnect
    Left = 360
    Top = 48
    UniqueId = '{1E8853CA-2353-4C02-8599-073DD29DA348}'
  end
  object UCDataSnapConn1: TUCDataSnapConn
    Connection = SQLConnection1
    ProviderName = 'ProviderAux'
    RemoteServer = DSProviderConnection1
    Left = 360
    Top = 224
  end
  object DSProviderConnection1: TDSProviderConnection
    ServerClassName = 'TSMUserControl'
    SQLConnection = SQLConnection1
    Left = 360
    Top = 128
  end
end
