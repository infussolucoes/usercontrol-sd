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

unit UCBase;

interface

{$I 'UserControl.inc'}

uses
  ActnList, ActnMan, ActnMenus, Classes, Controls, DB, ExtActns, Forms, Graphics,  
  Menus, StdCtrls, SysUtils,
  {.$IFDEF DELPHI5_UP}
  Variants,
  {.$ENDIF}
  Windows,
  {$IF CompilerVersion >= 23}
  System.UITypes,
  {$IFEND}
  UCmd5, UcConsts_Language, UCDataConnector, UCDataInfo, UCMail, UCMessages, UCSettings;

const
  llBaixo = 0;
  llNormal = 1;
  llMedio = 2;
  llCritico = 3;

  // Version
const
  UCVersion = '2.31 RC4';
{$WARNINGS OFF}

type
  // Pensando em usar GUID para gerar a chave das tabelas !!!!
  TUCGUID = class
    // Creates and returns a new globally unique identifier
    class function NovoGUID: TGUID;
    // sometimes we need to have an "empty" value, like NULL
    class function EmptyGUID: TGUID;
    // Checks whether a Guid is EmptyGuid
    class function IsEmptyGUID(GUID: TGUID): Boolean;
    // Convert to string
    class function ToString(GUID: TGUID): String;
    // convert to quoted string
    class function ToQuotedString(GUID: TGUID): String;
    // return a GUID from string
    class function FromString(Value: String): TGUID;
    // Indicates whether two TGUID values are the same
    class function EqualGUIDs(GUID1, GUID2: TGUID): Boolean;
    // Creates and returns a new globally unique identifier string
    class function NovoGUIDString: String;
  end;
{$WARNINGS ON}

  TUCAboutVar = String;

  // classe para armazenar usuario logado = currentuser
  TUCCurrentUser = class(TComponent)
  private
    FPerfilUsuario: TDataSet;
    FPerfilGrupo: TDataSet;
    FLotacao      : TDataSet;   //   Mauri
    FEmpresa      : TDataSet;   // Empresa
  public
    UserID: Integer;
    Profile: Integer;
    UserIDOld: Integer;
    IdLogon: String;
    UserName: String;
    UserLogin: String;
    Password: String;
    PassLivre: String;
    Email: String;
    DateExpiration: TDateTime;
    Privileged: Boolean;
    UserNotExpired: Boolean;
    UserDaysExpired: Integer;
    UserDepartment : String;  // Lotacao do Usuario - Mauri 21/02/2008
    UserEmpresa    : Integer; // EMpresa Mauri 26/01/2017
    UserType       : Integer;  //  Mauri 23/06/2012

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PerfilUsuario: TDataSet read FPerfilUsuario write FPerfilUsuario;  // Cadastro de Usuarios
    property PerfilGrupo: TDataSet read FPerfilGrupo write FPerfilGrupo;   // Cadastro de Perfil
    property Lotacao: TDataSet read FLotacao write FLotacao; //Cadastro de Lotacao - Mauri
    property Empresa: TDataSet read FEmpresa write FEmpresa;  // Cadastro de Empresa  06/01/2017
  end;

  TUCUser = class(TPersistent)
    // armazenar menuitem ou action responsavel pelo controle de usuarios
  private
    FAction: TAction;
    FMenuItem: TMenuItem;
    FUsePrivilegedField: Boolean;
    FProtectAdministrator: Boolean;
    procedure SetAction(const Value: TAction);
    procedure SetMenuItem(const Value: TMenuItem);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Action: TAction read FAction write SetAction;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property UsePrivilegedField: Boolean read FUsePrivilegedField
      write FUsePrivilegedField default False;
    property ProtectAdministrator: Boolean read FProtectAdministrator
      write FProtectAdministrator default True;
  end;

  TUCUserProfile = class(TPersistent)
    // armazenar menuitem ou action responsavel pelo Perfil de usuarios
  private
    FAtive: Boolean;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Active: Boolean read FAtive write FAtive default True;
  end;

  TUCUserPasswordChange = class(TPersistent)
    // armazenar menuitem ou action responsavel pelo Form trocar senha
  private
    FForcePassword: Boolean;
    FMinPasswordLength: Integer;
    FAction: TAction;
    FMenuItem: TMenuItem;
    procedure SetAction(const Value: TAction);
    procedure SetMenuItem(const Value: TMenuItem);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Action: TAction read FAction write SetAction;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property ForcePassword: Boolean read FForcePassword write FForcePassword
      default False;
    property MinPasswordLength: Integer read FMinPasswordLength
      write FMinPasswordLength default 0;
  end;

  TUCUserLogoff = class(TPersistent)
    // armazenar menuitem ou action responsavel pelo logoff
  private
    FAction: TAction;
    FMenuItem: TMenuItem;
    procedure SetAction(const Value: TAction);
    procedure SetMenuItem(const Value: TMenuItem);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Action: TAction read FAction write SetAction;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
  end;

  TUCAutoLogin = class(TPersistent) // armazenar configuracao de Auto-Logon
  private
    FActive: Boolean;
    FUser: String;
    FPassword: String;
    FMessageOnError: Boolean;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Active: Boolean read FActive write FActive default False;
    property User: String read FUser write FUser;
    property Password: String read FPassword write FPassword;
    property MessageOnError: Boolean read FMessageOnError write FMessageOnError
      default True;
  end;

  TUCInitialLogin = class(TPersistent)
    // armazenar Dados do Login que sera criado na primeira execucao do programa.
  private
    FUser: String;
    FPassword: String;
    FInitialRights: TStrings;
    FEmail: String;
    procedure SetInitialRights(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property User: String read FUser write FUser;
    property Email: String read FEmail write FEmail;
    property Password: String read FPassword write FPassword;
    property InitialRights: TStrings read FInitialRights write SetInitialRights;
  end;

  TUCGetLoginName = (lnNone, lnUserName, lnMachineName);

  TUCLogin = class(TPersistent)
  private
    FAutoLogin: TUCAutoLogin;
    FMaxLoginAttempts: Integer;
    FInitialLogin: TUCInitialLogin;
    FGetLoginName: TUCGetLoginName;
    fCharCaseUser: TEditCharCase;
    fCharCasePass: TEditCharCase;
    fDateExpireActive: Boolean;
    fDaysOfSunExpired: Word;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoLogin: TUCAutoLogin read FAutoLogin write FAutoLogin;
    property InitialLogin: TUCInitialLogin read FInitialLogin
      write FInitialLogin;
    property MaxLoginAttempts: Integer read FMaxLoginAttempts
      write FMaxLoginAttempts;
    property GetLoginName: TUCGetLoginName read FGetLoginName
      write FGetLoginName default lnNone;
    property CharCaseUser: TEditCharCase read fCharCaseUser write fCharCaseUser
      default ecNormal;
    property CharCasePass: TEditCharCase read fCharCasePass write fCharCasePass
      default ecNormal;
    property ActiveDateExpired: Boolean read fDateExpireActive
      write fDateExpireActive default False;
    property DaysOfSunExpired: Word read fDaysOfSunExpired
      write fDaysOfSunExpired default 30;
  end;

  TUCNotAllowedItems = class(TPersistent)
    // Ocultar e/ou Desabilitar os itens que o usuario nao tem acesso
  private
    FMenuVisible: Boolean;
    FActionVisible: Boolean;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property MenuVisible: Boolean read FMenuVisible write FMenuVisible
      default True;
    property ActionVisible: Boolean read FActionVisible write FActionVisible
      default True;
  end;

  TUCLogControl = class(TPersistent) // Responsavel pelo Controle de Log
  private
    FActive: Boolean;
    FTableLog: String;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Active: Boolean read FActive write FActive default True;
    property TableLog: String read FTableLog write FTableLog;
  end;

  TUCControlRight = class(TPersistent)
    // Menu / ActionList/ActionManager ou ActionMainMenuBar a serem Controlados
  private
    FActionList: TActionList;
    FActionManager: TActionManager;
    FActionMainMenuBar: TActionMainMenuBar;
    FMainMenu: TMenu;
    procedure SetActionList(const Value: TActionList);
    procedure SetActionManager(const Value: TActionManager);
    procedure SetActionMainMenuBar(const Value: TActionMainMenuBar);
    procedure SetMainMenu(const Value: TMenu);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ActionList: TActionList read FActionList write SetActionList;
    property MainMenu: TMenu read FMainMenu write SetMainMenu;
    property ActionManager: TActionManager read FActionManager
      write SetActionManager;
    property ActionMainMenuBar: TActionMainMenuBar read FActionMainMenuBar
      write SetActionMainMenuBar;
  end;

  TOnLogin = procedure(Sender: TObject; var User, Password: String) of object;
  TOnLoginSucess = procedure(Sender: TObject; IdUser: Integer;
    Usuario, Nome, Senha, Email: String; Privileged: Boolean) of object;
  TOnLoginError = procedure(Sender: TObject; Usuario, Senha: String) of object;
  TOnApplyRightsMenuItem = procedure(Sender: TObject; MenuItem: TMenuItem)
    of object;
  TOnApllyRightsActionItem = procedure(Sender: TObject; Action: TAction)
    of object;
  TCustomUserForm = procedure(Sender: TObject; var CustomForm: TCustomForm)
    of object;
  TCustomUserProfileForm = procedure(Sender: TObject;
    var CustomForm: TCustomForm) of object;
  TCustomLoginForm = procedure(Sender: TObject; var CustomForm: TCustomForm)
    of object;
  TCustomUserPasswordChangeForm = procedure(Sender: TObject;
    var CustomForm: TCustomForm) of object;
  TCustomLogControlForm = procedure(Sender: TObject;
    var CustomForm: TCustomForm) of object;
  TCustomInitialMessage = procedure(Sender: TObject;
    var CustomForm: TCustomForm; var Msg: TStrings) of object;
  TCustomUserLoggedForm = procedure(Sender: TObject;
    var CustomForm: TCustomForm) of object; // Cesar: 13/07/2005
  TOnAddUser = procedure(Sender: TObject; var Login, Password, Name,
    Mail: String; var Profile: Integer; var Privuser: Boolean) of object;
  TOnChangeUser = procedure(Sender: TObject; IDUser: Integer; var Login, Name, Mail: String; var Profile: Integer;
                  var Privuser: Boolean ; Department : string; UserType :Integer; UserEmpresa: Integer  ) of object;
                  // Mauri
  TOnDeleteUser = procedure(Sender: TObject; IdUser: Integer;
    var CanDelete: Boolean; var ErrorMsg: String) of object;
  TOnAddProfile = procedure(Sender: TObject; var Profile: String) of object;
  TOnDeleteProfile = procedure(Sender: TObject; IDProfile: Integer;
    var CanDelete: Boolean; var ErrorMsg: String) of object;
  TOnChangePassword = procedure(Sender: TObject; IdUser: Integer;
    Login, CurrentPassword, NewPassword: String) of object;
  TOnLogoff = procedure(Sender: TObject; IdUser: Integer) of object;

  TUCExtraRights = class;
  TUCExecuteThread = class;
  TUCApplicationMessage = class;
  TUCControls = class;
  TUCUsersLogged = class; // Cesar: 12/07/2005

  TUCLoginMode = (lmActive, lmPassive);
  TUCCriptografia = (cPadrao, cMD5);
  TUCUserType     = ( ucAdmin,ucSupervisor,ucGerente, ucUsuario );       // mauri  23/12/2012

  TUserControl = class(TComponent) // Classe principal
  private
    FCurrentUser: TUCCurrentUser;
    FUserSettings: TUCUserSettings;
    FApplicationID: String;
    FNotAllowedItems: TUCNotAllowedItems;
    FOnLogin: TOnLogin;
    FOnStartApplication: TNotifyEvent;
    FOnLoginError: TOnLoginError;
    FOnLoginSucess: TOnLoginSucess;
    FOnApplyRightsActionIt: TOnApllyRightsActionItem;
    FOnApplyRightsMenuIt: TOnApplyRightsMenuItem;
    FLogControl: TUCLogControl;
    FEncrytKey: Word;
    FUser: TUCUser;
    FLogin: TUCLogin;
    FUserProfile: TUCUserProfile;
    FUserPasswordChange: TUCUserPasswordChange;
    FControlRight: TUCControlRight;
    FOnCustomCadUsuarioForm: TCustomUserForm;
    FCustomLogControlForm: TCustomLogControlForm;
    FCustomLoginForm: TCustomLoginForm;
    FCustomPerfilUsuarioForm: TCustomUserProfileForm;
    FCustomTrocarSenhaForm: TCustomUserPasswordChangeForm;
    FOnAddProfile: TOnAddProfile;
    FOnAddUser: TOnAddUser;
    FOnChangePassword: TOnChangePassword;
    FOnChangeUser: TOnChangeUser;
    FOnDeleteProfile: TOnDeleteProfile;
    FOnDeleteUser: TOnDeleteUser;
    FOnLogoff: TOnLogoff;
    FCustomInicialMsg: TCustomInitialMessage;
    FAbout: TUCAboutVar;
    FExtraRights: TUCExtraRights;
    FThUCRun: TUCExecuteThread;
    FAutoStart: Boolean;
    FTableRights: TUCTableRights;
    FTableUsers: TUCTableUsers;
    FLoginMode: TUCLoginMode;
    FControlList: TList;
    FDataConnector: TUCDataConnector;
    FLoginMonitorList: TList;
    FAfterLogin: TNotifyEvent;
    FCheckValidationKey: Boolean;
    FCriptografia: TUCCriptografia;
    FUsersLogged: TUCUsersLogged;
    FTableUsersLogged: TUCTableUsersLogged;
    FUsersLogoff: TUCUserLogoff;
    fLanguage: TUCLanguage;
    FMailUserControl: TMailUserControl;
    FTableUserDepartment  : TUCTableUserDepartment;  //  Lotacao Mauri
    FTableUserEmpresa     : TUCTableUserEmpresa;         // Empresa  06/01/2017
    FUserType             : Integer;      //  Mauri 23/06/2012

    procedure SetExtraRights(Value: TUCExtraRights);
    procedure ActionCadUser(Sender: TObject);
    procedure ActionTrocaSenha(Sender: TObject);
    procedure ActionOKLogin(Sender: TObject);
    procedure TestaFecha(Sender: TObject; var CanClose: Boolean);
    procedure ApplySettings(SourceSettings: TUCSettings);
    procedure UnlockEX(FormObj: TCustomForm; ObjName: String);
    procedure LockEX(FormObj: TCustomForm; ObjName: String;
      naInvisible: Boolean);
    { .$IFDEF UCACTMANAGER }
    procedure TrataActMenuBarIt(IT: TActionClientItem; ADataset: TDataSet);
    procedure IncPermissActMenuBar(IdUser: Integer; Act: TAction);
    { .$ENDIF }
    procedure SetDataConnector(const Value: TUCDataConnector);
    procedure DoCheckValidationField;
    procedure SetfLanguage(const Value: TUCLanguage);
    procedure SetFMailUserControl(const Value: TMailUserControl);
    procedure ActionEsqueceuSenha(Sender: TObject);
  protected
    FRetry: Integer;
    // Formul�rios
    FFormTrocarSenha: TCustomForm;
    FFormLogin: TCustomForm;
    FFormGeral: TCustomForm;
    // -----

    procedure Loaded; override;
    // Criar Formul�rios
    procedure CriaFormTrocarSenha; dynamic;
    // -----

    procedure ActionLogoff(Sender: TObject); dynamic;
    procedure ActionTSBtGrava(Sender: TObject);
    procedure SetUserSettings(const Value: TUCUserSettings);
    procedure SetfrmLoginWindow(Form: TCustomForm);
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure RegistraCurrentUser(Dados: TDataSet; Pass: String);
    procedure ApplyRightsObj(ADataset: TDataSet; FProfile: Boolean = False);
    procedure ShowLogin;
    procedure ApplyRights;

    // Criar Tabelas
    procedure CriaTabelaLog;
    procedure CriaTabelaRights(ExtraRights: Boolean = False);
    procedure CriaTabelaUsuarios(TableExists: Boolean);
    procedure CriaTabelaMsgs(const TableName: String);
    procedure CriaTabelaDepartment(TableExists: Boolean);  {Mauri }
    procedure CriaTabelaEmpresa(TableExists : Boolean);     // Empresa  06/01/2017
    // -----

    // Atualiza Versao
    procedure AtualizarVersao;
    // --------

    procedure TryAutoLogon;
    procedure AddUCControlMonitor(UCControl: TUCControls);
    procedure DeleteUCControlMonitor(UCControl: TUCControls);
    procedure ApplyRightsUCControlMonitor;
    procedure LockControlsUCControlMonitor;
    procedure AddLoginMonitor(UCAppMessage: TUCApplicationMessage);
    procedure DeleteLoginMonitor(UCAppMessage: TUCApplicationMessage);
    procedure NotificationLoginMonitor;
    procedure ShowNewConfig;
  public
    procedure Logoff;
    procedure Execute;
    procedure StartLogin;
    procedure ShowChangePassword;
    procedure ChangeUser(IDUser: Integer; Login, Name, Mail: String; Profile, UserExpired, UserDaysSun,
              Status: Integer; PrivUser: Boolean; Department : String; UserType : Integer; UserEmpresa: Integer );
              // Mauri
    procedure ChangePassword(IdUser: Integer; NewPassword: String);
    procedure AddRight(IdUser: Integer; ItemRight: TObject;
      FullPath: Boolean = True); overload;
    procedure AddRight(IdUser: Integer; ItemRight: String); overload;
    procedure AddRightEX(IdUser: Integer; Module, FormName, ObjName: String);
    procedure HideField(Sender: TField; var Text: String; DisplayText: Boolean);
    procedure Log(Msg: String; Level: Integer = llNormal);
    function VerificaLogin(User, Password: String;
      SoVerificarUsuarioAdmin: Boolean = False): Integer; // Boolean;
    function GetLocalUserName: String;
    function GetLocalComputerName: String;
    function AddUser(Login, Password, Name, Mail: String; Profile, UserExpired,
              DaysExpired: Integer; PrivUser: Boolean; Department: String; UserType : Integer; UserEmpresa :Integer): Integer; // Mauri
    function ExisteUsuario(Login: String): Boolean;
    property CurrentUser: TUCCurrentUser read FCurrentUser write FCurrentUser;
    property UserSettings: TUCUserSettings read FUserSettings
      write SetUserSettings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAllUsers(Names: Boolean): TStringList;
  published
    property About: TUCAboutVar read FAbout write FAbout;
    property Criptografia: TUCCriptografia read FCriptografia
      write FCriptografia default cPadrao;
    property AutoStart: Boolean read FAutoStart write FAutoStart default False;
    property ApplicationID: String read FApplicationID write FApplicationID;
    property ControlRight: TUCControlRight read FControlRight
      write FControlRight;
    // Controle dos formularios
    property User: TUCUser read FUser write FUser;
    property UserProfile: TUCUserProfile read FUserProfile write FUserProfile;
    property UserPasswordChange: TUCUserPasswordChange read FUserPasswordChange
      write FUserPasswordChange;
    property UsersLogged: TUCUsersLogged read FUsersLogged write FUsersLogged;
    property UsersLogoff: TUCUserLogoff read FUsersLogoff write FUsersLogoff;
    property LogControl: TUCLogControl read FLogControl write FLogControl;

    property MailUserControl: TMailUserControl read FMailUserControl
      write SetFMailUserControl;

    property Language: TUCLanguage read fLanguage write SetfLanguage;

    property EncryptKey: Word read FEncrytKey write FEncrytKey;
    property NotAllowedItems: TUCNotAllowedItems read FNotAllowedItems
      write FNotAllowedItems;
    property Login: TUCLogin read FLogin write FLogin;
    property ExtraRights: TUCExtraRights read FExtraRights write SetExtraRights;
    property LoginMode: TUCLoginMode read FLoginMode write FLoginMode
      default lmActive;
    // Tabelas
    property TableUsers: TUCTableUsers read FTableUsers write FTableUsers;
    property TableRights: TUCTableRights read FTableRights write FTableRights;
    property TableUsersLogged: TUCTableUsersLogged read FTableUsersLogged
      write FTableUsersLogged;
    property TableUserDepartment: TUCTableUserDepartment read FTableUserDepartment write FTableUserDepartment;  // Bay Mauri
    property TableUserEmpresa : TUCTableUserEmpresa  read FTableUserEmpresa write FTableUserEmpresa;   // Mauri  Empresa  06/01/2017
    property UserType  : Integer read FUserType write FUserType ;  // mauri 23/06/2012


    property DataConnector: TUCDataConnector read FDataConnector
      write SetDataConnector;
    property CheckValidationKey: Boolean read FCheckValidationKey
      write FCheckValidationKey default False;


    // Eventos
    property OnLogin: TOnLogin read FOnLogin write FOnLogin;
    property OnStartApplication: TNotifyEvent read FOnStartApplication
      write FOnStartApplication;
    property OnLoginSucess: TOnLoginSucess read FOnLoginSucess
      write FOnLoginSucess;
    property OnLoginError: TOnLoginError read FOnLoginError write FOnLoginError;
    property OnApplyRightsMenuIt: TOnApplyRightsMenuItem
      read FOnApplyRightsMenuIt write FOnApplyRightsMenuIt;
    property OnApplyRightsActionIt: TOnApllyRightsActionItem
      read FOnApplyRightsActionIt write FOnApplyRightsActionIt;
    property OnCustomUsersForm: TCustomUserForm read FOnCustomCadUsuarioForm
      write FOnCustomCadUsuarioForm;
    property OnCustomUsersProfileForm: TCustomUserProfileForm
      read FCustomPerfilUsuarioForm write FCustomPerfilUsuarioForm;
    property OnCustomLoginForm: TCustomLoginForm read FCustomLoginForm
      write FCustomLoginForm;
    property OnCustomChangePasswordForm: TCustomUserPasswordChangeForm
      read FCustomTrocarSenhaForm write FCustomTrocarSenhaForm;
    property OnCustomLogControlForm: TCustomLogControlForm
      read FCustomLogControlForm write FCustomLogControlForm;
    property OnCustomInitialMsg: TCustomInitialMessage read FCustomInicialMsg
      write FCustomInicialMsg;
    property OnCustomUserLoggedForm: TCustomUserForm
      read FOnCustomCadUsuarioForm write FOnCustomCadUsuarioForm;
    // Cesar: 13/07/2005
    property OnAddUser: TOnAddUser read FOnAddUser write FOnAddUser;
    property OnChangeUser: TOnChangeUser read FOnChangeUser write FOnChangeUser;
    property OnDeleteUser: TOnDeleteUser read FOnDeleteUser write FOnDeleteUser;
    property OnAddProfile: TOnAddProfile read FOnAddProfile write FOnAddProfile;
    property OnDeleteProfile: TOnDeleteProfile read FOnDeleteProfile
      write FOnDeleteProfile;
    property OnChangePassword: TOnChangePassword read FOnChangePassword
      write FOnChangePassword;
    property OnLogoff: TOnLogoff read FOnLogoff write FOnLogoff;
    property OnAfterLogin: TNotifyEvent read FAfterLogin write FAfterLogin;
  end;

  TUCExtraRightsItem = class(TCollectionItem)
  private
    FFormName: String;
    FCompName: String;
    FCaption: String;
    FGroupName: String;
    procedure SetFormName(const Value: String);
    procedure SetCompName(const Value: String);
    procedure SetCaption(const Value: String);
    procedure SetGroupName(const Value: String);
  protected
    function GetDisplayName: String; override;
  public
  published
    property FormName: String read FFormName write SetFormName;
    property CompName: String read FCompName write SetCompName;
    property Caption: String read FCaption write SetCaption;
    property GroupName: String read FGroupName write SetGroupName;
  end;

  TUCExtraRights = class(TCollection)
  private
    FUCBase: TUserControl;
    function GetItem(Index: Integer): TUCExtraRightsItem;
    procedure SetItem(Index: Integer; Value: TUCExtraRightsItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(UCBase: TUserControl);
    function Add: TUCExtraRightsItem;
    property Items[Index: Integer]: TUCExtraRightsItem read GetItem
      write SetItem; default;
  end;

  TUCVerificaMensagemThread = class(TThread)
  private
    procedure VerNovaMansagem;
  public
    AOwner: TComponent;
  protected
    procedure Execute; override;
  end;

  TUCExecuteThread = class(TThread)
  private
    procedure UCStart;
  public
    AOwner: TComponent;
  protected
    procedure Execute; override;
  end;

  TUCApplicationMessage = class(TComponent)
  private
    FActive: Boolean;
    FReady: Boolean;
    FInterval: Integer;
    FUserControl: TUserControl;
    FVerifThread: TUCVerificaMensagemThread;
    FTableMessages: String;
    procedure SetActive(const Value: Boolean);
    procedure SetUserControl(const Value: TUserControl);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowMessages(Modal: Boolean = True);
    procedure SendAppMessage(ToUser: Integer; Subject, Msg: String);
    procedure DeleteAppMessage(IdMsg: Integer);
    procedure CheckMessages;
  published
    property Active: Boolean read FActive write SetActive;
    property Interval: Integer read FInterval write FInterval;
    property TableMessages: String read FTableMessages write FTableMessages;
    property UserControl: TUserControl read FUserControl write SetUserControl;
  end;

  TUCComponentsVar = String;

  TUCNotAllowed = (naInvisible, naDisabled);

  TUCControls = class(TComponent)
  private
    FGroupName: String;
    FComponents: TUCComponentsVar;
    FUserControl: TUserControl;
    FNotAllowed: TUCNotAllowed;
    function GetAccessType: String;
    function GetActiveForm: String;
    procedure SetGroupName(const Value: String);
    procedure SetUserControl(const Value: TUserControl);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
  public
    destructor Destroy; override;
    procedure ApplyRights;
    procedure LockControls;
    procedure ListComponents(Form: String; List: TStringList);
  published
    property AccessType: String read GetAccessType;
    property ActiveForm: String read GetActiveForm;
    property GroupName: String read FGroupName write SetGroupName;
    property UserControl: TUserControl read FUserControl write SetUserControl;
    property Components: TUCComponentsVar read FComponents write FComponents;
    property NotAllowed: TUCNotAllowed read FNotAllowed write FNotAllowed
      default naInvisible;
  end;

  TUCUsersLogged = class(TPersistent)
    // Cesar: 12/07/2005: classe que armazena os usuarios logados no sistema
  private
    FUserControl: TUserControl;
    FAtive: Boolean;
    procedure AddCurrentUser;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DelCurrentUser;
    procedure CriaTableUserLogado;
    function UsuarioJaLogado(ID: Integer): Boolean;
  published
    property Active: Boolean read FAtive write FAtive default True;
  end;

function Decrypt(const S: ansistring; Key: Word): ansistring;
function Encrypt(const S: ansistring; Key: Word): ansistring;
function MD5Sum(strValor: String): String;

{ TODO -oLuiz -cUpgrade : Mudar o GetLoginName para a Unit principal }

implementation

{$R UCLock.res}

uses
  DBGrids,
  Dialogs,
  LoginWindow_U,
  MsgRecForm_U,
  MsgsForm_U,
  pUCGeral,
  TrocaSenha_U,
  UserPermis_U;

{$IFDEF DELPHI9_UP} {$REGION 'TUSerControl'} {$ENDIF}
{ TUserControl }

constructor TUserControl.Create(AOwner: TComponent);
begin
  inherited;
  FCurrentUser := TUCCurrentUser.Create(Self);
  FControlRight := TUCControlRight.Create(Self);
  FLogin := TUCLogin.Create(Self);
  FLogControl := TUCLogControl.Create(Self);
  FUser := TUCUser.Create(Self);
  FUserProfile := TUCUserProfile.Create(Self);
  FUserPasswordChange := TUCUserPasswordChange.Create(Self);
  FUsersLogged := TUCUsersLogged.Create(Self);
  FUsersLogoff := TUCUserLogoff.Create(Self);
  FUserSettings := TUCUserSettings.Create(Self);
  FNotAllowedItems := TUCNotAllowedItems.Create(Self);
  FExtraRights := TUCExtraRights.Create(Self);
  FTableUsers := TUCTableUsers.Create(Self);
  FTableRights := TUCTableRights.Create(Self);
  FTableUsersLogged := TUCTableUsersLogged.Create(Self);
  FTableUserDepartment:= TUcTableUserDepartment.Create(Self);  // Mauri 03/07/2008
  FTableUserEmpresa    := TUCTableUserEmpresa.Create(Self);    // Mauri  Empresa  06/01/2017

  if csDesigning in ComponentState then
  begin
    with TableUsers do
    begin
      if TableName = '' then
        TableName := RetornaLingua(fLanguage, 'Const_TableUsers_TableName');
      if FieldUserID = '' then
        FieldUserID := RetornaLingua(fLanguage, 'Const_TableUsers_FieldUserID');
      if FieldUserName = '' then
        FieldUserName := RetornaLingua(fLanguage,
          'Const_TableUsers_FieldUserName');
      if FieldLogin = '' then
        FieldLogin := RetornaLingua(fLanguage, 'Const_TableUsers_FieldLogin');
      if FieldPassword = '' then
        FieldPassword := RetornaLingua(fLanguage,
          'Const_TableUsers_FieldPassword');
      if FieldEmail = '' then
        FieldEmail := RetornaLingua(fLanguage, 'Const_TableUsers_FieldEmail');
      if FieldPrivileged = '' then
        FieldPrivileged := RetornaLingua(fLanguage,
          'Const_TableUsers_FieldPrivileged');
      if FieldTypeRec = '' then
        FieldTypeRec := RetornaLingua(fLanguage,
          'Const_TableUsers_FieldTypeRec');
      if FieldProfile = '' then
        FieldProfile := RetornaLingua(fLanguage,
          'Const_TableUsers_FieldProfile');
      if FieldKey = '' then
        FieldKey := RetornaLingua(fLanguage, 'Const_TableUsers_FieldKey');

      if FieldDateExpired = '' then
        FieldDateExpired := RetornaLingua(fLanguage,
          'Const_TableUsers_FieldDateExpired');

      if FieldUserExpired = '' then
        FieldUserExpired := RetornaLingua(fLanguage,
          'Const_TableUser_FieldUserExpired');

      if FieldUserDaysSun = '' then
        FieldUserDaysSun := RetornaLingua(fLanguage,
          'Const_TableUser_FieldUserDaysSun');

      if FieldUserInative = '' then
        FieldUserInative := RetornaLingua(fLanguage,
          'Const_TableUser_FieldUserInative');
                //  Lotacao - Mauri
      if FieldUserDepartment = '' then
        FieldUserDepartment := RetornaLingua(fLanguage, 'Const_TableUser_FieldUserDepartment'); { Mauri Lima }
      //   Tipo Usuario      Mauri
      if FieldUserType = '' then
        FieldUserType := RetornaLingua(fLanguage, 'Const_TableUser_FieldUserType'); { mauri }

      if FieldUserEmpresa = '' then
        FieldUserEmpresa := RetornaLingua(fLanguage, 'Const_TableUser_FieldUserEmpresa'); // Empresa  Mauri Lima  26/01/2017

    end;

    with TableRights do
    begin
      if TableName = '' then
        TableName := RetornaLingua(fLanguage, 'Const_TableRights_TableName');
      if FieldUserID = '' then
        FieldUserID := RetornaLingua(fLanguage,
          'Const_TableRights_FieldUserID');
      if FieldModule = '' then
        FieldModule := RetornaLingua(fLanguage,
          'Const_TableRights_FieldModule');
      if FieldComponentName = '' then
        FieldComponentName := RetornaLingua(fLanguage,
          'Const_TableRights_FieldComponentName');
      if FieldFormName = '' then
        FieldFormName := RetornaLingua(fLanguage,
          'Const_TableRights_FieldFormName');
      if FieldKey = '' then
        FieldKey := RetornaLingua(fLanguage, 'Const_TableRights_FieldKey');
    end;

    with TableUsersLogged do
    begin
      if TableName = '' then
        TableName := RetornaLingua(fLanguage,
          'Const_TableUsersLogged_TableName');
      if FieldLogonID = '' then
        FieldLogonID := RetornaLingua(fLanguage,
          'Const_TableUsersLogged_FieldLogonID');
      if FieldUserID = '' then
        FieldUserID := RetornaLingua(fLanguage,
          'Const_TableUsersLogged_FieldUserID');
      if FieldApplicationID = '' then
        FieldApplicationID := RetornaLingua(fLanguage,
          'Const_TableUsersLogged_FieldApplicationID');
      if FieldMachineName = '' then
        FieldMachineName := RetornaLingua(fLanguage,
          'Const_TableUsersLogged_FieldMachineName');
      if FieldData = '' then
        FieldData := RetornaLingua(fLanguage,
          'Const_TableUsersLogged_FieldData');
    end;

    //  Lotacao - Department  - Mauri
    with TableUserDepartment do
    begin
      if TableName = '' then
        TableName := RetornaLingua(fLanguage, 'Const_TableDepartment_TableName');
      if FieldIDDepartment = '' then
        FieldIDDepartment := RetornaLingua(fLanguage, 'Const_TableDepartment_FieldIDDepartment');
      if FieldNameDepartment = '' then
        FieldNameDepartment := RetornaLingua(fLanguage, 'Const_TableDepartment_FieldNameDepartment');
      if FieldStatusDepartment = '' then
         FieldStatusDepartment := RetornaLingua(fLanguage, 'Const_TableDepartment_FieldStatusDepartment');

    end;

    // Empresa Mauri 26/01/2017

    with TableUserEmpresa do
    begin
      if TableName = '' then
        TableName := RetornaLingua(fLanguage, 'Const_TableEmpresa_TableName');
      if FieldIDEmpresa = '' then
        FieldIDEmpresa := RetornaLingua(fLanguage, 'Const_TableEmpresa_FieldIDEmpresa');
      if FieldNameEmpresa = '' then
        FieldNameEmpresa := RetornaLingua(fLanguage, 'Const_TableEmpresa_FieldNameEmpresa');
    end;

    if LogControl.TableLog = '' then
      LogControl.TableLog := 'UCLog';
    if ApplicationID = '' then
      ApplicationID := 'ProjetoNovo';
    if Login.InitialLogin.User = '' then
      Login.InitialLogin.User := 'admin';
    if Login.InitialLogin.Password = '' then
      Login.InitialLogin.Password := '123mudar';
    if Login.InitialLogin.Email = '' then
      Login.InitialLogin.Email := 'usercontrol@usercontrol.net';

    FLoginMode := lmActive;
    FCriptografia := cPadrao;
    FAutoStart := False;
    FUserProfile.Active := True;
    FLogControl.Active := True;
    FUser.UsePrivilegedField := False;
    FUser.ProtectAdministrator := True;
    FUsersLogged.Active := True;
    NotAllowedItems.MenuVisible := True;
    NotAllowedItems.ActionVisible := True;
  end
  else
  begin
    FControlList := TList.Create;
    FLoginMonitorList := TList.Create;
  end;

  UCSettings.IniSettings(UserSettings);
end;
{$WARNINGS OFF}

procedure TUserControl.Loaded;
var
  Contador: Integer;
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    If UpperCase(Owner.ClassParent.ClassName) = UpperCase('TDataModule') then
      raise Exception.Create
        ('O Componente "TUserControl" n�o pode ser definido em um "TDataModulo"');

    if not Assigned(DataConnector) then
      raise Exception.Create(RetornaLingua(fLanguage, 'MsgExceptConnector'));

    if ApplicationID = '' then
      raise Exception.Create(RetornaLingua(fLanguage, 'MsgExceptAppID'));

    if ((not Assigned(ControlRight.ActionList)) and
      (not Assigned(ControlRight.ActionManager)) and
      (not Assigned(ControlRight.MainMenu)) and
      (not Assigned(ControlRight.ActionMainMenuBar))) then
      raise Exception.Create(Format(RetornaLingua(fLanguage,
        'MsgExceptPropriedade'), ['ControlRight']));

    for Contador := 0 to Pred(Owner.ComponentCount) do
      if Owner.Components[Contador] is TUCSettings then
      begin
        Language := TUCSettings(Owner.Components[Contador]).Language;
        // torna a linguage do UCSETTINGS como padr�o
        FUserSettings.BancoDados := TUCSettings(Owner.Components[Contador])
          .BancoDados;
        ApplySettings(TUCSettings(Owner.Components[Contador]));
      end;

    if Assigned(User.MenuItem) and (not Assigned(User.MenuItem.OnClick)) then
      User.MenuItem.OnClick := ActionCadUser;

    if Assigned(User.Action) and (not Assigned(User.Action.OnExecute)) then
      User.Action.OnExecute := ActionCadUser;

    if ((not Assigned(User.Action)) and (not Assigned(User.MenuItem))) then
      raise Exception.Create(Format(RetornaLingua(fLanguage,
        'MsgExceptPropriedade'), ['User']));

    if Assigned(UserPasswordChange.MenuItem) and
      (not Assigned(UserPasswordChange.MenuItem.OnClick)) then
      UserPasswordChange.MenuItem.OnClick := ActionTrocaSenha;

    if Assigned(UserPasswordChange.Action) and
      (not Assigned(UserPasswordChange.Action.OnExecute)) then
      UserPasswordChange.Action.OnExecute := ActionTrocaSenha;

    if Assigned(UsersLogoff.MenuItem) and
      (not Assigned(UsersLogoff.MenuItem.OnClick)) then
      UsersLogoff.MenuItem.OnClick := ActionLogoff;

    if Assigned(UsersLogoff.Action) and
      (not Assigned(UsersLogoff.Action.OnExecute)) then
      UsersLogoff.Action.OnExecute := ActionLogoff;

    if ((not Assigned(UserPasswordChange.Action)) and
      (not Assigned(UserPasswordChange.MenuItem))) then
      raise Exception.Create(Format(RetornaLingua(fLanguage,
        'MsgExceptPropriedade'), ['UserPasswordChange']));

    if ((not Assigned(UsersLogoff.Action)) and
      (not Assigned(UsersLogoff.MenuItem))) then
      raise Exception.Create(Format(RetornaLingua(fLanguage,
        'MsgExceptPropriedade'), ['UsersLogoff']));

    with TableUsers do
    begin
      if TableName = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable'));
      if FieldUserID = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldUserID***');
      if FieldUserName = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldUserName***');
      if FieldLogin = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldLogin***');
      if FieldPassword = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldPassword***');
      if FieldEmail = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldEmail***');
      if FieldPrivileged = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldPrivileged***');
      if FieldTypeRec = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldTypeRec***');
      if FieldKey = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldKey***');
      if FieldProfile = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldProfile***');

      if FieldDateExpired = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldDateExpired***');

      if FieldUserExpired = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldUserExpired***');

      if FieldUserDaysSun = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldUserDaysSun***');

      if FieldUserInative = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptUsersTable') + #13 +
          #10 + 'FieldUserInative***');

      //  Lotacao - Mauri
      if FieldUserDepartment = '' then
        FieldUserDepartment := RetornaLingua(fLanguage, 'Const_TableUser_FieldUserDepartment'); { Mauri Lima }
      //   Tipo Usuario      Mauri
      if FieldUserType = '' then
        FieldUserType := RetornaLingua(fLanguage, 'Const_TableUser_FieldUserType'); { mauri }

      if FieldUserEmpresa = '' then
        FieldUserEmpresa := RetornaLingua(fLanguage, 'Const_TableUser_FieldUserEmpresa'); // Empresa  Mauri Lima  26/01/2017

    end;

    with TableRights do
    begin
      if TableName = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptRightsTable'));
      if FieldUserID = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptRightsTable') + #13
          + #10 + 'FieldProfile***');
      if FieldModule = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptRightsTable') + #13
          + #10 + 'FieldModule***');
      if FieldComponentName = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptRightsTable') + #13
          + #10 + 'FieldComponentName***');
      if FieldFormName = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptRightsTable') + #13
          + #10 + 'FieldFormName***');
      if FieldKey = '' then
        Exception.Create(RetornaLingua(fLanguage, 'MsgExceptRightsTable') + #13
          + #10 + 'FieldKey***');
    end;

    //  Lotacao - Department  - Mauri
    with TableUserDepartment do
    begin
      if TableName = '' then
        TableName := RetornaLingua(fLanguage, 'Const_TableDepartment_TableName');
      if FieldIDDepartment = '' then
        FieldIDDepartment := RetornaLingua(fLanguage, 'Const_TableDepartment_FieldIDDepartment');
      if FieldNameDepartment = '' then
        FieldNameDepartment := RetornaLingua(fLanguage, 'Const_TableDepartment_FieldSatusDepartment');
      if FieldStatusDepartment = '' then
         FieldStatusDepartment := RetornaLingua(fLanguage, 'Const_TableDepartment_FieldStatusDepartment');

    end;

    // Empresa Mauri 26/01/2017

    with TableUserEmpresa do
    begin
      if TableName = '' then
        TableName := RetornaLingua(fLanguage, 'Const_TableEmpresa_TableName');
      if FieldIDEmpresa = '' then
        FieldIDEmpresa := RetornaLingua(fLanguage, 'Const_TableEmpresa_FieldIDEmpresa');
      if FieldNameEmpresa = '' then
        FieldNameEmpresa := RetornaLingua(fLanguage, 'Const_TableEmpresa_FieldNameEmpresa');
    end;

    if Assigned(OnStartApplication) then
      OnStartApplication(Self);

    // desviar para thread monitorando conexao ao banco qmd 30/01/2004
    if FAutoStart then
    begin
      FThUCRun := TUCExecuteThread.Create(True);
      FThUCRun.AOwner := Self;
      FThUCRun.FreeOnTerminate := True;
      FThUCRun.Resume;
    end;
  end;
end;

procedure TUserControl.ActionCadUser(Sender: TObject);
begin
  ShowNewConfig;
end;

procedure TUserControl.ActionEsqueceuSenha(Sender: TObject);
var
  FDataset, FDataPer: TDataSet;
begin
  FDataset := DataConnector.UCGetSQLDataset('Select * from ' +
    TableUsers.TableName + ' Where ' + TableUsers.FieldLogin + ' = ' +
    QuotedStr(TfrmLoginWindow(FFormLogin).EditUsuario.Text));

  FDataPer := DataConnector.UCGetSQLDataset('select ' + TableUsers.FieldUserName
    + ' from ' + TableUsers.TableName + ' Where ' + TableUsers.FieldUserID +
    ' = ' + FDataset.FieldByName(TableUsers.FieldProfile).AsString);
  try
    if not FDataset.IsEmpty then
      MailUserControl.EnviaEsqueceuSenha
        (FDataset.FieldByName(TableUsers.FieldUserID).AsInteger,
        FDataset.FieldByName(TableUsers.FieldUserName).AsString,
        FDataset.FieldByName(TableUsers.FieldLogin).AsString,
        FDataset.FieldByName(TableUsers.FieldPassword).AsString,
        FDataset.FieldByName(TableUsers.FieldEmail).AsString,
        FDataPer.FieldByName(TableUsers.FieldUserName).AsString)
      // EncryptKey)

    else
      MessageDlg(UserSettings.CommonMessages.InvalidLogin, mtWarning,
        [mbOK], 0);
  finally
    FDataset.Close;
    FDataset.Free;
  end;
end;

procedure TUserControl.ActionTrocaSenha(Sender: TObject);
begin
  if Assigned(OnCustomChangePasswordForm) then
    OnCustomChangePasswordForm(Self, FFormTrocarSenha);

  if FFormTrocarSenha = nil then
    CriaFormTrocarSenha;

  FFormTrocarSenha.ShowModal;
  FreeAndNil(FFormTrocarSenha);
end;

function TUserControl.ExisteUsuario(Login: String): Boolean;
var
  SQLstmt: String;
  DataSet: TDataSet;
begin
  SQLstmt := Format('SELECT %s.%s FROM %s WHERE %s.%s=%s',
    [Self.TableUsers.TableName, Self.TableUsers.FieldLogin,
    Self.TableUsers.TableName, Self.TableUsers.TableName,
    Self.TableUsers.FieldLogin, QuotedStr(Login)]);

  DataSet := Self.DataConnector.UCGetSQLDataset(SQLstmt);
  try
    Result := (DataSet.RecordCount > 0);
  finally
    SysUtils.FreeAndNil(DataSet);
  end;
end;

function TUserControl.GetAllUsers(Names: Boolean): TStringList;
Var
  FDataset: TDataSet;
begin
  Result := TStringList.Create;
  if Names then
    FDataset := DataConnector.UCGetSQLDataset
      ('Select ' + TableUsers.FieldUserName + ' from ' + TableUsers.TableName +
      ' Where ' + TableUsers.FieldTypeRec + ' = ' + QuotedStr('U') +
      ' order by ' + TableUsers.FieldUserName)
  else
    FDataset := DataConnector.UCGetSQLDataset('Select ' + TableUsers.FieldLogin
      + ' from ' + TableUsers.TableName + ' Where ' + TableUsers.FieldTypeRec +
      ' = ' + QuotedStr('U') + ' order by ' + TableUsers.FieldUserName);
  if FDataset.IsEmpty = False then
  Begin
    while FDataset.Eof = False do
    begin
      Result.Add(FDataset.Fields[0].AsString);
      FDataset.Next;
    end;
  End;
  FDataset.Close;
end;

function TUserControl.GetLocalComputerName: String;
var
  Count: DWORD;
  Buffer: String;
begin
  Count := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Buffer, Count);
  if GetComputerName(PChar(Buffer), Count) then
    SetLength(Buffer, StrLen(PChar(Buffer)))
  else
    Buffer := '';
  Result := Buffer;
end;

function TUserControl.GetLocalUserName: String;
var
  Count: DWORD;
  Buffer: String;
begin
  Count := 254;
  SetLength(Buffer, Count);
  if GetUserName(PChar(Buffer), Count) then
    SetLength(Buffer, StrLen(PChar(Buffer)))
  else
    Buffer := '';
  Result := Buffer;
end;

procedure TUserControl.CriaFormTrocarSenha;
begin
  FFormTrocarSenha := TTrocaSenha.Create(Self);
  with Self.UserSettings.ChangePassword do
  begin
    TTrocaSenha(FFormTrocarSenha).FUserControl := Self;
    TTrocaSenha(FFormTrocarSenha).Caption := WindowCaption;
    TTrocaSenha(FFormTrocarSenha).lbDescricao.Caption := LabelDescription;
    TTrocaSenha(FFormTrocarSenha).lbSenhaAtu.Caption := LabelCurrentPassword;
    TTrocaSenha(FFormTrocarSenha).lbNovaSenha.Caption := LabelNewPassword;
    TTrocaSenha(FFormTrocarSenha).lbConfirma.Caption := LabelConfirm;
    TTrocaSenha(FFormTrocarSenha).btGrava.Caption := BtSave;
    TTrocaSenha(FFormTrocarSenha).btCancel.Caption := btCancel;
    TTrocaSenha(FFormTrocarSenha).ForcarTroca := False;
  end;
  TTrocaSenha(FFormTrocarSenha).Position := Self.UserSettings.WindowsPosition;

  TTrocaSenha(FFormTrocarSenha).btGrava.OnClick := ActionTSBtGrava;
  if CurrentUser.Password = '' then
    TTrocaSenha(FFormTrocarSenha).EditAtu.Enabled := False;
end;

procedure TUserControl.ActionTSBtGrava(Sender: TObject);
var
  AuxPass: String;
begin
  { Pelo que eu analizei, a grava��o da senha no Banco de Dados e feita criptografada
    Qdo a criptografia e padr�o, a funcao RegistraCurrentUser descriptografa a senha atual
    agora quando criptografia e MD5SUM, devemos criptografar a senha atual vinda do formulario de
    troca de senha para podemos comparar com a senha atual da classe TUCCurrentUser
    Modifica��o Feita por Vicente Barros Leonel
  }
  case Self.Criptografia of
    cPadrao:
      AuxPass := TTrocaSenha(FFormTrocarSenha).EditAtu.Text;
    cMD5:
      AuxPass := MD5Sum(TTrocaSenha(FFormTrocarSenha).EditAtu.Text);
  end;

  if CurrentUser.Password <> AuxPass then
  begin
    MessageDlg(UserSettings.CommonMessages.ChangePasswordError.
      InvalidCurrentPassword, mtWarning, [mbOK], 0);
    TTrocaSenha(FFormTrocarSenha).EditAtu.SetFocus;
    Exit;
  end;

  if TTrocaSenha(FFormTrocarSenha).EditNova.Text <>
    TTrocaSenha(FFormTrocarSenha).EditConfirma.Text then
  begin
    MessageDlg(UserSettings.CommonMessages.ChangePasswordError.
      InvalidNewPassword, mtWarning, [mbOK], 0);
    TTrocaSenha(FFormTrocarSenha).EditNova.SetFocus;
    Exit;
  end;

  case Self.Criptografia of
    cPadrao:
      AuxPass := TTrocaSenha(FFormTrocarSenha).EditNova.Text;
    cMD5:
      AuxPass := MD5Sum(TTrocaSenha(FFormTrocarSenha).EditNova.Text);
  end;

  if AuxPass = CurrentUser.Password then
  begin
    MessageDlg(UserSettings.CommonMessages.ChangePasswordError.NewEqualCurrent,
      mtWarning, [mbOK], 0);
    TTrocaSenha(FFormTrocarSenha).EditNova.SetFocus;
    Exit;
  end;

  if (UserPasswordChange.ForcePassword) and
    (TTrocaSenha(FFormTrocarSenha).EditNova.Text = '') then
  begin
    MessageDlg(UserSettings.CommonMessages.ChangePasswordError.PasswordRequired,
      mtWarning, [mbOK], 0);
    TTrocaSenha(FFormTrocarSenha).EditNova.Text;
    Exit;
  end;

  if Length(TTrocaSenha(FFormTrocarSenha).EditNova.Text) < UserPasswordChange.MinPasswordLength
  then
  begin
    MessageDlg(Format(UserSettings.CommonMessages.ChangePasswordError.
      MinPasswordLength, [UserPasswordChange.MinPasswordLength]), mtWarning,
      [mbOK], 0);
    TTrocaSenha(FFormTrocarSenha).EditNova.SetFocus;
    Exit;
  end;

  if Pos(LowerCase(TTrocaSenha(FFormTrocarSenha).EditNova.Text),
    'abcdeasdfqwerzxcv1234567890321654987teste' +
    LowerCase(CurrentUser.UserName) + LowerCase(CurrentUser.UserLogin)) > 0 then
  begin
    MessageDlg(UserSettings.CommonMessages.ChangePasswordError.
      InvalidNewPassword, mtWarning, [mbOK], 0);
    TTrocaSenha(FFormTrocarSenha).EditNova.SetFocus;
    Exit;
  end;

  if Assigned(OnChangePassword) then
    OnChangePassword(Self, CurrentUser.UserID, CurrentUser.UserLogin,
      CurrentUser.Password, TTrocaSenha(FFormTrocarSenha).EditNova.Text);

  ChangePassword(CurrentUser.UserID, TTrocaSenha(FFormTrocarSenha)
    .EditNova.Text);

  case Self.Criptografia of
    cPadrao:
      CurrentUser.Password := TTrocaSenha(FFormTrocarSenha).EditNova.Text;
    cMD5:
      CurrentUser.Password :=
        MD5Sum(TTrocaSenha(FFormTrocarSenha).EditNova.Text);
  end;

  if CurrentUser.Password = '' then
    MessageDlg(Format(UserSettings.CommonMessages.BlankPassword,
      [CurrentUser.UserLogin]), mtInformation, [mbOK], 0)
  else
    MessageDlg(UserSettings.CommonMessages.PasswordChanged, mtInformation,
      [mbOK], 0);

  if TTrocaSenha(FFormTrocarSenha).ForcarTroca = True then
    TTrocaSenha(FFormTrocarSenha).ForcarTroca := False;

  if (Assigned(FMailUserControl)) and (FMailUserControl.SenhaTrocada.Ativo) then
    with CurrentUser do
      try
        FMailUserControl.EnviaEmailSenhaTrocada(UserName, CurrentUser.UserLogin,
          TTrocaSenha(FFormTrocarSenha).EditNova.Text, Email, '', EncryptKey);
      except
        on e: Exception do
          Log(e.Message, 2);
      end;
  CurrentUser.PassLivre := TTrocaSenha(FFormTrocarSenha).EditNova.Text;
  TTrocaSenha(FFormTrocarSenha).Close;
end;

procedure TUserControl.SetUserSettings(const Value: TUCUserSettings);
begin
  UserSettings := Value;
end;

procedure TUserControl.SetfrmLoginWindow(Form: TCustomForm);
begin
  with UserSettings.Login, Form as TfrmLoginWindow do
  begin
    Caption := WindowCaption;
    LbUsuario.Caption := LabelUser;
    LbSenha.Caption := LabelPassword;
    btOK.Caption := UserSettings.Login.btOK;
    BtCancela.Caption := btCancel;

    if Assigned(FMailUserControl) then
    begin
      lbEsqueci.Visible := FMailUserControl.EsqueceuSenha.Ativo;
      lbEsqueci.Caption := FMailUserControl.EsqueceuSenha.LabelLoginForm;
    end;
    StatusBar.Visible := Login.FMaxLoginAttempts > 0;
    StatusBar.Panels[1].Text := '0';
    StatusBar.Panels[3].Text := IntToStr(Login.FMaxLoginAttempts);
  end;
end;

procedure TUserControl.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) then
  begin
    if AComponent = User.MenuItem then
      User.MenuItem := nil;
    if AComponent = User.Action then
      User.Action := nil;
    if AComponent = UserPasswordChange.Action then
      UserPasswordChange.Action := nil;
    if AComponent = UserPasswordChange.MenuItem then
      UserPasswordChange.MenuItem := nil;

    if AComponent = UsersLogoff.Action then
      UsersLogoff.Action := nil;
    if AComponent = UsersLogoff.MenuItem then
      UsersLogoff.MenuItem := nil;

    if AComponent = ControlRight.MainMenu then
      ControlRight.MainMenu := nil;
    if AComponent = ControlRight.ActionList then
      ControlRight.ActionList := nil;
    { .$IFDEF UCACTMANAGER }
    if AComponent = ControlRight.ActionManager then
      ControlRight.ActionManager := nil;
    if AComponent = ControlRight.ActionMainMenuBar then
      ControlRight.ActionMainMenuBar := nil;
    { .$ENDIF }

    if AComponent = FDataConnector then
    begin
      if CurrentUser.UserID <> 0 then
        UsersLogged.DelCurrentUser;
      FDataConnector := nil;
    end;

    if AComponent = FMailUserControl then
      FMailUserControl := nil;

  end;
  inherited Notification(AComponent, AOperation);
end;

procedure TUserControl.ActionLogoff(Sender: TObject);
begin
  Self.Logoff;
end;

procedure TUserControl.Log(Msg: String; Level: Integer);
begin
  // Adicionado ao log a identifica��o da Aplica��o
  if not LogControl.Active then
    Exit;

  if Assigned(DataConnector) then
    DataConnector.UCExecSQL('INSERT INTO ' + LogControl.TableLog +
      '(APPLICATIONID, IDUSER, MSG, DATA, NIVEL) VALUES ( ' +
      QuotedStr(Self.ApplicationID) + ', ' + IntToStr(CurrentUser.UserID) + ', '
      + QuotedStr(Copy(Msg, 1, 250)) + ', ' +
      QuotedStr(FormatDateTime('YYYYMMDDhhmmss', now)) + ', ' +
      IntToStr(Level) + ')');
end;

procedure TUserControl.RegistraCurrentUser(Dados: TDataSet; Pass: String);
var
  SQLstmt: String;
begin
  with CurrentUser do
  begin
    UserID := Dados.FieldByName(TableUsers.FieldUserID).AsInteger;
    UserName := Dados.FieldByName(TableUsers.FieldUserName).AsString;
    UserLogin := Dados.FieldByName(TableUsers.FieldLogin).AsString;
    DateExpiration := StrToDateDef(Dados.FieldByName(TableUsers.FieldDateExpired).AsString, now);
    UserNotExpired := Dados.FieldByName(TableUsers.FieldUserExpired).AsInteger = -1;
    UserDaysExpired := Dados.FieldByName(TableUsers.FieldUserDaysSun).AsInteger;
    PassLivre := Pass;
    UserDepartment  := Dados.FieldByName(TableUsers.FieldUserDepartment).AsString;
     // mauri
    UserEmpresa     := Dados.FieldByName(TableUsers.FieldUserEmpresa).AsInteger;
    // Empresa Mauri 26/01/2017
    UserType        :=Dados.FieldByName(TableUsers.FieldUserType).AsInteger;
    // mauri

    case Self.Criptografia of
      cPadrao:
        Password := Decrypt(Dados.FieldByName(TableUsers.FieldPassword)
          .AsString, EncryptKey);
      cMD5:
        Password := Dados.FieldByName(TableUsers.FieldPassword).AsString;
    end;

    Email := Dados.FieldByName(TableUsers.FieldEmail).AsString;
    Privileged := StrToBool(Dados.FieldByName(TableUsers.FieldPrivileged)
      .AsString);
    Profile := Dados.FieldByName(TableUsers.FieldProfile).AsInteger;

    SQLstmt := Format('SELECT %s AS ObjName,' + ' %s AS UCKey,' +
      ' %s AS UserID' + ' FROM %s' + ' WHERE %s = %s AND %s = %s',
      [TableRights.FieldComponentName, TableRights.FieldKey,
      TableRights.FieldUserID, TableRights.TableName, TableRights.FieldUserID,
      IntToStr(UserID), TableRights.FieldModule, QuotedStr(ApplicationID)]);

    if Assigned(PerfilUsuario) then
      PerfilUsuario := Nil;
    PerfilUsuario := DataConnector.UCGetSQLDataset(SQLstmt);

    // Aplica Permissoes do Perfil do usuario
    if CurrentUser.Profile > 0 then
    begin
      SQLstmt := Format('SELECT %s AS ObjName,' + ' %s AS UCKey,' +
        ' %s AS UserID' + ' FROM %s' + ' WHERE %s = %s AND %s = %s',
        [TableRights.FieldComponentName, TableRights.FieldKey,
        TableRights.FieldUserID, TableRights.TableName, TableRights.FieldUserID,
        IntToStr(CurrentUser.Profile), TableRights.FieldModule,
        QuotedStr(ApplicationID)]);

      if Assigned(PerfilGrupo) then
        PerfilGrupo := Nil;
      PerfilGrupo := DataConnector.UCGetSQLDataset(SQLstmt);

    end
    else
      PerfilGrupo := Nil;

    if Assigned(OnLoginSucess) then
      OnLoginSucess(Self, UserID, UserLogin, UserName, Password, Email,
        Privileged);
  end;

  // Cesar: 13/07/2005
  if (CurrentUser.UserID <> 0) then
    UsersLogged.AddCurrentUser;

  ApplyRightsUCControlMonitor;
  NotificationLoginMonitor;

  if ((FLogin.fDateExpireActive = True) and (Date > CurrentUser.DateExpiration)
    and (CurrentUser.UserNotExpired = False)) then
  begin
    MessageDlg(UserSettings.CommonMessages.PasswordExpired, mtInformation,
      [mbOK], 0);

    if FFormTrocarSenha = nil then
      CriaFormTrocarSenha;
    TTrocaSenha(FFormTrocarSenha).ForcarTroca := True;
    FFormTrocarSenha.ShowModal;
    FreeAndNil(FFormTrocarSenha);
    { Incrementa a Data de Expira��o em x dias ap�s a troca de senha }
    CurrentUser.DateExpiration := CurrentUser.DateExpiration +
      CurrentUser.UserDaysExpired;
  end;
end;

procedure TUserControl.TryAutoLogon;
begin
  if VerificaLogin(Login.AutoLogin.User, Login.AutoLogin.Password) <> 0 then
  begin
    if Login.AutoLogin.MessageOnError then
      MessageDlg(UserSettings.CommonMessages.AutoLogonError, mtWarning,
        [mbOK], 0);
    ShowLogin;
  end;
end;

function TUserControl.VerificaLogin(User, Password: String;
  SoVerificarUsuarioAdmin: Boolean = False): Integer; // Boolean;
var
  Senha: String;
  Key: String;
  SQLstmt: String;
  DataSet: TDataSet;
  VerifKey: String;
begin
  case Self.Criptografia of
    cPadrao:
      Senha := TableUsers.FieldPassword + ' = ' +
        QuotedStr(Encrypt(Password, EncryptKey));
    cMD5:
      Senha := TableUsers.FieldPassword + ' = ' + QuotedStr(MD5Sum(Password));
  end;
  if SoVerificarUsuarioAdmin = False then
    SQLstmt := 'SELECT * FROM ' + TableUsers.TableName + ' WHERE ' +
      TableUsers.FieldLogin + ' = ' + QuotedStr(User) + ' AND ' + Senha
  else
    SQLstmt := 'SELECT * FROM ' + TableUsers.TableName + ' WHERE ' +
      TableUsers.FieldLogin + ' = ' + QuotedStr(User) + ' AND ' + Senha +
      ' AND ' + TableUsers.FieldPrivileged + ' = ' +
      BoolToStr(SoVerificarUsuarioAdmin);

  DataSet := DataConnector.UCGetSQLDataset(SQLstmt);
  with DataSet do
    try
      if not IsEmpty then
      begin
        case Self.Criptografia of
          cPadrao:
            begin
              Key := Decrypt(DataSet.FieldByName(TableUsers.FieldKey).AsString,
                EncryptKey);
              VerifKey := DataSet.FieldByName(TableUsers.FieldUserID).AsString +
                DataSet.FieldByName(TableUsers.FieldLogin).AsString +
                Decrypt(DataSet.FieldByName(TableUsers.FieldPassword).AsString,
                EncryptKey);
            end;
          cMD5:
            begin
              Key := DataSet.FieldByName(TableUsers.FieldKey).AsString;
              VerifKey := MD5Sum(DataSet.FieldByName(TableUsers.FieldUserID)
                .AsString + DataSet.FieldByName(TableUsers.FieldLogin).AsString
                + DataSet.FieldByName(TableUsers.FieldPassword).AsString);
            end;
        end;
        if Key <> VerifKey then
        begin
          Result := 1;
          if Assigned(OnLoginError) then
            OnLoginError(Self, User, Password);
        end
        else
        begin
          if (DataSet.FieldByName(TableUsers.FieldUserInative).AsInteger = 0)
          then
          begin
            if SoVerificarUsuarioAdmin = False then
              RegistraCurrentUser(DataSet, Password);
            Result := 0;
          end
          else
            Result := 2;
        end;
      end
      else
      begin
        Result := 1;
        if Assigned(OnLoginError) then
          OnLoginError(Self, User, Password);
      end;
    finally
      Close;
      Free;
    end;
end;

procedure TUserControl.Logoff;
begin
  if Assigned(OnLogoff) then
    OnLogoff(Self, CurrentUser.UserID);

  LockControlsUCControlMonitor;
  UsersLogged.DelCurrentUser;
  CurrentUser.UserID := 0;
  if LoginMode = lmActive then
    ShowLogin;
  ApplyRights;
end;

function TUserControl.AddUser(Login, Password, Name, Mail: String; Profile, UserExpired, DaysExpired: Integer; PrivUser: Boolean;
                              Department: String; UserType : Integer; UserEmpresa: Integer): Integer;  // MAuri
var
  Key: String;
  SQLstmt: String;
  Senha: String;
begin
  case Self.Login.CharCasePass of
    ecNormal:
      ;
    ecUpperCase:
      Password := UpperCase(Password);
    ecLowerCase:
      Password := LowerCase(Password);
  end;

  case Self.Login.CharCaseUser of
    ecNormal:
      ;
    ecUpperCase:
      Login := UpperCase(Login);
    ecLowerCase:
      Login := LowerCase(Login);
  end;

  with DataConnector.UCGetSQLDataset('Select Max(' + TableUsers.FieldUserID +
    ') as IdUser from ' + TableUsers.TableName) do
  begin
    Result := StrToIntDef(FieldByName('idUser').AsString, 0) + 1;
    Close;
    Free;
  end;

  case Self.Criptografia of
    cPadrao:
      begin
        Key := Encrypt(IntToStr(Result) + Login + Password, EncryptKey);
        Senha := Encrypt(Password, EncryptKey);
      end;
    cMD5:
      begin
        Key := MD5Sum(IntToStr(Result) + Login + MD5Sum(Password));
        Senha := MD5Sum(Password);
      end;
  end;

  with TableUsers do
  begin

    SQLStmt := Format('INSERT INTO  %s( %s, %s, %s, %s, %s, %s, %s, %s, %s , %s , %s , %s , %s, %s, %s, %s ) '+  // Mauri
                              ' VALUES( %d, %s, %s, %s, %s, %s, %d, %s, %s , %s , %d , %d , %s, %s, %d, %d )',   // Mauri
      [TableName,
      FieldUserID,
      FieldUserName,
      FieldLogin,
      FieldPassword,
      FieldEmail,
      FieldPrivileged,
      FieldProfile,
      FieldTypeRec,
      FieldKey,
      FieldDateExpired,
      FieldUserExpired,
      FieldUserDaysSun,
      FieldUserInative,
      FieldUserDepartment,     { Mauri }
      FieldUserType ,          { Mauri }
      FieldUserEmpresa,        { Mauri Empresa 26/01/2017 }
      Result,                   // Id do Usuario
      QuotedStr(Name),
      QuotedStr(Login),
      QuotedStr(Senha),
      QuotedStr(Mail),
      BoolToStr(PrivUser),
      Profile,
      QuotedStr('U'),
      QuotedStr(Key),
      QuotedStr(FormatDateTime('dd/mm/yyyy', Date + FLogin.fDaysOfSunExpired)),
      UserExpired,
      DaysExpired,
       '0',
      QuotedStr(Department),  { Mauri}
      UserType,     //   Mauri
      UserEmpresa   { Mauri Empresa 26/01/2017 }
      ]);


    if Assigned(DataConnector) then
      DataConnector.UCExecSQL(SQLstmt);
  end;

  if Assigned(OnAddUser) then
    OnAddUser(Self, Login, Password, Name, Mail, Profile, Privuser);
end;

procedure TUserControl.ChangePassword(IdUser: Integer; NewPassword: String);
var
  Login: String;
  Senha: String;
  Key: String;
  SQLstmt: String;
begin
  inherited;

  case Self.Login.CharCasePass of
    ecNormal:
      ;
    ecUpperCase:
      NewPassword := UpperCase(NewPassword);
    ecLowerCase:
      NewPassword := LowerCase(NewPassword);
  end;

  SQLstmt := 'Select ' + TableUsers.FieldLogin + ' as login, ' +
    TableUsers.FieldPassword + ' as senha from ' + TableUsers.TableName + ' ' +
    'where ' + TableUsers.FieldUserID + ' = ' + IntToStr(IdUser);

  with DataConnector.UCGetSQLDataset(SQLstmt) do
  begin
    Login := FieldByName('Login').AsString;
    case Self.Criptografia of
      cPadrao:
        begin
          Key := Encrypt(IntToStr(IdUser) + Login + NewPassword, EncryptKey);
          Senha := Decrypt(FieldByName('Senha').AsString, EncryptKey);
        end;
      cMD5:
        begin
          Key := MD5Sum(IntToStr(IdUser) + Login + MD5Sum(NewPassword));
          Senha := FieldByName('Senha').AsString;
        end;
    end;

    Close;
    Free;
  end;

  case Self.Criptografia of
    cPadrao:
      SQLstmt := 'Update ' + TableUsers.TableName + ' Set ' +
        TableUsers.FieldPassword + ' = ' +
        QuotedStr(Encrypt(NewPassword, EncryptKey)) + ', ' + TableUsers.FieldKey
        + ' = ' + QuotedStr(Key) + ', ' + TableUsers.FieldDateExpired + ' = ' +
        QuotedStr(FormatDateTime('dd/mm/yyyy',
        Date + FCurrentUser.UserDaysExpired)) + ' Where ' +
        TableUsers.FieldUserID + ' = ' + IntToStr(IdUser);

    cMD5:
      SQLstmt := 'Update ' + TableUsers.TableName + ' Set ' +
        TableUsers.FieldPassword + ' = ' + QuotedStr(MD5Sum(NewPassword)) + ', '
        + TableUsers.FieldKey + ' = ' + QuotedStr(Key) + ', ' +
        TableUsers.FieldDateExpired + ' = ' +
        QuotedStr(FormatDateTime('dd/mm/yyyy',
        Date + FCurrentUser.UserDaysExpired)) + ' Where ' +
        TableUsers.FieldUserID + ' = ' + IntToStr(IdUser);
  end;

  if Assigned(DataConnector) then
    DataConnector.UCExecSQL(SQLstmt);

  if Assigned(OnChangePassword) then
    OnChangePassword(Self, IdUser, Login, Senha, NewPassword);
end;

procedure TUserControl.ChangeUser(IDUser: Integer; Login, Name, Mail: String; Profile,
          UserExpired, UserDaysSun, Status: Integer; PrivUser: Boolean; Department : String; UserType : Integer; UserEmpresa: Integer) ;
          //  Mauri
var
  Key: String;
  Password: String;
  SQLstmt: String;
begin
  SQLstmt := 'SELECT ' + TableUsers.FieldPassword + ' AS SENHA FROM ' +
    TableUsers.TableName + ' WHERE ' + TableUsers.FieldUserID + ' = ' +
    IntToStr(IdUser);

  with DataConnector.UCGetSQLDataset(SQLstmt) do
  begin
    case Self.Criptografia of
      cPadrao:
        begin
          Password := Decrypt(FieldByName('Senha').AsString, EncryptKey);
          Key := Encrypt(IntToStr(IdUser) + Login + Password, EncryptKey);
        end;
      cMD5:
        begin
          Password := FieldByName('Senha').AsString;
          Key := MD5Sum(IntToStr(IdUser) + Login + Password);
        end;
    end;
    Close;
    Free;
  end;

  with TableUsers do
    if Assigned(DataConnector) then
      DataConnector.UCExecSQL('Update ' + TableName + ' Set ' + FieldUserName +
        ' = ' + QuotedStr(Name) + ', ' + FieldLogin + ' = ' + QuotedStr(Login) +
        ', ' + FieldEmail + ' = ' + QuotedStr(Mail) + ', ' + FieldPrivileged +
        ' = ' + BoolToStr(Privuser) + ', ' + FieldProfile + ' = ' +
        IntToStr(Profile) + ', ' + FieldKey + ' = ' + QuotedStr(Key) + ', ' +
        FieldUserExpired + ' = ' + IntToStr(UserExpired) + ' , ' +
        FieldUserDaysSun + ' = ' + IntToStr(UserDaysSun) + ' , ' +
        FieldUserInative + ' = ' + IntToStr(Status) +  ' , ' +
        FieldUserDepartment + ' = ' + QuotedStr(Department) +' , '+      //  Lotacao Mauri
        FieldUserType + ' = '+ IntToStr(UserType)+' , '+                 // Tipo de Usaurio  Mauri
        FieldUserEmpresa +  '= ' + IntToStr(UserEmpresa)+                // Empresa Mauri 26/01/2017
        ' where ' + FieldUserID +        ' = ' + IntToStr(IdUser));
  if Assigned(OnChangeUser) then
    OnChangeUser(Self, IdUser, Login, Name, Mail, Profile, Privuser,  Department, UserType , UserEmPresa);  // Mauri;
                                                                      // Empresa 26/01/2017
end;

procedure TUserControl.CriaTabelaMsgs(const TableName: String);
var
   SQLStmt : string;
begin
  if Assigned(DataConnector) then
   begin
      SQLStmt := 'CREATE TABLE ' + TableName + ' ( ' + 'IdMsg ' +
      UserSettings.Type_Int + ' NOT NULL  , ' + 'UsrFrom ' + UserSettings.Type_Int + ' , '
      + 'UsrTo   ' + UserSettings.Type_Int + ' , ' + 'Subject ' +
      UserSettings.Type_VarChar + '(50),' + 'Msg     ' +
      UserSettings.Type_VarChar + '(255),' + 'DtSend  ' +
      UserSettings.Type_VarChar + '(12),' + 'DtReceive  ' +
      UserSettings.Type_VarChar + '(12) )';
      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(SQLstmt);
       //     Criar Chave Primaria para a Tabela     Mauri 06/05/2017
       SQLStmt := Format('ALTER TABLE %s ADD CONSTRAINT PK_%s PRIMARY KEY (IdMsg)',
                          [TableName, TableName, UserSettings.Type_Int]);

      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(SQLstmt);


   end;
end;

destructor TUserControl.Destroy;
begin
  if not(csDesigning in ComponentState) then
    FUsersLogged.DelCurrentUser;

  FCurrentUser.Free;
  FControlRight.Free;
  FLogin.Free;
  FLogControl.Free;
  FUser.Free;
  FUserProfile.Free;
  FUserPasswordChange.Free;
  FUsersLogoff.Free;
  FUsersLogged.Free;
  FUserSettings.Free;
  FNotAllowedItems.Free;
  FExtraRights.Free;
  FTableUsers.Free;
  FTableRights.Free;
  FTableUsersLogged.Free;
  FTableUserDepartment.Free;     // Mauri
  FTableUserEmpresa.Free;  // Mauri

  if Assigned(FControlList) then
    FControlList.Free;

  if Assigned(FLoginMonitorList) then
    FLoginMonitorList.Free;

  inherited Destroy;
end;

procedure TUserControl.SetExtraRights(Value: TUCExtraRights);
begin

end;

procedure TUserControl.HideField(Sender: TField; var Text: String;
  DisplayText: Boolean);
begin
  Text := '(Campo Bloqueado)';
end;

procedure TUserControl.StartLogin;
begin
  CurrentUser.UserID := 0;
  ShowLogin;
  ApplyRights;
end;

procedure TUserControl.Execute;
begin
  if Assigned(FThUCRun) then
    FThUCRun.Terminate;
  if not Assigned(DataConnector) then
    Exit;

  try

    if not DataConnector.UCFindTable(FTableRights.TableName) then
      CriaTabelaRights;

    if not DataConnector.UCFindTable(FTableRights.TableName + 'EX') then
      CriaTabelaRights(True); // extra rights table

    if not DataConnector.UCFindTable(TableUsersLogged.TableName) then
      UsersLogged.CriaTableUserLogado;

    if LogControl.Active then
    Begin
      if not DataConnector.UCFindTable(LogControl.TableLog) then
        CriaTabelaLog;
    End;

    CriaTabelaDepartment(DataConnector.UCFindTable(FTableUserDepartment.TableName)); //  Mauri Lima
    CriaTabelaEmpresa(DataConnector.UCFindTable(FTableUserEmpresa.TableName)); //  Empresa Mauri Lima 26/01/2017


    CriaTabelaUsuarios(DataConnector.UCFindTable(FTableUsers.TableName));

    // Atualizador de Versoes
    AtualizarVersao;

    // testa campo KEY qmd 28-02-2005
    if FCheckValidationKey then
      DoCheckValidationField;

  finally
    if LoginMode = lmActive then
      if not Login.AutoLogin.Active then
        ShowLogin
      else
        TryAutoLogon;
    ApplyRights;
  end;
end;

procedure TUserControl.AtualizarVersao;
var
  Sql: String;
  DataSet: TDataSet;
begin
  { Procura o campo FieldUserDaysSun na tabela de usuarios se o mesmo n�o existi cria }
  try
    Sql := Format('select * from %s', [FTableUsers.TableName]);
    DataSet := DataConnector.UCGetSQLDataset(Sql);

    if DataSet.FindField(FTableUsers.FieldDateExpired) = nil then
    begin
      Sql := Format('alter table %s add %s %s(10)',
        [FTableUsers.TableName, FTableUsers.FieldDateExpired,
        UserSettings.Type_Char]);

      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(Sql);
      Sql := Format('update %s set %s = %s where %s = ''U''',
        [FTableUsers.TableName, FTableUsers.FieldDateExpired,
        QuotedStr(FormatDateTime('dd/mm/yyyy', Date + FLogin.fDaysOfSunExpired)
        ), FTableUsers.FieldTypeRec]);
      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(Sql);
    end;

    if DataSet.FindField(FTableUsers.FieldUserExpired) = nil then
    begin
      Sql := Format('alter table %s add %s %s',
        [FTableUsers.TableName, FTableUsers.FieldUserExpired,
        UserSettings.Type_Int]);
      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(Sql);
      Sql := Format('update %s set %s = 1 where %s = ''U''',
        [FTableUsers.TableName, FTableUsers.FieldUserExpired,
        FTableUsers.FieldTypeRec]);
      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(Sql);
    end;

    if DataSet.FindField(FTableUsers.FieldUserDaysSun) = nil then
    begin // Cria campo  setado no FieldUserDaysSun na tabela de usuarios
      Sql := Format('alter table %s add %s %s',
        [FTableUsers.TableName, FTableUsers.FieldUserDaysSun,
        UserSettings.Type_Int]);
      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(Sql);
      Sql := Format('update %s set %s = 30 where %s = ''U''',
        [FTableUsers.TableName, FTableUsers.FieldUserDaysSun,
        FTableUsers.FieldTypeRec]);
      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(Sql);
    end;

    if DataSet.FindField(FTableUsers.FieldUserInative) = nil then
    begin // Cria campo  setado no FieldUserInative na tabela de usuarios
      Sql := Format('alter table %s add %s %s',
        [FTableUsers.TableName, FTableUsers.FieldUserInative,
        UserSettings.Type_Int]);
      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(Sql);

      Sql := Format('update %s set %s = 0 where %s = ''U''',
        [FTableUsers.TableName, FTableUsers.FieldUserInative,
        FTableUsers.FieldTypeRec]);
      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(Sql);
    end;

  finally
    FreeAndNil(DataSet);
  end;

end;

procedure TUserControl.DoCheckValidationField;
var
  TempDS: TDataSet;
  Key: String;
  Login: String;
  Senha: String;
  UserID: Integer;
begin
  // verifica tabela de usuarios
  TempDS := DataConnector.UCGetSQLDataset('SELECT * FROM ' +
    TableUsers.TableName);

  if TempDS.FindField(TableUsers.FieldKey) = nil then
  begin
    if Assigned(DataConnector) then
      DataConnector.UCExecSQL('ALTER TABLE ' + TableUsers.TableName + ' ADD ' +
        TableUsers.FieldKey + ' ' + UserSettings.Type_VarChar + ' (255)');
    TempDS.First;
    with TempDS do
      while not Eof do
      begin
        UserID := TempDS.FieldByName(TableUsers.FieldUserID).AsInteger;
        Login := TempDS.FieldByName(TableUsers.FieldLogin).AsString;
        case Self.Criptografia of
          cPadrao:
            begin
              Senha := Decrypt(TempDS.FieldByName(TableUsers.FieldPassword)
                .AsString, EncryptKey);
              Key := Encrypt(IntToStr(UserID) + Login + Senha, EncryptKey);
            end;
          cMD5:
            begin
              Senha := TempDS.FieldByName(TableUsers.FieldPassword).AsString;
              Key := MD5Sum(IntToStr(UserID) + Login + Senha);
            end;
        end;
        if Assigned(DataConnector) then
          DataConnector.UCExecSQL(Format('UPDATE %s SET %s = %s WHERE %s = %d',
            [TableUsers.TableName, TableUsers.FieldKey, QuotedStr(Key),
            TableUsers.FieldUserID, TempDS.FieldByName(TableUsers.FieldUserID)
            .AsInteger]));
        Next;
      end;
  end;

  TempDS.Close;
  FreeAndNil(TempDS);

  // verifica tabela de permissoes
  TempDS := DataConnector.UCGetSQLDataset('SELECT * FROM ' +
    TableRights.TableName);

  if TempDS.FindField(TableRights.FieldKey) = nil then
  begin
    if Assigned(DataConnector) then
      DataConnector.UCExecSQL('ALTER TABLE ' + TableRights.TableName + ' ADD ' +
        TableUsers.FieldKey + ' ' + UserSettings.Type_VarChar + ' (255)');
    TempDS.First;
    with TempDS do
      while not Eof do
      begin
        UserID := TempDS.FieldByName(TableRights.FieldUserID).AsInteger;
        Login := TempDS.FieldByName(TableRights.FieldComponentName).AsString;
        case Self.Criptografia of
          cPadrao:
            Key := Encrypt(IntToStr(UserID) + Login, EncryptKey);
          cMD5:
            Key := MD5Sum(IntToStr(UserID) + Login);
        end;
        if Assigned(DataConnector) then
          DataConnector.UCExecSQL
            (Format('UPDATE %s SET %s = %s where %s = %d and %s = %s and %s = %s',
            [TableRights.TableName, TableRights.FieldKey, QuotedStr(Key),
            TableRights.FieldUserID, TempDS.FieldByName(TableRights.FieldUserID)
            .AsInteger, TableRights.FieldModule, QuotedStr(ApplicationID),
            TableRights.FieldComponentName, QuotedStr(Login)]));
        Next;
      end;
  end;
  TempDS.Close;
  FreeAndNil(TempDS);

  // verifica tabela de permissoes extendidas
  TempDS := DataConnector.UCGetSQLDataset('SELECT * FROM ' +
    TableRights.TableName + 'EX');
  if TempDS.FindField(TableRights.FieldKey) = nil then
  begin
    if Assigned(DataConnector) then
      DataConnector.UCExecSQL('ALTER TABLE ' + TableRights.TableName + 'EX ADD '
        + TableUsers.FieldKey + '' + UserSettings.Type_VarChar + ' (255)');
    TempDS.First;
    with TempDS do
      while not Eof do
      begin
        UserID := TempDS.FieldByName(TableRights.FieldUserID).AsInteger;
        Login := TempDS.FieldByName(TableRights.FieldComponentName).AsString;
        // componentname
        Senha := TempDS.FieldByName(TableRights.FieldFormName).AsString;
        // formname
        case Self.Criptografia of
          cPadrao:
            Key := Encrypt(IntToStr(UserID) + Login, EncryptKey);
          cMD5:
            Key := MD5Sum(IntToStr(UserID) + Login);
        end;
        if Assigned(DataConnector) then
          DataConnector.UCExecSQL
            (Format('UPDATE %s SET %s = %s' + ' WHERE %s = %d AND' +
            ' %s = %s AND %s = %s AND' + ' %s = %s',
            [TableRights.TableName + 'EX', TableRights.FieldKey, QuotedStr(Key),
            TableRights.FieldUserID, TempDS.FieldByName(TableRights.FieldUserID)
            .AsInteger, TableRights.FieldModule, QuotedStr(ApplicationID),
            TableRights.FieldComponentName, QuotedStr(Login), // componente name
            TableRights.FieldFormName, QuotedStr(Senha)])); // formname
        Next;
      end;
  end;
  TempDS.Close;
  FreeAndNil(TempDS);
end;

procedure TUserControl.ShowChangePassword;
begin
  ActionTrocaSenha(Self);
end;

procedure TUserControl.ShowNewConfig;
var
  formHandle: THandle;
begin
  formHandle := FindWindow('TFormUserPerf', nil);
  if formHandle = 0 then { By Cleilson Sousa }
  begin
    FFormGeral := TFormUserPerf.Create(Application);
    with TFormUserPerf(FFormGeral) do
    begin
      Position := UserSettings.WindowsPosition;
      FUserControl := Self;
      Show;
    end;
  end
  else
  begin
    ShowWindow(formHandle, SW_SHOWNORMAL);
    SetForegroundWindow(formHandle);
  end;

end;

procedure TUserControl.AddUCControlMonitor(UCControl: TUCControls);
begin
  FControlList.Add(UCControl);
end;

procedure TUserControl.ApplyRightsUCControlMonitor;
var
  Contador: Integer;
begin
  for Contador := 0 to Pred(FControlList.Count) do
    TUCControls(FControlList.Items[Contador]).ApplyRights;
end;

procedure TUserControl.DeleteUCControlMonitor(UCControl: TUCControls);
var
  Contador: Integer;
  SLControls: TStringList;
begin
  if not Assigned(FControlList) then
    Exit;
  SLControls := TStringList.Create;

  try
    for Contador := 0 to Pred(FControlList.Count) do
      if TUCControls(FControlList.Items[Contador]) = UCControl then
        SLControls.Add(IntToStr(Contador));
  except
  end;

  try
    for Contador := 0 to Pred(SLControls.Count) do
      FControlList.Delete(StrToInt(SLControls[Contador]));
  except
  end;

  FreeAndNil(SLControls);
end;

procedure TUserControl.LockControlsUCControlMonitor;
var
  Contador: Integer;
begin
  for Contador := 0 to Pred(FControlList.Count) do
    TUCControls(FControlList.Items[Contador]).LockControls;
end;

procedure TUserControl.SetDataConnector(const Value: TUCDataConnector);
begin
  FDataConnector := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

procedure TUserControl.AddLoginMonitor(UCAppMessage: TUCApplicationMessage);
begin
  FLoginMonitorList.Add(UCAppMessage);
end;

procedure TUserControl.DeleteLoginMonitor(UCAppMessage: TUCApplicationMessage);
var
  Contador: Integer;
  SLControls: TStringList;
begin
  SLControls := TStringList.Create;
  if Assigned(FLoginMonitorList) then
    for Contador := 0 to Pred(FLoginMonitorList.Count) do
      if TUCApplicationMessage(FLoginMonitorList.Items[Contador]) = UCAppMessage
      then
        SLControls.Add(IntToStr(Contador));
  if Assigned(SLControls) then
    for Contador := 0 to Pred(SLControls.Count) do
      FLoginMonitorList.Delete(StrToInt(SLControls[Contador]));
 SysUtils.FreeAndNil(SLControls);
end;

procedure TUserControl.NotificationLoginMonitor;
var
  Contador: Integer;
begin
  for Contador := 0 to Pred(FLoginMonitorList.Count) do
    TUCApplicationMessage(FLoginMonitorList.Items[Contador]).CheckMessages;
end;

procedure TUserControl.ShowLogin;
begin
  FRetry := 0;
  if Assigned(OnCustomLoginForm) then
    OnCustomLoginForm(Self, FFormLogin);

  if FFormLogin = nil then
  begin
    FFormLogin := TfrmLoginWindow.Create(Self);
    with FFormLogin as TfrmLoginWindow do
    begin
      SetfrmLoginWindow(TfrmLoginWindow(FFormLogin));
      FUserControl := Self;
      btOK.OnClick := ActionOKLogin;
      onCloseQuery := TestaFecha;
      Position := Self.UserSettings.WindowsPosition;
      lbEsqueci.OnClick := ActionEsqueceuSenha;
    end;
  end;
  FFormLogin.ShowModal;

  FreeAndNil(FFormLogin);
end;

procedure TUserControl.ActionOKLogin(Sender: TObject);
var
  TempUser: String;
  TempPassword: String;
  retorno: Integer;
begin
  TfrmLoginWindow(FFormLogin).btOKClick(Sender); { By Cleilson Sousa }
  TempUser := TfrmLoginWindow(FFormLogin).EditUsuario.Text;
  TempPassword := TfrmLoginWindow(FFormLogin).EditSenha.Text;

  if Assigned(OnLogin) then
    OnLogin(Self, TempUser, TempPassword);
  retorno := VerificaLogin(TempUser, TempPassword);

  if retorno = 0 then
    TfrmLoginWindow(FFormLogin).Close
  else
  begin
    if retorno = 1 then
      MessageDlg(UserSettings.CommonMessages.InvalidLogin, mtWarning, [mbOK], 0)
    else if retorno = 2 then
      MessageDlg(UserSettings.CommonMessages.InactiveLogin, mtWarning,
        [mbOK], 0);

    Inc(FRetry);
    if TfrmLoginWindow(FFormLogin).StatusBar.Visible then
      TfrmLoginWindow(FFormLogin).StatusBar.Panels[1].Text := IntToStr(FRetry);

    if (Login.MaxLoginAttempts > 0) and (FRetry = Login.MaxLoginAttempts) then
    begin
      MessageDlg(Format(UserSettings.CommonMessages.MaxLoginAttemptsError,
        [Login.MaxLoginAttempts]), mtError, [mbOK], 0);
      Application.Terminate;
    end;
  end;
end;

procedure TUserControl.TestaFecha(Sender: TObject; var CanClose: Boolean);
begin
  // if FFormLogin.ModalResult = mrOk then
  CanClose := (CurrentUser.UserID > 0);
end;

procedure TUserControl.ApplyRights;
begin
  if Self.CurrentUser.UserID <> 0 then
  begin
    ApplyRightsObj(Self.CurrentUser.PerfilUsuario);

    // Aplica Permissoes do Perfil do usuario
    if CurrentUser.Profile > 0 then
      ApplyRightsObj(Self.CurrentUser.PerfilGrupo, True);

    if Assigned(FAfterLogin) then
      FAfterLogin(Self);
  end;
end;

procedure TUserControl.ApplyRightsObj(ADataset: TDataSet;
  FProfile: Boolean = False);
var
  Contador: Integer;
  Encontrado: Boolean;
  KeyField: String;
  Temp: String;
  ObjetoAction: TObject;
  OwnerMenu: TComponent;
begin
  // Permissao de Menus QMD
  Encontrado := False;

  if ADataset.State = dsInactive then
    ADataset.Open;

  if Assigned(ControlRight.MainMenu) then
  begin
    OwnerMenu := ControlRight.MainMenu.Owner;
    for Contador := 0 to Pred(OwnerMenu.ComponentCount) do
      if (OwnerMenu.Components[Contador].ClassType = TMenuItem) and
        (TMenuItem(OwnerMenu.Components[Contador])
        .GetParentMenu = ControlRight.MainMenu) then
      begin
        if not FProfile then
        begin
          Encontrado := ADataset.Locate('ObjName',
            OwnerMenu.Components[Contador].Name, []);
          KeyField := ADataset.FindField('UCKey').AsString;
          // verifica key
          if Encontrado then
            case Self.Criptografia of
              cPadrao:
                Encontrado :=
                  (KeyField = Encrypt(ADataset.FieldByName('UserID').AsString +
                  ADataset.FieldByName('ObjName').AsString, EncryptKey));
              cMD5:
                Encontrado :=
                  (KeyField = MD5Sum(ADataset.FieldByName('UserID').AsString +
                  ADataset.FieldByName('ObjName').AsString));
            end;
          TMenuItem(OwnerMenu.Components[Contador]).Enabled := Encontrado;
          if not Encontrado then
            TMenuItem(OwnerMenu.Components[Contador]).Visible :=
              NotAllowedItems.MenuVisible
          else
            TMenuItem(OwnerMenu.Components[Contador]).Visible := True;
        end
        else if ADataset.Locate('ObjName', OwnerMenu.Components[Contador]
          .Name, []) then
        begin
          KeyField := ADataset.FindField('UCKey').AsString;
          case Self.Criptografia of
            cPadrao:
              Encontrado :=
                (KeyField = Encrypt(ADataset.FieldByName('UserID').AsString +
                ADataset.FieldByName('ObjName').AsString, EncryptKey));
            cMD5:
              Encontrado :=
                (KeyField = MD5Sum(ADataset.FieldByName('UserID').AsString +
                ADataset.FieldByName('ObjName').AsString));
          end;
          TMenuItem(OwnerMenu.Components[Contador]).Enabled := Encontrado;
          TMenuItem(OwnerMenu.Components[Contador]).Visible := Encontrado;
        end;
        if Assigned(OnApplyRightsMenuIt) then
          OnApplyRightsMenuIt(Self, TMenuItem(OwnerMenu.Components[Contador]));
      end;
  end; // Fim do controle do MainMenu

  // Permissao de Actions
  if (Assigned(ControlRight.ActionList))
  { .$IFDEF UCACTMANAGER } or (Assigned(ControlRight.ActionManager)) { .$ENDIF }
  then
  begin
    if Assigned(ControlRight.ActionList) then
      ObjetoAction := ControlRight.ActionList
      { .$IFDEF UCACTMANAGER }
    else
      ObjetoAction := ControlRight.ActionManager
      { .$ENDIF };
    for Contador := 0 to TActionList(ObjetoAction).ActionCount - 1 do
    begin
      if not FProfile then
      begin
        Encontrado := ADataset.Locate('ObjName', TActionList(ObjetoAction)
          .Actions[Contador].Name, []);
        KeyField := ADataset.FindField('UCKey').AsString;
        // verifica key
        if Encontrado then
          case Self.Criptografia of
            cPadrao:
              Encontrado :=
                (KeyField = Encrypt(ADataset.FieldByName('UserID').AsString +
                ADataset.FieldByName('ObjName').AsString, EncryptKey));
            cMD5:
              Encontrado :=
                (KeyField = MD5Sum(ADataset.FieldByName('UserID').AsString +
                ADataset.FieldByName('ObjName').AsString));
          end;

        TAction(TActionList(ObjetoAction).Actions[Contador]).Enabled :=
          Encontrado;

        if not Encontrado then
          TAction(TActionList(ObjetoAction).Actions[Contador]).Visible :=
            NotAllowedItems.ActionVisible
        else
          TAction(TActionList(ObjetoAction).Actions[Contador]).Visible := True;
      end
      else if ADataset.Locate('ObjName', TActionList(ObjetoAction)
        .Actions[Contador].Name, []) then
      begin
        KeyField := ADataset.FindField('UCKey').AsString;
        case Self.Criptografia of
          cPadrao:
            Encontrado :=
              (KeyField = Encrypt(ADataset.FieldByName('UserID').AsString +
              ADataset.FieldByName('ObjName').AsString, EncryptKey));
          cMD5:
            Encontrado :=
              (KeyField = MD5Sum(ADataset.FieldByName('UserID').AsString +
              ADataset.FieldByName('ObjName').AsString));
        end;
        TAction(TActionList(ObjetoAction).Actions[Contador]).Enabled :=
          Encontrado;
        TAction(TActionList(ObjetoAction).Actions[Contador]).Visible :=
          Encontrado;
      end;

      if Assigned(OnApplyRightsActionIt) then
        OnApplyRightsActionIt(Self,
          TAction(TActionList(ObjetoAction).Actions[Contador]));
    end;
  end; // Fim das permiss�es de Actions

  { .$IFDEF UCACTMANAGER }
  if Assigned(ControlRight.ActionMainMenuBar) then
    for Contador := 0 to ControlRight.ActionMainMenuBar.ActionClient.Items.
      Count - 1 do
    begin
      Temp := IntToStr(Contador);
      if ControlRight.ActionMainMenuBar.ActionClient.Items[StrToInt(Temp)
        ].Items.Count > 0 then
      begin
        if Self.Criptografia = cPadrao then
          ControlRight.ActionMainMenuBar.ActionClient.Items[StrToInt(Temp)
            ].Visible :=
            (ADataset.Locate('ObjName',
            #1 + 'G' + ControlRight.ActionMainMenuBar.ActionClient.Items
            [StrToInt(Temp)].Caption, [])) and
            (ADataset.FieldByName('UCKey').AsString = Encrypt
            (ADataset.FieldByName('UserID').AsString +
            ADataset.FieldByName('ObjName').AsString, EncryptKey));

        if Self.Criptografia = cMD5 then
          ControlRight.ActionMainMenuBar.ActionClient.Items[StrToInt(Temp)
            ].Visible :=
            (ADataset.Locate('ObjName',
            #1 + 'G' + ControlRight.ActionMainMenuBar.ActionClient.Items
            [StrToInt(Temp)].Caption, [])) and
            (ADataset.FieldByName('UCKey').AsString = MD5Sum
            (ADataset.FieldByName('UserID').AsString +
            ADataset.FieldByName('ObjName').AsString));

        TrataActMenuBarIt(ControlRight.ActionMainMenuBar.ActionClient.Items
          [StrToInt(Temp)], ADataset);
      end;
    end;
  { .$ENDIF }
end;

procedure TUserControl.UnlockEX(FormObj: TCustomForm; ObjName: String);
begin
  if FormObj.FindComponent(ObjName) = nil then
    Exit;

  if FormObj.FindComponent(ObjName) is TControl then
  begin
    TControl(FormObj.FindComponent(ObjName)).Enabled := True;
    TControl(FormObj.FindComponent(ObjName)).Visible := True;
  end;

  if FormObj.FindComponent(ObjName) is TMenuItem then // TMenuItem
  begin
    TMenuItem(FormObj.FindComponent(ObjName)).Enabled := True;
    TMenuItem(FormObj.FindComponent(ObjName)).Visible := True;
    // chama evento OnApplyRightsMenuIt
    if Assigned(OnApplyRightsMenuIt) then
      OnApplyRightsMenuIt(Self, FormObj.FindComponent(ObjName) as TMenuItem);
  end;

  if FormObj.FindComponent(ObjName) is TAction then // TAction
  begin
    TAction(FormObj.FindComponent(ObjName)).Enabled := True;
    TAction(FormObj.FindComponent(ObjName)).Visible := True;
    // chama evento OnApplyRightsMenuIt
    if Assigned(OnApplyRightsActionIt) then
      OnApplyRightsActionIt(Self, FormObj.FindComponent(ObjName) as TAction);
  end;

  if FormObj.FindComponent(ObjName) is TField then // TField
  begin
    TField(FormObj.FindComponent(ObjName)).ReadOnly := False;
    TField(FormObj.FindComponent(ObjName)).Visible := True;
    TField(FormObj.FindComponent(ObjName)).onGetText := nil;
  end;
end;

procedure TUserControl.LockEX(FormObj: TCustomForm; ObjName: String;
  naInvisible: Boolean);
begin
  if FormObj.FindComponent(ObjName) = nil then
    Exit;

  if FormObj.FindComponent(ObjName) is TControl then
  begin
    TControl(FormObj.FindComponent(ObjName)).Enabled := False;
    TControl(FormObj.FindComponent(ObjName)).Visible := not naInvisible;
  end;

  if FormObj.FindComponent(ObjName) is TMenuItem then // TMenuItem
  begin
    TMenuItem(FormObj.FindComponent(ObjName)).Enabled := False;
    TMenuItem(FormObj.FindComponent(ObjName)).Visible := not naInvisible;
    // chama evento OnApplyRightsMenuIt
    if Assigned(OnApplyRightsMenuIt) then
      OnApplyRightsMenuIt(Self, FormObj.FindComponent(ObjName) as TMenuItem);
  end;

  if FormObj.FindComponent(ObjName) is TAction then // TAction
  begin
    TAction(FormObj.FindComponent(ObjName)).Enabled := False;
    TAction(FormObj.FindComponent(ObjName)).Visible := not naInvisible;
    // chama evento OnApplyRightsMenuIt
    if Assigned(OnApplyRightsActionIt) then
      OnApplyRightsActionIt(Self, FormObj.FindComponent(ObjName) as TAction);
  end;

  if FormObj.FindComponent(ObjName) is TField then // TField
  begin
    TField(FormObj.FindComponent(ObjName)).ReadOnly := True;
    TField(FormObj.FindComponent(ObjName)).Visible := not naInvisible;
    TField(FormObj.FindComponent(ObjName)).onGetText := HideField;
  end;
end;

{ .$IFDEF UCACTMANAGER }
procedure TUserControl.TrataActMenuBarIt(IT: TActionClientItem;
  ADataset: TDataSet);
var
  Contador: Integer;
begin
  for Contador := 0 to IT.Items.Count - 1 do
    if IT.Items[Contador].Caption <> '-' then
      if IT.Items[Contador].Items.Count > 0 then
      begin
        IT.Items[Contador].Visible :=
          (ADataset.Locate('ObjName', #1 + 'G' + IT.Items[Contador]
          .Caption, []));
        TrataActMenuBarIt(IT.Items[Contador], ADataset);
      end;
end;

{ .$ENDIF }

procedure TUserControl.CriaTabelaRights(ExtraRights: Boolean = False);
var
  SQLstmt: String;
  TipoCampo: String;
begin
  case Self.Criptografia of
    cPadrao:
      TipoCampo := UserSettings.Type_VarChar + '(250)';
    cMD5:
      TipoCampo := UserSettings.Type_VarChar + '(32)';
  end;

  with TableRights do
    if not ExtraRights then
    begin
      SQLstmt := Format('CREATE TABLE %s( %s %s, %s %s(50), %s %s(50), %s %s )',
        [TableName, FieldUserID, UserSettings.Type_Int, FieldModule,
        UserSettings.Type_VarChar, FieldComponentName,
        UserSettings.Type_VarChar, FieldKey, TipoCampo]);

      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(SQLstmt);
    end
    else
    begin
      SQLstmt :=
        Format('CREATE TABLE %sEX( %s %s, %s %s(50), %s %s(50), %s %s(50), %s %s )',
        [TableName, FieldUserID, UserSettings.Type_Int, FieldModule,
        UserSettings.Type_VarChar, FieldComponentName,
        UserSettings.Type_VarChar, FieldFormName, UserSettings.Type_VarChar,
        FieldKey, TipoCampo]);

      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(SQLstmt);
    end;
end;

procedure TUserControl.AddRightEX(IdUser: Integer;
  Module, FormName, ObjName: String);
var
  KeyField: String;
  SQLstmt: String;
begin
  case Self.Criptografia of
    cPadrao:
      KeyField := Encrypt(IntToStr(IdUser) + ObjName, EncryptKey);
    cMD5:
      KeyField := MD5Sum(IntToStr(IdUser) + ObjName);
  end;

  with TableRights do
    SQLstmt :=
      Format('INSERT INTO %sEX( %s, %s, %s, %s, %s) VALUES (%d, %s, %s, %s, %s)',
      [TableName, FieldUserID, FieldModule, FieldFormName, FieldComponentName,
      FieldKey, IdUser, QuotedStr(Module), QuotedStr(FormName),
      QuotedStr(ObjName), QuotedStr(KeyField)]);

  if Assigned(DataConnector) then
    DataConnector.UCExecSQL(SQLstmt);
end;

procedure TUserControl.AddRight(IdUser: Integer; ItemRight: String);
var
  KeyField: String;
  SQLstmt: String;
begin
  if ItemRight = '' then
    Exit;

  case Self.Criptografia of
    cPadrao:
      KeyField := Encrypt(IntToStr(IdUser) + ItemRight, EncryptKey);
    cMD5:
      KeyField := MD5Sum(IntToStr(IdUser) + ItemRight);
  end;

  SQLstmt := Format('Insert into %s( %s, %s, %s, %s) Values( %d, %s, %s, %s)',
    [TableRights.TableName, TableRights.FieldUserID, TableRights.FieldModule,
    TableRights.FieldComponentName, TableRights.FieldKey, IdUser,
    QuotedStr(ApplicationID), QuotedStr(ItemRight), QuotedStr(KeyField)]);

  if Assigned(DataConnector) then
    DataConnector.UCExecSQL(SQLstmt);
end;

procedure TUserControl.AddRight(IdUser: Integer; ItemRight: TObject;
  FullPath: Boolean = True);
var
  Obj: TObject;
begin
  if ItemRight = nil then
    Exit;

  Obj := ItemRight;

  if Obj.ClassType = TMenuItem then
    while Assigned(Obj) and (Obj.ClassType = TMenuItem) and
      (TComponent(Obj).Name <> '') do
    begin
      AddRight(IdUser, TComponent(Obj).Name);
      if FullPath then
        Obj := TMenuItem(Obj).Parent
      else
        Obj := nil;
    end
  else
    AddRight(IdUser, TComponent(Obj).Name);
end;

procedure TUserControl.CriaTabelaLog;
begin
  if Assigned(DataConnector) then
    DataConnector.UCExecSQL
      (Format('CREATE TABLE %S  (APPLICATIONID %s(250), IDUSER %s , MSG %s(250), DATA %s(14), NIVEL %s)',
      [LogControl.TableLog, UserSettings.Type_VarChar, UserSettings.Type_Int,
      UserSettings.Type_VarChar, UserSettings.Type_VarChar,
      UserSettings.Type_Int]));
end;

{ .$IFDEF UCACTMANAGER }
procedure TUserControl.IncPermissActMenuBar(IdUser: Integer; Act: TAction);
var
  Temp: TActionClientItem;
begin
  if Act = nil then
    Exit;

  Temp := ControlRight.ActionMainMenuBar.ActionManager.FindItemByAction(Act);
  while Temp <> nil do
  begin
    AddRight(IdUser, #1 + 'G' + Temp.Caption);
    Temp := (TActionClientItem(Temp).ParentItem as TActionClientItem);
  end;
end;

{ .$ENDIF }

procedure TUserControl.CriaTabelaUsuarios(TableExists: Boolean);
var
  Contador: Integer;
  IDUsuario: Integer;
  CustomForm: TCustomForm;
  Mensagens: TStrings;
  DataSetUsuario: TDataSet;
  DataSetPermissao: TDataSet;
  SQLstmt: String;
  TipoCampo: String;
  UsuarioInicial: String;
  PasswordInicial: String;
begin
  case Self.Criptografia of
    cPadrao:
      TipoCampo := UserSettings.Type_VarChar + '(250)';
    cMD5:
      TipoCampo := UserSettings.Type_VarChar + '(32)';
  end;

  if not TableExists then
    with TableUsers do
    begin

      SQLStmt := Format('Create Table %s ' + // TableName
        '( ' +
        '%s %s NOT NULL, ' +           // FieldUserID    Adicionado NOT null para poder usar como Chave Primaria
                                       //   Mauri 06/05/2017
        '%s %s(30), ' +                // FieldUserName
        '%s %s(30), ' +                // FieldLogin
        '%s %s, ' +                    // FieldPassword
        '%s %s(10), ' +                // FieldDateExpired
        '%s %s, ' +                    // FieldUserExpired
        '%s %s, ' +                    // FieldUserDaysSun
        '%s %s(150), ' +               // FieldMail
        '%s %s, ' +                    // Privileged
        '%s %s(1), ' +                 // TypeRec
        '%s %s, ' +                    // Profile
        '%s %s, ' +                    // FieldKey
        '%s %s, '+                     // UInative
        '%s %s, '+                     // UserType Mauri
        '%s %s(10), ' +                // User Department       Mauri
        '%s %s '+                      // UserEmpresa Mauri
        ')',
        [TableName,
        FieldUserID, UserSettings.Type_Int,
        FieldUserName, UserSettings.Type_VarChar,
        FieldLogin, UserSettings.Type_VarChar,
        FieldPassword, TipoCampo,
        FieldDateExpired, UserSettings.Type_Char,
        FieldUserExpired, UserSettings.Type_Int,
        FieldUserDaysSun, UserSettings.Type_Int,
        FieldEmail, UserSettings.Type_Varchar,
        FieldPrivileged, UserSettings.Type_Int,
        FieldTypeRec, UserSettings.Type_Char,
        FieldProfile, UserSettings.Type_Int,
        FieldKey, TipoCampo,
        FieldUserInative, UserSettings.Type_Int,
        FieldUserType, UserSettings.Type_Int,       // Mauri 26/01/2017   Tipo de Usaurio
        FieldUserDepartment, UserSettings.Type_VarChar,
        FieldUserEmpresa, UserSettings.Type_Int     // Mauri 26/01/2017 Empresa do Usaurio
        ]);

      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(SQLstmt);
       //     Criar Chave Primaria para a Tabela     Mauri 06/05/2017
       SQLStmt := Format('ALTER TABLE %s ADD CONSTRAINT PK_%s PRIMARY KEY (%s)',
                          [TableName, TableName, FieldUserID, UserSettings.Type_Int]);

      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(SQLstmt);

    end;

  case Self.Login.CharCaseUser of
    ecNormal:
      UsuarioInicial := Self.Login.InitialLogin.User;
    ecUpperCase:
      UsuarioInicial := UpperCase(Self.Login.InitialLogin.User);
    ecLowerCase:
      UsuarioInicial := LowerCase(Self.Login.InitialLogin.User);
  end;

  case Self.Login.CharCasePass of
    ecNormal:
      PasswordInicial := Self.Login.InitialLogin.Password;
    ecUpperCase:
      PasswordInicial := UpperCase(Self.Login.InitialLogin.Password);
    ecLowerCase:
      PasswordInicial := LowerCase(Self.Login.InitialLogin.Password);
  end;

  SQLstmt := 'SELECT ' + TableUsers.FieldUserID + ' as idUser ' + 'FROM ' +
    TableUsers.TableName + ' ' + 'WHERE ' + TableUsers.FieldLogin + ' = ' +
    QuotedStr(UsuarioInicial);

  try
    DataSetUsuario := DataConnector.UCGetSQLDataset(SQLstmt);

    // Inserir login inicial
    if DataSetUsuario.IsEmpty then
      IDUsuario := AddUser(UsuarioInicial, PasswordInicial,
        Login.InitialLogin.User, Login.InitialLogin.Email, 0, 0,
        Login.DaysOfSunExpired,
           True,'',0,0) //  Mauri      0 = admin

    else
      IDUsuario := DataSetUsuario.FieldByName('idUser').AsInteger;

  finally
    DataSetUsuario.Close;
    FreeAndNil(DataSetUsuario);
  end;

  SQLstmt := 'SELECT ' + TableRights.FieldUserID + ' AS IDUSER ' + 'FROM ' +
    TableRights.TableName + ' ' + 'WHERE ' + TableRights.FieldUserID + ' = ' +
    IntToStr(IDUsuario) + ' ' + 'AND ' + TableRights.FieldModule + ' = ' +
    QuotedStr(ApplicationID);

  try
    DataSetPermissao := DataConnector.UCGetSQLDataset(SQLstmt);

    if not DataSetPermissao.IsEmpty then
      Exit;

  finally
    DataSetPermissao.Close;
    FreeAndNil(DataSetPermissao);
  end;

  AddRight(IDUsuario, User.MenuItem);
  AddRight(IDUsuario, User.Action);

  AddRight(IDUsuario, UserPasswordChange.MenuItem);
  AddRight(IDUsuario, UserPasswordChange.Action);

  AddRight(IDUsuario, UsersLogoff.MenuItem);
  AddRight(IDUsuario, UsersLogoff.Action);

  { .$IFDEF UCACTMANAGER }
  if Assigned(ControlRight.ActionMainMenuBar) then
    IncPermissActMenuBar(IDUsuario, User.Action);

  if Assigned(ControlRight.ActionMainMenuBar) then
    IncPermissActMenuBar(IDUsuario, UserPasswordChange.Action);
  { .$ENDIF }

  for Contador := 0 to Pred(Login.InitialLogin.InitialRights.Count) do
    if Owner.FindComponent(Login.InitialLogin.InitialRights[Contador]) <> nil
    then
    begin
      AddRight(IDUsuario, Owner.FindComponent(Login.InitialLogin.InitialRights
        [Contador]));
      AddRightEX(IDUsuario, ApplicationID, TCustomForm(Owner).Name,
        Login.InitialLogin.InitialRights[Contador]);
    end;

  try
    Mensagens := TStringList.Create;
    Mensagens.Assign(UserSettings.CommonMessages.InitialMessage);
    Mensagens.Text := StringReplace(Mensagens.Text, ':user', UsuarioInicial,
      [rfReplaceAll]);
    Mensagens.Text := StringReplace(Mensagens.Text, ':password',
      PasswordInicial, [rfReplaceAll]);

    if Assigned(OnCustomInitialMsg) then
      OnCustomInitialMsg(Self, CustomForm, Mensagens);

    if CustomForm <> nil then
      CustomForm.ShowModal
    else
      MessageDlg(Mensagens.Text, mtInformation, [mbOK], 0);

  finally
    FreeAndNil(Mensagens);
  end;
end;

procedure TUserControl.SetfLanguage(const Value: TUCLanguage);
begin
  fLanguage := Value;
  Self.UserSettings.Language := Value;
  UCSettings.AlterLanguage(Self.UserSettings);
end;

procedure TUserControl.SetFMailUserControl(const Value: TMailUserControl);
begin
  FMailUserControl := Value;
  FMailUserControl.FUserControl := Self;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TUserControl.ApplySettings(SourceSettings: TUCSettings);
begin
  with UserSettings.CommonMessages do
  begin
    BlankPassword := SourceSettings.CommonMessages.BlankPassword;
    PasswordChanged := SourceSettings.CommonMessages.PasswordChanged;
    InitialMessage.Text := SourceSettings.CommonMessages.InitialMessage.Text;
    MaxLoginAttemptsError := SourceSettings.CommonMessages.
      MaxLoginAttemptsError;
    InvalidLogin := SourceSettings.CommonMessages.InvalidLogin;
    InactiveLogin := SourceSettings.CommonMessages.InactiveLogin;
    AutoLogonError := SourceSettings.CommonMessages.AutoLogonError;
    UsuarioExiste := SourceSettings.CommonMessages.UsuarioExiste;
    PasswordExpired := SourceSettings.CommonMessages.PasswordExpired;
    ForcaTrocaSenha := SourceSettings.CommonMessages.ForcaTrocaSenha;
  end;

  with UserSettings.Login do
  begin
    btCancel := SourceSettings.Login.btCancel;
    btOK := SourceSettings.Login.btOK;
    LabelPassword := SourceSettings.Login.LabelPassword;
    LabelUser := SourceSettings.Login.LabelUser;
    WindowCaption := SourceSettings.Login.WindowCaption;
    LabelTentativa := SourceSettings.Login.LabelTentativa;
    LabelTentativas := SourceSettings.Login.LabelTentativas;

    if Assigned(SourceSettings.Login.LeftImage.Bitmap) then
      LeftImage.Bitmap := SourceSettings.Login.LeftImage.Bitmap
    else
      LeftImage.Bitmap := nil;

    if Assigned(SourceSettings.Login.TopImage.Bitmap) then
      TopImage.Bitmap := SourceSettings.Login.TopImage.Bitmap
    else
      TopImage.Bitmap := nil;

    if Assigned(SourceSettings.Login.BottomImage.Bitmap) then
      BottomImage.Bitmap := SourceSettings.Login.BottomImage.Bitmap
    else
      BottomImage.Bitmap := nil;
  end;

  with UserSettings.UsersForm do
  begin
    WindowCaption := SourceSettings.UsersForm.WindowCaption;
    LabelDescription := SourceSettings.UsersForm.LabelDescription;
    ColName := SourceSettings.UsersForm.ColName;
    ColLogin := SourceSettings.UsersForm.ColLogin;
    ColEmail := SourceSettings.UsersForm.ColEmail;
    BtAdd := SourceSettings.UsersForm.BtAdd;
    BtChange := SourceSettings.UsersForm.BtChange;
    BtDelete := SourceSettings.UsersForm.BtDelete;
    BtRights := SourceSettings.UsersForm.BtRights;
    BtPassword := SourceSettings.UsersForm.BtPassword;
    BtClose := SourceSettings.UsersForm.BtClose;
    PromptDelete := SourceSettings.UsersForm.PromptDelete;
    PromptDelete_WindowCaption :=
      SourceSettings.UsersForm.PromptDelete_WindowCaption; // added by fduenas
  end;

  with UserSettings.UsersProfile do
  begin
    WindowCaption := SourceSettings.UsersProfile.WindowCaption;
    LabelDescription := SourceSettings.UsersProfile.LabelDescription;
    ColProfile := SourceSettings.UsersProfile.ColProfile;
    BtAdd := SourceSettings.UsersProfile.BtAdd;
    BtChange := SourceSettings.UsersProfile.BtChange;
    BtDelete := SourceSettings.UsersProfile.BtDelete;
    BtRights := SourceSettings.UsersProfile.BtRights; // added by fduenas
    BtClose := SourceSettings.UsersProfile.BtClose;
    PromptDelete := SourceSettings.UsersProfile.PromptDelete;
    PromptDelete_WindowCaption :=
      SourceSettings.UsersProfile.PromptDelete_WindowCaption;
    // added by fduenas
  end;

  with UserSettings.AddChangeUser do
  begin
    WindowCaption := SourceSettings.AddChangeUser.WindowCaption;
    LabelAdd := SourceSettings.AddChangeUser.LabelAdd;
    LabelChange := SourceSettings.AddChangeUser.LabelChange;
    LabelName := SourceSettings.AddChangeUser.LabelName;
    LabelLogin := SourceSettings.AddChangeUser.LabelLogin;
    LabelEmail := SourceSettings.AddChangeUser.LabelEmail;
    CheckPrivileged := SourceSettings.AddChangeUser.CheckPrivileged;
    BtSave := SourceSettings.AddChangeUser.BtSave;
    btCancel := SourceSettings.AddChangeUser.btCancel;
    CheckExpira := SourceSettings.AddChangeUser.CheckExpira;
    Day := SourceSettings.AddChangeUser.Day;
    ExpiredIn := SourceSettings.AddChangeUser.ExpiredIn;
  end;

  with UserSettings.AddChangeProfile do
  begin
    WindowCaption := SourceSettings.AddChangeProfile.WindowCaption;
    LabelAdd := SourceSettings.AddChangeProfile.LabelAdd;
    LabelChange := SourceSettings.AddChangeProfile.LabelChange;
    LabelName := SourceSettings.AddChangeProfile.LabelName;
    BtSave := SourceSettings.AddChangeProfile.BtSave;
    btCancel := SourceSettings.AddChangeProfile.btCancel;
  end;

  with UserSettings.Rights do
  begin
    WindowCaption := SourceSettings.Rights.WindowCaption;
    LabelUser := SourceSettings.Rights.LabelUser;
    LabelProfile := SourceSettings.Rights.LabelProfile;
    PageMenu := SourceSettings.Rights.PageMenu;
    PageActions := SourceSettings.Rights.PageActions;
    PageControls := SourceSettings.Rights.PageControls;
    BtUnlock := SourceSettings.Rights.BtUnlock;
    BtLock := SourceSettings.Rights.BtLock;
    BtSave := SourceSettings.Rights.BtSave;
    btCancel := SourceSettings.Rights.btCancel;
  end;

  with UserSettings.ChangePassword do
  begin
    WindowCaption := SourceSettings.ChangePassword.WindowCaption;
    LabelDescription := SourceSettings.ChangePassword.LabelDescription;
    LabelCurrentPassword := SourceSettings.ChangePassword.LabelCurrentPassword;
    LabelNewPassword := SourceSettings.ChangePassword.LabelNewPassword;
    LabelConfirm := SourceSettings.ChangePassword.LabelConfirm;
    BtSave := SourceSettings.ChangePassword.BtSave;
    btCancel := SourceSettings.ChangePassword.btCancel;
  end;

  with UserSettings.CommonMessages.ChangePasswordError do
  begin
    InvalidCurrentPassword := SourceSettings.CommonMessages.ChangePasswordError.
      InvalidCurrentPassword;
    NewPasswordError := SourceSettings.CommonMessages.ChangePasswordError.
      NewPasswordError;
    NewEqualCurrent := SourceSettings.CommonMessages.ChangePasswordError.
      NewEqualCurrent;
    PasswordRequired := SourceSettings.CommonMessages.ChangePasswordError.
      PasswordRequired;
    MinPasswordLength := SourceSettings.CommonMessages.ChangePasswordError.
      MinPasswordLength;
    InvalidNewPassword := SourceSettings.CommonMessages.ChangePasswordError.
      InvalidNewPassword;
  end;

  with UserSettings.ResetPassword do
  begin
    WindowCaption := SourceSettings.ResetPassword.WindowCaption;
    LabelPassword := SourceSettings.ResetPassword.LabelPassword;
  end;

  with UserSettings.Log do
  begin
    WindowCaption := SourceSettings.Log.WindowCaption;
    LabelDescription := SourceSettings.Log.LabelDescription;
    LabelUser := SourceSettings.Log.LabelUser;
    LabelDate := SourceSettings.Log.LabelDate;
    LabelLevel := SourceSettings.Log.LabelLevel;
    ColLevel := SourceSettings.Log.ColLevel;
    ColMessage := SourceSettings.Log.ColMessage;
    ColUser := SourceSettings.Log.ColUser;
    ColDate := SourceSettings.Log.ColDate;
    BtFilter := SourceSettings.Log.BtFilter;
    BtDelete := SourceSettings.Log.BtDelete;
    BtClose := SourceSettings.Log.BtClose;
    PromptDelete := SourceSettings.Log.PromptDelete;
    PromptDelete_WindowCaption := SourceSettings.Log.PromptDelete_WindowCaption;
    // added by fduenas
    OptionUserAll := SourceSettings.Log.OptionUserAll; // added by fduenas
    OptionLevelLow := SourceSettings.Log.OptionLevelLow; // added by fduenas
    OptionLevelNormal := SourceSettings.Log.OptionLevelNormal;
    // added by fduenas
    OptionLevelHigh := SourceSettings.Log.OptionLevelHigh; // added by fduenas
    OptionLevelCritic := SourceSettings.Log.OptionLevelCritic;
    // added by fduenas
    DeletePerformed := SourceSettings.Log.DeletePerformed; // added by fduenas
  end;

  with UserSettings.AppMessages do
  begin
    MsgsForm_BtNew := SourceSettings.AppMessages.MsgsForm_BtNew;
    MsgsForm_BtReplay := SourceSettings.AppMessages.MsgsForm_BtReplay;
    MsgsForm_BtForward := SourceSettings.AppMessages.MsgsForm_BtForward;
    MsgsForm_BtDelete := SourceSettings.AppMessages.MsgsForm_BtDelete;
    MsgsForm_BtClose := SourceSettings.AppMessages.MsgsForm_BtClose;
    // added by fduenas
    MsgsForm_WindowCaption := SourceSettings.AppMessages.MsgsForm_WindowCaption;
    MsgsForm_ColFrom := SourceSettings.AppMessages.MsgsForm_ColFrom;
    MsgsForm_ColSubject := SourceSettings.AppMessages.MsgsForm_ColSubject;
    MsgsForm_ColDate := SourceSettings.AppMessages.MsgsForm_ColDate;
    MsgsForm_PromptDelete := SourceSettings.AppMessages.MsgsForm_PromptDelete;
    MsgsForm_PromptDelete_WindowCaption :=
      SourceSettings.AppMessages.MsgsForm_PromptDelete_WindowCaption;
    // added by fduenas
    MsgsForm_NoMessagesSelected :=
      SourceSettings.AppMessages.MsgsForm_NoMessagesSelected;
    // added by fduenas
    MsgsForm_NoMessagesSelected_WindowCaption :=
      SourceSettings.AppMessages.MsgsForm_NoMessagesSelected_WindowCaption;
    // added by fduenas

    MsgRec_BtClose := SourceSettings.AppMessages.MsgRec_BtClose;
    MsgRec_WindowCaption := SourceSettings.AppMessages.MsgRec_WindowCaption;
    MsgRec_Title := SourceSettings.AppMessages.MsgRec_Title;
    MsgRec_LabelFrom := SourceSettings.AppMessages.MsgRec_LabelFrom;
    MsgRec_LabelDate := SourceSettings.AppMessages.MsgRec_LabelDate;
    MsgRec_LabelSubject := SourceSettings.AppMessages.MsgRec_LabelSubject;
    MsgRec_LabelMessage := SourceSettings.AppMessages.MsgRec_LabelMessage;
    MsgSend_BtSend := SourceSettings.AppMessages.MsgSend_BtSend;
    MsgSend_BtCancel := SourceSettings.AppMessages.MsgSend_BtCancel;
    MsgSend_WindowCaption := SourceSettings.AppMessages.MsgSend_WindowCaption;
    MsgSend_Title := SourceSettings.AppMessages.MsgSend_Title;
    MsgSend_GroupTo := SourceSettings.AppMessages.MsgSend_GroupTo;
    MsgSend_RadioUser := SourceSettings.AppMessages.MsgSend_RadioUser;
    MsgSend_RadioAll := SourceSettings.AppMessages.MsgSend_RadioAll;
    MsgSend_GroupMessage := SourceSettings.AppMessages.MsgSend_GroupMessage;
    MsgSend_LabelSubject := SourceSettings.AppMessages.MsgSend_LabelSubject;
    // added by fduenas
    MsgSend_LabelMessageText :=
      SourceSettings.AppMessages.MsgSend_LabelMessageText; // added by fduenas
  end;

  { with UserSettings.TypeFieldsDB do
    begin
    Type_VarChar   := SourceSettings.Type_VarChar;
    Type_Char      := SourceSettings.Type_Char;
    Type_Int       := SourceSettings.Type_Int;
    end;  aten��o mudar aqui }

  UserSettings.WindowsPosition := SourceSettings.WindowsPosition;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'Criptografia'} {$ENDIF}

const
  Codes64 = '0A1B2C3D4E5F6G7H89IjKlMnOPqRsTuVWXyZabcdefghijkLmNopQrStUvwxYz+/';
  C1 = 52845;
  C2 = 22719;

function Decode(const S: ansistring): ansistring;
const
  Map: array [Ansichar] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 62, 0, 0, 0, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
    18, 19, 20, 21, 22, 23, 24, 25, 0, 0, 0, 0, 0, 0, 26, 27, 28, 29, 30, 31,
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
    51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0);
var
  I: longint;
begin
  case Length(S) of
    2:
      begin
        I := Map[S[1]] + (Map[S[2]] shl 6);
        SetLength(Result, 1);
        Move(I, Result[1], Length(Result));
      end;
    3:
      begin
        I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12);
        SetLength(Result, 2);
        Move(I, Result[1], Length(Result));
      end;
    4:
      begin
        I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12) +
          (Map[S[4]] shl 18);
        SetLength(Result, 3);
        Move(I, Result[1], Length(Result));
      end
  end;
end;

function Encode(const S: ansistring): ansistring;
const
  Map: array [0 .. 63]
    of char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  I: longint;
begin
  I := 0;
  Move(S[1], I, Length(S));
  case Length(S) of
    1:
      Result := Map[I mod 64] + Map[(I shr 6) mod 64];
    2:
      Result := Map[I mod 64] + Map[(I shr 6) mod 64] + Map[(I shr 12) mod 64];
    3:
      Result := Map[I mod 64] + Map[(I shr 6) mod 64] + Map[(I shr 12) mod 64] +
        Map[(I shr 18) mod 64];
  end;
end;

function InternalDecrypt(const S: ansistring; Key: Word): ansistring;
var
  I: Word;
  Seed: int64;
begin
  Result := S;
  Seed := Key;
  for I := 1 to Length(Result) do
  begin
    Result[I] := Ansichar(byte(Result[I]) xor (Seed shr 8));
    Seed := (byte(S[I]) + Seed) * Word(C1) + Word(C2);
  end;
end;

function PreProcess(const S: ansistring): ansistring;
var
  SS: ansistring;
begin
  SS := S;
  Result := '';
  while SS <> '' do
  begin
    Result := Result + Decode(Copy(SS, 1, 4));
    Delete(SS, 1, 4);
  end;
end;

function Decrypt(const S: ansistring; Key: Word): ansistring;
begin
  Result := InternalDecrypt(PreProcess(S), Key);
end;

function PostProcess(const S: ansistring): ansistring;
var
  SS: ansistring;
begin
  SS := S;
  Result := '';
  while SS <> '' do
  begin
    Result := Result + Encode(Copy(SS, 1, 3));
    Delete(SS, 1, 3);
  end;
end;

function InternalEncrypt(const S: ansistring; Key: Word): ansistring;
var
  I: Word;
  Seed: int64;
begin
  Result := S;
  Seed := Key;
  for I := 1 to Length(Result) do
  begin
    Result[I] := Ansichar(byte(Result[I]) xor (Seed shr 8));
    Seed := (byte(Result[I]) + Seed) * Word(C1) + Word(C2);
  end;
end;

function Encrypt(const S: ansistring; Key: Word): ansistring;
begin
  Result := PostProcess(InternalEncrypt(S, Key));
end;

function MD5Sum(strValor: String): String;
begin
  Result := UCmd5.MD5Print(UCmd5.MD5String(strValor));
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUCAutoLogin'} {$ENDIF}
{ TUCAutoLogin }

procedure TUCAutoLogin.Assign(Source: TPersistent);
begin
  if Source is TUCAutoLogin then
  begin
    Self.Active := TUCAutoLogin(Source).Active;
    Self.User := TUCAutoLogin(Source).User;
    Self.Password := TUCAutoLogin(Source).Password;
  end
  else
    inherited;
end;

constructor TUCAutoLogin.Create(AOwner: TComponent);
begin
  inherited Create;
  Self.Active := False;
  Self.MessageOnError := True;
end;

destructor TUCAutoLogin.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TNaoPermitidos'} {$ENDIF}
{ TNaoPermitidos }

procedure TUCNotAllowedItems.Assign(Source: TPersistent);
begin
  if Source is TUCNotAllowedItems then
  begin
    Self.MenuVisible := TUCNotAllowedItems(Source).MenuVisible;
    Self.ActionVisible := TUCNotAllowedItems(Source).ActionVisible;
    // Consertado Luiz Benvenuto
  end
  else
    inherited;
end;

constructor TUCNotAllowedItems.Create(AOwner: TComponent);
begin
  inherited Create;
  Self.MenuVisible := True;
  Self.ActionVisible := True;
end;

destructor TUCNotAllowedItems.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TLogControl'} {$ENDIF}
{ TLogControl }

constructor TUCLogControl.Create(AOwner: TComponent);
begin
  inherited Create;
  Self.Active := True;
end;

destructor TUCLogControl.Destroy;
begin
  inherited Destroy;
end;

procedure TUCLogControl.Assign(Source: TPersistent);
begin
  if Source is TUCLogControl then
  begin
    Self.Active := TUCLogControl(Source).Active;
    Self.TableLog := TUCLogControl(Source).TableLog;
  end
  else
    inherited;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TCadastroUsuarios'} {$ENDIF}
{ TCadastroUsuarios }

procedure TUCUser.Assign(Source: TPersistent);
begin
  if Source is TUCUser then
  begin
    Self.MenuItem := TUCUser(Source).MenuItem;
    Self.Action := TUCUser(Source).Action;
  end
  else
    inherited;
end;

constructor TUCUser.Create(AOwner: TComponent);
begin
  inherited Create;
  Self.FProtectAdministrator := True;
  Self.FUsePrivilegedField := False;
end;

destructor TUCUser.Destroy;
begin
  inherited Destroy;
end;

procedure TUCUser.SetAction(const Value: TAction);
begin
  FAction := Value;
  if Value <> nil then
  begin
    Self.FMenuItem := nil;
    Value.FreeNotification(Self.Action);
  end;
end;

procedure TUCUser.SetMenuItem(const Value: TMenuItem);
begin
  FMenuItem := Value;
  if Value <> nil then
  begin
    Self.Action := nil;
    Value.FreeNotification(Self.MenuItem);
  end;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TLogin'} {$ENDIF}
{ TLogin }

constructor TUCLogin.Create(AOwner: TComponent);
begin
  inherited Create;
  AutoLogin := TUCAutoLogin.Create(nil);
  InitialLogin := TUCInitialLogin.Create(nil);
  if not AutoLogin.MessageOnError then
    AutoLogin.MessageOnError := True;

  fDateExpireActive := False;
  fDaysOfSunExpired := 30;
end;

destructor TUCLogin.Destroy;
begin
  SysUtils.FreeAndNil(Self.FAutoLogin);
  SysUtils.FreeAndNil(Self.FInitialLogin);

  inherited Destroy;
end;

procedure TUCLogin.Assign(Source: TPersistent);
begin
  if Source is TUCLogin then
    Self.MaxLoginAttempts := TUCLogin(Source).MaxLoginAttempts
  else
    inherited;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TPerfilUsuarios'} {$ENDIF}
{ TPerfilUsuarios }

constructor TUCUserProfile.Create(AOwner: TComponent);
begin
  inherited Create;
  Self.Active := True;
end;

destructor TUCUserProfile.Destroy;
begin
  inherited Destroy;
end;

procedure TUCUserProfile.Assign(Source: TPersistent);
begin
  if Source is TUCUserProfile then
    Self.Active := TUCUserProfile(Source).Active
  else
    inherited;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TTrocarSenha'} {$ENDIF}
{ TTrocarSenha }

procedure TUCUserPasswordChange.Assign(Source: TPersistent);
begin
  if Source is TUCUserPasswordChange then
  begin
    Self.MenuItem := TUCUserPasswordChange(Source).MenuItem;
    Self.Action := TUCUserPasswordChange(Source).Action;
    Self.ForcePassword := TUCUserPasswordChange(Source).ForcePassword;
    Self.MinPasswordLength := TUCUserPasswordChange(Source).MinPasswordLength;
  end
  else
    inherited;
end;

constructor TUCUserPasswordChange.Create(AOwner: TComponent);
begin
  inherited Create;
  Self.ForcePassword := False;
end;

destructor TUCUserPasswordChange.Destroy;
begin
  inherited Destroy;
end;

procedure TUCUserPasswordChange.SetAction(const Value: TAction);
begin
  FAction := Value;
  if Value <> nil then
  begin
    Self.MenuItem := nil;
    Value.FreeNotification(Self.Action);
  end;
end;

procedure TUCUserPasswordChange.SetMenuItem(const Value: TMenuItem);
begin
  FMenuItem := Value;
  if Value <> nil then
  begin
    Self.Action := nil;
    Value.FreeNotification(Self.MenuItem);
  end;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TInitialLogin'} {$ENDIF}
{ TInitialLogin }

procedure TUCInitialLogin.Assign(Source: TPersistent);
begin
  if Source is TUCInitialLogin then
  begin
    Self.User := TUCInitialLogin(Source).User;
    Self.Password := TUCInitialLogin(Source).Password;
  end
  else
    inherited;
end;

constructor TUCInitialLogin.Create(AOwner: TComponent);
begin
  inherited Create;
  FInitialRights := TStringList.Create;
end;

destructor TUCInitialLogin.Destroy;
begin
  if Assigned(Self.FInitialRights) then
    Self.InitialRights.Free;
  inherited Destroy;
end;

procedure TUCInitialLogin.SetInitialRights(const Value: TStrings);
begin
  FInitialRights.Assign(Value);
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUCControlRight'} {$ENDIF}
{ TUCControlRight }

procedure TUCControlRight.Assign(Source: TPersistent);
begin
  if Source is TUCControlRight then
    Self.ActionList := TUCControlRight(Source).ActionList
    { .$IFDEF UCACTMANAGER }
    { .$ENDIF }
  else
    inherited;
end;

constructor TUCControlRight.Create(AOwner: TComponent);
begin
  inherited Create;
end;

destructor TUCControlRight.Destroy;
begin
  inherited Destroy;
end;

procedure TUCControlRight.SetActionList(const Value: TActionList);
begin
  FActionList := Value;
  if Value <> nil then
    Value.FreeNotification(Self.ActionList);
end;

{ .$IFDEF UCACTMANAGER }
procedure TUCControlRight.SetActionMainMenuBar(const Value: TActionMainMenuBar);
begin
  FActionMainMenuBar := Value;
  if Value <> nil then
    Value.FreeNotification(Self.ActionMainMenuBar);
end;

procedure TUCControlRight.SetActionManager(const Value: TActionManager);
begin
  FActionManager := Value;
  if Value <> nil then
    Value.FreeNotification(Self.ActionManager);
end;

{ .$ENDIF }

procedure TUCControlRight.SetMainMenu(const Value: TMenu);
begin
  FMainMenu := Value;
  if Value <> nil then
    Value.FreeNotification(Self.MainMenu);
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUCAppMessage'} {$ENDIF}
{ TUCAppMessage }

procedure TUCApplicationMessage.CheckMessages;

  function FmtDtHr(dt: String): String;
  begin
    Result := Copy(dt, 7, 2) + '/' + Copy(dt, 5, 2) + '/' + Copy(dt, 1, 4) + ' '
      + Copy(dt, 9, 2) + ':' + Copy(dt, 11, 2);
  end;

begin
  if not FReady then
    Exit;

  with Self.UserControl.DataConnector.UCGetSQLDataset('SELECT UCM.IdMsg, ' +
    'UCC.' + Self.UserControl.TableUsers.FieldUserName + ' AS De, ' + 'UCC_1.' +
    Self.UserControl.TableUsers.FieldUserName + ' AS Para, ' + 'UCM.Subject, ' +
    'UCM.Msg, ' + 'UCM.DtSend, ' + 'UCM.DtReceive ' + 'FROM (' +
    Self.TableMessages + ' UCM INNER JOIN ' +
    Self.UserControl.TableUsers.TableName + ' UCC ON UCM.UsrFrom = UCC.' +
    Self.UserControl.TableUsers.FieldUserID + ') INNER JOIN ' +
    Self.UserControl.TableUsers.TableName + ' UCC_1 ON UCM.UsrTo = UCC_1.' +
    Self.UserControl.TableUsers.FieldUserID +
    ' where UCM.DtReceive is NULL and  UCM.UsrTo = ' +
    IntToStr(Self.UserControl.CurrentUser.UserID)) do
  begin
    while not Eof do
    begin
      MsgRecForm := TMsgRecForm.Create(Self);
      MsgRecForm.stDe.Caption := FieldByName('De').AsString;
      MsgRecForm.stData.Caption := FmtDtHr(FieldByName('DtSend').AsString);
      MsgRecForm.stAssunto.Caption := FieldByName('Subject').AsString;
      MsgRecForm.MemoMsg.Text := FieldByName('msg').AsString;
      if Assigned(Self.UserControl.DataConnector) then
        Self.UserControl.DataConnector.UCExecSQL('Update ' + Self.TableMessages
          + ' set DtReceive =  ' + QuotedStr(FormatDateTime('YYYYMMDDhhmm', now)
          ) + ' Where  idMsg = ' + FieldByName('idMsg').AsString);
      MsgRecForm.Show;
      Next;
    end;
    Close;
    Free;
  end;
end;

constructor TUCApplicationMessage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReady := False;
  if csDesigning in ComponentState then
  begin
    if Self.TableMessages = '' then
      Self.TableMessages := 'UCTABMESSAGES';
    Interval := 60000;
    Active := True;
  end;
  Self.FVerifThread := TUCVerificaMensagemThread.Create(False);
  Self.FVerifThread.AOwner := Self;
  Self.FVerifThread.FreeOnTerminate := True;
end;

destructor TUCApplicationMessage.Destroy;
begin

  if not(csDesigning in ComponentState) then
    if Assigned(UserControl) then
      UserControl.DeleteLoginMonitor(Self);

//  Self.FVerifThread.Terminate;
//  FreeAndNil(FVerifThread);
  inherited Destroy;
end;

procedure TUCApplicationMessage.DeleteAppMessage(IdMsg: Integer);
begin
  if MessageDlg(FUserControl.UserSettings.AppMessages.MsgsForm_PromptDelete,
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  if Assigned(UserControl.DataConnector) then
    UserControl.DataConnector.UCExecSQL('Delete from ' + TableMessages +
      ' where IdMsg = ' + IntToStr(IdMsg));
end;

procedure TUCApplicationMessage.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    if not Assigned(FUserControl) then
      raise Exception.Create('Component UserControl not defined!');
    UserControl.AddLoginMonitor(Self);
    if not FUserControl.DataConnector.UCFindTable(TableMessages) then
      FUserControl.CriaTabelaMsgs(TableMessages);
  end;
  FReady := True;
end;

procedure TUCApplicationMessage.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if AOperation = opRemove then
    if AComponent = FUserControl then
      FUserControl := nil;
  inherited Notification(AComponent, AOperation);
end;

procedure TUCApplicationMessage.SendAppMessage(ToUser: Integer;
  Subject, Msg: String);
var
  UltId: Integer;
begin
  with UserControl.DataConnector.UCGetSQLDataset('Select Max(idMsg) as nr from '
    + TableMessages) do
  begin
    UltId := FieldByName('nr').AsInteger + 1;
    Close;
    Free;
  end;
  if Assigned(UserControl.DataConnector) then
    UserControl.DataConnector.UCExecSQL('Insert into ' + TableMessages +
      '( idMsg, UsrFrom, UsrTo, Subject, Msg, DtSend) Values (' +
      IntToStr(UltId) + ', ' + IntToStr(UserControl.CurrentUser.UserID) + ', ' +
      IntToStr(ToUser) + ', ' + QuotedStr(Subject) + ', ' + QuotedStr(Msg) +
      ', ' + QuotedStr(FormatDateTime('YYYYMMDDHHMM', now)) + ')');

end;

procedure TUCApplicationMessage.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if (csDesigning in ComponentState) then
    Exit;
  if FActive then
    FVerifThread.Resume
  else
    FVerifThread.Suspend;
end;

procedure TUCApplicationMessage.SetUserControl(const Value: TUserControl);
begin
  FUserControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TUCApplicationMessage.ShowMessages;
begin
  try
    MsgsForm := TMsgsForm.Create(Self);
    with FUserControl.UserSettings.AppMessages do
    begin
      MsgsForm.Caption := MsgsForm_WindowCaption;
      MsgsForm.btnova.Caption := MsgsForm_BtNew;
      MsgsForm.btResponder.Caption := MsgsForm_BtReplay;
      MsgsForm.btEncaminhar.Caption := MsgsForm_BtForward;
      MsgsForm.btExcluir.Caption := MsgsForm_BtDelete;
      MsgsForm.BtClose.Caption := MsgsForm_BtClose;

      MsgsForm.ListView1.Columns[0].Caption := MsgsForm_ColFrom;
      MsgsForm.ListView1.Columns[1].Caption := MsgsForm_ColSubject;
      MsgsForm.ListView1.Columns[2].Caption := MsgsForm_ColDate;
    end;

    MsgsForm.DSMsgs := UserControl.DataConnector.UCGetSQLDataset
      ('SELECT UCM.IdMsg, UCM.UsrFrom, UCC.' +
      Self.UserControl.TableUsers.FieldUserName + ' AS De, UCC_1.' +
      Self.UserControl.TableUsers.FieldUserName +
      ' AS Para, UCM.Subject, UCM.Msg, UCM.DtSend, UCM.DtReceive ' + 'FROM (' +
      TableMessages + ' UCM INNER JOIN ' + UserControl.TableUsers.TableName +
      ' UCC ON UCM.UsrFrom = UCC.' + Self.UserControl.TableUsers.FieldUserID +
      ') ' + ' INNER JOIN ' + UserControl.TableUsers.TableName +
      ' UCC_1 ON UCM.UsrTo = UCC_1.' + Self.UserControl.TableUsers.FieldUserID +
      ' WHERE UCM.UsrTo = ' + IntToStr(UserControl.CurrentUser.UserID) +
      ' ORDER BY UCM.DtReceive DESC');
    MsgsForm.DSMsgs.Open;
    MsgsForm.DSUsuarios := UserControl.DataConnector.UCGetSQLDataset
      ('SELECT ' + UserControl.TableUsers.FieldUserID + ' as idUser, ' +
      UserControl.TableUsers.FieldLogin + ' as Login, ' +
      UserControl.TableUsers.FieldUserName + ' as Nome, ' +
      UserControl.TableUsers.FieldPassword + ' as Senha, ' +
      UserControl.TableUsers.FieldEmail + ' as Email, ' +
      UserControl.TableUsers.FieldPrivileged + ' as Privilegiado, ' +
      UserControl.TableUsers.FieldTypeRec + ' as Tipo, ' +
      UserControl.TableUsers.FieldProfile + ' as Perfil ' + ' FROM ' +
      UserControl.TableUsers.TableName + ' WHERE ' +
      UserControl.TableUsers.FieldUserID + ' <> ' +
      IntToStr(UserControl.CurrentUser.UserID) + ' AND ' +
      UserControl.TableUsers.FieldTypeRec + ' = ' + QuotedStr('U') +
      ' ORDER BY ' + UserControl.TableUsers.FieldUserName);
    MsgsForm.DSUsuarios.Open;

    MsgsForm.Position := Self.FUserControl.UserSettings.WindowsPosition;
    MsgsForm.ShowModal;
  finally
  end;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TVerifThread'} {$ENDIF}
{ TVerifThread }

procedure TUCVerificaMensagemThread.Execute;
begin
  if (Assigned(TUCApplicationMessage(AOwner).UserControl)) and
    (TUCApplicationMessage(AOwner).UserControl.CurrentUser.UserID <> 0) then
    Synchronize(VerNovaMansagem);
  Sleep(TUCApplicationMessage(AOwner).Interval);
end;

procedure TUCVerificaMensagemThread.VerNovaMansagem;
begin
  TUCApplicationMessage(AOwner).CheckMessages;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUCCollectionItem'} {$ENDIF}
{ TUCCollectionItem }

function TUCExtraRightsItem.GetDisplayName: String;
begin
  Result := FormName + '.' + CompName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TUCExtraRightsItem.SetFormName(const Value: String);
begin
  if FFormName <> Value then
    FFormName := Value;
end;

procedure TUCExtraRightsItem.SetCompName(const Value: String);
begin
  if FCompName <> Value then
    FCompName := Value;
end;

procedure TUCExtraRightsItem.SetCaption(const Value: String);
begin
  if FCaption <> Value then
    FCaption := Value;
end;

procedure TUCExtraRightsItem.SetGroupName(const Value: String);
begin
  if FGroupName <> Value then
    FGroupName := Value;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUCCollection'} {$ENDIF}
{ TUCCollection }

constructor TUCExtraRights.Create(UCBase: TUserControl);
begin
  inherited Create(TUCExtraRightsItem);
  FUCBase := UCBase;
end;

function TUCExtraRights.Add: TUCExtraRightsItem;
begin
  Result := TUCExtraRightsItem(inherited Add);
end;

function TUCExtraRights.GetItem(Index: Integer): TUCExtraRightsItem;
begin
  Result := TUCExtraRightsItem(inherited GetItem(Index));
end;

procedure TUCExtraRights.SetItem(Index: Integer; Value: TUCExtraRightsItem);
begin
  inherited SetItem(Index, Value);
end;

function TUCExtraRights.GetOwner: TPersistent;
begin
  Result := FUCBase;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUCRun'} {$ENDIF}
{ TUCRun }

procedure TUCExecuteThread.Execute;
begin
  while not Self.Terminated do
  begin
    if TUserControl(AOwner).DataConnector.UCFindDataConnection then
      Synchronize(UCStart);
    Sleep(50);
  end;
end;

procedure TUCExecuteThread.UCStart;
begin
  TUserControl(AOwner).Execute;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUControls'} {$ENDIF}
{ TUCControls }

function TUCControls.GetActiveForm: String;
begin
  Result := Owner.Name;
end;

function TUCControls.GetAccessType: String;
begin
  if not Assigned(UserControl) then
    Result := ''
  else
    Result := UserControl.ClassName;
end;

procedure TUCControls.ListComponents(Form: String; List: TStringList);
var
  Contador: Integer;
begin
  if not Assigned(List) then
    Exit;
  if not Assigned(UserControl) then
    Exit;
  List.Clear;
  for Contador := 0 to Pred(UserControl.ExtraRights.Count) do
    if UpperCase(UserControl.ExtraRights[Contador].FormName) = UpperCase(Form)
    then
      List.Add(UserControl.ExtraRights[Contador].CompName); // List.Append
end;

procedure TUCControls.ApplyRights;
var
  FListObj: TStringList;
  TempDS: TDataSet;
  Contador: Integer;
  SQLstmt: String;
  ExisteObj: Boolean;
  String1: String;
  String2: String;
begin
  // Apply Extra Rights

  if not Assigned(UserControl) then
    Exit;
  with UserControl do
  begin
    if (UserControl.LoginMode = lmActive) and (CurrentUser.UserID = 0) then
      Exit;

    FListObj := TStringList.Create;
    Self.ListComponents(Self.Owner.Name, FListObj);

    if UserControl.DataConnector.UCFindDataConnection then
    begin
      // permissoes do usuario
      SQLstmt := Format('SELECT %s AS UserID,' + '       %s AS ObjName,' +
        '       %s AS UCKey ' + 'FROM %sEX ' + 'WHERE %s = %d AND ' +
        '      %s = %s AND ' + '      %s = %s', [TableRights.FieldUserID,
        TableRights.FieldComponentName, TableRights.FieldKey,
        TableRights.TableName, TableRights.FieldUserID, CurrentUser.UserID,
        TableRights.FieldModule, QuotedStr(ApplicationID),
        TableRights.FieldFormName, QuotedStr(Self.Owner.Name)]);

      TempDS := DataConnector.UCGetSQLDataset(SQLstmt);

      for Contador := 0 to Pred(FListObj.Count) do
      begin
        UnlockEX(TCustomForm(Self.Owner), FListObj[Contador]);

        ExisteObj := (TempDS.Locate('ObjName', FListObj[Contador], []));

        case Self.UserControl.Criptografia of
          cPadrao:
            begin
              String1 := Decrypt(TempDS.FieldByName('UCKey').AsString,
                EncryptKey);
              String2 := TempDS.FieldByName('UserID').AsString +
                TempDS.FieldByName('ObjName').AsString;
            end;
          cMD5:
            begin
              String1 := TempDS.FieldByName('UCKey').AsString;
              String2 := MD5Sum(TempDS.FieldByName('UserID').AsString +
                TempDS.FieldByName('ObjName').AsString);
            end;
        end;

        if not ExisteObj or (String1 <> String2) then
          LockEX(TCustomForm(Self.Owner), FListObj[Contador],
            NotAllowed = naInvisible);
      end;
      TempDS.Close;
      FreeAndNil(TempDS); // By Cleilson Sousa

      // permissoes do grupo
      SQLstmt := Format('SELECT' + '      %s AS UserID,' +
        '      %s AS ObjName,' + '      %s AS UCKey ' + 'FROM %sEX ' +
        'WHERE %s = %d AND ' + '      %s = %s AND ' + '      %s = %s',
        [TableRights.FieldUserID, TableRights.FieldComponentName,
        TableRights.FieldKey, TableRights.TableName, TableRights.FieldUserID,
        CurrentUser.Profile, TableRights.FieldModule, QuotedStr(ApplicationID),
        TableRights.FieldFormName, QuotedStr(Self.Owner.Name)]);

      TempDS := DataConnector.UCGetSQLDataset(SQLstmt);

      for Contador := 0 to Pred(FListObj.Count) do
      begin
        ExisteObj := (TempDS.Locate('ObjName', FListObj[Contador], []));

        case Self.UserControl.Criptografia of
          cPadrao:
            begin
              String1 := Decrypt(TempDS.FieldByName('UCKey').AsString,
                EncryptKey);
              String2 := TempDS.FieldByName('UserID').AsString +
                TempDS.FieldByName('ObjName').AsString;
            end;
          cMD5:
            begin
              String1 := TempDS.FieldByName('UCKey').AsString;
              String2 := MD5Sum(TempDS.FieldByName('UserID').AsString +
                TempDS.FieldByName('ObjName').AsString);
            end;
        end;

        if ExisteObj and (String1 = String2) then
          UnlockEX(TCustomForm(Self.Owner), FListObj[Contador]);
      end;
      TempDS.Close;
      FreeAndNil(TempDS); // By Cleilson Sousa
    end
    else
      LockControls;
  end;
  FreeAndNil(FListObj);
end;
{$WARNINGS ON}

procedure TUCControls.LockControls;
var
  Contador: Integer;
  FListObj: TStringList;
begin
  FListObj := TStringList.Create;
  Self.ListComponents(Self.Owner.Name, FListObj);
  for Contador := 0 to Pred(FListObj.Count) do
    UserControl.LockEX(TCustomForm(Self.Owner), FListObj[Contador],
      NotAllowed = naInvisible);
  FreeAndNil(FListObj);
end;

procedure TUCControls.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    ApplyRights;
    UserControl.AddUCControlMonitor(Self);
  end;
end;

procedure TUCControls.SetGroupName(const Value: String);
var
  Contador: Integer;
begin
  if FGroupName = Value then
    Exit;
  FGroupName := Value;
  if Assigned(UserControl) then
    for Contador := 0 to Pred(UserControl.ExtraRights.Count) do
      if UpperCase(UserControl.ExtraRights[Contador].FormName)
        = UpperCase(Owner.Name) then
        UserControl.ExtraRights[Contador].GroupName := Value;
end;

destructor TUCControls.Destroy;
begin
  if not(csDesigning in ComponentState) then
    if Assigned(UserControl) then
      UserControl.DeleteUCControlMonitor(Self);

  inherited Destroy;
end;

procedure TUCControls.SetUserControl(const Value: TUserControl);
begin
  FUserControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self.UserControl);
end;

procedure TUCControls.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if AOperation = opRemove then
    if AComponent = FUserControl then
      FUserControl := nil;

  inherited Notification(AComponent, AOperation);
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUCGUID'} {$ENDIF}
{ TUCGUID }

class function TUCGUID.EmptyGUID: TGUID;
begin
  Result := FromString('{00000000-0000-0000-0000-000000000000}');
end;

class function TUCGUID.EqualGUIDs(GUID1, GUID2: TGUID): Boolean;
begin
  Result := IsEqualGUID(GUID1, GUID2);
end;

class function TUCGUID.FromString(Value: String): TGUID;
begin
  Result := StringToGuid(Value);
end;

class function TUCGUID.IsEmptyGUID(GUID: TGUID): Boolean;
begin
  Result := EqualGUIDs(GUID, EmptyGUID);
end;

class function TUCGUID.NovoGUID: TGUID;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUID;
end;

class function TUCGUID.NovoGUIDString: String;
begin
  Result := ToString(NovoGUID);
end;

class function TUCGUID.ToQuotedString(GUID: TGUID): String;
begin
  Result := QuotedStr(ToString(GUID));
end;

class function TUCGUID.ToString(GUID: TGUID): String;
begin
  Result := GuidToString(GUID);
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUSERLOGGED'} {$ENDIF}
{ TUserLogged }

procedure TUCUsersLogged.AddCurrentUser;
var
  SQLstmt: String;
begin
  if not Active then
    Exit;

  with FUserControl do
  begin
    CurrentUser.IdLogon := TUCGUID.NovoGUIDString;
    SQLstmt :=
      Format('INSERT INTO %s (%s, %s, %s, %s, %s) Values( %s, %d, %s, %s, %s)',
      [TableUsersLogged.TableName, TableUsersLogged.FieldLogonID,
      TableUsersLogged.FieldUserID, TableUsersLogged.FieldApplicationID,
      TableUsersLogged.FieldMachineName, TableUsersLogged.FieldData,
      QuotedStr(CurrentUser.IdLogon), CurrentUser.UserID,
      QuotedStr(ApplicationID), QuotedStr(GetLocalComputerName),
      QuotedStr(FormatDateTime('dd/mm/yy hh:mm', now))]);
    if Assigned(DataConnector) then
      DataConnector.UCExecSQL(SQLstmt);
  end;
end;

procedure TUCUsersLogged.Assign(Source: TPersistent);
begin
  if Source is TUCUsersLogged then
  begin
    Self.Active := TUCUsersLogged(Source).Active;
  end
  else
    inherited;
end;

constructor TUCUsersLogged.Create(AOwner: TComponent);
begin
  inherited Create;
  FUserControl := TUserControl(AOwner);
  Self.FAtive := True;
end;

procedure TUCUsersLogged.CriaTableUserLogado;
var
  SQLstmt: String;
begin
  if not Active then
    Exit;

  with FUserControl.TableUsersLogged do
    SQLstmt :=
      Format('CREATE TABLE %s (%s %s(38), %s %s, %s %s(50), %s %s(50), %s %s(14))',
      [TableName, FieldLogonID, FUserControl.UserSettings.Type_Char,

      FieldUserID, FUserControl.UserSettings.Type_Int,

      FieldApplicationID, FUserControl.UserSettings.Type_VarChar,

      FieldMachineName, FUserControl.UserSettings.Type_VarChar,

      FieldData, FUserControl.UserSettings.Type_VarChar]);
  if Assigned(FUserControl.DataConnector) then
    FUserControl.DataConnector.UCExecSQL(SQLstmt);
end;

procedure TUCUsersLogged.DelCurrentUser;
var
  SQLstmt: String;
begin
  if not Active then
    Exit;

  if Assigned(FUserControl.DataConnector) = False then
    Exit;

  with FUserControl do
  begin
    SQLstmt := Format('DELETE FROM %s WHERE %s = %s',
      [TableUsersLogged.TableName, TableUsersLogged.FieldLogonID,
      QuotedStr(CurrentUser.IdLogon)]);

    if Assigned(DataConnector) then
      DataConnector.UCExecSQL(SQLstmt);
  end;
end;

destructor TUCUsersLogged.Destroy;
begin
  inherited Destroy;
end;

function TUCUsersLogged.UsuarioJaLogado(ID: Integer): Boolean;
var
  SQLstmt: String;
  FDataset: TDataSet;
begin
  Result := False;
  if Assigned(FUserControl.DataConnector) = False then
    Exit;

  with FUserControl do
  begin
    SQLstmt := Format('SELECT * FROM %s WHERE %s = %s',
      [TableUsersLogged.TableName, TableUsersLogged.FieldUserID,
      QuotedStr(IntToStr(ID))]);

    if Assigned(DataConnector) then
    begin
      FDataset := DataConnector.UCGetSQLDataset(SQLstmt);
      Result := not(FDataset.IsEmpty);
    end;
  end;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUCUserLogoff'} {$ENDIF}
{ TUCUserLogoff }

procedure TUCUserLogoff.Assign(Source: TPersistent);
begin
  if Source is TUCUserLogoff then
  begin
    Self.MenuItem := TUCUserLogoff(Source).MenuItem;
    Self.Action := TUCUserLogoff(Source).Action;
  end
  else
    inherited;
end;

constructor TUCUserLogoff.Create(AOwner: TComponent);
begin
  inherited Create;
end;

destructor TUCUserLogoff.Destroy;
begin
  inherited Destroy;
end;

procedure TUCUserLogoff.SetAction(const Value: TAction);
begin
  FAction := Value;
  if Value <> nil then
  begin
    Self.MenuItem := nil;
    Value.FreeNotification(Self.Action);
  end;
end;

procedure TUCUserLogoff.SetMenuItem(const Value: TMenuItem);
begin
  FMenuItem := Value;
  if Value <> nil then
  begin
    Self.Action := nil;
    Value.FreeNotification(Self.MenuItem);
  end;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TUCCurrentUser'} {$ENDIF}
{ TUCCurrentUser }

constructor TUCCurrentUser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TUCCurrentUser.Destroy;
begin
  if Assigned(FPerfilUsuario) then
    SysUtils.FreeAndNil(FPerfilUsuario);
  if Assigned(FPerfilGrupo) then
    SysUtils.FreeAndNil(FPerfilGrupo);
  inherited;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI9_UP} {$REGION 'TDepartment'} {$ENDIF}


procedure TUserControl.CriaTabelaDepartment(TableExists: Boolean);   { MAuri }
Var
  SQLStmt:          String;

begin
  if not TableExists then
    with TableUserDepartment do
    begin
      SQLStmt := Format('Create Table %s( %s %s(10) NOT NULL, %s %s(50), %s %s(1))',
        [TableName,
          FieldIDDepartment, UserSettings.Type_VarChar,
          FieldNameDepartment, UserSettings.Type_VarChar,
          FieldStatusDepartment, UserSettings.Type_VarChar
        ]);

      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(SQLstmt);
       //     Criar Chave Primaria para a Tabela     Mauri 06/05/2017
       SQLStmt := Format('ALTER TABLE %s ADD CONSTRAINT PK_%s PRIMARY KEY (%s)',
                          [TableName, TableName, FieldIDDepartment, UserSettings.Type_VarChar]);

      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(SQLstmt);

    end;

end;
{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}

{$IFDEF DELPHI9_UP} {$REGION 'TEmpresa'} {$ENDIF}

procedure TUserControl.CriaTabelaEmpresa(TableExists: Boolean);
Var
  SQLStmt:          String;

begin
  if not TableExists then
    with TableUserEmpresa do
    begin
      SQLStmt := Format('Create Table %s( %s %s NOT NULL, %s %s(60))',
        [TableName,
        FieldIDEmpresa,
        UserSettings.Type_Int,

        FieldNameEmpresa,
        UserSettings.Type_VarChar

        ]);

      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(SQLstmt);
       //     Criar Chave Primaria para a Tabela     Mauri 06/05/2017
       SQLStmt := Format('ALTER TABLE %s ADD CONSTRAINT PK_%s PRIMARY KEY (%s)',
                          [TableName, TableName, FieldIDEmpresa, UserSettings.Type_Int]);

      if Assigned(DataConnector) then
        DataConnector.UCExecSQL(SQLstmt);


    end;
end;
{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}



end.
