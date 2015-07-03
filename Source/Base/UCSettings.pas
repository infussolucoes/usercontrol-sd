unit UCSettings;

interface

uses
  Classes,
  Forms,
  UCMessages,
  UcConsts_Language;

type
  TUCSettings = class(TComponent)
  private
    FAddProfileFormMSG:  TUCAddProfileFormMSG;
    FAddUserFormMSG:     TUCAddUserFormMSG;
    FCadUserFormMSG:     TUCCadUserFormMSG;
    FLogControlFormMSG:  TUCLogControlFormMSG;
    FLoginFormMSG:       TUCLoginFormMSG;
    FPermissFormMSG:     TUCPermissFormMSG;
    FProfileUserFormMSG: TUCProfileUserFormMSG;
    FResetPassword:      TUCResetPassword;
    FTrocaSenhaFormMSG:  TUCTrocaSenhaFormMSG;
    FUserCommomMSG:      TUCUserCommonMSG;
    FAppMessagesMSG:     TUCAppMessagesMSG;
    FPosition:           TPosition;
    fLanguage: TUCLanguage;
    fUsersLogged: TUCCadUserLoggedMSG;
    fBancoDados: TUCTypeBancoDados;
    procedure SetFAddProfileFormMSG(const Value: TUCAddProfileFormMSG);
    procedure SetFAddUserFormMSG(const Value: TUCAddUserFormMSG);
    procedure SetFCadUserFormMSG(const Value: TUCCadUserFormMSG);
    procedure SetFFormLoginMsg(const Value: TUCLoginFormMSG);
    procedure SetFLogControlFormMSG(const Value: TUCLogControlFormMSG);
    procedure SetFPermissFormMSG(const Value: TUCPermissFormMSG);
    procedure SetFProfileUserFormMSG(const Value: TUCProfileUserFormMSG);
    procedure SetFResetPassword(const Value: TUCResetPassword);
    procedure SetFTrocaSenhaFormMSG(const Value: TUCTrocaSenhaFormMSG);
    procedure SetFUserCommonMSg(const Value: TUCUserCommonMSG);
    procedure SetAppMessagesMSG(const Value: TUCAppMessagesMSG);
    procedure SetfLanguage(const Value: TUCLanguage);
    procedure SetfUsersLogged(const Value: TUCCadUserLoggedMSG);
    procedure SetfBancoDados(const Value: TUCTypeBancoDados);
  protected
  public
    Type_Int     ,
    Type_Char    ,
    Type_VarChar ,
    Type_Memo    : String;  
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AppMessages: TUCAppMessagesMSG read FAppMessagesMSG write SetAppMessagesMSG;
    property CommonMessages: TUCUserCommonMSG read FUserCommomMSG write SetFUserCommonMSg;
    property Login: TUCLoginFormMSG read FLoginFormMSG write SetFFormLoginMsg;
    property Log: TUCLogControlFormMSG read FLogControlFormMSG write SetFLogControlFormMSG;
    property UsersForm: TUCCadUserFormMSG read FCadUserFormMSG write SetFCadUserFormMSG;
    property AddChangeUser: TUCAddUserFormMSG read FAddUserFormMSG write SetFAddUserFormMSG;
    property AddChangeProfile: TUCAddProfileFormMSG read FAddProfileFormMSG write SetFAddProfileFormMSG;
    property UsersProfile: TUCProfileUserFormMSG read FProfileUserFormMSG write SetFProfileUserFormMSG;
    property Rights: TUCPermissFormMSG read FPermissFormMSG write SetFPermissFormMSG;
    property ChangePassword: TUCTrocaSenhaFormMSG read FTrocaSenhaFormMSG write SetFTrocaSenhaFormMSG;
    property ResetPassword: TUCResetPassword read FResetPassword write SetFResetPassword;
    Property BancoDados  : TUCTypeBancoDados read fBancoDados write SetfBancoDados;
    property WindowsPosition: TPosition read FPosition write FPosition default poMainFormCenter;
    Property Language : TUCLanguage read fLanguage write SetfLanguage;
    property UsersLogged : TUCCadUserLoggedMSG read fUsersLogged write SetfUsersLogged;    
  end;

procedure IniSettings (DestSettings: TUCUserSettings);
procedure IniSettings2(DestSettings: TUCSettings);

procedure AlterLanguage(DestSettings: TUCUserSettings);
procedure AlterLanguage2(DestSettings: TUCSettings);

procedure RetornaSqlBancoDados( fBanco : TUCTypeBancoDados ; var Int, Char, VarChar,
  Memo: String);

implementation

uses
  Graphics,
  SysUtils,
  UCBase;
//  UCConsts;

{$IFDEF DELPHI9_UP} {$REGION 'Inicializacao'} {$ENDIF}

procedure RetornaSqlBancoDados( fBanco : TUCTypeBancoDados ; var Int, Char, VarChar,
  Memo: String);
begin
  Int     := 'INT';
  Char    := 'CHAR';
  VarChar := 'VARCHAR';

  Case fBanco of
    Firebird   : Memo := 'BLOB SUB_TYPE 1 SEGMENT SIZE 1024';
    Interbase  : Memo := 'BLOB SUB_TYPE 1 SEGMENT SIZE 1024';
    MySql      : Memo := 'MEDIUMBLOB';
    PARADOX    : Memo := 'BLOB(1024,1)';
    Oracle     : Memo := 'LONG RAW';
    SqlServer  : Memo := 'NTEXT';
    PostgreSQL : Memo := 'TEXT';
  End;
end;

procedure IniSettings(DestSettings: TUCUserSettings);
var
  tmp: TBitmap;
begin
  with DestSettings.CommonMessages do
  begin
    if BlankPassword = '' then
      BlankPassword := RetornaLingua( ucPortuguesBr ,'Const_Men_SenhaDesabitada');
    if PasswordChanged = '' then
      PasswordChanged := RetornaLingua( ucPortuguesBr ,'Const_Men_SenhaAlterada');
    if InitialMessage.Text = '' then
      InitialMessage.Text := RetornaLingua( ucPortuguesBr ,'Const_Men_MsgInicial');
    if MaxLoginAttemptsError = '' then
      MaxLoginAttemptsError := RetornaLingua( ucPortuguesBr ,'Const_Men_MaxTentativas');
    if InvalidLogin = '' then
      InvalidLogin := RetornaLingua( ucPortuguesBr ,'Const_Men_LoginInvalido');
    if InactiveLogin = '' then
      InactiveLogin := RetornaLingua( ucPortuguesBr ,'Const_Men_LoginInativo');

    if AutoLogonError = '' then
      AutoLogonError := RetornaLingua( ucPortuguesBr ,'Const_Men_AutoLogonError');
    if UsuarioExiste = '' then
      UsuarioExiste := RetornaLingua( ucPortuguesBr ,'Const_Men_UsuarioExiste');
    if PasswordExpired  = '' then
      PasswordExpired := RetornaLingua( ucPortuguesBr ,'Const_Men_PasswordExpired');
    If ForcaTrocaSenha = '' then
      ForcaTrocaSenha := RetornaLingua( ucPortuguesBr ,'Const_ErrPass_ForcaTrocaSenha');
  end;

  with DestSettings.Login do
  begin
    if BtCancel = '' then
      BtCancel := RetornaLingua( ucPortuguesBr ,'Const_Log_BtCancelar');
    if BtOK = '' then
      BtOK := RetornaLingua( ucPortuguesBr ,'Const_Log_BtOK');
    if LabelPassword = '' then
      LabelPassword := RetornaLingua( ucPortuguesBr ,'Const_Log_LabelSenha');
    if LabelUser = '' then
      LabelUser := RetornaLingua( ucPortuguesBr ,'Const_Log_LabelUsuario');
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Log_WindowCaption');

    if LabelTentativa = '' then
      LabelTentativa := RetornaLingua( ucPortuguesBr ,'Const_Log_LabelTentativa');
    if LabelTentativas = '' then
      LabelTentativas := RetornaLingua( ucPortuguesBr ,'Const_Log_LabelTentativas');


    try
      Tmp := TBitmap.Create;
      Tmp.LoadFromResourceName(HInstance, 'UCLOCKLOGIN');
      LeftImage.Assign(tmp);
    finally
      FreeAndNil(tmp);
    end;
  end;

  with DestSettings.UsersForm do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Cad_WindowCaption');
    if LabelDescription = '' then
      LabelDescription := RetornaLingua( ucPortuguesBr ,'Const_Cad_LabelDescricao');
    if ColName = '' then
      ColName := RetornaLingua( ucPortuguesBr ,'Const_Cad_ColunaNome');
    if ColLogin = '' then
      ColLogin := RetornaLingua( ucPortuguesBr ,'Const_Cad_ColunaLogin');
    if ColEmail = '' then
      ColEmail := RetornaLingua( ucPortuguesBr ,'Const_Cad_ColunaEmail');
    if BtAdd = '' then
      BtAdd := RetornaLingua( ucPortuguesBr ,'Const_Cad_BtAdicionar');
    if BtChange = '' then
      BtChange := RetornaLingua( ucPortuguesBr ,'Const_Cad_BtAlterar');
    if BtDelete = '' then
      BtDelete := RetornaLingua( ucPortuguesBr ,'Const_Cad_BtExcluir');
    if BtRights = '' then
      BtRights := RetornaLingua( ucPortuguesBr ,'Const_Cad_BtPermissoes');
    if BtPassword = '' then
      BtPassword := RetornaLingua( ucPortuguesBr ,'Const_Cad_BtSenha');
    if BtClose = '' then
      BtClose := RetornaLingua( ucPortuguesBr ,'Const_Cad_BtFechar');
    if PromptDelete = '' then
      PromptDelete := RetornaLingua( ucPortuguesBr ,'Const_Cad_ConfirmaExcluir');
    if PromptDelete_WindowCaption = '' then
      PromptDelete_WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Cad_ConfirmaDelete_WindowCaption'); //added by fduenas
  end;

  with DestSettings.UsersProfile do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Prof_WindowCaption');
    if LabelDescription = '' then
      LabelDescription := RetornaLingua( ucPortuguesBr ,'Const_Prof_LabelDescricao');
    if ColProfile = '' then
      ColProfile := RetornaLingua( ucPortuguesBr ,'Const_Prof_ColunaNome');
    if BtAdd = '' then
      BtAdd := RetornaLingua( ucPortuguesBr ,' Const_Prof_BtAdicionar');
    if BtChange = '' then
      BtChange := RetornaLingua( ucPortuguesBr ,'Const_Prof_BtAlterar');
    if BtDelete = '' then
      BtDelete := RetornaLingua( ucPortuguesBr ,'Const_Prof_BtExcluir');
    if BtRights = '' then
      BtRights := RetornaLingua( ucPortuguesBr ,'Const_Prof_BtPermissoes');
    if BtClose = '' then
      BtClose := RetornaLingua( ucPortuguesBr ,'Const_Prof_BtFechar');
    if PromptDelete = '' then
      PromptDelete := RetornaLingua( ucPortuguesBr ,'Const_Prof_ConfirmaExcluir');
    if PromptDelete_WindowCaption = '' then
      PromptDelete_WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Prof_ConfirmaDelete_WindowCaption'); //added by fduenas
  end;

  with DestSettings.AddChangeUser do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Inc_WindowCaption');
    if LabelAdd = '' then
      LabelAdd := RetornaLingua( ucPortuguesBr ,'Const_Inc_LabelAdicionar');
    if LabelChange = '' then
      LabelChange := RetornaLingua( ucPortuguesBr ,'Const_Inc_LabelAlterar');
    if LabelName = '' then
      LabelName := RetornaLingua( ucPortuguesBr ,'Const_Inc_LabelNome');
    if LabelLogin = '' then
      LabelLogin := RetornaLingua( ucPortuguesBr ,'Const_Inc_LabelLogin');
    if LabelEmail = '' then
      LabelEmail := RetornaLingua( ucPortuguesBr ,'Const_Inc_LabelEmail');
    if LabelPerfil = '' then
      LabelPerfil := RetornaLingua( ucPortuguesBr ,'Const_Inc_LabelPerfil');
    if CheckPrivileged = '' then
      CheckPrivileged := RetornaLingua( ucPortuguesBr ,'Const_Inc_CheckPrivilegiado');
      
    if BtSave = '' then
      BtSave := RetornaLingua( ucPortuguesBr ,'Const_Inc_BtGravar');
    if BtCancel = '' then
      BtCancel := RetornaLingua( ucPortuguesBr ,'Const_Inc_BtCancelar');

    if CheckExpira = '' then
      CheckExpira := RetornaLingua( ucPortuguesBr ,'Const_Inc_CheckEspira');
    If Day = '' then
      Day := RetornaLingua( ucPortuguesBr ,'Const_Inc_Dia');
    If ExpiredIn = '' then
      ExpiredIn := RetornaLingua( ucPortuguesBr ,'Const_Inc_ExpiraEm');

    If LabelStatus = '' then
      LabelStatus := RetornaLingua( ucPortuguesBr ,'Const_Inc_LabelStatus');

    If StatusActive = '' then
      StatusActive := RetornaLingua( ucPortuguesBr ,'Const_Inc_StatusActive');

    If StatusDisabled = '' then
      StatusDisabled := RetornaLingua( ucPortuguesBr ,'Const_Inc_StatusDisabled');
  end;

  with DestSettings.AddChangeProfile do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_PInc_WindowCaption');
    if LabelAdd = '' then
      LabelAdd := RetornaLingua( ucPortuguesBr ,'Const_PInc_LabelAdicionar');
    if LabelChange = '' then
      LabelChange := RetornaLingua( ucPortuguesBr ,'Const_PInc_LabelAlterar');
    if LabelName = '' then
      LabelName := RetornaLingua( ucPortuguesBr ,'Const_PInc_LabelNome');
    if BtSave = '' then
      BtSave := RetornaLingua( ucPortuguesBr ,'Const_PInc_BtGravar');
    if BtCancel = '' then
      BtCancel := RetornaLingua( ucPortuguesBr ,'Const_PInc_BtCancelar');
  end;

  with DestSettings.Rights do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Perm_WindowCaption');
    if LabelUser = '' then
      LabelUser := RetornaLingua( ucPortuguesBr ,'Const_Perm_LabelUsuario');
    if LabelProfile = '' then
      LabelProfile := RetornaLingua( ucPortuguesBr ,'Const_Perm_LabelPerfil');
    if PageMenu = '' then
      PageMenu := RetornaLingua( ucPortuguesBr ,'Const_Perm_PageMenu');
    if PageActions = '' then
      PageActions := RetornaLingua( ucPortuguesBr ,'Const_Perm_PageActions');
    If PageControls = '' then
      PageControls := RetornaLingua( ucPortuguesBr ,'Const_Perm_PageControls'); // by vicente barros leonel
    if BtUnlock = '' then
      BtUnlock := RetornaLingua( ucPortuguesBr ,'Const_Perm_BtLibera');
    if BtLock = '' then
      BtLock := RetornaLingua( ucPortuguesBr ,'Const_Perm_BtBloqueia');
    if BtSave = '' then
      BtSave := RetornaLingua( ucPortuguesBr ,'Const_Perm_BtGravar');
    if BtCancel = '' then
      BtCancel := RetornaLingua( ucPortuguesBr ,'Const_Perm_BtCancelar');
  end;

  with DestSettings.ChangePassword do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Troc_WindowCaption');
    if LabelDescription = '' then
      LabelDescription := RetornaLingua( ucPortuguesBr ,'Const_Troc_LabelDescricao');
    if LabelCurrentPassword = '' then
      LabelCurrentPassword := RetornaLingua( ucPortuguesBr ,'Const_Troc_LabelSenhaAtual');
    if LabelNewPassword = '' then
      LabelNewPassword := RetornaLingua( ucPortuguesBr ,'Const_Troc_LabelNovaSenha');
    if LabelConfirm = '' then
      LabelConfirm := RetornaLingua( ucPortuguesBr ,'Const_Troc_LabelConfirma');
    if BtSave = '' then
      BtSave := RetornaLingua( ucPortuguesBr ,'Const_Troc_BtGravar');
    if BtCancel = '' then
      BtCancel := RetornaLingua( ucPortuguesBr ,'Const_Troc_BtCancelar');
  end;

  with DestSettings.CommonMessages.ChangePasswordError do
  begin
    if InvalidCurrentPassword = '' then
      InvalidCurrentPassword := RetornaLingua( ucPortuguesBr ,'Const_ErrPass_SenhaAtualInvalida');
    if NewPasswordError = '' then
      NewPasswordError := RetornaLingua( ucPortuguesBr ,'Const_ErrPass_ErroNovaSenha');
    if NewEqualCurrent = '' then
      NewEqualCurrent := RetornaLingua( ucPortuguesBr ,'Const_ErrPass_NovaIgualAtual');
    if PasswordRequired = '' then
      PasswordRequired := RetornaLingua( ucPortuguesBr ,'Const_ErrPass_SenhaObrigatoria');
    if MinPasswordLength = '' then
      MinPasswordLength := RetornaLingua( ucPortuguesBr ,'Const_ErrPass_SenhaMinima');
    if InvalidNewPassword = '' then
      InvalidNewPassword := RetornaLingua( ucPortuguesBr ,'Const_ErrPass_SenhaInvalida');
  end;

  with DestSettings.ResetPassword do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_DefPass_WindowCaption');
    if LabelPassword = '' then
      LabelPassword := RetornaLingua( ucPortuguesBr ,'Const_DefPass_LabelSenha');
  end;

  with DestSettings.Log do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_LogC_WindowCaption');
    if LabelDescription = '' then
      LabelDescription := RetornaLingua( ucPortuguesBr ,'Const_LogC_LabelDescricao');
    if LabelUser = '' then
      LabelUser := RetornaLingua( ucPortuguesBr ,'Const_LogC_LabelUsuario');
    if LabelDate = '' then
      LabelDate := RetornaLingua( ucPortuguesBr ,'Const_LogC_LabelData');
    if LabelLevel = '' then
      LabelLevel := RetornaLingua( ucPortuguesBr ,'Const_LogC_LabelNivel');
    if ColLevel = '' then
      ColLevel := RetornaLingua( ucPortuguesBr ,'Const_LogC_ColunaNivel');
    if ColAppID = '' then
      ColAppID := RetornaLingua( ucPortuguesBr ,'Const_LogC_ColunaAppID');
    if ColMessage = '' then
      ColMessage := RetornaLingua( ucPortuguesBr ,'Const_LogC_ColunaMensagem');
    if ColUser = '' then
      ColUser := RetornaLingua( ucPortuguesBr ,'Const_LogC_ColunaUsuario');
    if ColDate = '' then
      ColDate := RetornaLingua( ucPortuguesBr ,'Const_LogC_ColunaData');
    if BtFilter = '' then
      BtFilter := RetornaLingua( ucPortuguesBr ,'Const_LogC_BtFiltro');
    if BtDelete = '' then
      BtDelete := RetornaLingua( ucPortuguesBr ,'Const_LogC_BtExcluir');
    if BtClose = '' then
      BtClose := RetornaLingua( ucPortuguesBr ,'Const_LogC_BtFechar');
    if PromptDelete = '' then
      PromptDelete := RetornaLingua( ucPortuguesBr ,'Const_LogC_ConfirmaExcluir');
    if PromptDelete_WindowCaption = '' then
      PromptDelete_WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_LogC_ConfirmaDelete_WindowCaption'); //added by fduenas
    if OptionUserAll = '' then
      OptionUserAll := RetornaLingua( ucPortuguesBr ,'Const_LogC_Todos'); //added by fduenas
    if OptionLevelLow = '' then
      OptionLevelLow := RetornaLingua( ucPortuguesBr ,'Const_LogC_Low'); //added by fduenas
    if OptionLevelNormal = '' then
      OptionLevelNormal := RetornaLingua( ucPortuguesBr ,'Const_LogC_Normal'); //added by fduenas
    if OptionLevelHigh = '' then
      OptionLevelHigh := RetornaLingua( ucPortuguesBr ,'Const_LogC_High'); //added by fduenas
    if OptionLevelCritic = '' then
      OptionLevelCritic := RetornaLingua( ucPortuguesBr ,'Const_LogC_Critic'); //added by fduenas
    if DeletePerformed = '' then
      DeletePerformed := RetornaLingua( ucPortuguesBr ,'Const_LogC_ExcluirEfectuada'); //added by fduenas
  end;

  with DestSettings.AppMessages do
  begin
    if MsgsForm_BtNew = '' then
      MsgsForm_BtNew := RetornaLingua( ucPortuguesBr ,'Const_Msgs_BtNew');
    if MsgsForm_BtReplay = '' then
      MsgsForm_BtReplay := RetornaLingua( ucPortuguesBr ,'Const_Msgs_BtReplay');
    if MsgsForm_BtForward = '' then
      MsgsForm_BtForward := RetornaLingua( ucPortuguesBr ,'Const_Msgs_BtForward');
    if MsgsForm_BtDelete = '' then
      MsgsForm_BtDelete := RetornaLingua( ucPortuguesBr ,'Const_Msgs_BtDelete');
    if MsgsForm_BtClose = '' then
      MsgsForm_BtDelete := RetornaLingua( ucPortuguesBr ,'Const_Msgs_BtClose'); //added by fduenas
    if MsgsForm_WindowCaption = '' then
      MsgsForm_WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Msgs_WindowCaption');
    if MsgsForm_ColFrom = '' then
      MsgsForm_ColFrom := RetornaLingua( ucPortuguesBr ,'Const_Msgs_ColFrom');
    if MsgsForm_ColSubject = '' then
      MsgsForm_ColSubject := RetornaLingua( ucPortuguesBr ,'Const_Msgs_ColSubject');
    if MsgsForm_ColDate = '' then
      MsgsForm_ColDate := RetornaLingua( ucPortuguesBr ,'Const_Msgs_ColDate');
    if MsgsForm_PromptDelete = '' then
      MsgsForm_PromptDelete := RetornaLingua( ucPortuguesBr ,'Const_Msgs_PromptDelete');
    if MsgsForm_PromptDelete_WindowCaption = '' then
      MsgsForm_PromptDelete_WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Msgs_PromptDelete_WindowCaption');
    if MsgsForm_NoMessagesSelected = '' then
      MsgsForm_NoMessagesSelected := RetornaLingua( ucPortuguesBr ,'Const_Msgs_NoMessagesSelected');
    if MsgsForm_NoMessagesSelected_WindowCaption = '' then
      MsgsForm_NoMessagesSelected_WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_Msgs_NoMessagesSelected_WindowCaption');
    if MsgRec_BtClose = '' then
      MsgRec_BtClose := RetornaLingua( ucPortuguesBr ,'Const_MsgRec_BtClose');
    if MsgRec_WindowCaption = '' then
      MsgRec_WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_MsgRec_WindowCaption');
    if MsgRec_Title = '' then
      MsgRec_Title := RetornaLingua( ucPortuguesBr ,'Const_MsgRec_Title');
    if MsgRec_LabelFrom = '' then
      MsgRec_LabelFrom := RetornaLingua( ucPortuguesBr ,'Const_MsgRec_LabelFrom');
    if MsgRec_LabelDate = '' then
      MsgRec_LabelDate := RetornaLingua( ucPortuguesBr ,'Const_MsgRec_LabelDate');
    if MsgRec_LabelSubject = '' then
      MsgRec_LabelSubject := RetornaLingua( ucPortuguesBr ,'Const_MsgRec_LabelSubject');
    if MsgRec_LabelMessage = '' then
      MsgRec_LabelMessage := RetornaLingua( ucPortuguesBr ,'Const_MsgRec_LabelMessage');
    if MsgSend_BtSend = '' then
      MsgSend_BtSend := RetornaLingua( ucPortuguesBr ,'Const_MsgSend_BtSend');
    if MsgSend_BtCancel = '' then
      MsgSend_BtCancel := RetornaLingua( ucPortuguesBr ,'Const_MsgSend_BtCancel');
    if MsgSend_WindowCaption = '' then
      MsgSend_WindowCaption := RetornaLingua( ucPortuguesBr ,'Const_MsgSend_WindowCaption');
    if MsgSend_Title = '' then
      MsgSend_Title := RetornaLingua( ucPortuguesBr ,'Const_MsgSend_Title');
    if MsgSend_GroupTo = '' then
      MsgSend_GroupTo := RetornaLingua( ucPortuguesBr ,'Const_MsgSend_GroupTo');
    if MsgSend_RadioUser = '' then
      MsgSend_RadioUser := RetornaLingua( ucPortuguesBr ,'Const_MsgSend_RadioUser');
    if MsgSend_RadioAll = '' then
      MsgSend_RadioAll := RetornaLingua( ucPortuguesBr ,'Const_MsgSend_RadioAll');
    if MsgSend_GroupMessage = '' then
      MsgSend_GroupMessage := RetornaLingua( ucPortuguesBr ,'Const_MsgSend_GroupMessage');
    if MsgSend_LabelSubject = '' then
      MsgSend_LabelSubject := RetornaLingua( ucPortuguesBr ,'Const_MsgSend_LabelSubject'); //added by fduenas
    if MsgSend_LabelMessageText = '' then
      MsgSend_LabelMessageText := RetornaLingua( ucPortuguesBr ,'Const_MsgSend_LabelMessageText'); //added by fduenas
  end;

  DestSettings.WindowsPosition := poMainFormCenter;
 { mudar aqui
    With DestSettings.TypeFieldsDB do
     Begin
       If Type_VarChar = '' then
         Type_VarChar   := 'VarChar';
       if Type_Char = '' then
         Type_Char      := 'Char';
       if Type_Int = '' then
         Type_Int       := 'Int';
     end;   }


    with DestSettings.UsersLogged do
      Begin
        If BtnMessage = '' then
          BtnMessage     := RetornaLingua( ucPortuguesBr ,'Const_UserLogged_BtnMsg' );
        if BtnRefresh = '' then
          BtnRefresh     := RetornaLingua( ucPortuguesBr ,'Const_UserLogged_Refresh');
        if Btnclose = '' then
          BtnClose       := RetornaLingua( ucPortuguesBr ,'Const_Msgs_BtClose');
        if LabelDescricao = '' then
          LabelDescricao := RetornaLingua( ucPortuguesBr ,'Const_UserLogged_LabelDescricao');
        if LabelCaption = '' then
          LabelCaption   := RetornaLingua( ucPortuguesBr ,'Const_UserLogged_LabelCaption');
        if ColName = '' then
          ColName        := RetornaLingua( ucPortuguesBr ,'Const_Cad_ColunaNome');
        if colLogin = '' then
          ColLogin       := RetornaLingua( ucPortuguesBr ,'Const_Cad_ColunaLogin');
        if ColComputer = '' then
          ColComputer    := RetornaLingua( ucPortuguesBr ,'Const_CadColuna_Computer');
        if ColData = '' then
          ColData        := RetornaLingua( ucPortuguesBr ,'Const_CadColuna_Data');
        if InputCaption = '' then
          InputCaption   := RetornaLingua( ucPortuguesBr ,'Const_UserLogged_InputCaption');
        If InputText = '' then
          InputText      := RetornaLingua( ucPortuguesBr ,'Const_UserLogged_InputText');
        If MsgSystem = '' then
          MsgSystem      := RetornaLingua( ucPortuguesBr ,'Const_UserLogged_MsgSystem');
      End;

end;

procedure IniSettings2(DestSettings: TUCSettings);
var
  tmp: TBitmap;
begin
  with DestSettings.CommonMessages do
  begin
    if BlankPassword = '' then
      BlankPassword := RetornaLingua( DestSettings.fLanguage , 'Const_Men_SenhaDesabitada');
    if PasswordChanged = '' then
      PasswordChanged := RetornaLingua( DestSettings.fLanguage , 'Const_Men_SenhaAlterada');
    if InitialMessage.Text = '' then
      InitialMessage.Text := RetornaLingua( DestSettings.fLanguage , 'Const_Men_MsgInicial');
    if MaxLoginAttemptsError = '' then
      MaxLoginAttemptsError := RetornaLingua( DestSettings.fLanguage , 'Const_Men_MaxTentativas');
    if InvalidLogin = '' then
      InvalidLogin := RetornaLingua( DestSettings.fLanguage , 'Const_Men_LoginInvalido');
    if InactiveLogin = '' then
      InactiveLogin := RetornaLingua( DestSettings.fLanguage  ,'Const_Men_LoginInativo');
    if AutoLogonError = '' then
      AutoLogonError := RetornaLingua( DestSettings.fLanguage , 'Const_Men_AutoLogonError');
    if UsuarioExiste = '' then
      UsuarioExiste := RetornaLingua( DestSettings.fLanguage , 'Const_Men_UsuarioExiste');
    if PasswordExpired  = '' then
      PasswordExpired := RetornaLingua( DestSettings.fLanguage , 'Const_Men_PasswordExpired');
    If ForcaTrocaSenha = '' then
      ForcaTrocaSenha := RetornaLingua( DestSettings.fLanguage , 'Const_ErrPass_ForcaTrocaSenha');
  end;

  with DestSettings.Login do
  begin
    if BtCancel = '' then
      BtCancel := RetornaLingua( DestSettings.fLanguage , 'Const_Log_BtCancelar');
    if BtOK = '' then
      BtOK := RetornaLingua( DestSettings.fLanguage , 'Const_Log_BtOK');
    if LabelPassword = '' then
      LabelPassword := RetornaLingua( DestSettings.fLanguage , 'Const_Log_LabelSenha');
    if LabelUser = '' then
      LabelUser := RetornaLingua( DestSettings.fLanguage , 'Const_Log_LabelUsuario');
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Log_WindowCaption');

    if LabelTentativa = '' then
      LabelTentativa := RetornaLingua( DestSettings.fLanguage , 'Const_Log_LabelTentativa');
    if LabelTentativas = '' then
      LabelTentativas := RetornaLingua( DestSettings.fLanguage , 'Const_Log_LabelTentativas');

    try
      Tmp := TBitmap.Create;
      Tmp.LoadFromResourceName(HInstance, 'UCLOCKLOGIN');
      LeftImage.Assign(tmp);
    finally
      FreeAndNil(tmp);
    end;
  end;

  with DestSettings.UsersForm do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_WindowCaption');
    if LabelDescription = '' then
      LabelDescription := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_LabelDescricao');
    if ColName = '' then
      ColName := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_ColunaNome');
    if ColLogin = '' then
      ColLogin := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_ColunaLogin');
    if ColEmail = '' then
      ColEmail := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_ColunaEmail');
    if BtAdd = '' then
      BtAdd := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_BtAdicionar');
    if BtChange = '' then
      BtChange := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_BtAlterar');
    if BtDelete = '' then
      BtDelete := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_BtExcluir');
    if BtRights = '' then
      BtRights := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_BtPermissoes');
    if BtPassword = '' then
      BtPassword := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_BtSenha');
    if BtClose = '' then
      BtClose := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_BtFechar');
    if PromptDelete = '' then
      PromptDelete := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_ConfirmaExcluir');
    if PromptDelete_WindowCaption = '' then
      PromptDelete_WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Cad_ConfirmaDelete_WindowCaption');
  end;

  with DestSettings.UsersProfile do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Prof_WindowCaption');
    if LabelDescription = '' then
      LabelDescription := RetornaLingua( DestSettings.fLanguage , 'Const_Prof_LabelDescricao');
    if ColProfile = '' then
      ColProfile := RetornaLingua( DestSettings.fLanguage , 'Const_Prof_ColunaNome');
    if BtAdd = '' then
      BtAdd := RetornaLingua( DestSettings.Language,'Const_Prof_BtAdicionar');
    if BtChange = '' then
      BtChange := RetornaLingua( DestSettings.fLanguage , 'Const_Prof_BtAlterar');
    if BtDelete = '' then
      BtDelete := RetornaLingua( DestSettings.fLanguage , 'Const_Prof_BtExcluir');
    if BtRights = '' then
      BtRights := RetornaLingua( DestSettings.fLanguage , 'Const_Prof_BtPermissoes');
    if BtClose = '' then
      BtClose := RetornaLingua( DestSettings.fLanguage , 'Const_Prof_BtFechar');
    if PromptDelete = '' then
      PromptDelete := RetornaLingua( DestSettings.fLanguage , 'Const_Prof_ConfirmaExcluir');
    if PromptDelete_WindowCaption = '' then
      PromptDelete_WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Prof_ConfirmaDelete_WindowCaption'); //added by fduenas
  end;

  with DestSettings.AddChangeUser do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_WindowCaption');
    if LabelAdd = '' then
      LabelAdd := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_LabelAdicionar');
    if LabelChange = '' then
      LabelChange := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_LabelAlterar');
    if LabelName = '' then
      LabelName := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_LabelNome');
    if LabelLogin = '' then
      LabelLogin := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_LabelLogin');
    if LabelEmail = '' then
      LabelEmail := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_LabelEmail');
    if CheckPrivileged = '' then
      CheckPrivileged := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_CheckPrivilegiado');
    if BtSave = '' then
      BtSave := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_BtGravar');
    if BtCancel = '' then
      BtCancel := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_BtCancelar');
    if LabelPerfil = '' then
      LabelPerfil := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_LabelPerfil');

    if CheckExpira = '' then
      CheckExpira := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_CheckEspira');
    If Day = '' then
      Day := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_Dia');
    If ExpiredIn = '' then
      ExpiredIn := RetornaLingua( DestSettings.fLanguage , 'Const_Inc_ExpiraEm');
    If LabelStatus = '' then
      LabelStatus := RetornaLingua( DestSettings.fLanguage ,'Const_Inc_LabelStatus');
    If StatusActive = '' then
      StatusActive := RetornaLingua( DestSettings.fLanguage ,'Const_Inc_StatusActive');

    If StatusDisabled = '' then
      StatusDisabled := RetornaLingua( DestSettings.fLanguage ,'Const_Inc_StatusDisabled');

  end;

  with DestSettings.AddChangeProfile do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_PInc_WindowCaption');
    if LabelAdd = '' then
      LabelAdd := RetornaLingua( DestSettings.fLanguage , 'Const_PInc_LabelAdicionar');
    if LabelChange = '' then
      LabelChange := RetornaLingua( DestSettings.fLanguage , 'Const_PInc_LabelAlterar');
    if LabelName = '' then
      LabelName := RetornaLingua( DestSettings.fLanguage , 'Const_PInc_LabelNome');
    if BtSave = '' then
      BtSave := RetornaLingua( DestSettings.fLanguage , 'Const_PInc_BtGravar');
    if BtCancel = '' then
      BtCancel := RetornaLingua( DestSettings.fLanguage , 'Const_PInc_BtCancelar');
  end;

  with DestSettings.Rights do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Perm_WindowCaption');
    if LabelUser = '' then
      LabelUser := RetornaLingua( DestSettings.fLanguage , 'Const_Perm_LabelUsuario');
    if LabelProfile = '' then
      LabelProfile := RetornaLingua( DestSettings.fLanguage , 'Const_Perm_LabelPerfil');
    if PageMenu = '' then
      PageMenu := RetornaLingua( DestSettings.fLanguage , 'Const_Perm_PageMenu');
    if PageActions = '' then
      PageActions := RetornaLingua( DestSettings.fLanguage , 'Const_Perm_PageActions');
    If PageControls = '' then
      PageControls := RetornaLingua( DestSettings.fLanguage , 'Const_Perm_PageControls'); // by vicente barros leonel
    if BtUnlock = '' then
      BtUnlock := RetornaLingua( DestSettings.fLanguage , 'Const_Perm_BtLibera');
    if BtLock = '' then
      BtLock := RetornaLingua( DestSettings.fLanguage , 'Const_Perm_BtBloqueia');
    if BtSave = '' then
      BtSave := RetornaLingua( DestSettings.fLanguage , 'Const_Perm_BtGravar');
    if BtCancel = '' then
      BtCancel := RetornaLingua( DestSettings.fLanguage , 'Const_Perm_BtCancelar');
  end;

  with DestSettings.ChangePassword do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Troc_WindowCaption');
    if LabelDescription = '' then
      LabelDescription := RetornaLingua( DestSettings.fLanguage , 'Const_Troc_LabelDescricao');
    if LabelCurrentPassword = '' then
      LabelCurrentPassword := RetornaLingua( DestSettings.fLanguage , 'Const_Troc_LabelSenhaAtual');
    if LabelNewPassword = '' then
      LabelNewPassword := RetornaLingua( DestSettings.fLanguage , 'Const_Troc_LabelNovaSenha');
    if LabelConfirm = '' then
      LabelConfirm := RetornaLingua( DestSettings.fLanguage , 'Const_Troc_LabelConfirma');
    if BtSave = '' then
      BtSave := RetornaLingua( DestSettings.fLanguage , 'Const_Troc_BtGravar');
    if BtCancel = '' then
      BtCancel := RetornaLingua( DestSettings.fLanguage , 'Const_Troc_BtCancelar');
  end;

  with DestSettings.CommonMessages.ChangePasswordError do
  begin
    if InvalidCurrentPassword = '' then
      InvalidCurrentPassword := RetornaLingua( DestSettings.fLanguage , 'Const_ErrPass_SenhaAtualInvalida');
    if NewPasswordError = '' then
      NewPasswordError := RetornaLingua( DestSettings.fLanguage , 'Const_ErrPass_ErroNovaSenha');
    if NewEqualCurrent = '' then
      NewEqualCurrent := RetornaLingua( DestSettings.fLanguage , 'Const_ErrPass_NovaIgualAtual');
    if PasswordRequired = '' then
      PasswordRequired := RetornaLingua( DestSettings.fLanguage , 'Const_ErrPass_SenhaObrigatoria');
    if MinPasswordLength = '' then
      MinPasswordLength := RetornaLingua( DestSettings.fLanguage , 'Const_ErrPass_SenhaMinima');
    if InvalidNewPassword = '' then
      InvalidNewPassword := RetornaLingua( DestSettings.fLanguage , 'Const_ErrPass_SenhaInvalida');
  end;

  with DestSettings.ResetPassword do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_DefPass_WindowCaption');
    if LabelPassword = '' then
      LabelPassword := RetornaLingua( DestSettings.fLanguage , 'Const_DefPass_LabelSenha');
  end;

  with DestSettings.Log do
  begin
    if WindowCaption = '' then
      WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_WindowCaption');
    if LabelDescription = '' then
      LabelDescription := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_LabelDescricao');
    if LabelUser = '' then
      LabelUser := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_LabelUsuario');
    if LabelDate = '' then
      LabelDate := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_LabelData');
    if LabelLevel = '' then
      LabelLevel := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_LabelNivel');
    if ColLevel = '' then
      ColLevel := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_ColunaNivel');
    if ColAppID = '' then
      ColAppID := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_ColunaAppID');
    if ColMessage = '' then
      ColMessage := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_ColunaMensagem');
    if ColUser = '' then
      ColUser := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_ColunaUsuario');
    if ColDate = '' then
      ColDate := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_ColunaData');
    if BtFilter = '' then
      BtFilter := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_BtFiltro');
    if BtDelete = '' then
      BtDelete := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_BtExcluir');
    if BtClose = '' then
      BtClose := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_BtFechar');
    if PromptDelete = '' then
      PromptDelete := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_ConfirmaExcluir');
    if PromptDelete_WindowCaption = '' then
      PromptDelete_WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_ConfirmaDelete_WindowCaption'); //added by fduenas
    if OptionUserAll = '' then
      OptionUserAll := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_Todos'); //added by fduenas
    if OptionLevelLow = '' then
      OptionLevelLow := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_Low'); //added by fduenas
    if OptionLevelNormal = '' then
      OptionLevelNormal := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_Normal'); //added by fduenas
    if OptionLevelHigh = '' then
      OptionLevelHigh := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_High'); //added by fduenas
    if OptionLevelCritic = '' then
      OptionLevelCritic := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_Critic'); //added by fduenas
    if DeletePerformed = '' then
      DeletePerformed := RetornaLingua( DestSettings.fLanguage , 'Const_LogC_ExcluirEfectuada'); //added by fduenas
  end;

  with DestSettings.AppMessages do
  begin
    if MsgsForm_BtNew = '' then
      MsgsForm_BtNew := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_BtNew');
    if MsgsForm_BtReplay = '' then
      MsgsForm_BtReplay := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_BtReplay');
    if MsgsForm_BtForward = '' then
      MsgsForm_BtForward := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_BtForward');
    if MsgsForm_BtDelete = '' then
      MsgsForm_BtDelete := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_BtDelete');
    if MsgsForm_BtClose = '' then
      MsgsForm_BtClose := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_BtClose'); //added by fduenas
    if MsgsForm_WindowCaption = '' then
      MsgsForm_WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_WindowCaption');
    if MsgsForm_ColFrom = '' then
      MsgsForm_ColFrom := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_ColFrom');
    if MsgsForm_ColSubject = '' then
      MsgsForm_ColSubject := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_ColSubject');
    if MsgsForm_ColDate = '' then
      MsgsForm_ColDate := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_ColDate');
    if MsgsForm_PromptDelete = '' then
      MsgsForm_PromptDelete := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_PromptDelete');
    if MsgsForm_PromptDelete_WindowCaption = '' then
      MsgsForm_PromptDelete_WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_PromptDelete_WindowCaption'); //added by fduenas
    if MsgsForm_NoMessagesSelected = '' then
      MsgsForm_NoMessagesSelected := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_NoMessagesSelected'); //added by fduenas
    if MsgsForm_NoMessagesSelected_WindowCaption = '' then
      MsgsForm_NoMessagesSelected_WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_Msgs_NoMessagesSelected_WindowCaption'); //added by fduenas
    if MsgRec_BtClose = '' then
      MsgRec_BtClose := RetornaLingua( DestSettings.fLanguage , 'Const_MsgRec_BtClose');
    if MsgRec_WindowCaption = '' then
      MsgRec_WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_MsgRec_WindowCaption');
    if MsgRec_Title = '' then
      MsgRec_Title := RetornaLingua( DestSettings.fLanguage , 'Const_MsgRec_Title');
    if MsgRec_LabelFrom = '' then
      MsgRec_LabelFrom := RetornaLingua( DestSettings.fLanguage , 'Const_MsgRec_LabelFrom');
    if MsgRec_LabelDate = '' then
      MsgRec_LabelDate := RetornaLingua( DestSettings.fLanguage , 'Const_MsgRec_LabelDate');
    if MsgRec_LabelSubject = '' then
      MsgRec_LabelSubject := RetornaLingua( DestSettings.fLanguage , 'Const_MsgRec_LabelSubject');
    if MsgRec_LabelMessage = '' then
      MsgRec_LabelMessage := RetornaLingua( DestSettings.fLanguage , 'Const_MsgRec_LabelMessage');
    if MsgSend_BtSend = '' then
      MsgSend_BtSend := RetornaLingua( DestSettings.fLanguage , 'Const_MsgSend_BtSend');
    if MsgSend_BtCancel = '' then
      MsgSend_BtCancel := RetornaLingua( DestSettings.fLanguage , 'Const_MsgSend_BtCancel');
    if MsgSend_WindowCaption = '' then
      MsgSend_WindowCaption := RetornaLingua( DestSettings.fLanguage , 'Const_MsgSend_WindowCaption');
    if MsgSend_Title = '' then
      MsgSend_Title := RetornaLingua( DestSettings.fLanguage , 'Const_MsgSend_Title');
    if MsgSend_GroupTo = '' then
      MsgSend_GroupTo := RetornaLingua( DestSettings.fLanguage , 'Const_MsgSend_GroupTo');
    if MsgSend_RadioUser = '' then
      MsgSend_RadioUser := RetornaLingua( DestSettings.fLanguage , 'Const_MsgSend_RadioUser');
    if MsgSend_RadioAll = '' then
      MsgSend_RadioAll := RetornaLingua( DestSettings.fLanguage , 'Const_MsgSend_RadioAll');
    if MsgSend_GroupMessage = '' then
      MsgSend_GroupMessage := RetornaLingua( DestSettings.fLanguage , 'Const_MsgSend_GroupMessage');
    if MsgSend_LabelSubject = '' then
      MsgSend_LabelSubject := RetornaLingua( DestSettings.fLanguage , 'Const_MsgSend_LabelSubject'); //added by fduenas
    if MsgSend_LabelMessageText = '' then
      MsgSend_LabelMessageText := RetornaLingua( DestSettings.fLanguage , 'Const_MsgSend_LabelMessageText'); //added by fduenas
  end;

  DestSettings.WindowsPosition := poMainFormCenter;

 {  With DestSettings.TypeFieldsDB do
     Begin
       If Type_VarChar = '' then
         Type_VarChar   := 'VarChar';
       if Type_Char = '' then
         Type_Char      := 'Char';
       if Type_Int = '' then
         Type_Int       := 'Int';
     end;   mudar aqui }

  with DestSettings.UsersLogged do
      Begin
        If BtnMessage = '' then
          BtnMessage     := RetornaLingua( DestSettings.fLanguage ,'Const_UserLogged_BtnMsg' );
        if BtnRefresh = '' then
          BtnRefresh     := RetornaLingua( DestSettings.fLanguage ,'Const_UserLogged_Refresh');
        if Btnclose = '' then
          BtnClose       := RetornaLingua( DestSettings.fLanguage ,'Const_Msgs_BtClose');
        if LabelDescricao = '' then
          LabelDescricao := RetornaLingua( DestSettings.fLanguage ,'Const_UserLogged_LabelDescricao');
        if LabelCaption = '' then
          LabelCaption   := RetornaLingua( DestSettings.fLanguage ,'Const_UserLogged_LabelCaption');
        if ColName = '' then
          ColName        := RetornaLingua( DestSettings.fLanguage ,'Const_Cad_ColunaNome');
        if colLogin = '' then
          ColLogin       := RetornaLingua( DestSettings.fLanguage ,'Const_Cad_ColunaLogin');
        if ColComputer = '' then
          ColComputer    := RetornaLingua( DestSettings.fLanguage ,'Const_CadColuna_Computer');
        if ColData = '' then
          ColData        := RetornaLingua( DestSettings.fLanguage ,'Const_CadColuna_Data');
        if InputCaption = '' then
          InputCaption   := RetornaLingua( DestSettings.fLanguage ,'Const_UserLogged_InputCaption');
        If InputText = '' then
          InputText      := RetornaLingua( DestSettings.fLanguage ,'Const_UserLogged_InputText');
        If MsgSystem = '' then
          MsgSystem      := RetornaLingua( DestSettings.fLanguage ,'Const_UserLogged_MsgSystem');
      End;     
end;

{-------------------------------------------------------------------------------}

procedure AlterLanguage(DestSettings: TUCUserSettings);
begin
  with DestSettings.CommonMessages do
  begin
    BlankPassword := RetornaLingua( DestSettings.Language,'Const_Men_SenhaDesabitada');
    PasswordChanged := RetornaLingua( DestSettings.Language,'Const_Men_SenhaAlterada');
    InitialMessage.Text := RetornaLingua( DestSettings.Language,'Const_Men_MsgInicial');
    MaxLoginAttemptsError := RetornaLingua( DestSettings.Language,'Const_Men_MaxTentativas');
    InvalidLogin := RetornaLingua( DestSettings.Language,'Const_Men_LoginInvalido');
    InactiveLogin := RetornaLingua( DestSettings.Language  ,'Const_Men_LoginInativo');
    AutoLogonError := RetornaLingua( DestSettings.Language,'Const_Men_AutoLogonError');
    UsuarioExiste := RetornaLingua( DestSettings.Language,'Const_Men_UsuarioExiste');
    PasswordExpired := RetornaLingua( DestSettings.Language,'Const_Men_PasswordExpired');
  end;

  with DestSettings.Login do
  begin
    BtCancel :=  RetornaLingua( DestSettings.Language,'Const_Log_BtCancelar');
    BtOK :=  RetornaLingua( DestSettings.Language,'Const_Log_BtOK');
    LabelPassword :=  RetornaLingua( DestSettings.Language,'Const_Log_LabelSenha');
    LabelUser := RetornaLingua( DestSettings.Language,'Const_Log_LabelUsuario');
    WindowCaption :=  RetornaLingua( DestSettings.Language,'Const_Log_WindowCaption');
    LabelTentativa :=  RetornaLingua( DestSettings.Language,'Const_Log_LabelTentativa');
    LabelTentativas :=  RetornaLingua( DestSettings.Language,'Const_Log_LabelTentativas');
  end;

  with DestSettings.UsersForm do
  begin
    WindowCaption :=  RetornaLingua( DestSettings.Language,'Const_Cad_WindowCaption');
    LabelDescription :=  RetornaLingua( DestSettings.Language,'Const_Cad_LabelDescricao');
    ColName :=  RetornaLingua( DestSettings.Language,'Const_Cad_ColunaNome');
    ColLogin :=  RetornaLingua( DestSettings.Language,'Const_Cad_ColunaLogin');
    ColEmail :=  RetornaLingua( DestSettings.Language,'Const_Cad_ColunaEmail');
    BtAdd :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtAdicionar');
    BtChange :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtAlterar');
    BtDelete :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtExcluir');
    BtRights :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtPermissoes');
    BtPassword :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtSenha');
    BtClose :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtFechar');
    PromptDelete := RetornaLingua( DestSettings.Language,'Const_Cad_ConfirmaExcluir');
    PromptDelete_WindowCaption := RetornaLingua( DestSettings.Language,'Const_Cad_ConfirmaDelete_WindowCaption');
  end;

  with DestSettings.UsersProfile do
  begin
    WindowCaption    := RetornaLingua( DestSettings.Language,'Const_Prof_WindowCaption');
    LabelDescription := RetornaLingua( DestSettings.Language,'Const_Prof_LabelDescricao');
    ColProfile       := RetornaLingua( DestSettings.Language,'Const_Prof_ColunaNome');
    BtAdd            := RetornaLingua( DestSettings.Language,'Const_Prof_BtAdicionar');
    BtChange         := RetornaLingua( DestSettings.Language,'Const_Prof_BtAlterar');
    BtDelete         := RetornaLingua( DestSettings.Language,'Const_Prof_BtExcluir');
    BtRights         := RetornaLingua( DestSettings.Language,'Const_Prof_BtPermissoes');
    BtClose          := RetornaLingua( DestSettings.Language,'Const_Prof_BtFechar');
    PromptDelete     := RetornaLingua( DestSettings.Language,'Const_Prof_ConfirmaExcluir');
    PromptDelete_WindowCaption := RetornaLingua( DestSettings.Language,'Const_Prof_ConfirmaDelete_WindowCaption');
  end;

  with DestSettings.AddChangeUser do
  begin
    WindowCaption := RetornaLingua( DestSettings.Language,'Const_Inc_WindowCaption');
    LabelAdd := RetornaLingua( DestSettings.Language,'Const_Inc_LabelAdicionar');
    LabelChange := RetornaLingua( DestSettings.Language,'Const_Inc_LabelAlterar');
    LabelName := RetornaLingua( DestSettings.Language,'Const_Inc_LabelNome');
    LabelLogin := RetornaLingua( DestSettings.Language,'Const_Inc_LabelLogin');
    LabelEmail := RetornaLingua( DestSettings.Language,'Const_Inc_LabelEmail');
    CheckPrivileged := RetornaLingua( DestSettings.Language,'Const_Inc_CheckPrivilegiado');
    BtSave := RetornaLingua( DestSettings.Language,'Const_Inc_BtGravar');
    BtCancel := RetornaLingua( DestSettings.Language,'Const_Inc_BtCancelar');
    LabelPerfil := RetornaLingua( DestSettings.Language,'Const_Inc_LabelPerfil');
    CheckExpira := RetornaLingua( DestSettings.Language,'Const_Inc_CheckEspira');
    Day := RetornaLingua( DestSettings.Language,'Const_Inc_Dia');
    ExpiredIn := RetornaLingua( DestSettings.Language,'Const_Inc_ExpiraEm');
    LabelStatus := RetornaLingua( DestSettings.Language ,'Const_Inc_LabelStatus');
    StatusActive := RetornaLingua( DestSettings.Language ,'Const_Inc_StatusActive');
    StatusDisabled := RetornaLingua( DestSettings.Language ,'Const_Inc_StatusDisabled');
  end;

  with DestSettings.AddChangeProfile do
  begin
      WindowCaption := RetornaLingua( DestSettings.Language,'Const_PInc_WindowCaption');
      LabelAdd := RetornaLingua( DestSettings.Language,'Const_PInc_LabelAdicionar');
      LabelChange := RetornaLingua( DestSettings.Language,'Const_PInc_LabelAlterar');
      LabelName := RetornaLingua( DestSettings.Language,'Const_PInc_LabelNome');
      BtSave := RetornaLingua( DestSettings.Language,'Const_PInc_BtGravar');
      BtCancel := RetornaLingua( DestSettings.Language,'Const_PInc_BtCancelar');
  end;

  with DestSettings.Rights do
  begin
      WindowCaption := RetornaLingua( DestSettings.Language,'Const_Perm_WindowCaption');
      LabelUser := RetornaLingua( DestSettings.Language,'Const_Perm_LabelUsuario');
      LabelProfile := RetornaLingua( DestSettings.Language,'Const_Perm_LabelPerfil');
      PageMenu := RetornaLingua( DestSettings.Language,'Const_Perm_PageMenu');
      PageActions := RetornaLingua( DestSettings.Language,'Const_Perm_PageActions');
      PageControls := RetornaLingua( DestSettings.Language,'Const_Perm_PageControls');
      BtUnlock := RetornaLingua( DestSettings.Language,'Const_Perm_BtLibera');
      BtLock := RetornaLingua( DestSettings.Language,'Const_Perm_BtBloqueia');
      BtSave := RetornaLingua( DestSettings.Language,'Const_Perm_BtGravar');
      BtCancel := RetornaLingua( DestSettings.Language,'Const_Perm_BtCancelar');
  end;

  with DestSettings.ChangePassword do
  begin
      WindowCaption := RetornaLingua( DestSettings.Language,'Const_Troc_WindowCaption');
      LabelDescription := RetornaLingua( DestSettings.Language,'Const_Troc_LabelDescricao');
      LabelCurrentPassword := RetornaLingua( DestSettings.Language,'Const_Troc_LabelSenhaAtual');
      LabelNewPassword := RetornaLingua( DestSettings.Language,'Const_Troc_LabelNovaSenha');
      LabelConfirm := RetornaLingua( DestSettings.Language,'Const_Troc_LabelConfirma');
      BtSave := RetornaLingua( DestSettings.Language,'Const_Troc_BtGravar');
      BtCancel := RetornaLingua( DestSettings.Language,'Const_Troc_BtCancelar');
  end;

  with DestSettings.CommonMessages.ChangePasswordError do
  begin
      InvalidCurrentPassword := RetornaLingua( DestSettings.Language,'Const_ErrPass_SenhaAtualInvalida');
      NewPasswordError := RetornaLingua( DestSettings.Language,'Const_ErrPass_ErroNovaSenha');
      NewEqualCurrent := RetornaLingua( DestSettings.Language,'Const_ErrPass_NovaIgualAtual');
      PasswordRequired := RetornaLingua( DestSettings.Language,'Const_ErrPass_SenhaObrigatoria');
      MinPasswordLength := RetornaLingua( DestSettings.Language,'Const_ErrPass_SenhaMinima');
      InvalidNewPassword := RetornaLingua( DestSettings.Language,'Const_ErrPass_SenhaInvalida');
  end;

  with DestSettings.ResetPassword do
  begin
      WindowCaption := RetornaLingua( DestSettings.Language,'Const_DefPass_WindowCaption');
      LabelPassword := RetornaLingua( DestSettings.Language,'Const_DefPass_LabelSenha');
  end;

  with DestSettings.Log do
  begin
      WindowCaption := RetornaLingua( DestSettings.Language,'Const_LogC_WindowCaption');
      LabelDescription := RetornaLingua( DestSettings.Language,'Const_LogC_LabelDescricao');
      LabelUser := RetornaLingua( DestSettings.Language,'Const_LogC_LabelUsuario');
      LabelDate := RetornaLingua( DestSettings.Language,'Const_LogC_LabelData');
      LabelLevel := RetornaLingua( DestSettings.Language,'Const_LogC_LabelNivel');
      ColLevel := RetornaLingua( DestSettings.Language,'Const_LogC_ColunaNivel');
      ColAppID := RetornaLingua( DestSettings.Language,'Const_LogC_ColunaAppID');
      ColMessage := RetornaLingua( DestSettings.Language,'Const_LogC_ColunaMensagem');
      ColUser := RetornaLingua( DestSettings.Language,'Const_LogC_ColunaUsuario');
      ColDate := RetornaLingua( DestSettings.Language,'Const_LogC_ColunaData');
      BtFilter := RetornaLingua( DestSettings.Language,'Const_LogC_BtFiltro');
      BtDelete := RetornaLingua( DestSettings.Language,'Const_LogC_BtExcluir');
      BtClose := RetornaLingua( DestSettings.Language,'Const_LogC_BtFechar');
      PromptDelete := RetornaLingua( DestSettings.Language,'Const_LogC_ConfirmaExcluir');
      PromptDelete_WindowCaption := RetornaLingua( DestSettings.Language,'Const_LogC_ConfirmaDelete_WindowCaption');
      OptionUserAll := RetornaLingua( DestSettings.Language,'Const_LogC_Todos');
      OptionLevelLow := RetornaLingua( DestSettings.Language,'Const_LogC_Low');
      OptionLevelNormal := RetornaLingua( DestSettings.Language,'Const_LogC_Normal');
      OptionLevelHigh := RetornaLingua( DestSettings.Language,'Const_LogC_High');
      OptionLevelCritic := RetornaLingua( DestSettings.Language,'Const_LogC_Critic');
      DeletePerformed := RetornaLingua( DestSettings.Language,'Const_LogC_ExcluirEfectuada');
  end;

  with DestSettings.AppMessages do
  begin
      MsgsForm_BtNew := RetornaLingua( DestSettings.Language,'Const_Msgs_BtNew');
      MsgsForm_BtReplay := RetornaLingua( DestSettings.Language,'Const_Msgs_BtReplay');
      MsgsForm_BtForward := RetornaLingua( DestSettings.Language,'Const_Msgs_BtForward');
      MsgsForm_BtDelete := RetornaLingua( DestSettings.Language,'Const_Msgs_BtDelete');
      MsgsForm_BtClose := RetornaLingua( DestSettings.Language,'Const_Msgs_BtClose'); //added by fduenas
      MsgsForm_WindowCaption := RetornaLingua( DestSettings.Language,'Const_Msgs_WindowCaption');
      MsgsForm_ColFrom := RetornaLingua( DestSettings.Language,'Const_Msgs_ColFrom');
      MsgsForm_ColSubject := RetornaLingua( DestSettings.Language,'Const_Msgs_ColSubject');
      MsgsForm_ColDate := RetornaLingua( DestSettings.Language,'Const_Msgs_ColDate');
      MsgsForm_PromptDelete := RetornaLingua( DestSettings.Language,'Const_Msgs_PromptDelete');
      MsgsForm_PromptDelete_WindowCaption := RetornaLingua( DestSettings.Language,'Const_Msgs_PromptDelete_WindowCaption'); //added by fduenas
      MsgsForm_NoMessagesSelected := RetornaLingua( DestSettings.Language,'Const_Msgs_NoMessagesSelected'); //added by fduenas
      MsgsForm_NoMessagesSelected_WindowCaption := RetornaLingua( DestSettings.Language,'Const_Msgs_NoMessagesSelected_WindowCaption'); //added by fduenas
      MsgRec_BtClose := RetornaLingua( DestSettings.Language,'Const_MsgRec_BtClose');
      MsgRec_WindowCaption := RetornaLingua( DestSettings.Language,'Const_MsgRec_WindowCaption');
      MsgRec_Title := RetornaLingua( DestSettings.Language,'Const_MsgRec_Title');
      MsgRec_LabelFrom := RetornaLingua( DestSettings.Language,'Const_MsgRec_LabelFrom');
      MsgRec_LabelDate := RetornaLingua( DestSettings.Language,'Const_MsgRec_LabelDate');
      MsgRec_LabelSubject := RetornaLingua( DestSettings.Language,'Const_MsgRec_LabelSubject');
      MsgRec_LabelMessage := RetornaLingua( DestSettings.Language,'Const_MsgRec_LabelMessage');
      MsgSend_BtSend := RetornaLingua( DestSettings.Language,'Const_MsgSend_BtSend');
      MsgSend_BtCancel := RetornaLingua( DestSettings.Language,'Const_MsgSend_BtCancel');
      MsgSend_WindowCaption := RetornaLingua( DestSettings.Language,'Const_MsgSend_WindowCaption');
      MsgSend_Title := RetornaLingua( DestSettings.Language,'Const_MsgSend_Title');
      MsgSend_GroupTo := RetornaLingua( DestSettings.Language,'Const_MsgSend_GroupTo');
      MsgSend_RadioUser := RetornaLingua( DestSettings.Language,'Const_MsgSend_RadioUser');
      MsgSend_RadioAll := RetornaLingua( DestSettings.Language,'Const_MsgSend_RadioAll');
      MsgSend_GroupMessage := RetornaLingua( DestSettings.Language,'Const_MsgSend_GroupMessage');
      MsgSend_LabelSubject := RetornaLingua( DestSettings.Language,'Const_MsgSend_LabelSubject'); //added by fduenas
      MsgSend_LabelMessageText := RetornaLingua( DestSettings.Language,'Const_MsgSend_LabelMessageText'); //added by fduenas
  end;

  DestSettings.WindowsPosition := poMainFormCenter;

     with DestSettings.UsersLogged do
      Begin
        BtnMessage     := RetornaLingua( DestSettings.Language ,'Const_UserLogged_BtnMsg' );
        BtnRefresh     := RetornaLingua( DestSettings.Language ,'Const_UserLogged_Refresh');
        BtnClose       := RetornaLingua( DestSettings.Language ,'Const_Msgs_BtClose');
        LabelDescricao := RetornaLingua( DestSettings.Language ,'Const_UserLogged_LabelDescricao');
        LabelCaption   := RetornaLingua( DestSettings.Language ,'Const_UserLogged_LabelCaption');
        ColName        := RetornaLingua( DestSettings.Language ,'Const_Cad_ColunaNome');
        ColLogin       := RetornaLingua( DestSettings.Language ,'Const_Cad_ColunaLogin');
        ColComputer    := RetornaLingua( DestSettings.Language ,'Const_CadColuna_Computer');
        ColData        := RetornaLingua( DestSettings.Language ,'Const_CadColuna_Data');
        InputCaption   := RetornaLingua( DestSettings.Language ,'Const_UserLogged_InputCaption');
        InputText      := RetornaLingua( DestSettings.Language ,'Const_UserLogged_InputText');
        MsgSystem      := RetornaLingua( DestSettings.Language ,'Const_UserLogged_MsgSystem');
      End;
end;

procedure AlterLanguage2(DestSettings: TUCSettings);
begin
  with DestSettings.CommonMessages do
  begin
    BlankPassword := RetornaLingua( DestSettings.Language,'Const_Men_SenhaDesabitada');
    PasswordChanged := RetornaLingua( DestSettings.Language,'Const_Men_SenhaAlterada');
    InitialMessage.Text := RetornaLingua( DestSettings.Language,'Const_Men_MsgInicial');
    MaxLoginAttemptsError := RetornaLingua( DestSettings.Language,'Const_Men_MaxTentativas');
    InvalidLogin := RetornaLingua( DestSettings.Language,'Const_Men_LoginInvalido');
    InactiveLogin := RetornaLingua( DestSettings.fLanguage  ,'Const_Men_LoginInativo');    
    AutoLogonError := RetornaLingua( DestSettings.Language,'Const_Men_AutoLogonError');
    UsuarioExiste := RetornaLingua( DestSettings.Language,'Const_Men_UsuarioExiste');
    PasswordExpired := RetornaLingua( DestSettings.Language,'Const_Men_PasswordExpired');
  end;

  with DestSettings.Login do
  begin
    BtCancel :=  RetornaLingua( DestSettings.Language,'Const_Log_BtCancelar');
    BtOK :=  RetornaLingua( DestSettings.Language,'Const_Log_BtOK');
    LabelPassword :=  RetornaLingua( DestSettings.Language,'Const_Log_LabelSenha');
    LabelUser := RetornaLingua( DestSettings.Language,'Const_Log_LabelUsuario');
    WindowCaption :=  RetornaLingua( DestSettings.Language,'Const_Log_WindowCaption');
    LabelTentativa :=  RetornaLingua( DestSettings.Language,'Const_Log_LabelTentativa');
    LabelTentativas :=  RetornaLingua( DestSettings.Language,'Const_Log_LabelTentativas');
  end;

  with DestSettings.UsersForm do
  begin
    WindowCaption :=  RetornaLingua( DestSettings.Language,'Const_Cad_WindowCaption');
    LabelDescription :=  RetornaLingua( DestSettings.Language,'Const_Cad_LabelDescricao');
    ColName :=  RetornaLingua( DestSettings.Language,'Const_Cad_ColunaNome');
    ColLogin :=  RetornaLingua( DestSettings.Language,'Const_Cad_ColunaLogin');
    ColEmail :=  RetornaLingua( DestSettings.Language,'Const_Cad_ColunaEmail');
    BtAdd :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtAdicionar');
    BtChange :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtAlterar');
    BtDelete :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtExcluir');
    BtRights :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtPermissoes');
    BtPassword :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtSenha');
    BtClose :=  RetornaLingua( DestSettings.Language,'Const_Cad_BtFechar');
    PromptDelete := RetornaLingua( DestSettings.Language,'Const_Cad_ConfirmaExcluir');
    PromptDelete_WindowCaption := RetornaLingua( DestSettings.Language,'Const_Cad_ConfirmaDelete_WindowCaption');
  end;

  with DestSettings.UsersProfile do
  begin
    WindowCaption    := RetornaLingua( DestSettings.Language,'Const_Prof_WindowCaption');
    LabelDescription := RetornaLingua( DestSettings.Language,'Const_Prof_LabelDescricao');
    ColProfile       := RetornaLingua( DestSettings.Language,'Const_Prof_ColunaNome');
    BtAdd            := RetornaLingua( DestSettings.Language,'Const_Prof_BtAdicionar');
    BtChange         := RetornaLingua( DestSettings.Language,'Const_Prof_BtAlterar');
    BtDelete         := RetornaLingua( DestSettings.Language,'Const_Prof_BtExcluir');
    BtRights         := RetornaLingua( DestSettings.Language,'Const_Prof_BtPermissoes');
    BtClose          := RetornaLingua( DestSettings.Language,'Const_Prof_BtFechar');
    PromptDelete     := RetornaLingua( DestSettings.Language,'Const_Prof_ConfirmaExcluir');
    PromptDelete_WindowCaption := RetornaLingua( DestSettings.Language,'Const_Prof_ConfirmaDelete_WindowCaption');
  end;

  with DestSettings.AddChangeUser do
  begin
    WindowCaption := RetornaLingua( DestSettings.Language,'Const_Inc_WindowCaption');
    LabelAdd := RetornaLingua( DestSettings.Language,'Const_Inc_LabelAdicionar');
    LabelChange := RetornaLingua( DestSettings.Language,'Const_Inc_LabelAlterar');
    LabelName := RetornaLingua( DestSettings.Language,'Const_Inc_LabelNome');
    LabelLogin := RetornaLingua( DestSettings.Language,'Const_Inc_LabelLogin');
    LabelEmail := RetornaLingua( DestSettings.Language,'Const_Inc_LabelEmail');
    CheckPrivileged := RetornaLingua( DestSettings.Language,'Const_Inc_CheckPrivilegiado');
    BtSave := RetornaLingua( DestSettings.Language,'Const_Inc_BtGravar');
    BtCancel := RetornaLingua( DestSettings.Language,'Const_Inc_BtCancelar');
    LabelPerfil := RetornaLingua( DestSettings.Language,'Const_Inc_LabelPerfil');
    CheckExpira := RetornaLingua( DestSettings.Language,'Const_Inc_CheckEspira');
    Day := RetornaLingua( DestSettings.Language,'Const_Inc_Dia');
    ExpiredIn := RetornaLingua( DestSettings.Language,'Const_Inc_ExpiraEm');
    LabelStatus := RetornaLingua( DestSettings.Language ,'Const_Inc_LabelStatus');
    StatusActive := RetornaLingua( DestSettings.Language ,'Const_Inc_StatusActive');
    StatusDisabled := RetornaLingua( DestSettings.Language ,'Const_Inc_StatusDisabled');        
  end;

  with DestSettings.AddChangeProfile do
  begin
      WindowCaption := RetornaLingua( DestSettings.Language,'Const_PInc_WindowCaption');
      LabelAdd := RetornaLingua( DestSettings.Language,'Const_PInc_LabelAdicionar');
      LabelChange := RetornaLingua( DestSettings.Language,'Const_PInc_LabelAlterar');
      LabelName := RetornaLingua( DestSettings.Language,'Const_PInc_LabelNome');
      BtSave := RetornaLingua( DestSettings.Language,'Const_PInc_BtGravar');
      BtCancel := RetornaLingua( DestSettings.Language,'Const_PInc_BtCancelar');
  end;

  with DestSettings.Rights do
  begin
      WindowCaption := RetornaLingua( DestSettings.Language,'Const_Perm_WindowCaption');
      LabelUser := RetornaLingua( DestSettings.Language,'Const_Perm_LabelUsuario');
      LabelProfile := RetornaLingua( DestSettings.Language,'Const_Perm_LabelPerfil');
      PageMenu := RetornaLingua( DestSettings.Language,'Const_Perm_PageMenu');
      PageActions := RetornaLingua( DestSettings.Language,'Const_Perm_PageActions');
      PageControls := RetornaLingua( DestSettings.Language,'Const_Perm_PageControls');
      BtUnlock := RetornaLingua( DestSettings.Language,'Const_Perm_BtLibera');
      BtLock := RetornaLingua( DestSettings.Language,'Const_Perm_BtBloqueia');
      BtSave := RetornaLingua( DestSettings.Language,'Const_Perm_BtGravar');
      BtCancel := RetornaLingua( DestSettings.Language,'Const_Perm_BtCancelar');
  end;

  with DestSettings.ChangePassword do
  begin
      WindowCaption := RetornaLingua( DestSettings.Language,'Const_Troc_WindowCaption');
      LabelDescription := RetornaLingua( DestSettings.Language,'Const_Troc_LabelDescricao');
      LabelCurrentPassword := RetornaLingua( DestSettings.Language,'Const_Troc_LabelSenhaAtual');
      LabelNewPassword := RetornaLingua( DestSettings.Language,'Const_Troc_LabelNovaSenha');
      LabelConfirm := RetornaLingua( DestSettings.Language,'Const_Troc_LabelConfirma');
      BtSave := RetornaLingua( DestSettings.Language,'Const_Troc_BtGravar');
      BtCancel := RetornaLingua( DestSettings.Language,'Const_Troc_BtCancelar');
  end;

  with DestSettings.CommonMessages.ChangePasswordError do
  begin
      InvalidCurrentPassword := RetornaLingua( DestSettings.Language,'Const_ErrPass_SenhaAtualInvalida');
      NewPasswordError := RetornaLingua( DestSettings.Language,'Const_ErrPass_ErroNovaSenha');
      NewEqualCurrent := RetornaLingua( DestSettings.Language,'Const_ErrPass_NovaIgualAtual');
      PasswordRequired := RetornaLingua( DestSettings.Language,'Const_ErrPass_SenhaObrigatoria');
      MinPasswordLength := RetornaLingua( DestSettings.Language,'Const_ErrPass_SenhaMinima');
      InvalidNewPassword := RetornaLingua( DestSettings.Language,'Const_ErrPass_SenhaInvalida');
  end;

  with DestSettings.ResetPassword do
  begin
      WindowCaption := RetornaLingua( DestSettings.Language,'Const_DefPass_WindowCaption');
      LabelPassword := RetornaLingua( DestSettings.Language,'Const_DefPass_LabelSenha');
  end;

  with DestSettings.Log do
  begin
      WindowCaption := RetornaLingua( DestSettings.Language,'Const_LogC_WindowCaption');
      LabelDescription := RetornaLingua( DestSettings.Language,'Const_LogC_LabelDescricao');
      LabelUser := RetornaLingua( DestSettings.Language,'Const_LogC_LabelUsuario');
      LabelDate := RetornaLingua( DestSettings.Language,'Const_LogC_LabelData');
      LabelLevel := RetornaLingua( DestSettings.Language,'Const_LogC_LabelNivel');
      ColLevel := RetornaLingua( DestSettings.Language,'Const_LogC_ColunaNivel');
      ColAppID := RetornaLingua( DestSettings.Language,'Const_LogC_ColunaAppID');
      ColMessage := RetornaLingua( DestSettings.Language,'Const_LogC_ColunaMensagem');
      ColUser := RetornaLingua( DestSettings.Language,'Const_LogC_ColunaUsuario');
      ColDate := RetornaLingua( DestSettings.Language,'Const_LogC_ColunaData');
      BtFilter := RetornaLingua( DestSettings.Language,'Const_LogC_BtFiltro');
      BtDelete := RetornaLingua( DestSettings.Language,'Const_LogC_BtExcluir');
      BtClose := RetornaLingua( DestSettings.Language,'Const_LogC_BtFechar');
      PromptDelete := RetornaLingua( DestSettings.Language,'Const_LogC_ConfirmaExcluir');
      PromptDelete_WindowCaption := RetornaLingua( DestSettings.Language,'Const_LogC_ConfirmaDelete_WindowCaption');
      OptionUserAll := RetornaLingua( DestSettings.Language,'Const_LogC_Todos');
      OptionLevelLow := RetornaLingua( DestSettings.Language,'Const_LogC_Low');
      OptionLevelNormal := RetornaLingua( DestSettings.Language,'Const_LogC_Normal');
      OptionLevelHigh := RetornaLingua( DestSettings.Language,'Const_LogC_High');
      OptionLevelCritic := RetornaLingua( DestSettings.Language,'Const_LogC_Critic');
      DeletePerformed := RetornaLingua( DestSettings.Language,'Const_LogC_ExcluirEfectuada');
  end;

  with DestSettings.AppMessages do
  begin
      MsgsForm_BtNew := RetornaLingua( DestSettings.Language,'Const_Msgs_BtNew');
      MsgsForm_BtReplay := RetornaLingua( DestSettings.Language,'Const_Msgs_BtReplay');
      MsgsForm_BtForward := RetornaLingua( DestSettings.Language,'Const_Msgs_BtForward');
      MsgsForm_BtDelete := RetornaLingua( DestSettings.Language,'Const_Msgs_BtDelete');
      MsgsForm_BtClose := RetornaLingua( DestSettings.Language,'Const_Msgs_BtClose'); //added by fduenas
      MsgsForm_WindowCaption := RetornaLingua( DestSettings.Language,'Const_Msgs_WindowCaption');
      MsgsForm_ColFrom := RetornaLingua( DestSettings.Language,'Const_Msgs_ColFrom');
      MsgsForm_ColSubject := RetornaLingua( DestSettings.Language,'Const_Msgs_ColSubject');
      MsgsForm_ColDate := RetornaLingua( DestSettings.Language,'Const_Msgs_ColDate');
      MsgsForm_PromptDelete := RetornaLingua( DestSettings.Language,'Const_Msgs_PromptDelete');
      MsgsForm_PromptDelete_WindowCaption := RetornaLingua( DestSettings.Language,'Const_Msgs_PromptDelete_WindowCaption'); //added by fduenas
      MsgsForm_NoMessagesSelected := RetornaLingua( DestSettings.Language,'Const_Msgs_NoMessagesSelected'); //added by fduenas
      MsgsForm_NoMessagesSelected_WindowCaption := RetornaLingua( DestSettings.Language,'Const_Msgs_NoMessagesSelected_WindowCaption'); //added by fduenas
      MsgRec_BtClose := RetornaLingua( DestSettings.Language,'Const_MsgRec_BtClose');
      MsgRec_WindowCaption := RetornaLingua( DestSettings.Language,'Const_MsgRec_WindowCaption');
      MsgRec_Title := RetornaLingua( DestSettings.Language,'Const_MsgRec_Title');
      MsgRec_LabelFrom := RetornaLingua( DestSettings.Language,'Const_MsgRec_LabelFrom');
      MsgRec_LabelDate := RetornaLingua( DestSettings.Language,'Const_MsgRec_LabelDate');
      MsgRec_LabelSubject := RetornaLingua( DestSettings.Language,'Const_MsgRec_LabelSubject');
      MsgRec_LabelMessage := RetornaLingua( DestSettings.Language,'Const_MsgRec_LabelMessage');
      MsgSend_BtSend := RetornaLingua( DestSettings.Language,'Const_MsgSend_BtSend');
      MsgSend_BtCancel := RetornaLingua( DestSettings.Language,'Const_MsgSend_BtCancel');
      MsgSend_WindowCaption := RetornaLingua( DestSettings.Language,'Const_MsgSend_WindowCaption');
      MsgSend_Title := RetornaLingua( DestSettings.Language,'Const_MsgSend_Title');
      MsgSend_GroupTo := RetornaLingua( DestSettings.Language,'Const_MsgSend_GroupTo');
      MsgSend_RadioUser := RetornaLingua( DestSettings.Language,'Const_MsgSend_RadioUser');
      MsgSend_RadioAll := RetornaLingua( DestSettings.Language,'Const_MsgSend_RadioAll');
      MsgSend_GroupMessage := RetornaLingua( DestSettings.Language,'Const_MsgSend_GroupMessage');
      MsgSend_LabelSubject := RetornaLingua( DestSettings.Language,'Const_MsgSend_LabelSubject'); //added by fduenas
      MsgSend_LabelMessageText := RetornaLingua( DestSettings.Language,'Const_MsgSend_LabelMessageText'); //added by fduenas
  end;

  DestSettings.WindowsPosition := poMainFormCenter;

 
    with DestSettings.UsersLogged do
      Begin
        BtnMessage     := RetornaLingua( DestSettings.Language ,'Const_UserLogged_BtnMsg' );
        BtnRefresh     := RetornaLingua( DestSettings.Language ,'Const_UserLogged_Refresh');
        BtnClose       := RetornaLingua( DestSettings.Language ,'Const_Msgs_BtClose');
        LabelDescricao := RetornaLingua( DestSettings.Language ,'Const_UserLogged_LabelDescricao');
        LabelCaption   := RetornaLingua( DestSettings.Language ,'Const_UserLogged_LabelCaption');
        ColName        := RetornaLingua( DestSettings.Language ,'Const_Cad_ColunaNome');
        ColLogin       := RetornaLingua( DestSettings.Language ,'Const_Cad_ColunaLogin');
        ColComputer    := RetornaLingua( DestSettings.Language ,'Const_CadColuna_Computer');
        ColData        := RetornaLingua( DestSettings.Language ,'Const_CadColuna_Data');
        InputCaption   := RetornaLingua( DestSettings.Language ,'Const_UserLogged_InputCaption');
        InputText      := RetornaLingua( DestSettings.Language ,'Const_UserLogged_InputText');
        MsgSystem      := RetornaLingua( DestSettings.Language ,'Const_UserLogged_MsgSystem');
      End;

end;

{-------------------------------------------------------------------------------}

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}

{$IFDEF DELPHI9_UP} {$REGION 'TUCSettings'} {$ENDIF}

{ TUCSettings }

procedure TUCSettings.Assign(Source: TPersistent);
begin
  if Source is TUCUserSettings then
  begin
    Self.CommonMessages.Assign(TUCUserSettings(Source).CommonMessages); //modified by fduenas
    Self.AppMessages.Assign(TUCUserSettings(Source).AppMessages); //modified by fduenas
    Self.WindowsPosition := WindowsPosition;
  end
  else
    inherited;
end;

constructor TUCSettings.Create(AOwner: TComponent);
begin
  inherited;
  fLanguage           := ucPortuguesBr;       
  FAppMessagesMSG     := TUCAppMessagesMSG.Create(nil);
  FLoginFormMSG       := TUCLoginFormMSG.Create(nil);
  FUserCommomMSG      := TUCUserCommonMSG.Create(nil);
  FCadUserFormMSG     := TUCCadUserFormMSG.Create(nil);
  FAddUserFormMSG     := TUCAddUserFormMSG.Create(nil);
  FAddProfileFormMSG  := TUCAddProfileFormMSG.Create(nil);
  FPermissFormMSG     := TUCPermissFormMSG.Create(nil);
  FProfileUserFormMSG := TUCProfileUserFormMSG.Create(nil);
  FTrocaSenhaFormMSG  := TUCTrocaSenhaFormMSG.Create(nil);
  FResetPassword      := TUCResetPassword.Create(nil);
  FLogControlFormMSG  := TUCLogControlFormMSG.Create(nil);
  fBancoDados         := FireBird;
  RetornaSqlBancoDados( fBancoDados, Type_Int,Type_Char,Type_VarChar,Type_Memo );
  fUsersLogged        := TUCCadUserLoggedMSG.Create(Nil);
  if csDesigning in ComponentState then
    IniSettings2(Self);
end;

destructor TUCSettings.Destroy;
begin
  //added by fduenas
  FAppMessagesMSG.Free;
  FLoginFormMSG.Free;
  FUserCommomMSG.Free;
  FCadUserFormMSG.Free;
  FAddUserFormMSG.Free;
  FAddProfileFormMSG.Free;
  FPermissFormMSG.Free;
  FProfileUserFormMSG.Free;
  FTrocaSenhaFormMSG.Free;
  FResetPassword.Free;
  FLogControlFormMSG.Free;
  fUsersLogged.Free;
  inherited;
end;

procedure TUCSettings.SetAppMessagesMSG(const Value: TUCAppMessagesMSG);
begin
  FAppMessagesMSG := Value;
end;

procedure TUCSettings.SetFAddProfileFormMSG(const Value: TUCAddProfileFormMSG);
begin
  FAddProfileFormMSG := Value;
end;

procedure TUCSettings.SetFAddUserFormMSG(const Value: TUCAddUserFormMSG);
begin
  FAddUserFormMSG := Value;
end;

procedure TUCSettings.SetfBancoDados(const Value: TUCTypeBancoDados);
begin
  fBancoDados := Value;
  RetornaSqlBancoDados( fBancoDados, Type_Int,Type_Char,Type_VarChar,Type_Memo );  
end;

procedure TUCSettings.SetFCadUserFormMSG(const Value: TUCCadUserFormMSG);
begin
  FCadUserFormMSG := Value;
end;

procedure TUCSettings.SetFFormLoginMsg(const Value: TUCLoginFormMSG);
begin
  FLoginFormMSG := Value;
end;

procedure TUCSettings.SetfLanguage(const Value: TUCLanguage);
begin
  fLanguage := Value;
  AlterLanguage2(Self);
end;

procedure TUCSettings.SetFLogControlFormMSG(const Value: TUCLogControlFormMSG);
begin
  FLogControlFormMSG := Value;
end;

procedure TUCSettings.SetFPermissFormMSG(const Value: TUCPermissFormMSG);
begin
  FPermissFormMSG := Value;
end;

procedure TUCSettings.SetFProfileUserFormMSG(const Value: TUCProfileUserFormMSG);
begin
  FProfileUserFormMSG := Value;
end;

procedure TUCSettings.SetFResetPassword(const Value: TUCResetPassword);
begin
  FResetPassword := Value;
end;

procedure TUCSettings.SetFTrocaSenhaFormMSG(const Value: TUCTrocaSenhaFormMSG);
begin
  FTrocaSenhaFormMSG := Value;
end;

procedure TUCSettings.SetFUserCommonMSg(const Value: TUCUserCommonMSG);
begin
  FUserCommomMSG := Value;
end;

procedure TUCSettings.SetfUsersLogged(const Value: TUCCadUserLoggedMSG);
begin
  fUsersLogged := Value;
end;

{$IFDEF DELPHI9_UP} {$ENDREGION} {$ENDIF}

end.
