{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usuários }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{
Autor da versão Original: Rodrigo Alves Cordeiro

Colaboradores da versão original
Alexandre Oliveira Campioni - alexandre.rural@netsite.com.br
Bernard Grandmougin
Carlos Guerra
Daniel Wszelaki
Everton Ramos [BS2 Internet]
Francisco Dueñas - fduenas@flashmail.com
Germán H. Cravero
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
{ Versão ShowDelphi Edition                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015   Giovani Da Cruz                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{ Você pode obter a última versão desse arquivo na pagina do projeto           }
{ User Control ShowDelphi Edition                                              }
{ Componentes localizado em http://infussolucoes.github.io/usercontrol-sd/     }
{                                                                              }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
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
  |*  - Criação e distribuição da Primeira Versao ShowDelphi
  ******************************************************************************* }

unit UCEditorForm_U;

interface

{ .$I 'UserControl.inc' }

uses
  ActnCtrls,
  ActnList,
  ActnMan,
  ActnMenus,
  Buttons,
  Classes,
  ComCtrls,
  {$IF CompilerVersion >= 23}
  system.Contnrs,
  {$IFEND}
  Controls,
  Dialogs,
  ExtCtrls,
  ExtDlgs,
  Forms,
  Graphics,
  jpeg,
  Menus,
  Spin,
  StdCtrls,
  ToolWin,
  UCBase, 
  System.Actions;

type
  TUCEditorForm = class(TForm)
    pnlBotoes: TPanel;
    Panel2: TPanel;
    lbComponente: TLabel;
    pnlCentro: TPanel;
    PageControl: TPageControl;
    tabPrincipal: TTabSheet;
    tabLogin: TTabSheet;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    SpeedButton1: TSpeedButton;
    GroupBox1: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label12: TLabel;
    lblInitialRights: TLabel;
    edtInitialLoginUser: TEdit;
    edtInitialLoginPassword: TEdit;
    edtInitialLoginEmail: TEdit;
    mmInitialRights: TMemo;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    edtLoginAutoLoginUser: TEdit;
    edtLoginAutoLoginPassword: TEdit;
    ckLoginAutologinActive: TCheckBox;
    ckLoginAutoLoginMessageOnError: TCheckBox;
    cbGetLoginName: TComboBox;
    tabLogControl: TTabSheet;
    Label25: TLabel;
    Panel4: TPanel;
    Image4: TImage;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    edtLogControlTableLog: TEdit;
    ckLogControlActive: TCheckBox;
    cbLogControlAction: TComboBox;
    cbLogControlMenuItem: TComboBox;
    Panel6: TPanel;
    imgTop: TImage;
    Panel7: TPanel;
    imgLeft: TImage;
    Panel8: TPanel;
    imgBottom: TImage;
    Panel5: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edtApplicationID: TEdit;
    edtTableUsers: TEdit;
    edtTableRights: TEdit;
    edtTabelaPermissoesEX: TEdit;
    ckAutoStart: TCheckBox;
    btnTabelasPadrao: TButton;
    btnOK: TBitBtn;
    btnClose: TBitBtn;
    ckValidationKey: TCheckBox;
    cbCriptografia: TComboBox;
    Label29: TLabel;
    Label30: TLabel;
    Label32: TLabel;
    cbLoginMode: TComboBox;
    GroupBox3: TGroupBox;
    ckActionVisible: TCheckBox;
    ckMenuVisible: TCheckBox;
    tabUser: TTabSheet;
    tabUserProfile: TTabSheet;
    tabUserPasswordChange: TTabSheet;
    cbUserAction: TComboBox;
    cbUserMenuItem: TComboBox;
    Label3: TLabel;
    cbUserProfileAction: TComboBox;
    cbUserProfileMenuItem: TComboBox;
    Label4: TLabel;
    cbUserPasswordChangeAction: TComboBox;
    cbUserPasswordChangeMenuItem: TComboBox;
    Label1: TLabel;
    Label26: TLabel;
    tabControlRights: TTabSheet;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    cbActionList: TComboBox;
    cbActionMainMenuBar: TComboBox;
    cbActionManager: TComboBox;
    cbMainMenu: TComboBox;
    spedtEncryptKey: TSpinEdit;
    ckUserProtectAdministrator: TCheckBox;
    ckUserUsePrivilegedField: TCheckBox;
    Label2: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    ckUserProfileActive: TCheckBox;
    Label31: TLabel;
    Label37: TLabel;
    ckUserPassowrdChangeForcePassword: TCheckBox;
    spedtUserPasswordChangeMinPasswordLength: TSpinEdit;
    spedtMaxLoginAttempts: TSpinEdit;
    OpenPictureDialog: TOpenPictureDialog;
    ActionList: TActionList;
    acCarregarFigura: TAction;
    acVisualizarTelaLogin: TAction;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Image1: TImage;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);

    procedure ComboRightsChange(Sender: TObject);

    procedure ComboActionMenuItem(Sender: TObject);

    procedure btnTabelasPadraoClick(Sender: TObject);
    procedure edtTableRightsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure ActionsExecute(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
    FUserControl: TUserControl;
  public
    constructor Create(AOwner: TComponent; UserControl: TUserControl);
      reintroduce;
  end;

implementation

uses
  LoginWindow_U,
  ShellAPI,
  SysUtils,
  UcConsts_Language,
  UCMessages,
  Windows;

{$R *.dfm}

procedure TUCEditorForm.edtTableRightsChange(Sender: TObject);
begin
  edtTabelaPermissoesEX.Text := edtTableRights.Text + 'EX';
end;

procedure TUCEditorForm.ActionsExecute(Sender: TObject);
begin
  if Sender = imgTop then
    if OpenPictureDialog.Execute then
      imgTop.Picture.LoadFromFile(OpenPictureDialog.FileName);

  if Sender = imgBottom then
    if OpenPictureDialog.Execute then
      imgBottom.Picture.LoadFromFile(OpenPictureDialog.FileName);

  if Sender = imgLeft then
    if OpenPictureDialog.Execute then
      imgLeft.Picture.LoadFromFile(OpenPictureDialog.FileName);
end;

procedure TUCEditorForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TUCEditorForm.btnTabelasPadraoClick(Sender: TObject);
begin
  edtTableUsers.Text := RetornaLingua(FUserControl.Language,
    'Const_TableUsers_TableName');
  edtTableRights.Text := RetornaLingua(FUserControl.Language,
    'Const_TableRights_TableName');
end;

procedure TUCEditorForm.ComboActionMenuItem(Sender: TObject);
begin
  // Combo USER
  if (Sender = cbUserAction) and (cbUserAction.ItemIndex >= 0) then
    cbUserMenuItem.ItemIndex := -1;

  if (Sender = cbUserMenuItem) and (cbUserMenuItem.ItemIndex >= 0) then
    cbUserAction.ItemIndex := -1;

  // Combo USERPROFILE
  if (Sender = cbUserProfileAction) and (cbUserProfileAction.ItemIndex >= 0)
  then
    cbUserProfileMenuItem.ItemIndex := -1;

  if (Sender = cbUserProfileMenuItem) and (cbUserProfileMenuItem.ItemIndex >= 0)
  then
    cbUserProfileAction.ItemIndex := -1;

  // Combo USERPASSWORDCHANGE
  if (Sender = cbUserPasswordChangeAction) and
    (cbUserPasswordChangeAction.ItemIndex >= 0) then
    cbUserPasswordChangeMenuItem.ItemIndex := -1;

  if (Sender = cbUserPasswordChangeMenuItem) and
    (cbUserPasswordChangeMenuItem.ItemIndex >= 0) then
    cbUserPasswordChangeAction.ItemIndex := -1;

  // Combo LOGCONTROL
  if (Sender = cbLogControlAction) and (cbLogControlAction.ItemIndex >= 0) then
    cbLogControlMenuItem.ItemIndex := -1;

  if (Sender = cbLogControlMenuItem) and (cbLogControlMenuItem.ItemIndex >= 0)
  then
    cbLogControlAction.ItemIndex := -1;
end;

procedure TUCEditorForm.ComboRightsChange(Sender: TObject);
begin
  { if Sender = cbActionList then
    if cbActionList.ItemIndex >= 0 then
    begin
    cbActionMainMenuBar.ItemIndex := -1;
    cbActionManager.ItemIndex     := -1;
    cbMainMenu.ItemIndex          := -1;
    end;

    if Sender = cbActionMainMenuBar then
    if cbActionMainMenuBar.ItemIndex >= 0 then
    begin
    cbActionList.ItemIndex    := -1;
    cbActionManager.ItemIndex := -1;
    cbMainMenu.ItemIndex      := -1;
    end;

    if Sender = cbActionManager then
    if cbActionManager.ItemIndex >= 0 then
    begin
    cbActionList.ItemIndex        := -1;
    cbActionMainMenuBar.ItemIndex := -1;
    cbMainMenu.ItemIndex          := -1;
    end;

    if Sender = cbMainMenu then
    if cbMainMenu.ItemIndex >= 0 then
    begin
    cbActionList.ItemIndex        := -1;
    cbActionMainMenuBar.ItemIndex := -1;
    cbActionManager.ItemIndex     := -1;
    end; }

end;

constructor TUCEditorForm.Create(AOwner: TComponent; UserControl: TUserControl);
begin
  inherited Create(AOwner);
  FUserControl := UserControl;
end;

procedure TUCEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TUCEditorForm.FormCreate(Sender: TObject);
var
  I: Integer;
  Formulario: TForm;
begin
  lblInitialRights.Caption := 'Initial  ' + #13 + 'Rights :';
  PageControl.ActivePage := tabPrincipal;

  with FUserControl do
  begin
    edtApplicationID.Text := ApplicationID;
    ckAutoStart.Checked := AutoStart;
    ckValidationKey.Checked := CheckValidationKey;
    spedtEncryptKey.Value := EncryptKey;
    edtTableRights.Text := TableRights.TableName;
    edtTableUsers.Text := TableUsers.TableName;
    ckActionVisible.Checked := NotAllowedItems.ActionVisible;
    ckMenuVisible.Checked := NotAllowedItems.MenuVisible;
    cbCriptografia.ItemIndex := Integer(Criptografia);
    cbLoginMode.ItemIndex := Integer(LoginMode);
  end;

  Formulario := TForm(FUserControl.Owner);

  for I := 0 to Formulario.ComponentCount - 1 do
  begin
    if Formulario.Components[I] is TAction then
    begin
      cbUserAction.Items.Add(TAction(Formulario.Components[I]).Name);
      cbUserProfileAction.Items.Add(TAction(Formulario.Components[I]).Name);
      cbLogControlAction.Items.Add(TAction(Formulario.Components[I]).Name);
      cbUserPasswordChangeAction.Items.Add
        (TAction(Formulario.Components[I]).Name);
    end;

    if Formulario.Components[I] is TMenuItem then
    begin
      cbUserMenuItem.Items.Add(Formulario.Components[I].Name);
      cbUserProfileMenuItem.Items.Add(Formulario.Components[I].Name);
      cbLogControlMenuItem.Items.Add(Formulario.Components[I].Name);
      cbUserPasswordChangeMenuItem.Items.Add(Formulario.Components[I].Name);
    end;

    // Adicionar os valores dos "ControlRights"
    if Formulario.Components[I] is TActionList then
      cbActionList.Items.Add(Formulario.Components[I].Name);

    if Formulario.Components[I] is TActionMainMenuBar then
      cbActionMainMenuBar.Items.Add(Formulario.Components[I].Name);

    if Formulario.Components[I] is TActionManager then
      cbActionManager.Items.Add(Formulario.Components[I].Name);

    if Formulario.Components[I] is TMainMenu then
      cbMainMenu.Items.Add(Formulario.Components[I].Name);
  end;

  with FUserControl.ControlRight do
  begin
    if Assigned(ActionList) then
      cbActionList.ItemIndex := (cbActionList.Items.IndexOf(ActionList.Name));

    if Assigned(MainMenu) then
      cbMainMenu.ItemIndex := (cbMainMenu.Items.IndexOf(MainMenu.Name));

    if Assigned(ActionMainMenuBar) then
      cbActionMainMenuBar.ItemIndex :=
        (cbActionMainMenuBar.Items.IndexOf(ActionMainMenuBar.Name));

    if Assigned(ActionManager) then
      cbActionManager.ItemIndex :=
        (cbActionManager.Items.IndexOf(ActionManager.Name));
  end;

  // Action e MenuItem USER
  if Assigned(FUserControl.User.Action) then
    cbUserAction.ItemIndex :=
      (cbUserAction.Items.IndexOf(FUserControl.User.Action.Name));

  if Assigned(FUserControl.User.MenuItem) then
    cbUserMenuItem.ItemIndex :=
      (cbUserMenuItem.Items.IndexOf(FUserControl.User.MenuItem.Name));

  // Action e MenuItem USERPROFILE
  { if Assigned(FUserControl.UserProfile.Action) then
    cbUserProfileAction.ItemIndex := (cbUserProfileAction.Items.IndexOf(FUserControl.UserProfile.Action.Name));

    if Assigned(FUserControl.UserProfile.MenuItem) then
    cbUserProfileMenuItem.ItemIndex := (cbUserProfileMenuItem.Items.IndexOf(FUserControl.UserProfile.MenuItem.Name)); }

  // Action e MenuItem USERPASSWORDCHANGE
  if Assigned(FUserControl.UserPasswordChange.Action) then
    cbUserPasswordChangeAction.ItemIndex :=
      (cbUserPasswordChangeAction.Items.IndexOf
      (FUserControl.UserPasswordChange.Action.Name));

  if Assigned(FUserControl.UserPasswordChange.MenuItem) then
    cbUserPasswordChangeMenuItem.ItemIndex :=
      (cbUserPasswordChangeMenuItem.Items.IndexOf
      (FUserControl.UserPasswordChange.MenuItem.Name));

  // Action e MenuItem LOGCONTROL
  { if Assigned(FUserControl.LogControl.Action) then
    cbLogControlAction.ItemIndex := (cbLogControlAction.Items.IndexOf(FUserControl.LogControl.Action.Name));

    if Assigned(FUserControl.LogControl.MenuItem) then
    cbLogControlMenuItem.ItemIndex := (cbLogControlMenuItem.Items.IndexOf(FUserControl.LogControl.MenuItem.Name)); }

  ckUserProtectAdministrator.Checked := FUserControl.User.ProtectAdministrator;
  ckUserUsePrivilegedField.Checked := FUserControl.User.UsePrivilegedField;

  ckUserProfileActive.Checked := FUserControl.UserProfile.Active;

  ckUserPassowrdChangeForcePassword.Checked :=
    FUserControl.UserPasswordChange.ForcePassword;
  spedtUserPasswordChangeMinPasswordLength.Value :=
    FUserControl.UserPasswordChange.MinPasswordLength;

  edtLogControlTableLog.Text := FUserControl.LogControl.TableLog;
  ckLogControlActive.Checked := FUserControl.LogControl.Active;

  // Login
  spedtMaxLoginAttempts.Value := FUserControl.Login.MaxLoginAttempts;
  cbGetLoginName.ItemIndex := Integer(FUserControl.Login.GetLoginName);
  // login inicial
  edtInitialLoginUser.Text := FUserControl.Login.InitialLogin.User;
  edtInitialLoginPassword.Text := FUserControl.Login.InitialLogin.Password;
  edtInitialLoginEmail.Text := FUserControl.Login.InitialLogin.Email;
  mmInitialRights.Lines := FUserControl.Login.InitialLogin.InitialRights;
  // AutoLogin
  edtLoginAutoLoginUser.Text := FUserControl.Login.AutoLogin.User;
  edtLoginAutoLoginPassword.Text := FUserControl.Login.AutoLogin.Password;
  ckLoginAutologinActive.Checked := FUserControl.Login.AutoLogin.Active;
  ckLoginAutoLoginMessageOnError.Checked :=
    FUserControl.Login.AutoLogin.MessageOnError;
  // Figuras
  imgTop.Picture.Bitmap := FUserControl.UserSettings.Login.TopImage.Bitmap;
  imgLeft.Picture.Bitmap := FUserControl.UserSettings.Login.LeftImage.Bitmap;
  imgBottom.Picture.Bitmap := FUserControl.UserSettings.Login.
    BottomImage.Bitmap;
end;

procedure TUCEditorForm.SpeedButton1Click(Sender: TObject);
var
  frmLogin: TfrmLoginWindow;
begin
  try
    frmLogin := TfrmLoginWindow.Create(nil);
    with frmLogin do
    begin
      FUserControl := Self.FUserControl;
      btOK.onClick := BotoesClickVisualizacao;
      BtCancela.onClick := BotoesClickVisualizacao;
      Caption := Self.FUserControl.UserSettings.Login.WindowCaption;
      LbUsuario.Caption := Self.FUserControl.UserSettings.Login.LabelUser;
      LbSenha.Caption := Self.FUserControl.UserSettings.Login.LabelPassword;
      imgTop.Picture := Self.imgTop.Picture;
      imgLeft.Picture := Self.imgLeft.Picture;
      imgBottom.Picture := Self.imgBottom.Picture;
      btOK.Caption := Self.FUserControl.UserSettings.Login.btOK;
      BtCancela.Caption := Self.FUserControl.UserSettings.Login.BtCancel;
      Position := Self.FUserControl.UserSettings.WindowsPosition;
      ShowModal;
    end;
  finally
    SysUtils.FreeAndNil(frmLogin);
  end;
end;

procedure TUCEditorForm.SpeedButton2Click(Sender: TObject);
begin
  imgTop.Picture := nil;
end;

procedure TUCEditorForm.SpeedButton3Click(Sender: TObject);
begin
  imgLeft.Picture := nil;
end;

procedure TUCEditorForm.SpeedButton4Click(Sender: TObject);
begin
  imgBottom.Picture := nil;
end;

procedure TUCEditorForm.SpeedButton5Click(Sender: TObject);
begin
  Case TSpeedButton(Sender).Tag of
    0:
      cbActionList.ItemIndex := -1;
    1:
      cbActionMainMenuBar.ItemIndex := -1;
    2:
      cbActionManager.ItemIndex := -1;
    3:
      cbMainMenu.ItemIndex := -1;
  End;
end;

end.
