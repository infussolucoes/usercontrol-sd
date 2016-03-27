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
unit UCReg;

interface

{$I 'UserControl.inc'}

uses
  Classes, Controls, DesignEditors, DesignIntf,  TypInfo, UCBase,
	{$IFDEF DELPHI9_UP}ToolsApi, Windows, Graphics{$ENDIF};

type
  TUCComponentsVarProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: String; override;
  end;

  TUCControlsEditor = class(TComponentEditor)
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

  TUserControlEditor = class(TComponentEditor)
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

  TUCAboutVarProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: String; override;
  end;

procedure Register;
procedure ShowControlsEditor(Componente: TUCControls);
procedure ShowUserControlsEditor(Componente: TUserControl);

{$IFDEF  DELPHI9_UP}
	{$R UCReg.dcr}
{$ENDIF}

implementation

uses
  Dialogs, Forms, SysUtils, UCAbout, UCIdle, UCObjSel_U, UCEditorForm_U, ActnList,
  ActnMan, ActnMenus, Menus, StdCtrls, UCSettings, Variants, UcMail, UcConsts_Language;

{$IFDEF  RTL170_UP}
var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;

procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  Assert(Assigned(AboutBoxServices), '');
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'USERCONTROL24');
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(rsSobreTitulo , rsSobreDescricao,
    ProductImage, False, rsSobreLicencaStatus);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure AddSplash;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.LoadFromResourceName(HInstance, 'USERCONTROL48');
  SplashScreenServices.AddPluginBitmap(rsSobreTitulo,bmp.Handle,false,rsSobreLicencaStatus,'');
  bmp.Free;
end;
{$ENDIF}
	
procedure Register;
begin
  RegisterComponents('SWDelphi - UC Main', [TUserControl, TUCSettings,
    TUCControls, TUCApplicationMessage, TUCIdle, TMailUserControl]);

  RegisterPropertyEditor(TypeInfo(TUCAboutVar), TUserControl, 'About',
    TUCAboutVarProperty);
  RegisterPropertyEditor(TypeInfo(TUCComponentsVar), TUserControl, 'Components',
    TUCComponentsVarProperty);
  RegisterComponentEditor(TUCControls, TUCControlsEditor);
  RegisterComponentEditor(TUserControl, TUserControlEditor);
end;

{ TUCComponentsVarProperty }
procedure TUCComponentsVarProperty.Edit;
begin
  ShowControlsEditor(TUCControls(GetComponent(0)));
end;

function TUCComponentsVarProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TUCComponentsVarProperty.GetValue: String;
begin
  Result := 'Components...';
end;

{ TUCAboutVarProperty }

procedure TUCAboutVarProperty.Edit;
begin
  with TAboutForm.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

function TUCAboutVarProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TUCAboutVarProperty.GetValue: String;
begin
  Result := 'Versao ' + UCVersion;
end;

procedure ShowUserControlsEditor(Componente: TUserControl);
var
  Editor: IOTAEditor;
  Modulo: IOTAModule;
  FormEditor: IOTAFormEditor;
  I: Integer;
  Formulario: TUCEditorForm;
  UserControl: TUserControl;
  Controle_Action, Controle_MainMenu, Controle_ActionManager,
    Controle_ActionMainMenuBar: String;
  UserActionMenuItem: String;
  UserProfileActionMenuItem: String;
  LogControlActionMeuItem: String;
  UserPasswordChangeActionMenuItem: String;
  FormularioDono: TForm;
begin
  UserControl := Componente;
  FormularioDono := TForm(UserControl.Owner);
  try
    Formulario := TUCEditorForm.Create(nil, UserControl);

    if Formulario.ShowModal = mrOk then
    begin
      with UserControl do
      begin
        ApplicationID := Formulario.edtApplicationID.Text;
        AutoStart := Formulario.ckAutoStart.Checked;
        CheckValidationKey := Formulario.ckValidationKey.Checked;
        EncryptKey := Formulario.spedtEncryptKey.Value;
        TableRights.TableName := Formulario.edtTableRights.Text;
        TableUsers.TableName := Formulario.edtTableUsers.Text;
        NotAllowedItems.ActionVisible := Formulario.ckActionVisible.Checked;
        NotAllowedItems.MenuVisible := Formulario.ckMenuVisible.Checked;
        Criptografia := TUCCriptografia(Formulario.cbCriptografia.ItemIndex);
        LoginMode := TUCLoginMode(Formulario.cbLoginMode.ItemIndex);

        if Formulario.cbActionList.ItemIndex >= 0 then
          Controle_Action := Formulario.cbActionList.Text;

        if Formulario.cbActionMainMenuBar.ItemIndex >= 0 then
          Controle_ActionMainMenuBar := Formulario.cbActionMainMenuBar.Text;

        if Formulario.cbActionManager.ItemIndex >= 0 then
          Controle_ActionManager := Formulario.cbActionManager.Text;

        if Formulario.cbMainMenu.ItemIndex >= 0 then
          Controle_MainMenu := Formulario.cbMainMenu.Text;

        if Formulario.cbUserAction.ItemIndex >= 0 then
          UserActionMenuItem := Formulario.cbUserAction.Text;

        if Formulario.cbUserMenuItem.ItemIndex >= 0 then
          UserActionMenuItem := Formulario.cbUserMenuItem.Text;

        if Formulario.cbUserProfileAction.ItemIndex >= 0 then
          UserProfileActionMenuItem := Formulario.cbUserProfileAction.Text;
        if Formulario.cbUserProfileMenuItem.ItemIndex >= 0 then
          UserProfileActionMenuItem := Formulario.cbUserProfileMenuItem.Text;

        if Formulario.cbLogControlAction.ItemIndex >= 0 then
          LogControlActionMeuItem := Formulario.cbLogControlAction.Text;
        if Formulario.cbLogControlMenuItem.ItemIndex >= 0 then
          LogControlActionMeuItem := Formulario.cbLogControlMenuItem.Text;

        if Formulario.cbUserPasswordChangeAction.ItemIndex >= 0 then
          UserPasswordChangeActionMenuItem :=
            Formulario.cbUserPasswordChangeAction.Text;
        if Formulario.cbUserPasswordChangeMenuItem.ItemIndex >= 0 then
          UserPasswordChangeActionMenuItem :=
            Formulario.cbUserPasswordChangeMenuItem.Text;

        for I := 0 to FormularioDono.ComponentCount - 1 do
        begin
          if (FormularioDono.Components[I].Name = Controle_Action) and
            (Formulario.cbActionList.ItemIndex >= 0) then
            ControlRight.ActionList :=
              TActionList(FormularioDono.Components[I]);

          if (FormularioDono.Components[I].Name = Controle_ActionMainMenuBar)
            and (Formulario.cbActionMainMenuBar.ItemIndex >= 0) then
            ControlRight.ActionMainMenuBar :=
              TActionMainMenuBar(UserControl.Owner.Components[I]);

          if (FormularioDono.Components[I].Name = Controle_ActionManager) and
            (Formulario.cbActionManager.ItemIndex >= 0) then
            ControlRight.ActionManager :=
              TActionManager(FormularioDono.Components[I]);

          if (FormularioDono.Components[I].Name = Controle_MainMenu) and
            (Formulario.cbMainMenu.ItemIndex >= 0) then
            ControlRight.MainMenu := TMainMenu(FormularioDono.Components[I]);

          if (FormularioDono.Components[I].Name = UserActionMenuItem) and
            (Formulario.cbUserAction.ItemIndex >= 0) then
            User.Action := TAction(FormularioDono.Components[I]);
          if (FormularioDono.Components[I].Name = UserActionMenuItem) and
            (Formulario.cbUserMenuItem.ItemIndex >= 0) then
            User.MenuItem := TMenuItem(FormularioDono.Components[I]);
          if (FormularioDono.Components[I]
            .Name = UserPasswordChangeActionMenuItem) and
            (Formulario.cbUserPasswordChangeAction.ItemIndex >= 0) then
            UserPasswordChange.Action := TAction(FormularioDono.Components[I]);
          if (FormularioDono.Components[I]
            .Name = UserPasswordChangeActionMenuItem) and
            (Formulario.cbUserPasswordChangeMenuItem.ItemIndex >= 0) then
            UserPasswordChange.MenuItem :=
              TMenuItem(FormularioDono.Components[I]);
        end;

        User.UsePrivilegedField := Formulario.ckUserUsePrivilegedField.Checked;
        User.ProtectAdministrator :=
          Formulario.ckUserProtectAdministrator.Checked;
        UserProfile.Active := Formulario.ckUserProfileActive.Checked;
        UserPasswordChange.ForcePassword :=
          Formulario.ckUserPassowrdChangeForcePassword.Checked;
        UserPasswordChange.MinPasswordLength :=
          Formulario.spedtUserPasswordChangeMinPasswordLength.Value;

        LogControl.TableLog := Formulario.edtLogControlTableLog.Text;
        LogControl.Active := Formulario.ckLogControlActive.Checked;

        Login.MaxLoginAttempts := Formulario.spedtMaxLoginAttempts.Value;
        Login.GetLoginName :=
          TUCGetLoginName(Formulario.cbGetLoginName.ItemIndex);
        Login.InitialLogin.User := Formulario.edtInitialLoginUser.Text;
        Login.InitialLogin.Password := Formulario.edtInitialLoginPassword.Text;
        Login.InitialLogin.Email := Formulario.edtInitialLoginEmail.Text;
        Login.InitialLogin.InitialRights := Formulario.mmInitialRights.Lines;
        Login.AutoLogin.Active := Formulario.ckLoginAutologinActive.Checked;
        Login.AutoLogin.User := Formulario.edtLoginAutoLoginUser.Text;
        Login.AutoLogin.Password := Formulario.edtLoginAutoLoginPassword.Text;
        Login.AutoLogin.MessageOnError :=
          Formulario.ckLoginAutoLoginMessageOnError.Checked;
        UserSettings.Login.TopImage := Formulario.imgTop.Picture;
        UserSettings.Login.LeftImage := Formulario.imgLeft.Picture;
        UserSettings.Login.BottomImage := Formulario.imgBottom.Picture;
      end;

      Modulo := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
      for I := 0 to Modulo.GetModuleFileCount - 1 do
      begin
        Editor := Modulo.GetModuleFileEditor(I);
        Editor.QueryInterface(IOTAFormEditor, FormEditor);
        if FormEditor <> nil then
        begin
          FormEditor.MarkModified;
          Break;
        end;
      end;
    end;
  finally
    SysUtils.FreeAndNil(Formulario);
  end;
end;

procedure ShowControlsEditor(Componente: TUCControls);
var
  FUCControl: TUCControls;
  FEditor: IOTAEditor;
  FModulo: IOTAModule;
  FFormEditor: IOTAFormEditor;
  I: Integer;
begin
  FUCControl := Componente;
  if not Assigned(FUCControl.UserControl) then
  begin
    MessageDlg('A propriedade UserControl tem que ser informada e o componente '
      + #13 + #10 + 'tem que estar visível!', mtInformation, [mbOK], 0);
    Exit;
  end;

  with TUCObjSel.Create(nil) do
  begin
    FForm := TCustomForm(FUCControl.Owner);
    FUserControl := FUCControl.UserControl;
    FInitialObjs := TStringList.Create;
    FUCControl.ListComponents(FForm.Name, FInitialObjs);
    // TUCControls(Componente).ListComponents(FForm.Name, FInitialObjs);
    lbGroup.Caption := FUCControl.GroupName;
    // TUCControls(Componente).GroupName;
    Show;
  end;

  try
    FModulo := (BorlandIDEServices as IOTAModuleServices)
      .FindFormModule(FUCControl.UserControl.Owner.Name);
  except
    FModulo := Nil;
  end;

  if FModulo = nil then
  begin
    ShowMessage('Modulo ' + FUCControl.UserControl.Owner.Name +
      ' não encontrado!');
    Exit;
  end
  else
    for I := 0 to FModulo.GetModuleFileCount - 1 do
    begin
      FEditor := FModulo.GetModuleFileEditor(I);
      FEditor.QueryInterface(IOTAFormEditor, FFormEditor);
      if FFormEditor <> nil then
      begin
        FFormEditor.MarkModified;
        Break;
      end;
    end;
end;

{ TUCControlsEditor }

procedure TUCControlsEditor.Edit;
begin
  ShowControlsEditor(TUCControls(Component));
end;

procedure TUCControlsEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TUCControlsEditor.GetVerb(Index: Integer): String;
begin
  Result := '&Selecionar Componentes...';
end;

function TUCControlsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TUserControlEditor }

procedure TUserControlEditor.Edit;
begin
  ShowUserControlsEditor(TUserControl(Component));
end;

procedure TUserControlEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TUserControlEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Configurar...';
end;

function TUserControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF RTL170_UP}
initialization
	AddSplash;
	RegisterAboutBox;
	
finalization
	UnregisterAboutBox;
{$ENDIF}

{$IFDEF FPC}
initialization
   //{$I XXXXX.lrs}
{$ENDIF}

end.
