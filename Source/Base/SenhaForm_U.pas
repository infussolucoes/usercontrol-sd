unit SenhaForm_U;

interface

{$I 'UserControl.inc'}

uses
  Buttons,
  Classes,
  Controls,
  Dialogs,
  Forms,
  Graphics,
  Messages,
  StdCtrls,
  SysUtils,
  Variants,
  Windows,

  {$IF CompilerVersion >= 23} {Delphi XE2}
  System.UITypes,
  {$IFEND}

  UCBase;

type
  TSenhaForm = class(TForm)
    edtSenha: TEdit;
    edtConfirmaSenha: TEdit;
    btnOK: TBitBtn;
    BtCancel: TBitBtn;
    LabelSenha: TLabel;
    LabelConfirma: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    function CompararSenhas(Senha, ConfirmaSenha: String): Boolean;
  public
    fUserControl: TUserControl;
    { Public declarations }
  end;

implementation

{$R *.dfm}
{ TSenhaForm }

function TSenhaForm.CompararSenhas(Senha, ConfirmaSenha: String): Boolean;
begin
  Result := False;
  With fUserControl do
  begin
    if (UserPasswordChange.ForcePassword) and (Senha = '') then
      MessageDlg(UserSettings.CommonMessages.ChangePasswordError.
        PasswordRequired, mtWarning, [mbOK], 0)
    else if Length(Senha) < UserPasswordChange.MinPasswordLength then
      MessageDlg(Format(UserSettings.CommonMessages.ChangePasswordError.
        MinPasswordLength, [UserPasswordChange.MinPasswordLength]), mtWarning,
        [mbOK], 0)
    else if Pos(LowerCase(Senha), 'abcdeasdfqwerzxcv1234567890321654987teste' +
      LowerCase(CurrentUser.UserName) + LowerCase(CurrentUser.UserLogin)) > 0
    then
      MessageDlg(UserSettings.CommonMessages.ChangePasswordError.
        InvalidNewPassword, mtWarning, [mbOK], 0)
    else if (Senha <> ConfirmaSenha) then
      MessageDlg(UserSettings.CommonMessages.ChangePasswordError.
        NewPasswordError, mtWarning, [mbOK], 0)
    else
      Result := true;
  End;
end;

procedure TSenhaForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TSenhaForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not(ModalResult = mrCancel) then
  begin
    CanClose := CompararSenhas(edtSenha.Text, edtConfirmaSenha.Text);
    if not CanClose then
      edtSenha.SetFocus;
  end;
end;

procedure TSenhaForm.FormCreate(Sender: TObject);
begin
  edtSenha.Clear;
  edtConfirmaSenha.Clear;
end;

procedure TSenhaForm.FormShow(Sender: TObject);
begin
  edtSenha.CharCase := fUserControl.Login.CharCasePass;
  edtConfirmaSenha.CharCase := fUserControl.Login.CharCasePass;
  LabelSenha.Caption := fUserControl.UserSettings.Login.LabelPassword;
  LabelConfirma.Caption := fUserControl.UserSettings.ChangePassword.
    LabelConfirm;
  btnOK.Caption := fUserControl.UserSettings.Login.BtOk;
  BtCancel.Caption := fUserControl.UserSettings.Login.BtCancel;
end;

end.
