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

unit IncUser_U;

interface

{$I 'UserControl.inc'}

uses
  Variants,
  Buttons,
  Classes,
  Controls,
  DB,
  DBCtrls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  Messages,
  Spin,
  StdCtrls,
  SysUtils,
  Windows,

  {$IF CompilerVersion >= 23}
  System.UITypes,
  {$IFEND}
  
  UCBase;

type
  TfrmIncluirUsuario = class(TForm)
    Panel1: TPanel;
    LbDescricao: TLabel;
    Image1: TImage;
    Panel3: TPanel;
    btGravar: TBitBtn;
    btCancela: TBitBtn;
    Panel2: TPanel;
    lbNome: TLabel;
    EditNome: TEdit;
    lbLogin: TLabel;
    EditLogin: TEdit;
    lbEmail: TLabel;
    EditEmail: TEdit;
    ckPrivilegiado: TCheckBox;
    lbPerfil: TLabel;
    ComboPerfil: TDBLookupComboBox;
    btlimpa: TSpeedButton;
    ckUserExpired: TCheckBox;
    LabelExpira: TLabel;
    SpinExpira: TSpinEdit;
    LabelDias: TLabel;
    ComboStatus: TComboBox;
    Label1: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btCancelaClick(Sender: TObject);
    procedure btGravarClick(Sender: TObject);
    function GetNewIdUser: Integer;
    procedure btlimpaClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ckUserExpiredClick(Sender: TObject);
  private
    FormSenha: TCustomForm;
    { Private declarations }
  public
    { Public declarations }
    FAltera: Boolean;
    FUserControl: TUserControl;
    FDataSetCadastroUsuario: TDataSet;
    vNovoIDUsuario: Integer;
  end;

implementation

uses
  SenhaForm_U;

{$R *.dfm}

procedure TfrmIncluirUsuario.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmIncluirUsuario.FormCreate(Sender: TObject);
begin
  Self.BorderIcons := [];
  Self.BorderStyle := bsDialog;
end;

procedure TfrmIncluirUsuario.btCancelaClick(Sender: TObject);
begin
  Close;
end;
{$WARNINGS OFF}

procedure TfrmIncluirUsuario.btGravarClick(Sender: TObject);
var
  vNovaSenha: String;
  vNome: String;
  vLogin: String;
  vEmail: String;
  vUserExpired: Integer;
  vPerfil: Integer;
  vPrivilegiado: Boolean;
begin
  if ComboPerfil.ListSource.DataSet.RecordCount > 0 and ComboPerfil.KeyValue = NULL then
  begin
    ShowMessage('Falta Informar o Perfil');
    Exit; // Cleilson Sousa
  end;

  btGravar.Enabled := False;

  with FUserControl do
    if not FAltera then
    begin // inclui user
      if Self.FUserControl.ExisteUsuario(EditLogin.Text) then
      begin
        MessageDlg
          (Format(FUserControl.UserSettings.CommonMessages.UsuarioExiste,
          [EditLogin.Text]), mtWarning, [mbOK], 0);
        Exit;
      end;

      FormSenha := TSenhaForm.Create(Self);
      TSenhaForm(FormSenha).Position := UserSettings.WindowsPosition;
      TSenhaForm(FormSenha).FUserControl := FUserControl;
      TSenhaForm(FormSenha).Caption :=
        Format(FUserControl.UserSettings.ResetPassword.WindowCaption,
        [EditLogin.Text]);
      if TSenhaForm(FormSenha).ShowModal <> mrOk then
      begin
        btGravar.Enabled := True;
        Exit;
      end;
      vNovaSenha := TSenhaForm(FormSenha).edtSenha.Text;
      vNovoIDUsuario := GetNewIdUser;
      vNome := EditNome.Text;
      vLogin := EditLogin.Text;
      vEmail := EditEmail.Text;
      FreeAndNil(FormSenha);

      if ComboPerfil.KeyValue = NULL then
        vPerfil := 0
      else
        vPerfil := ComboPerfil.KeyValue;

      vPrivilegiado := ckPrivilegiado.Checked;
      vUserExpired := StrToInt(BoolToStr(ckUserExpired.Checked));

      AddUser(vLogin, vNovaSenha, vNome, vEmail, vPerfil, vUserExpired,
        SpinExpira.Value, vPrivilegiado);

      if (Assigned(FUserControl.MailUserControl)) and
        (FUserControl.MailUserControl.AdicionaUsuario.Ativo) then
        try
          FUserControl.MailUserControl.EnviaEmailAdicionaUsuario(vNome, vLogin,
            Encrypt(vNovaSenha, EncryptKey), vEmail, IntToStr(vPerfil),
            EncryptKey);
        except
          on E: Exception do
            Log(E.Message, 0);
        end;

    end
    else
    begin // alterar user
      // vNovoIDUsuario := TfrmCadastrarUsuario(Self.Owner).FDataSetCadastroUsuario.FieldByName('IdUser').AsInteger;
      vNome := EditNome.Text;
      vLogin := EditLogin.Text;
      vEmail := EditEmail.Text;
      if ComboPerfil.KeyValue = NULL then
        vPerfil := 0
      else
        vPerfil := ComboPerfil.KeyValue;

      vUserExpired := StrToInt(BoolToStr(ckUserExpired.Checked));
      // Added by Petrus van Breda 28/04/2007
      vPrivilegiado := ckPrivilegiado.Checked;
      ChangeUser(vNovoIDUsuario, vLogin, vNome, vEmail, vPerfil, vUserExpired,
        SpinExpira.Value, ComboStatus.ItemIndex, vPrivilegiado);

      if (Assigned(FUserControl.MailUserControl)) and
        (FUserControl.MailUserControl.AlteraUsuario.Ativo) then
        try
          FUserControl.MailUserControl.EnviaEmailAlteraUsuario(vNome, vLogin,
            'Não Alterada', vEmail, IntToStr(vPerfil), EncryptKey);
        except
          on E: Exception do
            Log(E.Message, 2);
        end;

    end;

  { With TfrmCadastrarUsuario(Owner) do
    Begin }
  FDataSetCadastroUsuario.Close;
  FDataSetCadastroUsuario.Open;
  FDataSetCadastroUsuario.Locate('idUser', vNovoIDUsuario, []);
  // End;
  Close;
end;
{$WARNINGS ON}

function TfrmIncluirUsuario.GetNewIdUser: Integer;
var
  DataSet: TDataSet;
  SQLStmt: String;
begin
  with FUserControl do
  begin
    SQLStmt := Format('SELECT %s.%s FROM %s ORDER BY %s DESC',
      [TableUsers.TableName, TableUsers.FieldUserID, TableUsers.TableName,
      TableUsers.FieldUserID]);
    try
      DataSet := DataConnector.UCGetSQLDataSet(SQLStmt);
      Result := DataSet.Fields[0].AsInteger + 1;
      DataSet.Close;
    finally
      SysUtils.FreeAndNil(DataSet);
    end;
  end;
end;

procedure TfrmIncluirUsuario.btlimpaClick(Sender: TObject);
begin
  ComboPerfil.KeyValue := NULL;
end;

procedure TfrmIncluirUsuario.FormShow(Sender: TObject);
begin
  if not FUserControl.UserProfile.Active then
  begin
    lbPerfil.Visible := False;
    ComboPerfil.Visible := False;
    btlimpa.Visible := False;
  end
  else
  begin
    ComboPerfil.ListSource.DataSet.Close;
    ComboPerfil.ListSource.DataSet.Open;
  end;

  // Opção de senha so deve aparecer qdo setada como true no componente By Vicente Barros Leonel
  ckUserExpired.Visible := FUserControl.Login.ActiveDateExpired;

  ckPrivilegiado.Visible := FUserControl.User.UsePrivilegedField;
  EditLogin.CharCase := Self.FUserControl.Login.CharCaseUser;

  SpinExpira.Visible := ckUserExpired.Visible;
  LabelExpira.Visible := ckUserExpired.Visible;
  LabelDias.Visible := ckUserExpired.Visible;

  if (FUserControl.User.ProtectAdministrator) and
    (EditLogin.Text = FUserControl.Login.InitialLogin.User) then
    EditLogin.Enabled := False;

end;

procedure TfrmIncluirUsuario.ckUserExpiredClick(Sender: TObject);
begin
  SpinExpira.Enabled := not ckUserExpired.Checked;
end;

end.
