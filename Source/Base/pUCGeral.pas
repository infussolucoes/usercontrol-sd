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
unit pUCGeral;

interface

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
  StdCtrls,
  SysUtils,
  Windows,

  UcBase;

type
  TFormUserPerf = class(TForm)
    Panel1: TPanel;
    LbDescricao: TLabel;
    Image1: TImage;
    Panel2: TPanel;
    SpeedUser: TSpeedButton;
    SpeedPerfil: TSpeedButton;
    Panel3: TPanel;
    SpeedLog: TSpeedButton;
    SpeedUserLog: TSpeedButton;
    SBSair: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SpeedUserClick(Sender: TObject);
    procedure SpeedPerfilClick(Sender: TObject);
    procedure SpeedLogClick(Sender: TObject);
    procedure SpeedUserLogClick(Sender: TObject);
    procedure SpeedUserMouseEnter(Sender: TObject);
    procedure SpeedUserMouseLeave(Sender: TObject);
    procedure SBSairClick(Sender: TObject);
  protected
    FrmFrame: TCustomFrame;
  private
    { Private declarations }
  public
    FUsercontrol: TUserControl;
    { Public declarations }
  end;

var
  FormUserPerf: TFormUserPerf;

implementation

uses
  pUCFrame_Log,
  pUcFrame_Profile,
  pUcFrame_User,
  pUcFrame_UserLogged,
  UCMessages;

{$R *.dfm} { ------ FORM ------------------------------------------ }

procedure TFormUserPerf.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.Release;
end;

procedure TFormUserPerf.FormShow(Sender: TObject);
begin
  with FUsercontrol do
  begin
    FUsercontrol.CurrentUser.PerfilUsuario := Nil;
    FUsercontrol.CurrentUser.PerfilUsuario := DataConnector.UCGetSQLDataset(
      Format('Select %s as IdUser, %s as Login, %s as Nome, %s as Email, %s as Perfil, %s as Privilegiado, '+
      '%s as Tipo, %s as Senha, %s as UserNaoExpira, %s as DaysOfExpire , %s as UserInative,  %s as Lotacao, %s as UserType, %s as Empresa from %s Where '+
      '%s  = %s ORDER BY %s', [TableUsers.FieldUserID, TableUsers.FieldLogin, TableUsers.FieldUserName,
      TableUsers.FieldEmail, TableUsers.FieldProfile, TableUsers.FieldPrivileged, TableUsers.FieldTypeRec,
      TableUsers.FieldPassword, TableUsers.FieldUserExpired, TableUsers.FieldUserDaysSun,
      TableUsers.FieldUserInative,TableUsers.FieldUserDepartment, TableUsers.FieldUserType, TableUsers.FieldUserEmpresa, TableUsers.TableName, TableUsers.FieldTypeRec,
      QuotedStr('U'), TableUsers.FieldLogin]));

    FUsercontrol.CurrentUser.PerfilGrupo := Nil;
    FUsercontrol.CurrentUser.PerfilGrupo := DataConnector.UCGetSQLDataset
      (Format('Select %s as IdUser, %s as Login, %s as Nome, %s as Tipo from %s Where %s  = %s ORDER BY %s',
      [TableUsers.FieldUserID, TableUsers.FieldLogin, TableUsers.FieldUserName,
      TableUsers.FieldTypeRec, TableUsers.TableName, TableUsers.FieldTypeRec,
      QuotedStr('P'), TableUsers.FieldUserName]));
    //   Lotacao -   Mauri
    FUsercontrol.CurrentUser.Lotacao:= DataConnector.UCGetSQLDataset(
           Format('Select %s as OrgaoDescricao, %s as OrgaoCodigo from %s where %s = '+QuotedStr('A')+' or %s is null  ORDER BY %s',
                [TableUserDepartment.FieldNameDepartment,
                TableUserDepartment.FieldIDDepartment,
                TableUserDepartment.TableName,
                TableUserDepartment.FieldStatusDepartment,
                TableUserDepartment.FieldStatusDepartment,
                TableUserDepartment.FieldNameDepartment
                ]
                ));

    //  Empresa Mauri 26/01/2017
    FUsercontrol.CurrentUser.Empresa:= DataConnector.UCGetSQLDataset( Format('Select %s as Empresa, %s as IdEmp from %s ORDER BY %s',
                [TableUserEmpresa.FieldNameEmpresa,
                TableUserEmpresa.FieldIDEmpresa,
                TableUserEmpresa.TableName,
                TableUserEmpresa.FieldNameEmpresa
                ]
                ));


  end;

  SpeedPerfil.Visible := FUsercontrol.UserProfile.Active;
  SpeedLog.Visible := FUsercontrol.LogControl.Active;
  SpeedUserLog.Visible := FUsercontrol.UsersLogged.Active;

  SpeedUserClick(Sender);
  Caption := FUsercontrol.UserSettings.UsersForm.WindowCaption;

  SpeedUser.Caption := FUsercontrol.UserSettings.Log.ColUser;
  SpeedPerfil.Caption := FUsercontrol.UserSettings.UsersProfile.ColProfile;
  SpeedUserLog.Caption := FUsercontrol.UserSettings.UsersLogged.LabelDescricao;

end;

procedure TFormUserPerf.SpeedPerfilClick(Sender: TObject);
begin
  if FrmFrame is TFrame_Profile then
    Exit;
  if Assigned(FrmFrame) then
    FreeAndNil(FrmFrame);

  FrmFrame := TFrame_Profile.Create(Self);
  TFrame_Profile(FrmFrame).DataPerfil.DataSet :=
    FUsercontrol.CurrentUser.PerfilGrupo;
  TFrame_Profile(FrmFrame).Height := Panel3.Height;
  TFrame_Profile(FrmFrame).Width := Panel3.Width;
  TFrame_Profile(FrmFrame).FDataSetPerfilUsuario :=
    FUsercontrol.CurrentUser.PerfilGrupo;
  TFrame_Profile(FrmFrame).FUsercontrol := FUsercontrol;
  TFrame_Profile(FrmFrame).DbGridPerf.Columns[0].Title.Caption :=
    FUsercontrol.UserSettings.UsersProfile.ColProfile;

  with FUsercontrol.UserSettings.UsersProfile, TFrame_Profile(FrmFrame) do
  begin
    LbDescricao.Caption := LabelDescription;
    BtnAddPer.Caption := BtAdd;
    BtnAltPer.Caption := BtChange;
    BtnExcPer.Caption := BtDelete;
    BtnAcePer.Caption := BtRights;
  end;
  FrmFrame.Parent := Panel3;

end;

procedure TFormUserPerf.SpeedUserClick(Sender: TObject);
begin
  if FrmFrame is TUCFrame_User then
    Exit;

  if Assigned(FrmFrame) then
    FreeAndNil(FrmFrame);

  FrmFrame := TUCFrame_User.Create(Self);
  TUCFrame_User(FrmFrame).FDataSetCadastroUsuario := FUsercontrol.CurrentUser.PerfilUsuario;
  TUCFrame_User(FrmFrame).DataUser.DataSet        := TUCFrame_User(FrmFrame).FDataSetCadastroUsuario;
  TUCFrame_User(FrmFrame).DataPerfil.DataSet      := FUsercontrol.CurrentUser.PerfilGrupo;
  TUCFrame_User(FrmFrame).FUsercontrol := FUsercontrol;
//  Lotacao   -   Mauri
  TUCFrame_User(FrmFrame).DataLotacao.DataSet     := FUsercontrol.CurrentUser.Lotacao; // mauri
//  Empresa Mauri 26/01/2017
  TUCFrame_User(FrmFrame).DataEmpresa.DataSet     := FUsercontrol.CurrentUser.Empresa; // mauri

  TUCFrame_User(FrmFrame).Height := Panel3.Height;
  TUCFrame_User(FrmFrame).Width := Panel3.Width;
  TUCFrame_User(FrmFrame).SetWindow;
  LbDescricao.Caption := FUsercontrol.UserSettings.UsersForm.LabelDescription;

  FrmFrame.Parent := Panel3;
end;

procedure TFormUserPerf.SpeedUserLogClick(Sender: TObject);
begin
  if FrmFrame is TUCFrame_UsersLogged then
    Exit;

  if Assigned(FrmFrame) then
    FreeAndNil(FrmFrame);

  FrmFrame := TUCFrame_UsersLogged.Create(Self);
  LbDescricao.Caption := FUsercontrol.UserSettings.UsersLogged.LabelDescricao;
  TUCFrame_UsersLogged(FrmFrame).FUsercontrol := FUsercontrol;
  TUCFrame_UsersLogged(FrmFrame).SetWindow;
  TUCFrame_UsersLogged(FrmFrame).Height := Panel3.Height;
  TUCFrame_UsersLogged(FrmFrame).Width := Panel3.Width;
  FrmFrame.Parent := Panel3;
end;

procedure TFormUserPerf.SpeedUserMouseEnter(Sender: TObject);
begin
  with TSpeedButton(Sender) do
  begin
    Font.Style := [fsUnderline];
    Cursor := crHandPoint;
  end;
end;

procedure TFormUserPerf.SpeedUserMouseLeave(Sender: TObject);
begin
  with TSpeedButton(Sender) do
  begin
    Font.Style := [];
    Cursor := crDefault;
  end;
end;

procedure TFormUserPerf.SBSairClick(Sender: TObject);
begin
  Close;
end;

procedure TFormUserPerf.SpeedLogClick(Sender: TObject);
begin
  if FrmFrame is TUCFrame_Log then
    Exit;

  if Assigned(FrmFrame) then
    FreeAndNil(FrmFrame);

  FrmFrame := TUCFrame_Log.Create(Self);
  LbDescricao.Caption := FUsercontrol.UserSettings.Log.LabelDescription;
  TUCFrame_Log(FrmFrame).FUsercontrol := FUsercontrol;
  TUCFrame_Log(FrmFrame).SetWindow;
  TUCFrame_Log(FrmFrame).Height := Panel3.Height;
  TUCFrame_Log(FrmFrame).Width := Panel3.Width;
  FrmFrame.Parent := Panel3;

end;

end.
