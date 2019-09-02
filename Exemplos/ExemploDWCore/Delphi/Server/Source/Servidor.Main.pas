{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usuários }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{                                                                              }
{             APLICAÇÃO DE EXEMPLO - REST DATAWARE CORE CONECTOR               }
{******************************************************************************}
{ Versão ShowDelphi Edition                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2019   Giovani Da Cruz                      }
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
{ Comunidade Show Delphi - showdelphi.com.br                                   }
{                                                                              }
{ Giovani Da Cruz  -  giovani@infus.inf.br  -  www.infus.inf.br                }
{                                                                              }
{ **************************************************************************** }

{ AJUDE O PROJETO COM UMA XÍCARA DE CAFÉ OU DUAS. CONSIDERE UMA DOAÇÃO!        }
{                                                                              }
{ VIA PAGSEGURO: https://pag.ae/7VccpnuCN                                      }
{ APOIE COM BITCOIN: 13JUHQpT7zAU7pC1q6cQBYGpq5EF8XoLcL                        }
{
{ *****************************************************************************}
unit Servidor.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, uRESTDWPoolerDB, FireDAC.Phys.IBBase,
  FireDAC.Stan.StorageJSON, Data.DB, FireDAC.Comp.Client, uRestDWDriverFD,
  uDWAbout, uRESTDWBase, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask, Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage;

type
  TFrmMain = class(TForm)
    RESTServicePooler1: TRESTServicePooler;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    Panel2: TPanel;
    ButtonStart: TButton;
    ButtonStop: TButton;
    lSeguro: TLabel;
    cbPoolerState: TCheckBox;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    labPorta: TLabel;
    labUsuario: TLabel;
    labSenha: TLabel;
    lbPasta: TLabel;
    labNomeBD: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    labConexao: TLabel;
    labDBConfig: TLabel;
    labVersao: TLabel;
    Panel3: TPanel;
    Image7: TImage;
    Panel4: TPanel;
    Image8: TImage;
    edPortaDW: TEdit;
    edUserNameDW: TEdit;
    edPasswordDW: TEdit;
    edPortaBD: TEdit;
    edUserNameBD: TEdit;
    edPasswordBD: TEdit;
    edPasta: TEdit;
    edBD: TEdit;
    edURL: TEdit;
    Panel5: TPanel;
    lblUrlUserControl1: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Label7: TLabel;
    Label14: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure URLClick(Sender: TObject);
  private
    procedure StartServer;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses
  IOUtils, ShellAPI,

  uDmService;

procedure TFrmMain.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TFrmMain.ButtonStopClick(Sender: TObject);
begin
  RESTServicePooler1.Active := False;
  ServerMethodDM.Server_FDConnection.Close;

  cbPoolerState.Checked := RESTServicePooler1.Active;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  edPasta.Text :=
    IncludeTrailingPathDelimiter(
    TPath.GetFullPath(
      ExtractFilePath(Application.ExeName) + '..\..\db'));

  RESTServicePooler1.ServerMethodClass := TServerMethodDM;
end;

procedure TFrmMain.StartServer;
begin
  ServerMethodDM.Server_FDConnection.Close;

  ServerMethodDM.Server_FDConnection.Params.Database := edPasta.Text + '\' + edBD.Text;
  ServerMethodDM.Server_FDConnection.Params.UserName := edUserNameBD.Text;
  ServerMethodDM.Server_FDConnection.Params.Password := edPasswordBD.Text;
  ServerMethodDM.Server_FDConnection.Params.Values['Server'] := '127.0.0.1';
  ServerMethodDM.Server_FDConnection.Params.Values['Port'] := '3050';
  ServerMethodDM.Server_FDConnection.Params.SaveToFile(
    IncludeTrailingPathDelimiter(
    TPath.GetFullPath(
      ExtractFilePath(Application.ExeName) + '..\')) + 'config.ini'
  );

  if not RESTServicePooler1.Active then
  begin
    RESTServicePooler1.ServerParams.HasAuthentication := True;
    RESTServicePooler1.ServerParams.UserName := EdUserNameDW.Text;
    RESTServicePooler1.ServerParams.Password := EdPasswordDW.Text;
    RESTServicePooler1.ServicePort           := StrToInt(EdPortaDW.Text);
    RESTServicePooler1.SSLPrivateKeyFile     := '';
    RESTServicePooler1.SSLPrivateKeyPassword := '';
    RESTServicePooler1.SSLCertFile           := '';
    RESTServicePooler1.ForceWelcomeAccess    := False;
    RESTServicePooler1.Active                := True;

    cbPoolerState.Checked := RESTServicePooler1.Active;

    if Not RESTServicePooler1.Active then
      Exit;
  End;

  if RESTServicePooler1.Secure then
  Begin
    LSeguro.Font.Color := ClBlue;
    LSeguro.Caption    := 'SSL : Sim';
  end
  else
  begin
    LSeguro.Font.Color := ClRed;
    LSeguro.Caption    := 'SSL : Não';
  end;
end;

procedure TFrmMain.URLClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(TLabel(Sender).Caption), '', '', 1);
end;

end.
