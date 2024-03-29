{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usu�rios }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{                                                                              }
{               APLICA��O DE EXEMPLO - ZEOS CONNECTOR                          }
{******************************************************************************}
{ Vers�o ShowDelphi Edition                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 - Giovani Da Cruz                      }
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
{ Comunidade Show Delphi - showdelphi.com.br                                   }
{                                                                              }
{ Blog do Giovani - giovanidacruz.com.br                                       }
{                                                                              }
{ Giovani Da Cruz  -  giovani@infus.inf.br  -  www.infus.inf.br                }
{                                                                              }
{ **************************************************************************** }
{                                                                              }
{ AJUDE O PROJETO COM UMA X�CARA DE CAF�!                                      }
{                                                                              }
{ Doe no PIX (chave aleat�ria): 5943007d-4332-4e5c-ac66-06486a10cbfb           }
{                                                                              }
{ *****************************************************************************}
unit UPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, UCBase, UCDataConnector, UCZEOSConn, UCSettings, Vcl.ComCtrls,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFrmPrincipal = class(TForm)
    MainMenu1: TMainMenu;
    Cadastro1: TMenuItem;
    Bancos1: TMenuItem;
    Clientes1: TMenuItem;
    Cidades1: TMenuItem;
    Produtos1: TMenuItem;
    N1: TMenuItem;
    Sair1: TMenuItem;
    Seguranca1: TMenuItem;
    CadastrodeUsurios1: TMenuItem;
    Mensagens1: TMenuItem;
    N2: TMenuItem;
    rocarSenha1: TMenuItem;
    N3: TMenuItem;
    Executarlogon1: TMenuItem;
    ucMyControl: TUserControl;
    estes1: TMenuItem;
    GerarLog1: TMenuItem;
    UCApplicationMessage1: TUCApplicationMessage;
    UCZEOSConn1: TUCZEOSConn;
    lblUrlForum1: TLabel;
    lblUrlPIX: TLabel;
    Label19: TLabel;
    Label21: TLabel;
    Label20: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Image1: TImage;
    Label2: TLabel;
    Label4: TLabel;
    StatusBar1: TStatusBar;
    UCSettings1: TUCSettings;
    Label5: TLabel;
    procedure Sair1Click(Sender: TObject);
    procedure GerarLog1Click(Sender: TObject);
    procedure Mensagens1Click(Sender: TObject);
    procedure Bancos1Click(Sender: TObject);
    procedure Clientes1Click(Sender: TObject);
    procedure Cidades1Click(Sender: TObject);
    procedure Produtos1Click(Sender: TObject);
    procedure URLClick(Sender: TObject);
    procedure lblUrlPIXClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

uses
  ShellAPI,
  Clipbrd,

  { Unit da aplica��o }
  UDmUC,
  UBanco,
  UCliente;

{$R *.dfm}

procedure TFrmPrincipal.Bancos1Click(Sender: TObject);
begin
  Application.CreateForm(TFrmBanco, FrmBanco);
  FrmBanco.ShowModal;
end;

procedure TFrmPrincipal.Cidades1Click(Sender: TObject);
begin
  ShowMessage('Cidade');
end;

procedure TFrmPrincipal.Clientes1Click(Sender: TObject);
begin
  Application.CreateForm(TFrmCliente, FrmCliente);
  FrmCliente.ShowModal;
end;

procedure TFrmPrincipal.GerarLog1Click(Sender: TObject);
begin
  ucMyControl.Log('Teste de log', 1);
end;

procedure TFrmPrincipal.lblUrlPIXClick(Sender: TObject);
begin
  Clipboard.asText := lblUrlPIX.Caption;

  Application.MessageBox('Chave copiada para a �rea de transfer�ncia!',
    'Apoio ao Projeto', MB_ICONINFORMATION + MB_OK);
end;

procedure TFrmPrincipal.Mensagens1Click(Sender: TObject);
begin
  UCApplicationMessage1.CheckMessages;
  UCApplicationMessage1.ShowMessages();
end;

procedure TFrmPrincipal.Produtos1Click(Sender: TObject);
begin
  ShowMessage('Produtos');
end;

procedure TFrmPrincipal.Sair1Click(Sender: TObject);
begin
  Close;
end;

procedure TFrmPrincipal.URLClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(TLabel(Sender).Caption), '', '', 1);
end;

end.
