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
unit UPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, UCBase, UCDataConnector, UCFireDACConn, Vcl.ComCtrls,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls, UCRestDWCoreConn;

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
    lblUrlForum1: TLabel;
    lblUrlUserControl1: TLabel;
    Label19: TLabel;
    Label21: TLabel;
    Label20: TLabel;
    Label1: TLabel;
    Image1: TImage;
    Label3: TLabel;
    Label14: TLabel;
    StatusBar1: TStatusBar;
    UCRestDWCoreConn1: TUCRestDWCoreConn;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure Sair1Click(Sender: TObject);
    procedure GerarLog1Click(Sender: TObject);
    procedure Mensagens1Click(Sender: TObject);
    procedure Bancos1Click(Sender: TObject);
    procedure Clientes1Click(Sender: TObject);
    procedure Cidades1Click(Sender: TObject);
    procedure Produtos1Click(Sender: TObject);
    procedure URLClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private

  public
    { Public declarations }
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

uses
  ShellAPI,
  UDmUC, UBanco, UCliente;

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

procedure TFrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ucMyControl.Free;
end;

procedure TFrmPrincipal.GerarLog1Click(Sender: TObject);
begin
  ucMyControl.Log('Teste de log', 1);
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
