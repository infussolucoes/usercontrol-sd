{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usu�rios }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{                                                                              }
{               APLICA��O DE EXEMPLO - IBX CONECTOR                            }
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
unit UBanco;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.ExtCtrls, Vcl.DBCtrls,
  Vcl.Grids, Vcl.DBGrids;

type
  TFrmBanco = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DSBanco: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmBanco: TFrmBanco;

implementation

{$R *.dfm}

uses
  UDmUC;

procedure TFrmBanco.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if dmUC.IBTransaction1.InTransaction then
    dmUC.IBTransaction1.CommitRetaining;
end;

procedure TFrmBanco.FormCreate(Sender: TObject);
begin
  if not (dmUC.QryBanco.Active) then
    dmUC.QryBanco.Open;
end;

end.
