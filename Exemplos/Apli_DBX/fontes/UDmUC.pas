{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usu�rios }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{                                                                              }
{               APLICA��O DE EXEMPLO - DBX CONNECTOR                           }
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
unit UDmUC;

interface

uses
  SysUtils, Classes, DB, UCBase,
  UCDataConnector,
  Data.FMTBcd, Datasnap.DBClient,
  Datasnap.Provider, Data.SqlExpr, Data.DBXFirebird;

type
  TdmUC = class(TDataModule)
    SQLConnection1: TSQLConnection;
    SQLDataSetBancos: TSQLDataSet;
    ProviderBancos: TDataSetProvider;
    CDSBancos: TClientDataSet;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmUC: TdmUC;

implementation

uses
  Forms,
  IoUtils;

{$R *.dfm}

procedure TdmUC.DataModuleCreate(Sender: TObject);
var
  vFile : String;
begin
  vFile := TPath.GetFullPath(ExtractFilePath(Application.ExeName) + '..\');
  vFile := vFile + 'DBase\APLICACAO_UC.FDB';

  SQLConnection1.Close;
  SQLConnection1.Params.Text :=
    StringReplace( SQLConnection1.Params.Text, 'Database=database.fdb',
    'Database='+vFile, []);
  SQLConnection1.Open;
end;

end.
