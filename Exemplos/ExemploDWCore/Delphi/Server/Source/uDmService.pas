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
unit uDmService;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, uRESTDWPoolerDB, uDWAbout,
  uRestDWDriverFD, Data.DB, FireDAC.Comp.Client, uDWDatamodule;

type
  TServerMethodDM = class(TServerMethodDataModule)
    Server_FDConnection: TFDConnection;
    RESTDWDriverFD1: TRESTDWDriverFD;
    RESTDWPoolerFD: TRESTDWPoolerDB;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ServerMethodDM: TServerMethodDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  IOUtils, Forms;

procedure TServerMethodDM.DataModuleCreate(Sender: TObject);
var
  vFile : TFileName;
begin
  Server_FDConnection.Close;

  vFile :=
    IncludeTrailingPathDelimiter(
    TPath.GetFullPath(
      ExtractFilePath(Application.ExeName) + '..\')) + 'config.ini';

  { Na primeira vez que rodar o servidor, o arquivo ainda não existe. }
  if FileExists(vFile) then
  begin
    Server_FDConnection.Params.LoadFromFile(vFile);
  end;

  RESTDWPoolerFD.Active  := True;
end;

end.
