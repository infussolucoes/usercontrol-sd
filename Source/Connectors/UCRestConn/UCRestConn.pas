{ -----------------------------------------------------------------------------
  Unit Name: UCDataSnapConn
  Author   : Giovani Da Cruz
  Date     : 26/12/2015
  Purpose  : DataSnap Suporte
  E-mail   : giovani@infus.inf.br
  URL      : www.infus.inf.br
  UC SW    : http://infussolucoes.github.io/usercontrol-sd/

  registered in UCDataSnapConnReg.pas
  ----------------------------------------------------------------------------- }

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
{ **************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 26/12/2015: Giovani Da Cruz
  |*  - Criação e distribuição da Primeira deste connector
  |*
  **************************************************************************** }
unit UCRestConn;

interface

uses
  System.Classes,
  Data.DB, Datasnap.DSClientRest,
  System.SysUtils, UCRestProxy, Data.FireDACJSONReflect,
  UCDataConnector, FireDAC.Comp.Client;

type
  TUCRestConn = class(TUCDataConnector)
  private
    FInstanceOwner: Boolean;
    FSchema: String;
    FDSClient: TDSUserRemote;
    FConnection: TDSRestConnection;
    procedure SetConnection(const Value: TDSRestConnection);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDBObjectName: String; override;
    function GetTransObjectName: String; override;
    function UCFindDataConnection: Boolean; override;
    function UCFindTable(const Tablename: String): Boolean; override;
    function UCGetSQLDataset(FSQL: String): TDataset; override;
    procedure UCExecSQL(FSQL: String); override;
  published
    property Connection: TDSRestConnection read FConnection write SetConnection;
    property DSClient: TDSUserRemote read FDSClient write FDSClient;
    property SchemaName: String read FSchema write FSchema;
  end;

implementation

{ TUCRestConn }

constructor TUCRestConn.Create(AOwner: TComponent);
begin
  inherited;
  FInstanceOwner := True;
  FSchema := '';
end;

destructor TUCRestConn.Destroy;
begin

  inherited;
end;

function TUCRestConn.GetDBObjectName: String;
begin
  if Assigned(FConnection) then
  begin
    if Owner = FConnection.Owner then
      Result := FConnection.Name
    else
      Result := FConnection.Owner.Name + '.' + FConnection.Name;
  end
  else
    Result := '';
end;

function TUCRestConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCRestConn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TUCRestConn.SetConnection(const Value: TDSRestConnection);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TUCRestConn.UCExecSQL(FSQL: String);
begin
  inherited;
  DSClient.ExecuteSQL(FSQL);
end;

function TUCRestConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection);
end;

function TUCRestConn.UCFindTable(const Tablename: String): Boolean;
begin
  Result := True;//DSClient.FindTable(Tablename, Self.SchemaName);
end;

function TUCRestConn.UCGetSQLDataset(FSQL: String): TDataset;
var
  FList: TFDJSONDataSets;
begin
  FList := DSClient.GetDataSet2(FSQL, '');

  Result := TFDMemTable.Create(Self);
  Result.Close;

  Result.Fields.Clear;
  TFDMemTable(Result).AppendData(
  TFDJSONDataSetsReader.GetListValue(FList, 0));
end;

end.
