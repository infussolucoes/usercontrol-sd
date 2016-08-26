{-----------------------------------------------------------------------------
 Unit Name: UCIBXConn
 Author:    QmD
 Date:      08-nov-2004
 Purpose:   IBX Support

 registered in UCReg.pas
-----------------------------------------------------------------------------}
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
unit UCIBXConn;

interface

uses
  Classes,
  DB,
  IBDataBase,
  IBQuery,
  SysUtils,
  UCDataConnector;

type
  TUCIBXConn = class(TUCDataConnector)
  private
    FConnection:  TIBDatabase;
    FTransaction: TIBTransaction;
    procedure SetTransaction(const Value: TIBTransaction);
    procedure SetConnection(const Value: TIBDatabase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function GetDBObjectName: String; override;
    function GetTransObjectName: String; override;
    function UCFindDataConnection: Boolean; override;
    function UCFindTable(const Tablename: String): Boolean; override;
    function UCGetSQLDataset(FSQL: String): TDataset; override;
    procedure UCExecSQL(FSQL: String); override;
  published
    property Connection: TIBDatabase read FConnection write SetConnection;
    property Transaction: TIBTransaction read FTransaction write SetTransaction;
  end;

implementation

{ TUCIBXConn }

procedure TUCIBXConn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
  if (Operation = opRemove) and (AComponent = FTransaction) then
    FTransaction := nil;
  inherited Notification(AComponent, Operation);
end;

function TUCIBXConn.UCFindTable(const TableName: String): Boolean;
var
  TempList: TStringList;
begin
  TempList := TStringList.Create;
  try
    FConnection.GetTableNames(TempList, False);
    TempList.Text := UpperCase(TempList.Text);
    Result        := TempList.IndexOf(UpperCase(TableName)) > -1;
  finally
    SysUtils.FreeAndNil(TempList);
  end;
end;

function TUCIBXConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCIBXConn.GetDBObjectName: String;
begin
  if Assigned(FConnection) then
  begin
    if Owner = FConnection.Owner then
      Result := FConnection.Name
    else
    begin
      Result := FConnection.Owner.Name + '.' + FConnection.Name;
    end;
  end
  else
    Result := '';
end;

function TUCIBXConn.GetTransObjectName: String;
begin
  if Assigned(FTransaction) then
  begin
    if Owner = FTransaction.Owner then
      Result := FTransaction.Name
    else
    begin
      Result := FTransaction.Owner.Name + '.' + FTransaction.Name;
    end;
  end
  else
    Result := '';
end;

procedure TUCIBXConn.UCExecSQL(FSQL: String);
var
  Qry: TIBQuery;
begin
  Qry := TIBQuery.Create(nil);
  try
    Qry.Database    := FConnection;
    Qry.Transaction := FTransaction;
    if not Qry.Transaction.Active then
      Qry.Transaction.Active := True;
    Qry.SQL.Text := FSQL;
    Qry.ExecSQL;
    Qry.Transaction.Commit;
  finally
    Qry.Free;
  end;
end;

function TUCIBXConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TIBQuery.Create(nil);
  with Result as TIBQuery do
  begin
    Database    := FConnection;
    Transaction := FTransaction;
    SQL.Text    := FSQL;
    Open;
  end;
end;


procedure TUCIBXConn.SetTransaction(const Value: TIBTransaction);
begin
  FTransaction := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TUCIBXConn.SetConnection(const Value: TIBDatabase);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

end.

