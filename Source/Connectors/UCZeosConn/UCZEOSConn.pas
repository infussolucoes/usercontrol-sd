{-----------------------------------------------------------------------------
 Unit Name: UCZEOSConn
 Author:    QmD
 Date:      08-nov-2004
 Purpose:   ZEOS 6 Support

 registered in UCZEOSReg.pas
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

unit UCZEOSConn;

interface

uses
  Classes,
  DB,
  SysUtils,
  UCDataConnector,
  ZConnection;

type
  TUCZEOSConn = class(TUCDataConnector)
  private
    FConnection: TZConnection;
    procedure SetFConnection(const Value: TZConnection);
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
    property Connection: TZConnection read FConnection write SetFConnection;
  end;

implementation

uses
  ZDataset, Dialogs;

{ TUCZEOSConn }

procedure TUCZEOSConn.SetFConnection(const Value: TZConnection);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TUCZEOSConn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
  inherited Notification(AComponent, Operation);
end;

function TUCZEOSConn.UCFindTable(const TableName: String): Boolean;
var
  TempList: TStringList;
begin
  try
    TempList := TStringList.Create;
    FConnection.GetTableNames('', TempList);
    TempList.Text := UpperCase(TempList.Text);
    Result        := TempList.IndexOf(UpperCase(TableName)) > -1;
  Except
    Result        := false;
  end;
  FreeAndNil(TempList);  
end;

function TUCZEOSConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCZEOSConn.GetDBObjectName: String;
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

function TUCZEOSConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCZEOSConn.UCExecSQL(FSQL: String);
begin
  with TZQuery.Create(nil) do
  begin
    Connection := FConnection;
    SQL.Text   := FSQL;
    try ExecSQL; except end;
    If FConnection.AutoCommit = False then
      FConnection.Commit;
    Free;
  end;
end;

function TUCZEOSConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TZQuery.Create(nil);
  with Result as TZQuery do
  begin
    Connection := FConnection;
    SQL.Text   := FSQL;
    Open;
  end;
end;

end.

