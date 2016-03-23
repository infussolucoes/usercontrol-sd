{-----------------------------------------------------------------------------
 Unit Name: UCMDOConn
 Author:    Alexandre Oliveira - 05/11/2004
 Change:    QmD
 Date:      10-nov-2004
 Purpose:   MDO Support

 registered in UCMDOReg.pas
-----------------------------------------------------------------------------}

unit UCMDOConn;

interface

uses
  Classes,
  DB,
  MDODatabase,
  MDOQuery,
  SysUtils,
  UCDataConnector;

type
  TUCMDOConn = class(TUCDataConnector)
  private
    FConnection:  TMDODatabase;
    FTransaction: TMDOTransaction;
    procedure SetFTransaction(const Value: TMDOTransaction);
    procedure SetMDOConnection(const Value: TMDODatabase);
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
    property Connection: TMDODatabase read FConnection write SetMDOConnection;
    property Transaction: TMDOTransaction read FTransaction write SetFTransaction;
  end;

implementation

{ TUCMDOConn }

procedure TUCMDOConn.SetFTransaction(const Value: TMDOTransaction);
begin
  FTransaction := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TUCMDOConn.SetMDOConnection(const Value: TMDODatabase);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TUCMDOConn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
  if (Operation = opRemove) and (AComponent = FTransaction) then
    FTransaction := nil;
  inherited Notification(AComponent, Operation);
end;

function TUCMDOConn.UCFindTable(const TableName: String): Boolean;
var
  TempList: TStringList;
begin
  try
    TempList := TStringList.Create;
    FConnection.GetTableNames(TempList, False);
    TempList.Text := UpperCase(TempList.Text);
    Result        := TempList.IndexOf(UpperCase(TableName)) > -1;
  finally
    FreeAndNil(TempList);
  end;
end;

function TUCMDOConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCMDOConn.GetDBObjectName: String;
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

function TUCMDOConn.GetTransObjectName: String;
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

procedure TUCMDOConn.UCExecSQL(FSQL: String);
begin
  with TMDOQuery.Create(nil) do
  begin
    Database    := FConnection;
    Transaction := FTransaction;
    if not Transaction.Active then
      Transaction.Active := True;
    SQL.Text := FSQL;
    ExecSQL;
    FTransaction.Commit;
    Free;
  end;
end;

function TUCMDOConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TMDOQuery.Create(nil);
  with Result as TMDOQuery do
  begin
    Database    := FConnection;
    Transaction := FTransaction;
    SQL.Text    := FSQL;
    Open;
  end;
end;

end.

