unit UCUIBConn;

interface

uses
  Classes,
  DB,
  jvuib,
  jvuibdataset,
  jvuibmetadata,
  UCDataConnector;

type
  TUCUIBConn = class(TUCDataConnector)
  private
    FConnection:  TJvUIBDataBase;
    FTransaction: TJvUIBTransaction;
    procedure SetConnection(const Value: TJvUIBDataBase);
    procedure SetTransaction(const Value: TJvUIBTransaction);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure UCExecSQL(FSQL: String); override;
    function UCGetSQLDataset(FSQL: String): TDataset; override;
    function UCFindTable(const Tablename: String): Boolean; override;
    function UCFindDataConnection: Boolean; override;
    function GetDBObjectName: String; override;
    function GetTransObjectName: String; override;
  published
    property Connection: TJvUIBDataBase read FConnection write SetConnection;
    property Transaction: TJvUIBTransaction read FTransaction write SetTransaction;
  end;

implementation

uses
  SysUtils;

{ TUCUIBConn }

function TUCUIBConn.GetDBObjectName: String;
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

function TUCUIBConn.GetTransObjectName: String;
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

procedure TUCUIBConn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
  if (Operation = opRemove) and (AComponent = FTransaction) then
    FTransaction := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TUCUIBConn.SetConnection(const Value: TJvUIBDataBase);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TUCUIBConn.SetTransaction(const Value: TJvUIBTransaction);
begin
  FTransaction := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TUCUIBConn.UCExecSQL(FSQL: String);
var
  Query: TJvUIBQuery;
begin
  try
    Query             := TJvUIBQuery.Create(nil);
    Query.DataBase    := FConnection;
    Query.Transaction := FTransaction;
    Query.SQL.Text    := FSQL;
    FTransaction.StartTransaction;
    Query.ExecSQL;
    FTransaction.Commit;
  finally
    SysUtils.FreeAndNil(Query);
  end;
end;

function TUCUIBConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCUIBConn.UCFindTable(const Tablename: String): Boolean;
var
  MetaData: TMetaDataBase;
  Table:    TMetaTable;
begin
  Result   := False;
  MetaData := TMetaDataBase(FConnection.GetMetadata(True));
  Table    := MetaData.FindTableName(Tablename);
  if Assigned(Table) then
    Result := True;
end;

function TUCUIBConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  try
    Result := TJvUIBDataSet.Create(nil);
    with (Result as TJvUIBDataSet) do
    begin
      DataBase    := FConnection;
      Transaction := FTransaction;
      SQL.Text    := FSQL;
      Open;
    end;
  except
    SysUtils.FreeAndNil(Result);
  end;
end;

end.

