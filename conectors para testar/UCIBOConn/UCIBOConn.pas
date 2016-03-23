{-----------------------------------------------------------------------------
 Unit Name: UCIBOConn
 Author:    QmD
 Date:      22-nov-2004
 Purpose:   IBO Dataset Support

 registered in UCIBOReg.pas
-----------------------------------------------------------------------------}

unit UCIBOConn;

interface

uses
  Classes,
  DB,
  IB_Components,
  IBODataset,
  SysUtils,
  UCDataConnector;

type
  TUCIBOConn = class(TUCDataConnector)
  private
    FConnection: TIBODatabase;
    procedure SetIBOConnection(const Value: TIBODatabase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function GetDBObjectName: String; override;
    function UCFindDataConnection: Boolean; override;
    function UCFindTable(const Tablename: String): Boolean; override;
    function UCGetSQLDataset(FSQL: String): TDataset; override;
    procedure UCExecSQL(FSQL: String); override;
  published
    property Connection: TIBODatabase read FConnection write SetIBOConnection;
  end;

implementation

{ TUCIBOConn }


procedure TUCIBOConn.SetIBOConnection(const Value: TIBODatabase);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TUCIBOConn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
  inherited Notification(AComponent, Operation);
end;

function TUCIBOConn.UCFindTable(const TableName: String): Boolean;
begin
  with TIBOQuery.Create(nil) do
  begin
    IB_Connection := FConnection;
    SQL.Text      := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS WHERE RDB$SYSTEM_FLAG = 0 and RDB$RELATION_NAME = ' + QuotedStr(Uppercase(TableName));
    Open;
    Result := FieldByName('RDB$RELATION_NAME').AsString = Uppercase(TableName);
    Close;
    Free;
  end;
end;

function TUCIBOConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCIBOConn.GetDBObjectName: String;
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


procedure TUCIBOConn.UCExecSQL(FSQL: String);
begin
  FConnection.DefaultTransaction.ExecuteImmediate(FSQL);
  FConnection.DefaultTransaction.Commit;
end;

function TUCIBOConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TIBOQuery.Create(nil);
  with Result as TIBOQuery do
  begin
    IB_Connection := FConnection;
    SQL.Text      := FSQL;
    Open;
  end;
end;

end.

