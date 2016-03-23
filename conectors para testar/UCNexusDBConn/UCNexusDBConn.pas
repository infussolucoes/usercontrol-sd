{Connector para NexusDB v2.xx adaptado por WanX}

unit UCNexusDBConn;

interface

{.$I 'UserControl.inc'}

uses
  SysUtils, Classes, DB, UCDataConnector, nxdb, nxsdFmtBCD;

type
  TUCNexusDBConnector = class(TUCDataConnector)
  private
    FConnection: TnxDatabase;
    procedure SetConnection(const Value: TnxDatabase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    procedure UCExecSQL(FSQL: String); override;
    function UCGetSQLDataset(FSQL: String): TDataset; override;
    function UCFindTable(const Tablename: String): Boolean; override;
    function UCFindDataConnection: Boolean; override;
    function GetDBObjectName: String; override;
  published
    property Connection: TnxDatabase read FConnection write SetConnection;
  end;

implementation

{ TUCNexusDBConnector }

function TUCNexusDBConnector.GetDBObjectName: String;
begin
 if Assigned(FConnection) then
  begin
    if Owner = FConnection.Owner then Result := FConnection.Name
    else begin
      Result := FConnection.Owner.Name+'.'+FConnection.Name;
    end;
  end else Result := '';
end;

procedure TUCNexusDBConnector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
 if ((Operation = opRemove) and (AComponent = FConnection)) then
  FConnection := nil;
 inherited Notification(AComponent, Operation);
end;

procedure TUCNexusDBConnector.SetConnection(const Value: TnxDatabase);
begin
  if FConnection <> Value then FConnection := Value;
  if FConnection <> nil then FConnection.FreeNotification(Self);
end;

procedure TUCNexusDBConnector.UCExecSQL(FSQL: String);
var
 TempQuery: TnxQuery;
begin
 TempQuery:= TnxQuery.Create(nil);
 try
  TempQuery.Database:= FConnection;
  TempQuery.SQL.text := FSQL;
  TempQuery.ExecSQL;
 finally
  FreeAndNil(TempQuery);
 end;
end;

function TUCNexusDBConnector.UCFindDataConnection: Boolean;
begin
 Result := (Assigned(FConnection) and (FConnection.Connected));
end;

function TUCNexusDBConnector.UCFindTable(const Tablename: String): Boolean;
var
 Lista : TStringList;
begin
 Lista := TStringList.Create;
 try
  FConnection.GetTableNames(Lista);
  Result := Lista.IndexOf(TableName) > -1;
 finally
  FreeAndNil(Lista);
 end;
end;

function TUCNexusDBConnector.UCGetSQLDataset(FSQL: String): TDataset;
var
  TempQuery : TnxQuery;
begin
 TempQuery := TnxQuery.Create(nil);
 TempQuery.Database:= FConnection;
 with TempQuery do
  begin
   SQL.text := FSQL;
   Open;
  end;
 Result := TempQuery;
end;

end.
