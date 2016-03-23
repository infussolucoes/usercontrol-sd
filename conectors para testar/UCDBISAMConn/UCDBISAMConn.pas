{-----------------------------------------------------------------------------
 Unit Name: UCDBISAMConn
 Changed:   Carlos Guerra 
 Date:      01-dez-2004
 Author:    QmD
 Purpose:   DBISAM Support

 registered in UCDBISAMReg.pas
-----------------------------------------------------------------------------}

unit UCDBISAMConn;

interface

uses
  SysUtils, Classes, UCBase, DB, dbisamtb;

type
  TUCDBISAMConn = class(TUCDataConn)
  private
    FConnection : TDBISAMDatabase;
    FSessionName: TDBISAMSession; //Carlos Guerra 12/01/2004
    procedure SetFDatabase(Value : TDBISAMDatabase);
    procedure SetFSessionName(const Value: TDBISAMSession); //Carlos Guerra 12/01/2004
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    function GetDBObjectName : String; override;
//    function GetTransObjectName : String; override;
    function UCFindDataConnection : Boolean; override;
    function UCFindTable(const Tablename : String) : Boolean; override;
    function UCGetSQLDataset(FSQL : String) : TDataset;override;
    procedure UCExecSQL(FSQL: String);override;
  published
    property Connection : TDBISAMDatabase read FConnection write SetFDatabase;
    property SessionName : TDBISAMSession read FSessionName write SetFSessionName; //Carlos Guerra 12/01/2004
  end;

implementation

{ TUCDBISAMConn }

procedure TUCDBISAMConn.SetFDatabase(Value: TDBISAMDatabase);
begin
  if FConnection <> Value then FConnection := Value;
  if FConnection <> nil then FConnection.FreeNotification(Self);
end;

procedure TUCDBISAMConn.SetFSessionName(const Value: TDBISAMSession);
begin
  FSessionName := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;


procedure TUCDBISAMConn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
  begin
    FConnection := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

function TUCDBISAMConn.UCFindTable(const TableName: String): Boolean;
var
  Lista : TStringList;
begin
  Lista := TStringList.Create;

  FConnection.Session.GetTableNames(FConnection.RemoteDatabase,Lista);
  Result := Lista.IndexOf(TableName) > -1;
  FreeAndNil(Lista);
end;

function TUCDBISAMConn.UCFindDataConnection: Boolean;
begin
    Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCDBISAMConn.GetDBObjectName: String;
begin
  if Assigned(FConnection) then
  begin
    if Owner = FConnection.Owner then Result := FConnection.Name
    else begin
      Result := FConnection.Owner.Name+'.'+FConnection.Name;
    end;
  end else Result := '';
end;

{function TUCDBISAMConn.GetTransObjectName: String;
begin
  Result := '';
end;}

procedure TUCDBISAMConn.UCExecSQL(FSQL: String);
begin
  with TDBISAMQuery.Create(nil) do
  begin
    //DatabaseName := FConnection.DatabaseName; //Cancel by Carlos Guerra 12/01/2004
    DatabaseName := FConnection.RemoteDatabase; //Carlos Guerra 12/01/2004
    SessionName := FConnection.SessionName; //Carlos Guerra 12/01/2004
    SQL.text := FSQL;
    ExecSQL;
    Free;
  end;

end;

function TUCDBISAMConn.UCGetSQLDataset(FSQL: String): TDataset;
var
  TempQuery : TDBISAMQuery;
begin
  TempQuery := TDBISAMQuery.Create(nil);
  TempQuery.SessionName := FConnection.SessionName; //Carlos Guerra 12/01/2004
  TempQuery.DatabaseName := FConnection.RemoteDatabase; //Carlos Guerra 12/01/2004
  with TempQuery do
  begin
    //DatabaseName := FConnection.DatabaseName; //Cancel by Carlos Guerra 12/01/2004
    SQL.text := FSQL;
    Open;
  end;
  Result := TempQuery;
end;

end.
