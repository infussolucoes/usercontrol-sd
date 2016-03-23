{-----------------------------------------------------------------------------
 Unit Name: UCOADCCon
 Author:    QmD
 Date:      23-jun-2007
 Purpose:   ODAC Support

 registered in UCODACReg.pas
-----------------------------------------------------------------------------}

unit UCODACConn;

interface

uses
  SysUtils, Classes, UCBase, DB, DBAccess, Ora;

type
  TUCODACConn = class(TUCDataConn)
  private
    FConnection : TOraSession;
    procedure SetFConnection(Value : TOraSession);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    function GetDBObjectName : String; override;
    function GetTransObjectName : String; override;
    function UCFindDataConnection : Boolean; override;
    function UCFindTable(const Tablename : String) : Boolean; override;
    function UCGetSQLDataset(FSQL : String) : TDataset;override;
    procedure UCExecSQL(FSQL: String);override;
  published
    property Connection : TOraSession read FConnection write SetFConnection;
  end;

implementation

{ TUCODACConn }

procedure TUCODACConn.SetFConnection(Value: TOraSession);
begin
  if FConnection <> Value then FConnection := Value;
  if FConnection <> nil then FConnection.FreeNotification(Self);
end;

procedure TUCODACConn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
  begin
    FConnection := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

function TUCODACConn.UCFindTable(const TableName: String): Boolean;
var
  TempList : TStringList;
begin
  try
    TempList := TStringList.Create;
    FConnection.GetTableNames(TempList);
    TempList.Text := UpperCase(TempList.Text);
    Result := TempList.IndexOf(UpperCase(TableName)) > -1;
  finally
    FreeAndNil(TempList);
  end;
end;

function TUCODACConn.UCFindDataConnection: Boolean;
begin
    Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCODACConn.GetDBObjectName: String;
begin
  if Assigned(FConnection) then
  begin
    if Owner = FConnection.Owner then Result := FConnection.Name
    else begin
      Result := FConnection.Owner.Name+'.'+FConnection.Name;
    end;
  end else Result := '';
end;

function TUCODACConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCODACConn.UCExecSQL(FSQL: String);
Var Query : TOraSql;
begin
  Try
    Query := TOraSql.create(self);
    With Query as TOraSql do
      Begin
        Connecton := FConnection;
        Sql.Add( fSql );
        Execute;
        Connecton.Commit;
      End;
  Finally
    FreeAndNil( Query );
  End;
end;

function TUCODACConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TOraQuery.Create(nil);
  with Result as TOraQuery do
  begin
    Session  := FConnection;
    SQL.Clear;
    Sql.Add(FSQL);
    Open;
  end;
end;

end.
