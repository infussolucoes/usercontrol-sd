{-----------------------------------------------------------------------------
 Unit Name: UCBDEConn
 Author:    QmD
 Date:      10-nov-2004
 Purpose:   BDE Support

 registered in UCReg.pas
-----------------------------------------------------------------------------}

unit UCBDEConn;

interface

{$I 'UserControl.inc'}

uses
  Classes,
  DB,
  DBTables,
  SysUtils,
  UCDataConnector;

type
  TUCBDEConn = class(TUCDataConnector)
  private
    FConnection: TDatabase;
    procedure SetFDatabase(Value: TDatabase);
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
    property Connection: TDatabase read FConnection write SetFDatabase;
  end;

implementation

{ TUCBDEConn }

procedure TUCBDEConn.SetFDatabase(Value: TDatabase);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TUCBDEConn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
  inherited Notification(AComponent, Operation);
end;

function TUCBDEConn.UCFindTable(const TableName: String): Boolean;
var
  Lista: TStringList;
begin
  Lista := TStringList.Create;
{$IFDEF DELPHI5}
  FConnection.Session.GetTableNames(FDatabase.Databasename,'*.db*', False, False, Lista);
{$ELSE}
  FConnection.GetTableNames(Lista);
{$ENDIF}
  Result := Lista.IndexOf(TableName) > -1;
  FreeAndNil(Lista);
end;

function TUCBDEConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCBDEConn.GetDBObjectName: String;
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

function TUCBDEConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCBDEConn.UCExecSQL(FSQL: String);
begin
  with TQuery.Create(nil) do
  begin
    DatabaseName := FConnection.DatabaseName;
    SQL.Text     := FSQL;
    ExecSQL;
    Free;
  end;

end;

function TUCBDEConn.UCGetSQLDataset(FSQL: String): TDataset;
var
  TempQuery: TQuery;
begin
  TempQuery := TQuery.Create(nil);

  with TempQuery do
  begin
    DatabaseName := FConnection.DatabaseName;
    SQL.Text     := FSQL;
    Open;
  end;
  Result := TempQuery;
end;

end.

