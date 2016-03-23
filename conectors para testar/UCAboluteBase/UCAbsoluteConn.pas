{-----------------------------------------------------------------------------
 Unit Name: UCZEOSConn
 Author:    Vicente Barros Leonel
 Date:      29-outubro-2007
 Purpose:   Absolute Database

 registered in UCAbsoluteConn.pas
-----------------------------------------------------------------------------}

unit UCAbsoluteConn;

interface

{$I 'UserControl.inc'}

uses
  Classes,
  DB,
  SysUtils,
  UCDataConnector,
  ABSMain;

type
  TUCAbsoluteConn = class(TUCDataConnector)
  private
    FConnection: TABSDatabase;
    procedure SetFConnection(const Value: TABSDatabase);
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
    property Connection: TABSDatabase read FConnection write SetFConnection;
  end;
implementation

{ TUCAbsoluteConn }

function TUCAbsoluteConn.GetDBObjectName: String;
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

function TUCAbsoluteConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCAbsoluteConn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TUCAbsoluteConn.SetFConnection(const Value: TABSDatabase);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TUCAbsoluteConn.UCExecSQL(FSQL: String);
begin
  with TABSQuery.Create(nil) do
  begin
    DataBaseName := FConnection.Name;
    SQL.Clear;
    Sql.Add(fSql);
    ExecSQL;
    Free;
  end;
end;

function TUCAbsoluteConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCAbsoluteConn.UCFindTable(const Tablename: String): Boolean;
var
  TempList: TStringList;
begin
  try
    TempList := TStringList.Create;
    FConnection.GetTablesList(TempList);
    TempList.Text := UpperCase(TempList.Text);
    Result        := TempList.IndexOf(UpperCase(TableName)) > -1;
  finally
    FreeAndNil(TempList);
  end;
end;

function TUCAbsoluteConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TABSQuery.Create(nil);
  with Result as TABSQuery do
  begin
    DataBaseName := FConnection.Name;
    SQL.Clear;
    Sql.Add(fSql);
    Open;
  end;
end;

end.
