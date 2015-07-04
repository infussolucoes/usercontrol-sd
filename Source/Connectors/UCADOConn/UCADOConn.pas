{-----------------------------------------------------------------------------
 Unit Name: UCADOConn
 Author:    QmD
 Date:      08-nov-2004
 Purpose:   ADO Support

 registered in UCReg.pas
-----------------------------------------------------------------------------}

unit UCADOConn;

interface

uses
  ADODB,
  Classes,
  DB,
  SysUtils,
  UCDataConnector;

type
  TUCADOConn = class(TUCDataConnector)
  private
    FConnection: TADOConnection;
    procedure SetADOConnection(Value: TADOConnection);
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
    property Connection: TADOConnection read FConnection write SetADOConnection;
  end;

implementation

{ TUCADOConn }

procedure TUCADOConn.SetADOConnection(Value: TADOConnection);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TUCADOConn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
  inherited Notification(AComponent, Operation);
end;

function TUCADOConn.UCFindTable(const TableName: String): Boolean;
var
  TempList: TStringList;
begin
  try
    TempList := TStringList.Create;
    FConnection.GetTableNames(TempList, True);
    TempList.Text := UpperCase(TempList.Text);
    Result        := TempList.IndexOf(UpperCase(TableName)) > -1;
  finally
    FreeAndNil(TempList);
  end;
end;

function TUCADOConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCADOConn.GetDBObjectName: String;
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

function TUCADOConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCADOConn.UCExecSQL(FSQL: String);
begin
  FConnection.Execute(FSQL);
end;

function TUCADOConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TADOQuery.Create(nil);
  with Result as TADOQuery do
  begin
    Connection := FConnection;
    SQL.Text   := FSQL;
    Open;
  end;
end;

end.

