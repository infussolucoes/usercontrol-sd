{-----------------------------------------------------------------------------
 Unit Name: UCUniDACConn
 Author:    QmD
 Date:      22-nov-2004
 Purpose:   UniDAC Support

 registered in UCUniDACReg.pas
-----------------------------------------------------------------------------}

unit UCUniDACConn;

interface

uses
  UCDataConnector,SysUtils, Classes, UCBase, DB, DBAccess, Uni, MemDS;

type
  TUCUniDACConn = class(TUCDataConnector)
  private
    FConnection : TUniConnection;
    procedure SetFConnection(Value : TUniConnection);
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
    property Connection : TUniConnection read FConnection write SetFConnection;
  end;

implementation

{ TUCUniDACConn }

procedure TUCUniDACConn.SetFConnection(Value: TUniConnection);
begin
  if FConnection <> Value then FConnection := Value;
  if FConnection <> nil then FConnection.FreeNotification(Self);
end;

procedure TUCUniDACConn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
  begin
    FConnection := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

function TUCUniDACConn.UCFindTable(const TableName: String): Boolean;
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

function TUCUniDACConn.UCFindDataConnection: Boolean;
begin
    Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCUniDACConn.GetDBObjectName: String;
begin
  if Assigned(FConnection) then
  begin
    if Owner = FConnection.Owner then Result := FConnection.Name
    else begin
      Result := FConnection.Owner.Name+'.'+FConnection.Name;
    end;
  end else Result := '';
end;

function TUCUniDACConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCUniDACConn.UCExecSQL(FSQL: String);
begin
  FConnection.ExecSQL(FSQL,[]);
end;

function TUCUniDACConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TUniQuery.Create(nil);
  with Result as TUniQuery do
  begin
    Connection := FConnection;
    SQL.Text := FSQL;
    Open;
  end;
end;

end.
