{-----------------------------------------------------------------------------
 Unit Name: UCMyDACConn
 Author:    QmD
 Date:      22-nov-2004
 Purpose:   MyDAC Support

 registered in UCMyDACReg.pas
-----------------------------------------------------------------------------}

unit UCMyDACConn;

interface

uses
  UCDataConnector,SysUtils, Classes, UCBase, DB, DBAccess, MyAccess, MemDS;

type
  TUCMyDACConn = class(TUCDataConnector)
  private
    FConnection : TMyConnection;
    procedure SetFConnection(Value : TMyConnection);
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
    property Connection : TMyConnection read FConnection write SetFConnection;
  end;

implementation

{ TUCMyDACConn }

procedure TUCMyDACConn.SetFConnection(Value: TMyConnection);
begin
  if FConnection <> Value then FConnection := Value;
  if FConnection <> nil then FConnection.FreeNotification(Self);
end;

procedure TUCMyDACConn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
  begin
    FConnection := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

function TUCMyDACConn.UCFindTable(const TableName: String): Boolean;
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

function TUCMyDACConn.UCFindDataConnection: Boolean;
begin
    Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCMyDACConn.GetDBObjectName: String;
begin
  if Assigned(FConnection) then
  begin
    if Owner = FConnection.Owner then Result := FConnection.Name
    else begin
      Result := FConnection.Owner.Name+'.'+FConnection.Name;
    end;
  end else Result := '';
end;

function TUCMyDACConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCMyDACConn.UCExecSQL(FSQL: String);
begin
  FConnection.ExecSQL(FSQL,[]);
end;

function TUCMyDACConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TMyQuery.Create(nil);
  with Result as TMyQuery do
  begin
    Connection := FConnection;
    SQL.Text := FSQL;
    Open;
  end;
end;

end.
