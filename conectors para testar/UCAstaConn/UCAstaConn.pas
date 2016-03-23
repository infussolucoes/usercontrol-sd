{-----------------------------------------------------------------------------
 Unit Name: UCAstaConn
 Author:    QmD
 Date:      24-nov-2004
 Purpose:   Asta 3 Support

 registered in UCAstaReg.pas
-----------------------------------------------------------------------------}

unit UCAstaConn;

interface

uses
  SysUtils, Classes, UCBase, DB, AstaDrv2, AstaClientDataset, ScktComp,
  AstaCustomSocket, AstaClientSocket,AstaDBTypes;
type
  TUCAstaConn = class(TUCDataConn)
  private
    FAstaClientSocket: TAstaClientSocket;
    procedure SetFAstaClientSocket(const Value: TAstaClientSocket);
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
    property AstaClientSocket : TAstaClientSocket read FAstaClientSocket write SetFAstaClientSocket;
  end;

implementation

{ TUCAstaConn }

procedure TUCAstaConn.SetFAstaClientSocket(
  const Value: TAstaClientSocket);
begin
  FAstaClientSocket := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TUCAstaConn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAstaClientSocket) then
  begin
    FAstaClientSocket := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

function TUCAstaConn.UCFindTable(const TableName: String): Boolean;
var
  TempQuery : TAstaClientDataSet;
begin
  TempQuery := TAstaClientDataset.Create(nil);
  TempQuery.AstaClientSocket := FAstaClientSocket;
  TempQuery.MetaDataRequest := mdTables;
  TempQuery.Open;
  Result := TempQuery.Locate('TableName',UpperCase(TableName),[]);
  TempQuery.Close;
  FreeAndNil(TempQuery);
end;


function TUCAstaConn.UCFindDataConnection: Boolean;
begin
    Result := Assigned(FAstaClientSocket) and (FAstaClientSocket.Active);
end;

function TUCAstaConn.GetDBObjectName: String;
begin
  if Assigned(FAstaClientSocket) then
  begin
    if Owner = FAstaClientSocket.Owner then Result := FAstaClientSocket.Name
    else begin
      Result := FAstaClientSocket.Owner.Name+'.'+FAstaClientSocket.Name;
    end;
  end else Result := '';
end;

function TUCAstaConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCAstaConn.UCExecSQL(FSQL: String);
begin
  with TAstaClientDataset.Create(nil) do
  begin
    AstaClientSocket := FAstaClientSocket;
    SQL.text := FSQL;
    ExecSQL;
    Free;
  end;
end;

function TUCAstaConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TAstaClientDataset.Create(nil);
  with Result as TAstaClientDataset do
  begin
    AstaClientSocket := FAstaClientSocket;
    SQL.text := FSQL;
    Open;
  end;
end;

end.
