{-----------------------------------------------------------------------------
 Unit Name: UCFIBConn
 Author:    QmD
 Date:      08-nov-2004
 Purpose:   FIB Support

 registered in UCReg.pas
-----------------------------------------------------------------------------}

unit UCFIBConn;

interface

uses
  SysUtils, Classes, UCBase, DB, pFIBDatabase, pFIBDataset;

type
  TUCFIBConn = class(TUCDataConn)
  private
    FConnection : TpFIBDatabase;
    FTransaction: TpFIBTransaction;
    procedure SetFTransaction(const Value: TpFIBTransaction);
    procedure SetFConnection(const Value: TpFIBDatabase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    function GetDBObjectName : String; override;
    function GetTransObjectName : String; override;
    function UCFindDataConnection : Boolean; override;
    function UCFindTable(const Tablename : String) : Boolean; override;
    function UCGetSQLDataset(FSQL : String) : TDataset; override;
    procedure UCExecSQL(FSQL: String);override;
  published
    property Connection : TpFIBDatabase read FConnection write SetFConnection;
    property Transaction : TpFIBTransaction read FTransaction write SetFTransaction;
  end;

implementation

{ TUCFIBConn }

procedure TUCFIBConn.SetFTransaction(const Value: TpFIBTransaction);
begin
  FTransaction := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TUCFIBConn.SetFConnection(const Value: TpFIBDatabase);
begin
  if FConnection <> Value then FConnection := Value;
  if FConnection <> nil then FConnection.FreeNotification(Self);
end;

procedure TUCFIBConn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then FConnection := nil;
  if (Operation = opRemove) and (AComponent = FTransaction) then FTransaction := nil;
  inherited Notification(AComponent, Operation);
end;

function TUCFIBConn.UCFindTable(const TableName: String): Boolean;
var
  TempList : TStringList;
begin
  try
    TempList := TStringList.Create;
    FConnection.GetTableNames(TempList,False);
    TempList.Text := UpperCase(TempList.Text);
    Result := TempList.IndexOf(UpperCase(TableName)) > -1;
  finally
    FreeAndNil(TempList);
  end;
end;

function TUCFIBConn.UCFindDataConnection: Boolean;
begin
    Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCFIBConn.GetDBObjectName: String; 
begin
  if Assigned(FConnection) then
  begin
    if Owner = FConnection.Owner then Result := FConnection.Name
    else begin
      Result := FConnection.Owner.Name+'.'+FConnection.Name;
    end;
  end else Result := '';
end;

function TUCFIBConn.GetTransObjectName: String;
begin
  if Assigned(FTransaction) then
  begin
    if Owner = FTransaction.Owner then Result := FTransaction.Name
    else begin
      Result := FTransaction.Owner.Name+'.'+FTransaction.Name;
    end;
  end else Result := '';
end;

procedure TUCFIBConn.UCExecSQL(FSQL: String);
begin
  FConnection.Execute(FSQL);
end;

function TUCFIBConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TpFIBDataset.Create(nil);
  with Result as TpFIBDataset do
  begin
    Database := FConnection;
    Transaction := FTransaction;
    SelectSQL.text := FSQL;
    Open;
  end;
end;

end.
