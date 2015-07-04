{ -----------------------------------------------------------------------------
  Unit Name: UCFireDACConn
  Author:    sergio@lsisistemas.com.br
  Date:      12-Junho-2013
  Purpose:   FireDac Support

  registered in UCFireDACReg.pas
  ----------------------------------------------------------------------------- }

unit UCFireDACConn;

interface

uses
  UCDataConnector, SysUtils, Classes, UCBase, DB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Comp.Client, FireDAC.VCLUI.Wait, FireDAC.Comp.UI;

type
  TUCFireDACConn = class(TUCDataConnector)
  private
    FConnection: TFDConnection;
    procedure SetFConnection(Value: TFDConnection);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    function GetDBObjectName: String; override;
    function GetTransObjectName: String; override;
    function UCFindDataConnection: Boolean; override;
    function UCFindTable(const Tablename: String): Boolean; override;
    function UCGetSQLDataset(FSQL: String): TDataset; override;
    procedure UCExecSQL(FSQL: String); override;
  published
    property Connection: TFDConnection read FConnection write SetFConnection;
  end;

implementation

{ TUCUniDACConn }

procedure TUCFireDACConn.SetFConnection(Value: TFDConnection);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TUCFireDACConn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
  begin
    FConnection := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

function TUCFireDACConn.UCFindTable(const Tablename: String): Boolean;
var
  TempList: TStringList;
begin
  try
    TempList := TStringList.Create;;
    FConnection.GetTableNames('', '', '', TempList, [osMy], [tkTable]);
    TempList.Text := UpperCase(TempList.Text);
    Result := TempList.IndexOf(UpperCase(Tablename)) > -1;
  finally
    FreeAndNil(TempList);
  end;
end;

function TUCFireDACConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCFireDACConn.GetDBObjectName: String;
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

function TUCFireDACConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCFireDACConn.UCExecSQL(FSQL: String);
begin
  if Assigned(FConnection) then
    FConnection.ExecSQL(FSQL, []);
end;

function TUCFireDACConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TFDQuery.Create(nil);
  with Result as TFDQuery do
  begin
    Connection := FConnection;
    SQL.Text := FSQL;
    Open;
  end;
end;

end.
