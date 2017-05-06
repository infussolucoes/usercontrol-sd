// 
// Created by the DataSnap proxy generator.
// 06/05/2017 00:33:56
//

{ O proxy precisa ser editado...
Exemplo criado por Giovani Da Cruz }

unit Proxy;

interface

uses System.JSON, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap,
Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils,
Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders,
Data.DBXJSONReflect,

{ Importante dar uses desta unit  }
ucdatasnapproxy;

type
  TSMUserControlClient = class(TDSUserRemote)   // <- Importante aqui trocar a classe da herança
  private
    FDSServerModuleCreateCommand: TDBXCommand;
    FGetDataSetCommand: TDBXCommand;
    FExecuteSQLCommand: TDBXCommand;
    FFindTableCommand: TDBXCommand;
  public
    constructor Create(ADBXConnection: TDBXConnection); overload;
    constructor Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    procedure DSServerModuleCreate(Sender: TObject);
    procedure GetDataSet(SQL: string); override; // <- Necessario colocar a sobrescrita
    procedure ExecuteSQL(SQL: string); override; // <- Necessario colocar a sobrescrita
    function FindTable(TableName: string; SchemaName: string): Boolean; override; // <- Necessario colocar a sobrescrita
  end;

implementation

procedure TSMUserControlClient.DSServerModuleCreate(Sender: TObject);
begin
  if FDSServerModuleCreateCommand = nil then
  begin
    FDSServerModuleCreateCommand := FDBXConnection.CreateCommand;
    FDSServerModuleCreateCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FDSServerModuleCreateCommand.Text := 'TSMUserControl.DSServerModuleCreate';
    FDSServerModuleCreateCommand.Prepare;
  end;
  if not Assigned(Sender) then
    FDSServerModuleCreateCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDBXClientCommand(FDSServerModuleCreateCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FDSServerModuleCreateCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(Sender), True);
      if FInstanceOwner then
        Sender.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FDSServerModuleCreateCommand.ExecuteUpdate;
end;

procedure TSMUserControlClient.GetDataSet(SQL: string);
begin
  if FGetDataSetCommand = nil then
  begin
    FGetDataSetCommand := FDBXConnection.CreateCommand;
    FGetDataSetCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FGetDataSetCommand.Text := 'TSMUserControl.GetDataSet';
    FGetDataSetCommand.Prepare;
  end;
  FGetDataSetCommand.Parameters[0].Value.SetWideString(SQL);
  FGetDataSetCommand.ExecuteUpdate;
end;

procedure TSMUserControlClient.ExecuteSQL(SQL: string);
begin
  if FExecuteSQLCommand = nil then
  begin
    FExecuteSQLCommand := FDBXConnection.CreateCommand;
    FExecuteSQLCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FExecuteSQLCommand.Text := 'TSMUserControl.ExecuteSQL';
    FExecuteSQLCommand.Prepare;
  end;
  FExecuteSQLCommand.Parameters[0].Value.SetWideString(SQL);
  FExecuteSQLCommand.ExecuteUpdate;
end;

function TSMUserControlClient.FindTable(TableName: string; SchemaName: string): Boolean;
begin
  if FFindTableCommand = nil then
  begin
    FFindTableCommand := FDBXConnection.CreateCommand;
    FFindTableCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FFindTableCommand.Text := 'TSMUserControl.FindTable';
    FFindTableCommand.Prepare;
  end;
  FFindTableCommand.Parameters[0].Value.SetWideString(TableName);
  FFindTableCommand.Parameters[1].Value.SetWideString(SchemaName);
  FFindTableCommand.ExecuteUpdate;
  Result := FFindTableCommand.Parameters[2].Value.GetBoolean;
end;


constructor TSMUserControlClient.Create(ADBXConnection: TDBXConnection);
begin
  inherited Create(ADBXConnection);
end;


constructor TSMUserControlClient.Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ADBXConnection, AInstanceOwner);
end;


destructor TSMUserControlClient.Destroy;
begin
  FDSServerModuleCreateCommand.DisposeOf;
  FGetDataSetCommand.DisposeOf;
  FExecuteSQLCommand.DisposeOf;
  FFindTableCommand.DisposeOf;
  inherited;
end;

end.
