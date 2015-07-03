{ -----------------------------------------------------------------------------
  Unit Name: UCMidasConn
  Author   : Luiz Benevenuto
  Date     : 31/07/2005
  Purpose  : Midas Suporte ( DataSnap )
  E-mail   : luiz@siffra.com
  URL      : www.siffra.com
  UC       : www.usercontrol.net

  registered in UCMidasConnReg.pas
  ----------------------------------------------------------------------------- }

unit UCMidasConn;

interface

{$I 'UserControl.inc'}

uses
  Classes,
  DB,
  DBClient,
  SysUtils,
  UCDataConnector;

type
  TBancoDados = (bdFirebird, bdMSSQL, bdOracle, bdPostgreSQL, bdMySQL,
    bdParadox);

  TUCMidasConn = class(TUCDataConnector)
  private
    FResultado: OleVariant;
    FSQLStmt: String;
    FParams: OleVariant;
    FOwnerData: OleVariant;
    FRecsOut: Integer;
    FConnection: TCustomRemoteServer;
    FProviderName: String;
    FBancoDados: TBancoDados;
    procedure SetConnection(const Value: TCustomRemoteServer);
    procedure SetProviderName(const Value: String);
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
    property Connection: TCustomRemoteServer Read FConnection
      Write SetConnection;
    property ProviderName: String Read FProviderName Write SetProviderName;
    property BancoDados: TBancoDados Read FBancoDados Write FBancoDados;
  end;

const
  // Select para as tabelas de sistema !!! Para outro tipo de banco implemente aqui !!!!!

  // Para banco novo !!!
  // Não esquecer de colocar em TBancoDados, o tipo de banco !!!!!!
  // Não esquecer de colocar no 'case' de UCFindTable

  SQL_Firebird = 'SELECT ' +
    '  UPPER(RDB$RELATIONS.RDB$RELATION_NAME) RDB$RELATION_NAME ' + 'FROM ' +
    '  RDB$RELATIONS ' + 'WHERE ' +
    '  RDB$RELATIONS.RDB$FLAGS = 1 AND UPPER(RDB$RELATIONS.RDB$RELATION_NAME) = '
    + '  UPPER(''%s'')';

  SQL_MSSQL = '';

  SQL_Oracle = '';

  SQL_PostgreSQL = 'SELECT ' + '  UPPER(PG_CLASS.RELNAME) ' + 'FROM ' +
    '  PG_CLASS ' + 'WHERE ' + '  PG_CLASS.RELKIND = ''r'' AND ' +
    '  UPPER(PG_CLASS.RELNAME) LIKE UPPER(''%s'')';

  SQL_MySQL = '';

  SQL_Paradox = '';

implementation

uses
  Midas,
  Provider;

{ TUCMidasConn }

function TUCMidasConn.GetDBObjectName: String;
begin
  if Assigned(FConnection) then
  begin
    if Owner = FConnection.Owner then
      Result := FConnection.Name
    else
      Result := FConnection.Owner.Name + '.' + FConnection.Name;
  end
  else
    Result := '';
end;

function TUCMidasConn.GetTransObjectName: String;
begin
  Result := '';
end;

procedure TUCMidasConn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TUCMidasConn.SetConnection(const Value: TCustomRemoteServer);
begin
  if FConnection <> Value then
    FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TUCMidasConn.SetProviderName(const Value: String);
begin
  FProviderName := Value;
end;

procedure TUCMidasConn.UCExecSQL(FSQL: String);
begin
  IAppServer(FConnection.GetServer).AS_Execute(FProviderName, FSQL, FParams,
    FOwnerData);
end;

function TUCMidasConn.UCFindDataConnection: Boolean;
begin
  Result := Assigned(FConnection) and (FConnection.Connected);
end;

function TUCMidasConn.UCFindTable(const Tablename: String): Boolean;
begin
  case FBancoDados of
    bdFirebird:
      FSQLStmt := SQL_Firebird;
    bdMSSQL:
      FSQLStmt := SQL_MSSQL;
    bdOracle:
      FSQLStmt := SQL_Oracle;
    bdPostgreSQL:
      FSQLStmt := SQL_PostgreSQL;
    bdMySQL:
      FSQLStmt := SQL_MySQL;
    bdParadox:
      FSQLStmt := SQL_Paradox;
  end;

  FSQLStmt := Format(FSQLStmt, [Tablename]);

  FResultado := IAppServer(FConnection.GetServer).AS_GetRecords(FProviderName,
    -1, FRecsOut, 0, FSQLStmt, FParams, FOwnerData);

  Result := FRecsOut > 0;
end;

function TUCMidasConn.UCGetSQLDataset(FSQL: String): TDataset;
begin
  Result := TClientDataSet.Create(Self);

  with TClientDataSet(Result) do
  begin
    if FConnection is TConnectionBroker then
      ConnectionBroker := TConnectionBroker(FConnection)
    else
      RemoteServer := FConnection;

    ProviderName := FProviderName;
    CommandText := FSQL;
    Open;
  end;
end;

end.
