{ Criado por Giovani Da Cruz - showdelphi.com.br }

unit ServerMethodsUser;

interface

uses
  System.SysUtils, System.Classes, Datasnap.DSServer, 
  Datasnap.DSAuth, Datasnap.DSProviderDataModuleAdapter, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, FireDAC.Phys.IBBase,
  FireDAC.Stan.StorageJSON, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.UI,
  Datasnap.Provider, FireDAC.Comp.DataSet;

type
  TSMUserControl = class(TDSServerModule)
    FDConnection1: TFDConnection;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    QryAux: TFDQuery;
    ProviderAux: TDataSetProvider;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    procedure DSServerModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure GetDataSet(SQL: String);
    procedure ExecuteSQL(SQL: String);
    function FindTable(const TableName, SchemaName: String): Boolean;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  IWSystem;

procedure TSMUserControl.DSServerModuleCreate(Sender: TObject);
begin
  FDConnection1.Close;
  FDConnection1.Params.Database := gsAppPath + 'DBase\APLICATIVO_UC.FDB';
  FDConnection1.Open;
end;

procedure TSMUserControl.ExecuteSQL(SQL: String);
begin
  FDConnection1.Close;
  FDConnection1.Open;
  FDConnection1.ExecSQL(SQL, []);
  FDConnection1.Commit;
end;

function TSMUserControl.FindTable(const TableName,
  SchemaName: String): Boolean;
var
  TempList: TStringList;
begin
  try
    TempList := TStringList.Create;;
    FDConnection1.GetTableNames('', '', '', TempList, [osMy], [tkTable], False);
    TempList.Text := UpperCase(TempList.Text);
    Result := TempList.IndexOf(UpperCase(Tablename)) > -1;
  finally
    FreeAndNil(TempList);
  end;
end;

procedure TSMUserControl.GetDataSet(SQL: String);
begin
  QryAux.Close;
  QryAux.SQL.Text := SQL;
end;

end.

