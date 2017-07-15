unit UDmUC;

interface

uses
  SysUtils, Classes, DB, UCBase,
  UCDataConnector,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Phys.IBBase,
  FireDAC.Stan.StorageJSON, FireDAC.Comp.UI;

type
  TdmUC = class(TDataModule)
    FDConnection1: TFDConnection;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    QryBanco: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmUC: TdmUC;

implementation

uses
  IWSystem;

{$R *.dfm}

procedure TdmUC.DataModuleCreate(Sender: TObject);
var
path:string;
begin
  // Mauri 11/05/2017
  // Usando ".." o FDConection Nao entedeu muito o que ele siginifica no meio do path
  // Por iso fiz essa "GAMBIARA" aqui->>
  path := Copy(gsAppPath, 1, LastDelimiter('\',gsAppPath)-1);
  path := Copy(Path, 1, LastDelimiter('\',Path));
  
//  path := gsAppPath +'..';
  path := path+ 'DBase\APLICATIVO_UC.FDB';
  FDConnection1.Close;
  FDConnection1.Params.Values['Database'] := path;
  FDConnection1.Open;
end;

end.
