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
  FireDAC.Stan.StorageJSON;

type
  TdmUC = class(TDataModule)
    FDConnection1: TFDConnection;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    QryBanco: TFDQuery;
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
begin
  FDConnection1.Close;
  FDConnection1.Params.Database := gsAppPath + '..\DBase\APLICATIVO_UC.FDB';
  FDConnection1.Open;
end;

end.
