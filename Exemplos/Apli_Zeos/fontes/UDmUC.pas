unit UDmUC;

interface

uses
  SysUtils, Classes, DB, UCBase,
  UCDataConnector,
  ZAbstractConnection, ZConnection,
  ZAbstractRODataset, ZAbstractDataset, ZDataset;

type
  TdmUC = class(TDataModule)
    con1: TZConnection;
    QryBanco: TZQuery;
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
//  FDConnection1.Close;
//  FDConnection1.Params.Database :=
//  gsAppPath + '..\DBase\APLICATIVO_UC.FDB';
//  FDConnection1.Open;
//gsAppPath
end;

end.
