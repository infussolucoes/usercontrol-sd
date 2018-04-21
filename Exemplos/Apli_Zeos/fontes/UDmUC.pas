unit UDmUC;

interface

uses
  SysUtils, Classes, DB, UCBase,
  UCDataConnector,
  ZAbstractRODataset, ZAbstractDataset, ZDataset,
  ZAbstractConnection, ZConnection;

type
  TdmUC = class(TDataModule)
    ZConnection1: TZConnection;
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
  ZConnection1.Disconnect;
  ZConnection1.Database := gsAppPath + '..\DBase\DB_UC.FDB';
  ZConnection1.Connect;
end;

end.
