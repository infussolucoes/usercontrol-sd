unit UDmUC;

{$MODE Delphi}

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
  Forms;

{$R *.lfm}

procedure TdmUC.DataModuleCreate(Sender: TObject);
begin
  ZConnection1.Disconnect;
  ZConnection1.Database := ExtractFilePath(Application.ExeName) +
    '..\..\..\DBase\DB_UC.FDB';
  ZConnection1.HostName:= '127.0.0.1';
  ZConnection1.Port:= 3050;

  ZConnection1.Connect;
end;

end.
