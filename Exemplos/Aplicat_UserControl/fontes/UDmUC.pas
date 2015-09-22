unit UDmUC;

interface

uses
  SysUtils, Classes, DB, SqlExpr, UCBase, UCDBXConn, Data.DBXFirebird,
  UCDataConnector, IBX.IBDatabase, UCIBXConn;

type
  TdmUC = class(TDataModule)
    cnxUC: TSQLConnection;
    IBDatabase1: TIBDatabase;
    IBTransaction1: TIBTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmUC: TdmUC;

implementation

uses UPrincipal;

{$R *.dfm}

procedure TdmUC.DataModuleCreate(Sender: TObject);
begin
   //cnxUC.Connected := True;
   IBDatabase1.Open;
end;

end.
