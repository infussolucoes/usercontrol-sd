unit UDmUC;

interface

uses
  SysUtils, Classes, DB, SqlExpr, UCBase, UCDBXConn, Data.DBXFirebird,
  UCDataConnector;

type
  TdmUC = class(TDataModule)
    cnxUC: TSQLConnection;
    MyConn: TUCDBXConn;
    ucMyControl: TUserControl;
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
   cnxUC.Connected := True;
end;

end.
