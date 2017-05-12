unit UDmUC;

interface

uses
  SysUtils,
  Forms,
  Classes,
  DB,
  UCBase,
  UCDataConnector,
  ZAbstractConnection,
  ZConnection,
  ZAbstractRODataset,
  ZAbstractDataset, ZDataset,
  {$IFDEF DELPHI22_UP}
  Data.Win.ADODB,
  {$ENDIF}
   ADODB;

type
  TdmUC = class(TDataModule)
    ADOConnection1: TADOConnection;
    ADOQuery1: TADOQuery;
    ADOTable1: TADOTable;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  var
  dmUC: TdmUC;

implementation

{$IFDEF DELPHI22_UP}
  uses
    IWSystem;
{$ENDIF}



{$R *.dfm}

procedure TdmUC.DataModuleCreate(Sender: TObject);
begin
   ADOConnection1.Open;
end;

end.
