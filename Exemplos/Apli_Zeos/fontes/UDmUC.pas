unit UDmUC;

interface

uses
  SysUtils, Forms, Classes, DB, UCBase,
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
{$IFDEF DELPHI17_UP}   //
 uses
   IWSystem;
{$ENDIF}   

{$R *.dfm}

procedure TdmUC.DataModuleCreate(Sender: TObject);
var path : string;
begin
  path :=  ExtractFilePath(Application.ExeName);
  path := Copy(path, 1, LastDelimiter('\',Path)-1);
  path := Copy(Path, 1, LastDelimiter('\',Path));
  path := path+ 'DBase\APLICATIVO_UC.FDB';
  Con1.Connected := False;
  Con1.Database:= path;
  Con1.Connected := True;
  end;

end.
