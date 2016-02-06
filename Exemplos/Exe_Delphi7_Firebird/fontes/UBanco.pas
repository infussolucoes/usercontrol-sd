unit UBanco;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DB,
  Grids, DBGrids, ExtCtrls, DBCtrls;

type
  TFrmBanco = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DSBanco: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmBanco: TFrmBanco;

implementation

{$R *.dfm}

uses
  UDmUC;

procedure TFrmBanco.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if dmUC.IBTransaction1.InTransaction then
    dmUC.IBTransaction1.CommitRetaining;
end;

procedure TFrmBanco.FormCreate(Sender: TObject);
begin
  if not (dmUC.QryBanco.Active) then
    dmUC.QryBanco.Open;
end;

end.
