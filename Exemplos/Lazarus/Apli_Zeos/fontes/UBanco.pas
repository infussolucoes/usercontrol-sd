unit UBanco;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DB, ExtCtrls, DBCtrls,
  Grids, DBGrids;

type
  TFrmBanco = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DSBanco: TDataSource;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmBanco: TFrmBanco;

implementation

{$R *.lfm}

uses
  UDmUC;

procedure TFrmBanco.FormCreate(Sender: TObject);
begin
  if not (dmUC.QryBanco.Active) then
    dmUC.QryBanco.Open;
end;

end.
