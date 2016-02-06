unit UCliente;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DB,
  Grids, DBGrids, ExtCtrls, DBCtrls, UCBase, StdCtrls;

type
  TFrmCliente = class(TForm)
    BtnIncluir: TButton;
    BtnAlterar: TButton;
    BtnExcluir: TButton;
    UCControls1: TUCControls;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCliente: TFrmCliente;

implementation

{$R *.dfm}

uses
  UPrincipal;

end.
