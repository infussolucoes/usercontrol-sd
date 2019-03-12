unit UCliente;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DB, ExtCtrls, DBCtrls,
  Grids, DBGrids, UCBase, StdCtrls;

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

{$R *.lfm}

uses
  UPrincipal;

end.
