unit UCliente;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, UCBase;

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
