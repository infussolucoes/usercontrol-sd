program Sample1;

uses
  Vcl.Forms,
  UfrmPrincipal in 'UfrmPrincipal.pas' {frmPrincipal},
  Ufrmform1 in 'Ufrmform1.pas' {frmform1},
  Ufrmform2 in 'Ufrmform2.pas' {frmform2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
