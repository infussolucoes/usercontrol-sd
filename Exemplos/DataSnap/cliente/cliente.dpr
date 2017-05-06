program cliente;

uses
  Vcl.Forms,
  TelaPrincipal in 'TelaPrincipal.pas' {FrmPrincipal},
  Proxy in 'Proxy.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;
end.
