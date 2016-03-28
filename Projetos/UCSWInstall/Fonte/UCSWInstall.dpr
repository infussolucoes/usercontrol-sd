program UCSWInstall;

uses
  Vcl.Forms,
  Visual.Main in 'Visual.Main.pas' {FrmPrincipal},
  uFrameLista in 'uFrameLista.pas' {framePacotes: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Instalador UserControl Show delphi Edition';
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;

end.
