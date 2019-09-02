program Servidor;

uses
  Vcl.Forms,
  Servidor.Main in 'Servidor.Main.pas' {FrmMain},
  uDmService in 'uDmService.pas' {ServerMethodDM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TServerMethodDM, ServerMethodDM);
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
