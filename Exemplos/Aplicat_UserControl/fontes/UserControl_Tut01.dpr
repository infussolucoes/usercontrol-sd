program UserControl_Tut01;

uses
  Forms,
  UPrincipal in 'UPrincipal.pas' {Form1},
  UDmUC in 'UDmUC.pas' {dmUC: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TdmUC, dmUC);
  Application.Run;
end.
