program UserControl_Tut01;

uses
  Forms,
  UPrincipal in 'UPrincipal.pas' {FrmPrincipal},
  UDmUC in 'UDmUC.pas' {dmUC: TDataModule},
  UBanco in 'UBanco.pas' {FrmBanco},
  UCliente in 'UCliente.pas' {FrmCliente};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmUC, dmUC);
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;
end.
