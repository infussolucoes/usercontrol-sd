program Exemplo_UserControl_DBXConnector;

uses
  Forms,
  UPrincipal in 'UPrincipal.pas' {FrmPrincipal},
  UDmUC in 'UDmUC.pas' {dmUC: TDataModule},
  UBanco in 'UBanco.pas' {FrmBanco},
  UCliente in 'UCliente.pas' {FrmCliente};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'UserControl - Exemplo com Zeos Connector';
  Application.CreateForm(TdmUC, dmUC);
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;
end.
