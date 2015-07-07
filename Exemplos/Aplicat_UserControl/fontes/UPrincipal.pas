unit UPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Cadastro1: TMenuItem;
    Bancos1: TMenuItem;
    Clientes1: TMenuItem;
    Cidades1: TMenuItem;
    Produtos1: TMenuItem;
    N1: TMenuItem;
    Sair1: TMenuItem;
    Sistema1: TMenuItem;
    CadastrodeUsurios1: TMenuItem;
    CadastrodePerfil1: TMenuItem;
    Mensagens1: TMenuItem;
    N2: TMenuItem;
    RegistrodeLogins1: TMenuItem;
    rocarSenha1: TMenuItem;
    N3: TMenuItem;
    Executarlogon1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses UDmUC;

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   dmUC.cnxUC.Connected := False;
end;

end.
