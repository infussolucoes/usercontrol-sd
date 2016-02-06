unit UPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, UCBase, UCDataConnector, UCIBXConn;

type
  TFrmPrincipal = class(TForm)
    MainMenu1: TMainMenu;
    Cadastro1: TMenuItem;
    Bancos1: TMenuItem;
    Clientes1: TMenuItem;
    Cidades1: TMenuItem;
    Produtos1: TMenuItem;
    N1: TMenuItem;
    Sair1: TMenuItem;
    Seguranca1: TMenuItem;
    CadastrodeUsurios1: TMenuItem;
    Mensagens1: TMenuItem;
    N2: TMenuItem;
    rocarSenha1: TMenuItem;
    N3: TMenuItem;
    Executarlogon1: TMenuItem;
    ucMyControl: TUserControl;
    UCIBXConn1: TUCIBXConn;
    estes1: TMenuItem;
    GerarLog1: TMenuItem;
    UCApplicationMessage1: TUCApplicationMessage;
    procedure Sair1Click(Sender: TObject);
    procedure GerarLog1Click(Sender: TObject);
    procedure Mensagens1Click(Sender: TObject);
    procedure Bancos1Click(Sender: TObject);
    procedure Clientes1Click(Sender: TObject);
    procedure Cidades1Click(Sender: TObject);
    procedure Produtos1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

uses
  UDmUC, UBanco, UCliente;

{$R *.dfm}

procedure TFrmPrincipal.Bancos1Click(Sender: TObject);
begin
  Application.CreateForm(TFrmBanco, FrmBanco);
  FrmBanco.ShowModal;
end;

procedure TFrmPrincipal.Cidades1Click(Sender: TObject);
begin
  ShowMessage('Cidade');
end;

procedure TFrmPrincipal.Clientes1Click(Sender: TObject);
begin
  Application.CreateForm(TFrmCliente, FrmCliente);
  FrmCliente.ShowModal;
end;

procedure TFrmPrincipal.GerarLog1Click(Sender: TObject);
begin
  ucMyControl.Log('Teste de log', 1);
end;

procedure TFrmPrincipal.Mensagens1Click(Sender: TObject);
begin
  UCApplicationMessage1.CheckMessages;
  UCApplicationMessage1.ShowMessages();
end;

procedure TFrmPrincipal.Produtos1Click(Sender: TObject);
begin
  ShowMessage('Produtos');
end;

procedure TFrmPrincipal.Sair1Click(Sender: TObject);
begin
  Close;
end;

end.
