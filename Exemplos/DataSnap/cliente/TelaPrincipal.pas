{ Criado por Giovani Da Cruz - showdelphi.com.br }

unit TelaPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UCBase, Vcl.Menus, Data.DBXDataSnap,
  Data.DBXCommon, IPPeerClient, UCDataConnector, UCDataSnapConn, Data.DB,
  Data.SqlExpr, Vcl.StdCtrls, Datasnap.DBClient, Datasnap.DSConnect,

  { importante declarar aqui }
  Proxy;

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
    rocarSenha1: TMenuItem;
    N3: TMenuItem;
    Executarlogon1: TMenuItem;
    N2: TMenuItem;
    Mensagens1: TMenuItem;
    estes1: TMenuItem;
    GerarLog1: TMenuItem;
    ucMyControl: TUserControl;
    SQLConnection1: TSQLConnection;
    UCDataSnapConn1: TUCDataSnapConn;
    DSProviderConnection1: TDSProviderConnection;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure SQLConnection1AfterConnect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Sair1Click(Sender: TObject);
    procedure Produtos1Click(Sender: TObject);
    procedure Cidades1Click(Sender: TObject);
    procedure Clientes1Click(Sender: TObject);
    procedure Bancos1Click(Sender: TObject);
    procedure GerarLog1Click(Sender: TObject);
    procedure SQLConnection1BeforeDisconnect(Sender: TObject);
  private
    FClasseLigacao: TSMUserControlClient;
    function GetClasseLigacao: TSMUserControlClient;
    property ClasseLigacao : TSMUserControlClient read GetClasseLigacao;
  public
    { Public declarations }
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

{$R *.dfm}

procedure TFrmPrincipal.Bancos1Click(Sender: TObject);
begin
  ShowMessage('Bancos');
end;

procedure TFrmPrincipal.Cidades1Click(Sender: TObject);
begin
  ShowMessage('Cidades');
end;

procedure TFrmPrincipal.Clientes1Click(Sender: TObject);
begin
  ShowMessage('Clientes');
end;

procedure TFrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { Importante aqui,para evitar erros }
  if (ucMyControl <> nil) then
    FreeAndNil(ucMyControl);

  if Assigned(FClasseLigacao) then
    FreeAndNil(FClasseLigacao);
end;

procedure TFrmPrincipal.FormCreate(Sender: TObject);
begin
  SQLConnection1.Close;

  try
    SQLConnection1.Open;
  except on E: Exception do
    begin
      Application.MessageBox(
      PWideChar('Erro ao conectar com o servidor!' + #13 +
      E.Message),
      'Exemplo UserControl', MB_ICONERROR + MB_OK);

      Application.Terminate;
    end;
  end;

  ucMyControl.StartLogin;
end;

procedure TFrmPrincipal.GerarLog1Click(Sender: TObject);
begin
  ucMyControl.Log('Teste de log', 1);
end;

function TFrmPrincipal.GetClasseLigacao: TSMUserControlClient;
begin
  if not SQLConnection1.Connected then
    raise Exception.Create('Não há conexão com o servidor!');

  if (FClasseLigacao = nil) then
    FClasseLigacao := TSMUserControlClient.Create(SQLConnection1.DBXConnection,
    True);

  Result := FClasseLigacao;
end;

procedure TFrmPrincipal.Produtos1Click(Sender: TObject);
begin
  ShowMessage('Produtos');
end;

procedure TFrmPrincipal.Sair1Click(Sender: TObject);
begin
  Close;
end;

procedure TFrmPrincipal.SQLConnection1AfterConnect(Sender: TObject);
begin
  UCDataSnapConn1.DSClient := Self.ClasseLigacao; // <- Importante para o funcionamento
end;

procedure TFrmPrincipal.SQLConnection1BeforeDisconnect(Sender: TObject);
begin
  if Assigned(FClasseLigacao) then
    FreeAndNil(FClasseLigacao);
end;

end.
