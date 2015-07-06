unit UCAbout;

interface

{$I 'UserControl.inc'}

uses
  Messages,
  Buttons,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  jpeg,
  StdCtrls;

type
  TAboutForm = class(TForm)
    Panel1: TPanel;
    lblVersao: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label9: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MemoAgrd: TMemo;
    Image3: TImage;
    Label12: TLabel;
    BitBtn1: TBitBtn;
    pnlFundo: TPanel;
    pnlComponentes: TPanel;
    Label10: TLabel;
    Label13: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Label5: TLabel;
    Label11: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Label4Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure WMNChitTest(var M: TWMNchitTest); message WM_NCHITTEST;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  ShellAPI,
  UCBase,
  Windows;

{$R *.dfm}

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TAboutForm.Label4Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'mailto:qmd@usercontrol.com.br', '', '', SW_SHOW);
end;

procedure TAboutForm.Label6Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://infussolucoes.github.io/usercontrol-sd', '',
    '', SW_SHOW);
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Self.BorderStyle := bsNone;
  lblVersao.Caption := 'Versão ' + UCVersion;
end;

procedure TAboutForm.WMNChitTest(var M: TWMNchitTest);
begin
  inherited;

  if (M.result = HTCLIENT) then
    M.result := HTCAPTION;
end;

end.
