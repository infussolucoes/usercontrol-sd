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
    Panel1:         TPanel;
    Image1:         TImage;
    Image2:         TImage;
    lblVersao:      TLabel;
    Panel2:         TPanel;
    Label2:         TLabel;
    Label3:         TLabel;
    Label8:         TLabel;
    Label4:         TLabel;
    Label5:         TLabel;
    Label11:        TLabel;
    Panel3:         TPanel;
    Label9:         TLabel;
    Label6:         TLabel;
    Label7:         TLabel;
    MemoAgrd:       TMemo;
    Label1:         TLabel;
    Image3:         TImage;
    Label12:        TLabel;
    BitBtn1:        TBitBtn;
    pnlFundo:       TPanel;
    pnlComponentes: TPanel;
    Image4: TImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Label4Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  procedure WMNChitTest(var M: TWMNchitTest);message WM_NCHITTEST;
    { Private declarations }
  public
    { Public declarations }
  end;

{
var
  AboutForm: TAboutForm;
}

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
  //ShellExecute(0, 'open', 'mailto:qmd@usercontrol.com.br', '', '', SW_SHOW);
end;

procedure TAboutForm.Label6Click(Sender: TObject);
begin
  //ShellExecute(0, 'open', 'http://www.usercontrol.net', '', '', SW_SHOW);
end;

procedure TAboutForm.Image2Click(Sender: TObject);
begin
  //ShellExecute(0, 'open', 'http://delphiland.dyns.cx', nil, nil, SW_SHOW);
end;

procedure TAboutForm.Image3Click(Sender: TObject);
begin
  //ShellExecute(0, 'open', 'http://www.curitiba.pr.gov.br/', nil, nil, SW_SHOW);
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Self.BorderStyle  := bsNone;
  lblVersao.Caption := 'Versão ' + UCVersion;
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
//  pnlComponentes.Top  := Round((pnlFundo.Height - pnlComponentes.Height) / 2);
//  pnlComponentes.Left := Round((pnlFundo.Width - pnlComponentes.Width) / 2);
end;

procedure TAboutForm.WMNChitTest(var M: TWMNchitTest);
begin
  inherited;
  if M.result = htclient then
  M.result := htCaption;
end;

end.

