unit UPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UCADOConn, UCIBXConn, UCMidasConn,
  UCDataConnector, UCFireDACConn, UCHist_Base, UCHistDataset, UCMail, UCIdle,
  UCBase, UCSettings;

type
  TForm1 = class(TForm)
    UserControl1: TUserControl;
    UCSettings1: TUCSettings;
    UCControls1: TUCControls;
    UCApplicationMessage1: TUCApplicationMessage;
    UCIdle1: TUCIdle;
    MailUserControl1: TMailUserControl;
    UCHist_DataSet1: TUCHist_DataSet;
    UCControlHistorico1: TUCControlHistorico;
    UCFireDACConn1: TUCFireDACConn;
    UCMidasConn1: TUCMidasConn;
    UCIBXConn1: TUCIBXConn;
    UCADOConn1: TUCADOConn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
