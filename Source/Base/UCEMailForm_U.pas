unit UCEMailForm_U;

interface

{$I 'UserControl.inc'}

uses
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  Graphics,
  StdCtrls;

type
  TUCEMailForm = class(TForm)
    Panel1: TPanel;
    img: TImage;
    lbStatus: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UCEMailForm: TUCEMailForm;

implementation

{$R *.dfm}

end.
