unit UfrmPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UCDataConnector, UCFireDACConn, UCBase,
  UCSettings, uADStanIntf, uADStanOption, uADStanError, uADGUIxIntf,
  uADPhysIntf, uADStanDef, uADStanPool, uADStanAsync, uADPhysManager, Data.DB,
  uADCompClient, System.Actions, Vcl.ActnList, uADPhysODBCBase, uADPhysMSSQL,
  uADGUIxFormsWait, uADCompGUIx;

type
  TfrmPrincipal = class(TForm)
    UserControl1: TUserControl;
    UCSettings1: TUCSettings;
    UCFireDACConn1: TUCFireDACConn;
    ADConnection1: TADConnection;
    ActionList1: TActionList;
    actform1: TAction;
    actform2: TAction;
    actUsuarios: TAction;
    actTrocadoUsuario: TAction;
    actLog: TAction;
    actlongoff: TAction;
    Action1: TAction;
    ADPhysMSSQLDriverLink1: TADPhysMSSQLDriverLink;
    ADGUIxWaitCursor1: TADGUIxWaitCursor;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

{$R *.dfm}

end.
