unit UCIdle;

interface

uses Classes, UCBase, Dialogs, Windows, Forms, ExtCtrls, Messages,SysUtils;

type

  TUCIdle = class;
  TUCIdleTimeLeft = procedure (TimeLeft : Integer) of Object;

  TThUCIdle = class(TThread)
  private
    procedure DoIdle;
    procedure TimeLeftSinc;
  protected
    procedure Execute; override;
  public
    CurrentMilisec : Integer;
    UCIdle : TUCIdle;
  end;

  TUCIdle = class(TComponent)
  private
    FThIdle : TThUCIdle;
    FTimeOut: Integer;
    FOnIdle: TNotifyEvent;
    FUserControl: TUserControl; //changed from FUCComp to FUserControl
    FOnAppMessage : TMessageEvent;
    FTimeLeftNotify: TUCIdleTimeLeft;
    procedure UCAppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure SetUserControl(const Value: TUserControl);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override; //added by fduenas
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoIdle;
  published
    property UserControl : TUserControl read FUserControl write SetUserControl; //changed by fduenas
    property OnIdle : TNotifyEvent read FOnIdle write FOnIdle;
    property OnTimeLeftNotify : TUCIdleTimeLeft read FTimeLeftNotify write FTimeLeftNotify;
    property Timeout : Integer read FTimeOut write FTimeOut;
  end;


implementation

{ TUCIdle }

constructor TUCIdle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TUCIdle.Destroy;
begin
  FreeAndNil( FThIdle );
  inherited;
end;

procedure TUCIdle.DoIdle;
begin
  if Assigned(UserControl) and (UserControl.CurrentUser.UserID <> 0 ) then
    UserControl.Logoff;
  if Assigned(OnIdle) then OnIdle(Self);
end;

procedure TUCIdle.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    if (Assigned(UserControl)) or (Assigned(OnIdle))then
    begin
      if Assigned(Application.OnMessage) then FOnAppMessage := Application.OnMessage;
      Application.OnMessage := UCAppMessage;
      FThIdle := TThUCIdle.Create(True);
      FThIdle.CurrentMilisec := 0;
      FThIdle.UCIdle := Self;
      FThIdle.Resume;
    end;
end;

procedure TUCIdle.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  If AOperation = opRemove then
     If AComponent = FUserControl then
        FUserControl := nil;
  inherited Notification(AComponent, AOperation) ;

end;

procedure TUCIdle.SetUserControl(const Value: TUserControl);
begin
  FUserControl := Value;
  if Value <> nil then
     Value.FreeNotification(self);
end;

procedure TUCIdle.UCAppMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if (msg.message = wm_mousemove) or (msg.message = wm_keydown) then FThIdle.CurrentMilisec := 0;

  if Assigned(FOnAppMessage) then FOnAppMessage(Msg, Handled);
end;

{ TThUCIdle }

procedure TThUCIdle.DoIdle;
begin
  UCIdle.DoIdle;
end;

procedure TThUCIdle.TimeLeftSinc;
begin
  if Assigned(UCIdle.OnTimeLeftNotify) then UCIdle.OnTimeLeftNotify(UCIdle.Timeout - CurrentMilisec);
end;

procedure TThUCIdle.Execute;
begin
  while not Terminated do
  begin
    Sleep(1000);
    if UCIdle.Timeout <= CurrentMilisec then
    begin
      CurrentMilisec := 0;
      Synchronize(DoIdle);
    end else begin
      Inc(CurrentMilisec, 1000);
      Synchronize(TimeLeftSinc);
    end;
  end;
end;

end.
