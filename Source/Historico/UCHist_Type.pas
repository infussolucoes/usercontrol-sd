unit UCHist_Type;

interface

  uses SysUtils,Classes,Menus,ActnList;

Type
  TUCHistoryMSG = class(TPersistent)
  private
    fEvento_edit: String;
    fEvento_NewRecord: String;
    fEvento_Insert: String;
    fEvento_delete: String;
    fLabelTabela: String;
    fMsg_LogEmptyHistory: String;
    fMsg_MensConfirma: String;
    fLabelDescricao: String;
    fHist_BtnExcluir: String;
    fHist_BtnFiltro: String;
    fLabelForm: String;
    fHist_BtnFechar: String;
    fLabelDataEvento: String;
    fLabelEvento: String;
    fMsg_NewRecord: String;
    fHist_All: String;
    fMsg_LimpHistorico: String;
    fLabelHoraEvento: String;
    fLabelUser: String;
    fHist_MsgExceptPropr: String;
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    Property Evento_Insert       : String read fEvento_Insert write fEvento_Insert;
    Property Evento_Delete       : String read fEvento_delete write fEvento_delete;
    Property Evento_Edit         : String read fEvento_edit write fEvento_edit;
    Property Evento_NewRecord    : String read fEvento_NewRecord write fEvento_NewRecord;
    Property Hist_All            : String read fHist_All Write fHist_All ;
    Property Msg_LimpHistorico   : String read fMsg_LimpHistorico Write fMsg_LimpHistorico ;
    Property Msg_MensConfirma    : String read fMsg_MensConfirma Write fMsg_MensConfirma ;
    Property Msg_LogEmptyHistory : String read fMsg_LogEmptyHistory Write fMsg_LogEmptyHistory;
    Property LabelDescricao      : String read fLabelDescricao Write fLabelDescricao ;
    Property LabelUser           : String read fLabelUser Write fLabelUser;
    Property LabelForm           : String read fLabelForm Write fLabelForm ;
    Property LabelEvento         : String read fLabelEvento Write fLabelEvento;
    Property LabelTabela         : String read fLabelTabela Write fLabelTabela;
    Property LabelDataEvento     : String read fLabelDataEvento Write fLabelDataEvento;
    Property LabelHoraEvento     : String read fLabelHoraEvento Write fLabelHoraEvento;
    Property Msg_NewRecord       : String read fMsg_NewRecord Write fMsg_NewRecord;
    Property Hist_MsgExceptPropr : String read fHist_MsgExceptPropr write fHist_MsgExceptPropr;
    Property Hist_BtnFiltro      : String read fHist_BtnFiltro write fHist_BtnFiltro;
    Property Hist_BtnExcluir     : String read fHist_BtnExcluir write fHist_BtnExcluir;
    Property Hist_BtnFechar      : String read fHist_BtnFechar write fHist_BtnFechar;
  end;


 TUCUserHistory = class(TPersistent) // armazenar menuitem ou action responsavel pelo historico
  private
    FAction:   TAction;
    FMenuItem: TMenuItem;
    procedure SetAction(const Value: TAction);
    procedure SetMenuItem(const Value: TMenuItem);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Action: TAction read FAction write SetAction;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
  end;

 TUCTableHistorico = class(TPersistent)
  private
    FTable: String;
    FApplicationID: String;
    FUserID: String;
    fDateEvent: String;
    fFieldForm: String;
    fFieldEvent: String;
    fFieldObs: String;
    fCaptionForm: string;
    fEventTime: String;
    fFieldTableName: String;
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TableName          : String read FTable write FTable; // nome da tabela
    property FieldApplicationID : String read FApplicationID write FApplicationID;
    property FieldUserID        : String read FUserID write FUserID;
    property FieldEventDate     : String read fDateEvent write fDateEvent;
    property FieldEventTime     : String read fEventTime Write fEventTime;
    property FieldForm          : String read fFieldForm write fFieldForm;
    property FieldCaptionForm   : string read fCaptionForm write fCaptionForm;
    Property FieldEvent         : String read fFieldEvent write fFieldEvent;
    property FieldObs           : String read fFieldObs write fFieldObs;
    property FieldTableName     : String read fFieldTableName write fFieldTableName; // grava o nome da tabela monitorada
  end;

  TUCHistTypeSavePostEdit = ( tpSaveAllFields , tpSaveModifiedFields );

  TUCHistOptions = Class( TPersistent )
  private
    fSavePostEdit: Boolean;
    fSavePostInsert: Boolean;
    fSaveDelete: Boolean;
    fSaveNewRecord: Boolean;
    fTypeSave: TUCHistTypeSavePostEdit;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
     property SaveNewRecord    : Boolean read fSaveNewRecord write fSaveNewRecord;
     property SaveDelete       : Boolean read fSaveDelete write fSaveDelete;
     property SavePostInsert   : Boolean read fSavePostInsert write fSavePostInsert;
     property SavePostEdit     : Boolean read fSavePostEdit Write fSavePostEdit;
     Property TypeSavePostEdit : TUCHistTypeSavePostEdit read fTypeSave Write fTypeSave;
  end;


implementation


{ TUCHistoryMSG }

procedure TUCHistoryMSG.Assign(Source: TPersistent);
begin
  if Source is TUCHistoryMSG then
    Begin
      Self.Evento_edit         := TUCHistoryMSG(Source).Evento_edit;
      Self.Evento_NewRecord    := TUCHistoryMSG(Source).Evento_NewRecord;
      Self.Evento_Insert       := TUCHistoryMSG(Source).Evento_Insert;
      Self.Evento_delete       := TUCHistoryMSG(Source).Evento_Delete;
      Self.LabelTabela         := TUCHistoryMSG(Source).LabelTabela;
      Self.Msg_LogEmptyHistory := TUCHistoryMSG(Source).Msg_LogEmptyHistory;
      Self.Msg_MensConfirma    := TUCHistoryMSG(Source).Msg_MensConfirma;
      Self.LabelDescricao      := TUCHistoryMSG(Source).LabelDescricao;
      Self.Hist_BtnExcluir     := TUCHistoryMSG(Source).Hist_BtnExcluir;
      Self.Hist_BtnFiltro      := TUCHistoryMSG(Source).Hist_BtnFiltro;
      Self.LabelForm           := TUCHistoryMSG(Source).LabelForm;
      Self.Hist_BtnFechar      := TUCHistoryMSG(Source).Hist_BtnFechar;
      Self.LabelDataEvento     := TUCHistoryMSG(Source).LabelDataEvento;
      Self.LabelEvento         := TUCHistoryMSG(Source).LabelEvento;
      Self.Msg_NewRecord       := TUCHistoryMSG(Source).Msg_NewRecord;
      Self.Hist_All            := TUCHistoryMSG(Source).Hist_All;
      Self.Msg_LimpHistorico   := TUCHistoryMSG(Source).Msg_LimpHistorico;
      Self.LabelHoraEvento     := TUCHistoryMSG(Source).LabelHoraEvento;
      Self.LabelUser           := TUCHistoryMSG(Source).LabelUser;
      Self.Hist_MsgExceptPropr := TUCHistoryMSG(Source).Hist_MsgExceptPropr;
    End
  else
    inherited;
end;

constructor TUCHistoryMSG.Create(AOwner: TComponent);
begin
  inherited Create;
end;

destructor TUCHistoryMSG.Destroy;
begin
  inherited;
end;


//------------------------------------------------------------------------------//

{ TUCHistOptions }

procedure TUCHistOptions.Assign(Source: TPersistent);
begin
  if Source is TUCHistOptions then
  begin
    Self.SaveNewRecord    := TUCHistOptions(Source).SaveNewRecord;
    Self.SaveDelete       := TUCHistOptions(Source).SaveDelete;
    Self.SavePostInsert   := TUCHistOptions(Source).SavePostInsert;
    Self.SavePostEdit     := TUCHistOptions(Source).SavePostEdit;
    Self.TypeSavePostEdit := TUCHistOptions(Source).TypeSavePostEdit;
  end
  else
    inherited;
end;

constructor TUCHistOptions.Create(AOwner: TComponent);
begin
  fSavePostEdit   := true;
  fSavePostInsert := true;
  fSaveDelete     := true;
  fSaveNewRecord  := true;
  fTypeSave       := tpSaveAllFields;
end;

destructor TUCHistOptions.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------//

{ TUCTableHistorico }

procedure TUCTableHistorico.Assign(Source: TPersistent);
begin
  if Source is TUCTableHistorico then
  begin
    Self.FieldApplicationID := TUCTableHistorico(Source).FieldApplicationID;
    Self.FieldUserID        := TUCTableHistorico(Source).FieldUserID;
    Self.FieldEventDate     := TUCTableHistorico(Source).FieldEventDate;
    Self.TableName          := TUCTableHistorico(Source).TableName;
    Self.FieldForm          := TUCTableHistorico(Source).FieldForm;
    Self.FieldEvent         := TUCTableHistorico(Source).FieldEvent;
    Self.FieldObs           := TUCTableHistorico(Source).FieldObs;
    Self.FieldCaptionForm   := TUCTableHistorico(Source).FieldCaptionForm;
    Self.FieldEventTime     := TUCTableHistorico(Source).FieldEventTime;
    Self.FieldTableName     := TUCTableHistorico(Source).FieldTableName;
  end
  else
    inherited;
end;

constructor TUCTableHistorico.Create(AOwner: TComponent);
begin
end;

destructor TUCTableHistorico.Destroy;
begin
  inherited;
end;

{ TUCUserHistory }

procedure TUCUserHistory.Assign(Source: TPersistent);
begin
  if Source is TUCUserHistory then
  begin
    Self.MenuItem := TUCUserHistory(Source).MenuItem;
    Self.Action   := TUCUserHistory(Source).Action;
  end
  else
    inherited;
end;

constructor TUCUserHistory.Create(AOwner: TComponent);
begin
  inherited Create;
end;

destructor TUCUserHistory.Destroy;
begin
  inherited;
end;

procedure TUCUserHistory.SetAction(const Value: TAction);
begin
  FAction := Value;
  if Value <> nil then
  begin
    Self.MenuItem := nil;
    Value.FreeNotification(Self.Action);
  end;
end;

procedure TUCUserHistory.SetMenuItem(const Value: TMenuItem);
begin
  FMenuItem := Value;
  if Value <> nil then
  begin
    Self.Action := nil;
    Value.FreeNotification(Self.MenuItem);
  end;
end;

end.
