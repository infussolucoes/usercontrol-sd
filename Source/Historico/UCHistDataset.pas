unit UCHistDataset;

interface

  uses SysUtils, Classes, UCHist_Type,UCHist_Base,Forms, Db, UCConsts_Language;

Type
  TUCHist_DataSet = class(TComponent)
  private
    fDataSet: TDataSet;
    fOnNewRecord    ,
    fOnBeforeDelete ,
    fOnBeforeEdit   ,
    fOnAfterPost    : TDataSetNotifyEvent;
    fOptions: TUCHistOptions;
    fControl: TUCControlHistorico;
    procedure SetDataSet(const Value: TDataSet);
    procedure SetfControl(const Value: TUCControlHistorico);
    { Private declarations }
  protected
    DataSetInEdit : Boolean;
    AFields       : Array of Variant;
    procedure NewRecord(DataSet: TDataSet);
    procedure BeforeDelete(DataSet: TDataSet);
    procedure BeforeEdit(DataSet: TDataSet);
    procedure AfterPost(DataSet: TDataSet);
    procedure AddHistory( AppID , Form , FormCaption, Event , Obs  , TableName : String; UserId : Integer );
    Function  GetValueFields : String;
    procedure Loaded; override;    
    procedure Notification(AComponent: TComponent; AOperation: TOperation);override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Assign(Source: TPersistent); override;
    { Public declarations }
  published
    { Published declarations }
    Property DataSet          : TDataSet Read fDataSet Write SetDataSet;
    Property ControlHistorico : TUCControlHistorico read fControl write SetfControl;
  end;

implementation


procedure TUCHist_DataSet.AddHistory(AppID, Form, FormCaption, Event, Obs,
  TableName: String; UserId: Integer);
begin
   If fControl.Active then
  fControl.UserControl.DataConnector.UCExecSQL
  (
   Format('INSERT INTO %s VALUES( %s, %d , %s , %s , %s , %s ,%s ,%s , %s )',
    [ fControl.TableHistory.TableName ,
      QuotedStr(AppID),
      UserID,
      QuotedStr( FormatDateTime('dd/mm/yyyy',date) ),
      QuotedStr( FormatDateTime('hh:mm:ss',time) ),
      QuotedStr( Form ),
      QuotedStr( FormCaption ),
      QuotedStr( Event ),
      QuotedStr( Obs ) ,
      QuotedStr( TableName )
    ]));
end;

procedure TUCHist_DataSet.AfterPost(DataSet: TDataSet);
begin
  If Assigned( fOnAfterPost ) then
     fOnAfterPost( DataSet );

  If ( ( DataSetInEdit = False ) and ( fControl.Options.SavePostInsert ) ) then   // quando inserindo
    AddHistory(fControl.UserControl.ApplicationID,
               Screen.ActiveCustomForm.Name ,
               Screen.ActiveCustomForm.Caption ,
               fControl.HistoryMsg.Evento_Insert,
               GetValueFields,
               DataSet.Name,
               fControl.UserControl.CurrentUser.UserID);

  If ( ( DataSetInEdit = True ) and ( fControl.Options.SavePostEdit ) ) then   // quando editando
    AddHistory(fControl.UserControl.ApplicationID,
               Screen.ActiveCustomForm.Name ,
               Screen.ActiveCustomForm.Caption ,
               fControl.HistoryMsg.Evento_Edit,
               GetValueFields,
               DataSet.Name,
               fControl.UserControl.CurrentUser.UserID);

   DataSetInEdit := False;
   SetLength( AFields , 0 );
end;

procedure TUCHist_DataSet.Assign(Source: TPersistent);
begin
  if Source is TUCHist_DataSet then
    begin
      Self.DataSet          := TUCHist_DataSet(Source).DataSet;
      Self.ControlHistorico := TUCHist_DataSet(Source).ControlHistorico;
    end
  else
    inherited;
end;

procedure TUCHist_DataSet.BeforeDelete(DataSet: TDataSet);
begin
  If Assigned( fOnBeforeDelete ) then
     fOnBeforeDelete( DataSet );

  DataSetInEdit := False;
  SetLength( AFields , 0 );

  If fControl.Options.SaveDelete then
    AddHistory(fControl.UserControl.ApplicationID,
               Screen.ActiveCustomForm.Name ,
               Screen.ActiveCustomForm.Caption ,
               fControl.HistoryMsg.Evento_Delete,
               GetValueFields,
               DataSet.Name,
               fControl.UserControl.CurrentUser.UserID);
end;

procedure TUCHist_DataSet.BeforeEdit(DataSet: TDataSet);
Var I : Integer;
begin
  // Antes de Editar
  If Assigned( fOnBeforeEdit ) then
     fOnBeforeEdit( DataSet );

  DataSetInEdit := True;

  SetLength( AFields , DataSet.FieldCount );
  For I := 0 to DataSet.FieldCount - 1 do
    Begin
      If DataSet.Fields[ I ].IsBlob = False then
        AFields[ i ] := DataSet.Fields[ i ].Value
      else AFields[ I ] := 'Blob';
    End;
end;

constructor TUCHist_DataSet.Create(AOwner: TComponent);
begin
  inherited;
  DataSetInEdit   := False;
  fOptions        := TUCHistOptions.Create(Self);
  fDataSet        := Nil;
  fControl        := Nil;
end;

destructor TUCHist_DataSet.Destroy;
begin
  FreeAndNil( fOptions );
  inherited;
end;

function TUCHist_DataSet.GetValueFields: String;
Var Aux : Integer;
begin
  Result := '';
  For Aux := 0 to DataSet.FieldCount - 1 do
    Begin
      If DataSet.Fields[ Aux ].IsBlob = False then
        Begin
          With DataSet.Fields[ Aux ] do
            Begin
              If DataSetInEdit = false then  // inserindo ou deletando
                try Result := Result + Format('%-20s = %s ',[ FieldName , AsString ] ) + #13#10; except end
              else
                Begin //editando
                  If fControl.Options.TypeSavePostEdit = tpSaveModifiedFields then
                    Begin
                      If Value <> AFields[ Aux ] then
                      try Result := Result + Format('%s||%s||%s',[FieldNAme, AFields[ Aux ] , Value ] ) + #13#10; except end;
                    End
                  else
                    try Result := Result + Format('%s||%s||%s',[FieldNAme, AFields[ Aux ] , Value  ] )+ #13#10; except end;
                End;
            End;
        end;
    End; // for
end;

procedure TUCHist_DataSet.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
    begin
      if not Assigned( ControlHistorico ) then
        raise Exception.Create( Format( RetornaLingua( ucPortuguesBr ,'Const_Hist_MsgExceptPropr'),['ControlHistorico']) );

      If fControl.Active = false then exit;

      if not Assigned(DataSet) then
        raise Exception.Create( Format( RetornaLingua( fControl.UserControl.Language,'Const_Hist_MsgExceptPropr'),['DataSet']) );

      fOnNewRecord    := Nil;
      fOnBeforeDelete := Nil;
      fOnBeforeEdit   := Nil;
      fOnAfterPost    := Nil;

      If Assigned( DataSet.OnNewRecord ) then
        fOnNewRecord := DataSet.OnNewRecord;

      If Assigned( DataSet.BeforeDelete ) then
        fOnBeforeDelete := DataSet.BeforeDelete;

      If Assigned( DataSet.AfterPost ) then
        fOnAfterPost  := DataSet.AfterPost;

      If Assigned( DataSet.BeforeEdit ) then
        fOnBeforeEdit  := DataSet.BeforeEdit;

      DataSet.OnNewRecord  := NewRecord;
      DataSet.BeforeDelete := BeforeDelete;
      DataSet.AfterPost    := AfterPost;
      DataSet.BeforeEdit   := BeforeEdit;
     end;
end;

procedure TUCHist_DataSet.NewRecord(DataSet: TDataSet);
begin
  If Assigned( fOnNewRecord ) then
     fOnNewRecord( DataSet );

  DataSetInEdit := False; // Inserindo novo registro
  SetLength( AFields , 0 );

  If fControl.Options.SaveNewRecord then
    AddHistory( fControl.UserControl.ApplicationID,
                Screen.ActiveCustomForm.Name ,
                Screen.ActiveCustomForm.Caption,
                fControl.HistoryMsg.Evento_NewRecord,
                Format(RetornaLingua( fControl.UserControl.Language,'Const_Msg_NewRecord'),[fControl.UserControl.CurrentUser.UserName]),
                DataSet.Name,
                fControl.UserControl.CurrentUser.UserID);
end;

procedure TUCHist_DataSet.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
   if (AOperation = opRemove) then
    begin
      If AComponent = fControl then
        fControl := Nil;

      if AComponent = fDataSet then
        fDataSet := Nil;
    end;

  inherited Notification(AComponent, AOperation);
end;

procedure TUCHist_DataSet.SetDataSet(const Value: TDataSet);
begin
  fDataSet := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

procedure TUCHist_DataSet.SetfControl(const Value: TUCControlHistorico);
begin
  fControl := Value;
  if Value <> nil then
    Value.FreeNotification(self);
end;

end.
