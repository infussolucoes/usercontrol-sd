unit pUCFrame_Log;

interface

{$I 'UserControl.inc'}

uses
{$IFDEF DELPHI5_UP}
{$ELSE}
  Variants,
{$ENDIF}
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  DB,
  DBGrids,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  Grids,
  ImgList,
  Messages,
  StdCtrls,
  SysUtils,
  UCBase,
  Windows;

type
  TUCFrame_Log = class(TFrame)
    DataSource1:  TDataSource;
    ImageList1:   TImageList;
    DBGrid1:      TDBGrid;
    Panel1:       TPanel;
    lbUsuario:    TLabel;
    lbData:       TLabel;
    lbNivel:      TLabel;
    Bevel3:       TBevel;
    btfiltro:     TBitBtn;
    btexclui:     TBitBtn;
    ComboUsuario: TComboBox;
    Data1:        TDateTimePicker;
    Data2:        TDateTimePicker;
    ComboNivel:   TComboBox;
    procedure ComboNivelDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure ComboUsuarioChange(Sender: TObject);
    procedure btexcluiClick(Sender: TObject);
    procedure Data1Change(Sender: TObject);
    procedure btfiltroClick(Sender: TObject);
  private
    procedure AplicaFiltro;
  public
    ListIdUser:   TStringList;
    DSLog, DSCmd: TDataset;
    FUsercontrol: TUserControl;
    procedure SetWindow;
    destructor Destroy; override;
  end;

implementation

uses
  UCDataInfo;

{$R *.dfm}
destructor TUCFrame_Log.Destroy;
begin
  FreeAndnil( DSLog );
  FreeAndnil( DSCmd );
  FreeAndNil( ListIdUser );
  inherited;
end;

procedure TUCFrame_Log.ComboNivelDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  TempImg: Graphics.TBitmap;
begin
  TempImg := Graphics.TBitmap.Create;
  Imagelist1.GetBitmap(Index, TempImg);
  ComboNivel.Canvas.Draw(Rect.Left + 5, Rect.Top + 1, TempImg);
  ComboNivel.Canvas.TextRect(Rect, Rect.Left + 30, Rect.Top + 2, ComboNivel.items[Index]);
  ComboNivel.Canvas.Draw(Rect.Left + 5, Rect.Top + 1, TempImg);
  FreeAndNil(TempImg);
end;

procedure TUCFrame_Log.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  TempImg:  Graphics.TBitmap;
  FData:    System.TDateTime;
  TempData: String;
begin
  if DSLog.IsEmpty then
    Exit;

  if UpperCase(Column.FieldName) = 'NIVEL' then
  begin
    if Column.Field.AsInteger >= 0 then
    begin
      TempImg := Graphics.TBitmap.Create;
      imagelist1.GetBitmap(Column.Field.AsInteger, TempImg);
      DbGrid1.Canvas.Draw((((Rect.Left + Rect.Right) - TempImg.Width) div 2), rect.Top, Tempimg);
      FreeAndNil(TempImg);
    end
    else
      DbGrid1.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, Column.Field.AsString);
  end
  else
  if UpperCase(Column.FieldName) = 'DATA' then
  begin
    TempData := Column.Field.AsString;
    FData    := EncodeDate(StrToInt(Copy(Tempdata, 1, 4)), StrToInt(Copy(Tempdata, 5, 2)), StrToInt(Copy(Tempdata, 7, 2))) +
      EncodeTime(StrToInt(Copy(TempData, 9, 2)), StrToInt(Copy(TempData, 11, 2)), StrToInt(Copy(TempData, 13, 2)), 0);
    DbGrid1.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, DateTimeToStr(FData));
  end
  else
    DbGrid1.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, Column.Field.AsString);
end;

procedure TUCFrame_Log.ComboUsuarioChange(Sender: TObject);
begin
  btFiltro.Enabled := True;
end;

procedure TUCFrame_Log.btexcluiClick(Sender: TObject);
var
  FTabLog, Temp: String;
begin
  //modified by fduenas
  if MessageBox(Handle, PChar(FUsercontrol.UserSettings.Log.PromptDelete),
    PChar(FUsercontrol.UserSettings.Log.PromptDelete_WindowCaption), mb_YesNo) <> mrYes then
    Exit;

  btFiltro.Enabled := False;
  FTabLog          := FUsercontrol.LogControl.TableLog;
  Temp             := 'Delete from ' + FTabLog +
    ' Where (Data >=' + QuotedStr(FormatDateTime('yyyyMMddhhmmss', data1.DateTime)) + ') ' +
    ' and (Data <=' + QuotedStr(FormatDateTime('yyyyMMddhhmmss', data2.DateTime)) + ') ' +
    ' and nivel >=' + IntToStr(ComboNivel.ItemIndex);

  if ComboUsuario.ItemIndex > 0 then
    Temp := Temp + ' and ' + FTabLog + '.idUser = ' + ListIdUser[ComboUsuario.ItemIndex];

  try
    FUsercontrol.DataConnector.UCExecSQL(Temp);
    AplicaFiltro;
    DBGrid1.Repaint;
  except
  end;

  try
    FUsercontrol.Log(Format(FUsercontrol.UserSettings.Log.DeletePerformed, [comboUsuario.Text, DateTimeToStr(Data1.datetime), DateTimeToStr(Data2.datetime), ComboNivel.Text]), 2);
  except;
  end;

end;

procedure TUCFrame_Log.Data1Change(Sender: TObject);
begin
  btFiltro.Enabled := True;
end;

procedure TUCFrame_Log.btfiltroClick(Sender: TObject);
begin
  AplicaFiltro;
end;

procedure TUCFrame_Log.AplicaFiltro;
var
  FTabUser, FTabLog: String;
  Temp:              String;
begin
  btFiltro.Enabled := False;
  DSLog.Close;
  FTabLog  := FUsercontrol.LogControl.TableLog;
  FTabUser := FUsercontrol.TableUsers.TableName;

  Temp := Format('Select TabUser.' + FUsercontrol.TableUsers.FieldUserName + ' as nome, ' + FTabLog + '.* ' +
    'from ' + FTabLog +
    '  Left outer join %s TabUser on ' + FTabLog + '.idUser = TabUser.%s ' +
    'Where (data >= ' + QuotedStr(FormatDateTime('yyyyMMddhhmmss', data1.DateTime)) + ') ' +
    'and (Data <= ' + QuotedStr(FormatDateTime('yyyyMMddhhmmss', data2.DateTime)) + ') ' +
    'and nivel >= ' + IntToStr(ComboNivel.ItemIndex),
    [FUsercontrol.TableUsers.TableName, FUsercontrol.TableUsers.FieldUserID]);

  if ComboUsuario.ItemIndex > 0 then
    Temp := Temp + ' and ' + FTabLog + '.idUser = ' + ListIdUser[ComboUsuario.ItemIndex];

  Temp := Temp + ' order by data desc';

  FreeAndNil(DSLog);
  DataSource1.DataSet := nil;
  DSLog               := FUsercontrol.DataConnector.UCGetSQLDataset(Temp);
  DataSource1.DataSet := DSLog;
  btexclui.Enabled    := not DsLog.IsEmpty;
end;

procedure TUCFrame_Log.SetWindow;
var
  TabelaLog: String;
  SQLStmt:   String;
begin
  ComboNivel.Items.Clear;
  ComboNivel.Items.Append(FUsercontrol.UserSettings.Log.OptionLevelLow);        //BGM
  ComboNivel.Items.Append(FUsercontrol.UserSettings.Log.OptionLevelNormal);     //BGM
  ComboNivel.Items.Append(FUsercontrol.UserSettings.Log.OptionLevelHigh);       //BGM
  ComboNivel.Items.Append(FUsercontrol.UserSettings.Log.OptionLevelCritic);     //BGM
  ComboNivel.ItemIndex := 0;
  ComboUsuario.Items.Clear;
  data1.Date     := EncodeDate(StrToInt(FormatDateTime('yyyy', Date)), 1, 1);
  data2.DateTime := Now;
  
  if Assigned( ListIdUser ) = False then
    ListIdUser     := TStringList.Create
  else ListIdUser.Clear;


    with FUsercontrol do
      if ((FUsercontrol.CurrentUser.Privileged = True) or
        (FUsercontrol.CurrentUser.UserLogin = FUsercontrol.Login.InitialLogin.User)) then
      begin
        DSCmd := DataConnector.UCGetSQLDataset(
          Format('SELECT %s AS IDUSER, %s AS NOME , %s AS LOGIN FROM %s WHERE %s  = %s ORDER BY %s',
          [TableUsers.FieldUserID,
          TableUsers.FieldUserName,
          TableUsers.FieldLogin,
          TableUsers.TableName,
          TableUsers.FieldTypeRec,
          QuotedStr('U'),
          TableUsers.FieldUserName]));
        ComboUsuario.Items.Append(FUsercontrol.UserSettings.Log.OptionUserAll);
        ListIdUser.Append('0');
      end
      else
        DSCmd := DataConnector.UCGetSQLDataset(
          Format('SELECT %s AS IDUSER, %s AS NOME , %s AS LOGIN FROM %s WHERE %s  = %s and %s = %s ORDER BY %s',
          [TableUsers.FieldUserID,
          TableUsers.FieldUserName,
          TableUsers.FieldLogin,
          TableUsers.TableName,
          TableUsers.FieldTypeRec,
          QuotedStr('U'),
          TableUsers.FieldLogin,
          QuotedStr(FUsercontrol.CurrentUser.UserLogin),
          TableUsers.FieldUserName]));

    while not DSCmd.EOF do
    begin
      ComboUsuario.Items.Append(DSCmd.FieldByName('Nome').AsString);
      ListIdUser.Append(DSCmd.FieldByName('idUser').AsString);
      DSCmd.Next;
    end;


  DSCmd.Close;
  FreeAndNil(DSCmd);

  ComboUsuario.ItemIndex := 0;


  TabelaLog := FUsercontrol.LogControl.TableLog;
  with FUsercontrol do
  begin
    SQLStmt := 'SELECT ' + TableUsers.TableName + '.' + TableUsers.FieldUserName + ' AS NOME, ' + TabelaLog + '.* from ' + TabelaLog +
      ' LEFT OUTER JOIN ' + TableUsers.TableName + ' on ' + TabelaLog + '.idUser = ' + TableUsers.TableName + '.' + TableUsers.FieldUserID +
      ' WHERE (DATA >=' + QuotedStr(FormatDateTime('yyyyMMddhhmmss', data1.DateTime)) + ') AND (DATA<=' + QuotedStr(FormatDateTime('yyyyMMddhhmmss', data2.DateTime)) + ') ORDER BY DATA DESC';
    DSLog   := DataConnector.UCGetSQLDataset(SQLStmt);
  end;
  DataSource1.Dataset := DSLog;
  btexclui.Enabled    := not DsLog.IsEmpty;


  with FUsercontrol.UserSettings.Log, DBGrid1 do
  begin
    lbUsuario.Caption := LabelUser;
    lbData.Caption    := LabelDate;
    lbNivel.Caption   := LabelLevel;
    BtFiltro.Caption  := BtFilter;
    BtExclui.Caption  := BtDelete;

{    Columns[0].Title.Caption := ColAppID;
    Columns[0].FieldName     := 'APPLICATIONID';
    Columns[0].Width         := 60;   }
    Columns[0].Title.Caption := ColLevel;
    Columns[0].FieldName     := 'NIVEL';
    Columns[0].Width         := 32;
    Columns[1].Title.Caption := ColMessage;
    Columns[1].FieldName     := 'MSG';
    Columns[1].Width         := 290;
    Columns[2].Title.Caption := ColUser;
    Columns[2].FieldName     := 'NOME';
    Columns[2].Width         := 120;
    Columns[3].Title.Caption := ColDate;
    Columns[3].FieldName     := 'DATA';
    Columns[3].Width         := 120;
  end;

  Bevel3.Width := Panel1.Width - 32;
  Bevel3.Left  := 16;
end;

end.
