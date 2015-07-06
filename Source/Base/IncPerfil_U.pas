unit IncPerfil_U;

interface

{$I 'UserControl.inc'}

uses
  {.$IFDEF DELPHI5_UP}
  Variants,
  {.$ENDIF}
  Buttons,
  Classes,
  Controls,
  DB,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  Messages,
  StdCtrls,
  SysUtils,
  Windows,

  UCBase;

type
  TfrmIncluirPerfil = class(TForm)
    PanelTitulo: TPanel;
    LbDescricao: TLabel;
    Image1: TImage;
    PanelButao: TPanel;
    btGravar: TBitBtn;
    btCancela: TBitBtn;
    PanelAddPerfil: TPanel;
    lbNome: TLabel;
    EditDescricao: TEdit;
    ComboBoxPerfil: TComboBox;
    procedure btCancelaClick(Sender: TObject);
    procedure btGravarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    IDPerfilNovo: Integer;
    PerfilDs: TDataset;
    function GetNewIdUser: Integer;
    procedure IncluiPerfil();
    procedure AlteraPerfil();
    procedure CopiaPerfil();
    { Private declarations }
  public
    FAltera: Boolean;
    FUserControl: TUserControl;
    FNewIdUser: Integer;
    FDataSetPerfilUsuario: TDataset;
  end;

implementation

{$R *.dfm}

procedure TfrmIncluirPerfil.AlteraPerfil;
begin
  with FUserControl do
  begin
    DataConnector.UCExecSQL(Format('UPDATE %s SET %s = %s WHERE %s = %d',
      [TableUsers.TableName, TableUsers.FieldUserName,
      QuotedStr(EditDescricao.Text), TableUsers.FieldUserID, FNewIdUser]));
  end;
end;

procedure TfrmIncluirPerfil.btCancelaClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmIncluirPerfil.btGravarClick(Sender: TObject);
begin
  btGravar.Enabled := False;

  if LbDescricao.Caption = 'Selecione o Perfil para Copiar' then
  begin
    CopiaPerfil; { By Cleilson Sousa }
  end
  else
  begin
    if FAltera then
    begin // alterar perfil
      AlteraPerfil;
    end
    else
    begin // inclui perfil
      IncluiPerfil;
    end;
  end;
  FDataSetPerfilUsuario.Close;
  FDataSetPerfilUsuario.Open;
  FDataSetPerfilUsuario.Locate('IDUser', FNewIdUser, []);
  Close;
end;

procedure TfrmIncluirPerfil.CopiaPerfil; { By Cleilson Sousa }
var
  IDPerfilVelho, I: Integer;
  DSPermiss, DSPermissEX: TDataset;
begin
  PerfilDs.Filtered := False;
  PerfilDs.Filter := 'Upper(' + FUserControl.TableUsers.FieldUserName +
    ') like ' + AnsiUpperCase(QuotedStr(ComboBoxPerfil.Text));
  PerfilDs.Filtered := True;

  if PerfilDs.RecordCount = 1 then
  begin
    with FUserControl do
    begin
      IDPerfilVelho := PerfilDs.FieldByName(TableUsers.FieldUserID).AsInteger;

      DSPermiss := DataConnector.UCGetSQLDataSet
        ('SELECT * FROM ' + TableRights.TableName + ' WHERE ' +
        TableRights.FieldUserID + ' = ' + IntToStr(IDPerfilVelho));

      DSPermissEX := DataConnector.UCGetSQLDataSet
        ('SELECT * FROM ' + TableRights.TableName + 'EX' + ' WHERE ' +
        TableRights.FieldUserID + ' = ' + IntToStr(IDPerfilVelho));

      with FUserControl.TableRights do
      begin
        FUserControl.DataConnector.UCExecSQL('Delete from ' + TableName +
          ' Where ' + FieldUserID + ' = ' + IntToStr(IDPerfilNovo) + ' and ' +
          FieldModule + ' = ' + QuotedStr(FUserControl.ApplicationID));
        FUserControl.DataConnector.UCExecSQL('Delete from ' + TableName +
          'EX Where ' + FieldUserID + ' = ' + IntToStr(IDPerfilNovo) + ' and ' +
          FieldModule + ' = ' + QuotedStr(FUserControl.ApplicationID));
      end;

      // Salva os dados utilizando o IDPerfilNovo pra onde fiz a copia.
      DSPermiss.First;
      for I := 1 to DSPermiss.RecordCount do
      begin
        FUserControl.AddRight(IDPerfilNovo,
          DSPermiss.FieldByName(TableRights.FieldComponentName).AsString);
        DSPermiss.Next;
      end;

      DSPermissEX.First; // Extra Rights
      for I := 1 to DSPermissEX.RecordCount do
      begin
        FUserControl.AddRightEX(IDPerfilNovo, ApplicationID,
          DSPermissEX.FieldByName(TableRights.FieldFormName).AsString,
          DSPermissEX.FieldByName(TableRights.FieldComponentName).AsString);
        DSPermissEX.Next;
      end;

    end;
  end
  else
  begin
    ShowMessage('Existe Perfis com nomes iguais');
  end;
  Close;
end;

procedure TfrmIncluirPerfil.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FreeAndNil(PerfilDs);
end;

procedure TfrmIncluirPerfil.FormShow(Sender: TObject);
var
  I: Integer;
begin
  if LbDescricao.Caption = 'Selecione o Perfil para Copiar'
  then { By Cleilson Sousa }
  begin
    EditDescricao.Visible := False;
    ComboBoxPerfil.Visible := True;

    with FUserControl do
    begin
      IDPerfilNovo := FDataSetPerfilUsuario.FieldByName('IdUser').AsInteger;

      PerfilDs := DataConnector.UCGetSQLDataSet
        ('SELECT ' + TableUsers.FieldUserID + ',' + TableUsers.FieldUserName +
        ' FROM ' + TableUsers.TableName + ' WHERE ' + TableUsers.FieldTypeRec +
        ' LIKE ''P''');

      PerfilDs.First;
      for I := 1 to PerfilDs.RecordCount do
      begin
        if PerfilDs.FieldByName(TableUsers.FieldUserID).AsInteger <> IDPerfilNovo
        then
          ComboBoxPerfil.Items.Add
            (PerfilDs.FieldByName(TableUsers.FieldUserName).AsString);

        PerfilDs.Next;
      end;
    end;
  end;
end;

function TfrmIncluirPerfil.GetNewIdUser: Integer;
var
  TempDs: TDataset;
begin
  with FUserControl do
    TempDs := DataConnector.UCGetSQLDataSet('SELECT ' + TableUsers.FieldUserID +
      ' as MaxUserID from ' + TableUsers.TableName + ' ORDER BY ' +
      TableUsers.FieldUserID + ' DESC');
  Result := TempDs.FieldByName('MaxUserID').AsInteger + 1;
  TempDs.Close;
  FreeAndNil(TempDs);
end;

procedure TfrmIncluirPerfil.IncluiPerfil;
var
  FProfile: String;
begin
  with FUserControl do
  begin
    FNewIdUser := GetNewIdUser;
    FProfile := EditDescricao.Text;

    if Assigned(onAddProfile) then
      onAddProfile(TObject(Self.Owner.Owner), FProfile);

    DataConnector.UCExecSQL
      (Format('INSERT INTO %s(%s, %s, %s) Values(%d,%s,%s)',
      [TableUsers.TableName, TableUsers.FieldUserID, TableUsers.FieldUserName,
      TableUsers.FieldTypeRec, FNewIdUser, QuotedStr(FProfile),
      QuotedStr('P')]));

  end;
end;

end.
