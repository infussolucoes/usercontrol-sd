{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usuários }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{
Autor da versão Original: Rodrigo Alves Cordeiro

Colaboradores da versão original
Alexandre Oliveira Campioni - alexandre.rural@netsite.com.br
Bernard Grandmougin
Carlos Guerra
Daniel Wszelaki
Everton Ramos [BS2 Internet]
Francisco Dueñas - fduenas@flashmail.com
Germán H. Cravero
Luciano Almeida Pimenta [ClubeDelphi.net]
Luiz Benevenuto - luiz@siffra.com
Luiz Fernando Severnini
Peter van Mierlo
Rodolfo Ferezin Moreira - rodolfo.fm@bol.com.br
Rodrigo Palhano (WertherOO)
Ronald Marconi
Sergiy Sekela (Dr.Web)
Stefan Nawrath
Vicente Barros Leonel [ Fknyght ]

*******************************************************************************}
{ Versão ShowDelphi Edition                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015   Giovani Da Cruz                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{ Você pode obter a última versão desse arquivo na pagina do projeto           }
{ User Control ShowDelphi Edition                                              }
{ Componentes localizado em http://infussolucoes.github.io/usercontrol-sd/     }
{                                                                              }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{                                                                              }
{ Comunidade Show Delphi - www.showdelphi.com.br                               }
{                                                                              }
{ Giovani Da Cruz  -  giovani@infus.inf.br  -  www.infus.inf.br                }
{                                                                              }
{ ****************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 01/07/2015: Giovani Da Cruz
  |*  - Criação e distribuição da Primeira Versao ShowDelphi
  ******************************************************************************* }
unit UCHist_Form;

interface

{$I '..\Base\UserControl.inc'}

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  UCHist_Base, Dialogs, StdCtrls, Buttons, Grids, DBGrids,
  ExtCtrls, DBCtrls,
  DB;

Type
  TAuxObj = class(TObject)
  Private
    IdUser: Integer;
  end;

type
  TFrmHistorico = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    LabelDescricao: TLabel;
    Panel2: TPanel;
    DataSource1: TDataSource;
    Panel3: TPanel;
    DBMemo1: TDBMemo;
    Splitter1: TSplitter;
    DBGrid1: TDBGrid;
    LabelUser: TLabel;
    ComboUser: TComboBox;
    LabelForm: TLabel;
    ComboForm: TComboBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    LabelEvento: TLabel;
    ComboEvento: TComboBox;
    LabelTabela: TLabel;
    ComboTabela: TComboBox;
    StringGrid1: TStringGrid;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ComboUserChange(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    DataSetHist: TDataSet;
    procedure SetComBoValues;
    { Private declarations }
  public
    fControl: TUCControlHistorico
    { Public declarations }
    end;

implementation

{$R *.dfm}

procedure TFrmHistorico.BitBtn1Click(Sender: TObject);
Var
  cSql: String;
begin
  cSql := Format('Select UH.%s as UserId, Uh.%s as Form , ' +
    'Uh.%s as Evento , Uh.%s as Obs, Uh.%s as FormCaption, Uh.%s as EventDate, Uh.%s as EventTime, uh.%s as TableName, '
    + 'US.%s As UserName from %s UH, %s US where UH.%s = US.%s and Uh.%s = %s ',
    [fControl.TableHistory.FieldUserID, fControl.TableHistory.FieldForm,
    fControl.TableHistory.FieldEvent, fControl.TableHistory.FieldObs,
    fControl.TableHistory.FieldCaptionForm,
    fControl.TableHistory.FieldEventDate, fControl.TableHistory.FieldEventTime,
    fControl.TableHistory.FieldTableName,
    fControl.UserControl.TableUsers.FieldLogin, fControl.TableHistory.TableName,
    fControl.UserControl.TableUsers.TableName,
    fControl.TableHistory.FieldUserID,
    fControl.UserControl.TableUsers.FieldUserID,
    fControl.TableHistory.FieldApplicationID,
    QuotedStr(fControl.UserControl.ApplicationID)]);

  if ComboUser.ItemIndex <> 0 then
    cSql := cSql + Format(' and US.%s = %s ',
      [fControl.UserControl.TableUsers.FieldLogin, QuotedStr(ComboUser.Text)]);

  if ComboForm.ItemIndex <> 0 then
    cSql := cSql + Format(' and UH.%s = %s ',
      [fControl.TableHistory.FieldCaptionForm, QuotedStr(ComboForm.Text)]);

  if ComboEvento.ItemIndex <> 0 then
    cSql := cSql + Format(' and UH.%s = %s ', [fControl.TableHistory.FieldEvent,
      QuotedStr(ComboEvento.Text)]);

  if ComboTabela.ItemIndex <> 0 then
    cSql := cSql + Format(' and UH.%s = %s ',
      [fControl.TableHistory.FieldTableName, QuotedStr(ComboTabela.Text)]);

  cSql := cSql + Format(' order by Uh.%s, Uh.%s',
    [fControl.TableHistory.FieldTableName,
    fControl.TableHistory.FieldEventDate]);

  DataSetHist.Close;
  DataSetHist := nil;
  DataSetHist := fControl.UserControl.DataConnector.UCGetSQLDataset(cSql);
  DataSetHist.Open;
  DataSource1.DataSet := DataSetHist;
  BitBtn1.Enabled := False;
end;

procedure TFrmHistorico.BitBtn2Click(Sender: TObject);
var
  cSql: String;
begin
  if MessageBox(Handle, pChar(fControl.Historymsg.Msg_LimpHistorico),
    pChar(fControl.Historymsg.Msg_MensConfirma), mb_YesNo) = ID_YES then
  begin
    cSql := '';

    if ComboUser.ItemIndex <> 0 then
      cSql := cSql + Format(' %s = %d ', [fControl.TableHistory.FieldUserID,
        TAuxObj(ComboUser.Items.Objects[ComboUser.ItemIndex]).IdUser]);

    if ComboForm.ItemIndex <> 0 then
      cSql := cSql + Format(' and %s = %s ',
        [fControl.TableHistory.FieldCaptionForm, QuotedStr(ComboForm.Text)]);

    if ComboEvento.ItemIndex <> 0 then
      cSql := cSql + Format(' and %s = %s ', [fControl.TableHistory.FieldEvent,
        QuotedStr(ComboEvento.Text)]);

    if ComboTabela.ItemIndex <> 0 then
      cSql := cSql + Format(' and %s = %s ',
        [fControl.TableHistory.FieldTableName, QuotedStr(ComboTabela.Text)]);

    If Length(trim(cSql)) <> 0 then
    Begin
      If UpperCase(trim(Copy(cSql, 1, 4))) = 'AND' then
        Delete(cSql, 1, 4);
      cSql := Format('delete from %s where ',
        [fControl.TableHistory.TableName]) + cSql;
    End
    else
      cSql := Format('delete from %s ',
        [fControl.TableHistory.TableName]) + cSql;
    fControl.UserControl.DataConnector.UCExecSQL(cSql);
    DataSetHist := Nil;
    SetComBoValues;

    If fControl.UserControl.LogControl.Active then
      fControl.UserControl.Log(Format(fControl.Historymsg.Msg_LogEmptyHistory,
        [fControl.UserControl.CurrentUser.UserName,
        QuotedStr(FormatDateTime('YYYYMMDDhhmmss', now))]), 3);
  end;
end;

procedure TFrmHistorico.BitBtn3Click(Sender: TObject);
begin
  Close;
end;

procedure TFrmHistorico.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmHistorico.SetComBoValues;
Var
  Aux: TAuxObj;
Begin
  Aux := TAuxObj.Create;
  Aux.IdUser := -1;

  ComboUser.Items.Clear;
  ComboForm.Items.Clear;
  ComboEvento.Items.Clear;
  ComboTabela.Items.Clear;

  ComboUser.Items.AddObject(fControl.Historymsg.Hist_All, Aux);

  ComboForm.Items.Add(fControl.Historymsg.Hist_All);

  ComboEvento.Items.Add(fControl.Historymsg.Hist_All);
  ComboEvento.Items.Add(fControl.Historymsg.Evento_Insert);
  ComboEvento.Items.Add(fControl.Historymsg.Evento_Delete);
  ComboEvento.Items.Add(fControl.Historymsg.Evento_Edit);
  ComboEvento.Items.Add(fControl.Historymsg.Evento_NewRecord);

  ComboTabela.Items.Add(fControl.Historymsg.Hist_All);

  DataSetHist := fControl.UserControl.DataConnector.UCGetSQLDataset
    (Format('Select distinct %s from %s', [fControl.TableHistory.FieldTableName,
    fControl.TableHistory.TableName]));
  while DataSetHist.Eof = False do
  Begin
    ComboTabela.Items.Add(DataSetHist.Fields[0].AsString);
    DataSetHist.Next;
  End;

  DataSetHist := nil;
  DataSetHist := fControl.UserControl.DataConnector.UCGetSQLDataset
    (Format('Select distinct %s from %s',
    [fControl.TableHistory.FieldCaptionForm, fControl.TableHistory.TableName]));
  while DataSetHist.Eof = False do
  Begin
    ComboForm.Items.Add(DataSetHist.Fields[0].AsString);
    DataSetHist.Next;
  End;

  DataSetHist := nil;
  DataSetHist := fControl.UserControl.DataConnector.UCGetSQLDataset
    (Format('Select %s, %s as Usuario from %s where %s = %s order by %s',
    [fControl.UserControl.TableUsers.FieldUserID,
    fControl.UserControl.TableUsers.FieldLogin,
    fControl.UserControl.TableUsers.TableName,
    fControl.UserControl.TableUsers.FieldTypeRec, QuotedStr('U'),
    fControl.UserControl.TableUsers.FieldLogin]));

  If fControl.UserControl.CurrentUser.Privileged = true then
  Begin
    while DataSetHist.Eof = False do
    Begin
      Aux := TAuxObj.Create;
      Aux.IdUser := DataSetHist.Fields[0].AsInteger;
      ComboUser.Items.AddObject(DataSetHist.Fields[1].AsString, Aux);
      DataSetHist.Next;
    End;
  End
  else
  begin
    Aux := TAuxObj.Create;
    Aux.IdUser := fControl.UserControl.CurrentUser.UserID;
    ComboUser.Items.AddObject(fControl.UserControl.CurrentUser.UserLogin, Aux);
  end;

  ComboUser.ItemIndex := 0;
  ComboForm.ItemIndex := 0;
  ComboEvento.ItemIndex := 0;
  ComboTabela.ItemIndex := 0;

  DataSetHist := nil;
  DataSetHist := fControl.UserControl.DataConnector.UCGetSQLDataset
    (Format('Select UH.%s as UserId, Uh.%s as Form , ' +
    'Uh.%s as Evento , Uh.%s as Obs, Uh.%s as FormCaption, Uh.%s as EventDate, Uh.%s as EventTime, uh.%s as TableName, '
    + 'US.%s As UserName from %s UH, %s US where UH.%s = US.%s and Uh.%s = %s order by Uh.%s, uh.%s',
    [fControl.TableHistory.FieldUserID, fControl.TableHistory.FieldForm,
    fControl.TableHistory.FieldEvent, fControl.TableHistory.FieldObs,
    fControl.TableHistory.FieldCaptionForm,
    fControl.TableHistory.FieldEventDate, fControl.TableHistory.FieldEventTime,
    fControl.TableHistory.FieldTableName,
    fControl.UserControl.TableUsers.FieldLogin, fControl.TableHistory.TableName,
    fControl.UserControl.TableUsers.TableName,
    fControl.TableHistory.FieldUserID,
    fControl.UserControl.TableUsers.FieldUserID,
    fControl.TableHistory.FieldApplicationID,
    QuotedStr(fControl.UserControl.ApplicationID),
    fControl.TableHistory.FieldTableName,
    fControl.TableHistory.FieldEventDate]));

  DataSource1.DataSet := DataSetHist;
end;

procedure TFrmHistorico.FormShow(Sender: TObject);
begin
  SetComBoValues;

  Self.Caption := fControl.Historymsg.LabelDescricao;
  BitBtn1.Caption := fControl.Historymsg.Hist_BtnFiltro;
  BitBtn2.Caption := fControl.Historymsg.Hist_BtnExcluir;
  BitBtn3.Caption := fControl.Historymsg.Hist_BtnFechar;

  BitBtn2.Enabled := fControl.UserControl.CurrentUser.Privileged;

  LabelDescricao.Caption := fControl.Historymsg.LabelDescricao;
  LabelUser.Caption := fControl.Historymsg.LabelUser;
  LabelForm.Caption := fControl.Historymsg.LabelForm;
  LabelEvento.Caption := fControl.Historymsg.LabelEvento;
  LabelTabela.Caption := fControl.Historymsg.LabelTabela;

  DBGrid1.Columns[0].Title.Caption := fControl.Historymsg.LabelUser;
  DBGrid1.Columns[1].Title.Caption := fControl.Historymsg.LabelForm;
  DBGrid1.Columns[2].Title.Caption := fControl.Historymsg.LabelEvento;
  DBGrid1.Columns[3].Title.Caption := fControl.Historymsg.LabelDataEvento;
  DBGrid1.Columns[4].Title.Caption := fControl.Historymsg.LabelHoraEvento;
  DBGrid1.Columns[5].Title.Caption := fControl.Historymsg.LabelTabela;

  StringGrid1.Cells[0, 0] := 'Campo';
  StringGrid1.Cells[1, 0] := 'Original';
  StringGrid1.Cells[2, 0] := 'Modificado para';
end;

procedure TFrmHistorico.ComboUserChange(Sender: TObject);
begin
  BitBtn1.Enabled := true;
end;

procedure TFrmHistorico.DataSource1DataChange(Sender: TObject; Field: TField);
Var
  List: TStringList;
  Aux: Integer;
  S: String;
begin
  DBMemo1.Visible := DataSource1.DataSet.FieldValues['Evento'] <>
    fControl.Historymsg.Evento_Edit;
  StringGrid1.Visible := DataSource1.DataSet.FieldValues['Evento']
    = fControl.Historymsg.Evento_Edit;
  If StringGrid1.Visible Then
  Begin
    try
      List := TStringList.Create;
      List.Text := DataSource1.DataSet.FieldValues['Obs'];
      StringGrid1.RowCount := List.Count + 1;
      For Aux := 0 to List.Count - 1 do
      Begin
        S := List[Aux];
        StringGrid1.Cells[0, Aux + 1] := Copy(S, 1, Pos('||', S) - 1);
        Delete(S, 1, Pos('||', S) + 1);
        StringGrid1.Cells[1, Aux + 1] := Copy(S, 1, Pos('||', S) - 1);
        Delete(S, 1, Pos('||', S) + 1);
        StringGrid1.Cells[2, Aux + 1] := S;
      End;

    finally
      FreeAndNil(List);
    end;
  End;
end;

end.
