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
unit UCHist_Base;

interface

uses
  SysUtils,
  Classes, 
  Forms,
  UCConsts_Language,
  UCHist_Type,
  UCBase;

type
  TUCControlHistorico = class(TComponent)
  private
    fUserControl: TUserControl;
    fActive: Boolean;
    fOptions: TUCHistOptions;
    fTableHistory: TUCTableHistorico;
    fUsersHistory: TUCUserHistory;
    fHistoryMsg: TUCHistoryMSG;
    procedure SetfUserControl(const Value: TUserControl);
    { Private declarations }
  protected
    FrmHistorico: TCustomForm;
    procedure Loaded; override;
    procedure IniSettings(Language: TUCLanguage);
    procedure ActionUserHistory(Sender: TObject);
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Public declarations }
  published
    property Active: Boolean read fActive write fActive default True;
    property UserControl: TUserControl read fUserControl Write SetfUserControl;
    Property Options: TUCHistOptions read fOptions write fOptions;
    property TableHistory: TUCTableHistorico read fTableHistory
      write fTableHistory;
    property UsersHistory: TUCUserHistory read fUsersHistory
      write fUsersHistory;
    property HistoryMsg: TUCHistoryMSG read fHistoryMsg Write fHistoryMsg;
    { Published declarations }
  end;

procedure Register;

implementation

uses UCHistDataset, UCHist_Form;

{$R UcHistReg.dcr}

procedure Register;
begin
  RegisterComponents('SWDelphi - UC Main',
    [TUCHist_DataSet, TUCControlHistorico]);
end;

{ TUCControlHistorico }

procedure TUCControlHistorico.Assign(Source: TPersistent);
begin
  if Source is TUCHist_DataSet then
  begin
    TUCHistOptions(Source).Assign(Options);
    TUCTableHistorico(Source).Assign(TableHistory);
    TUCUserHistory(Source).Assign(UsersHistory);
    TUCHistoryMSG(Source).Assign(HistoryMsg);
  end
  else
    inherited
end;

constructor TUCControlHistorico.Create(AOwner: TComponent);
begin
  inherited;
  fUserControl := Nil;
  Active := True;
  fOptions := TUCHistOptions.Create(Self);
  fTableHistory := TUCTableHistorico.Create(Self);
  fUsersHistory := TUCUserHistory.Create(Self);
  fHistoryMsg := TUCHistoryMSG.Create(Self);
  if csDesigning in ComponentState then
    IniSettings(ucPortuguesBr);
end;

destructor TUCControlHistorico.Destroy;
begin
  fUserControl := Nil;
  FreeAndNil(fOptions);
  FreeAndNil(fTableHistory);
  FreeAndNil(fUsersHistory);
  FreeAndNil(fHistoryMsg);
  inherited;
end;

procedure TUCControlHistorico.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    if not Assigned(fUserControl) then
      raise Exception.Create(Format(RetornaLingua(ucPortuguesBr,
        'Const_Hist_MsgExceptPropr'), ['UserControl']));

    IniSettings(fUserControl.Language);
    If Active then
      If not fUserControl.DataConnector.UCFindTable(TableHistory.TableName) then
        fUserControl.DataConnector.UCExecSQL
          (Format('CREATE TABLE %s ( %s %s(250), %s %s , %s %s(10), %s %s(8), %s %s(250), %s %s(100), %s %s(50) , %s %s, %s %s(50) )  ',
          [TableHistory.TableName, TableHistory.FieldApplicationID,
          fUserControl.UserSettings.Type_VarChar,

          TableHistory.FieldUserID, fUserControl.UserSettings.Type_Int,

          TableHistory.FieldEventDate, fUserControl.UserSettings.Type_Char,

          TableHistory.FieldEventTime, fUserControl.UserSettings.Type_Char,

          TableHistory.FieldForm, fUserControl.UserSettings.Type_VarChar,

          TableHistory.FieldCaptionForm, fUserControl.UserSettings.Type_VarChar,

          TableHistory.FieldEvent, fUserControl.UserSettings.Type_VarChar,

          TableHistory.FieldObs, fUserControl.UserSettings.Type_Memo,

          TableHistory.FieldTableName,
          fUserControl.UserSettings.Type_VarChar]));

    if Assigned(fUsersHistory.MenuItem) and
      (not Assigned(fUsersHistory.MenuItem.OnClick)) then
      fUsersHistory.MenuItem.OnClick := ActionUserHistory;

    if Assigned(fUsersHistory.Action) and
      (not Assigned(fUsersHistory.Action.OnExecute)) then
      fUsersHistory.Action.OnExecute := ActionUserHistory;
  end;
end;

procedure TUCControlHistorico.SetfUserControl(const Value: TUserControl);
begin
  fUserControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self.UserControl);
end;

procedure TUCControlHistorico.IniSettings(Language: TUCLanguage);
Begin
  With HistoryMsg do
  Begin
    Evento_Insert := RetornaLingua(Language, 'Const_Evento_Insert');
    Evento_Delete := RetornaLingua(Language, 'Const_Evento_Delete');
    Evento_Edit := RetornaLingua(Language, 'Const_Evento_Edit');
    Evento_NewRecord := RetornaLingua(Language, 'Const_Evento_NewRecord');
    Hist_All := RetornaLingua(Language, 'Const_Hist_All');
    Msg_LimpHistorico := RetornaLingua(Language, 'Const_Msg_LimpHistorico');
    Msg_MensConfirma := RetornaLingua(Language, 'Const_Msg_MensConfirma');
    Msg_LogEmptyHistory := RetornaLingua(Language, 'Const_Msg_LogEmptyHistory');
    LabelDescricao := RetornaLingua(Language, 'Const_LabelDescricao');
    LabelUser := RetornaLingua(Language, 'Const_LabelUser');
    LabelForm := RetornaLingua(Language, 'Const_LabelForm');
    LabelEvento := RetornaLingua(Language, 'const_LabelEvento');
    LabelTabela := RetornaLingua(Language, 'const_LabelTabela');
    LabelDataEvento := RetornaLingua(Language, 'const_LabelDataEvento');
    LabelHoraEvento := RetornaLingua(Language, 'const_LabelHoraEvento');
    Msg_NewRecord := RetornaLingua(Language, 'const_Msg_NewRecord');
    Hist_MsgExceptPropr := RetornaLingua(Language, 'Const_Hist_MsgExceptPropr');
    Hist_BtnFiltro := RetornaLingua(Language, 'const_Hist_BtnFiltro');
    Hist_BtnExcluir := RetornaLingua(Language, 'const_Hist_BtnExcluir');
    Hist_BtnFechar := RetornaLingua(Language, 'const_Hist_BtnFechar');
  End;

  With TableHistory do
  Begin
    TableName := RetornaLingua(Language, 'Const_Hist_TableName');
    FieldApplicationID := RetornaLingua(Language,
      'Const_Hist_FieldApplicationID');
    FieldUserID := RetornaLingua(Language, 'Const_Hist_FieldUserID');
    FieldEventDate := RetornaLingua(Language, 'Const_Hist_FieldEventDate');
    FieldEventTime := RetornaLingua(Language, 'Const_Hist_FieldEventTime');
    FieldForm := RetornaLingua(Language, 'Const_Hist_FieldForm');
    FieldCaptionForm := RetornaLingua(Language, 'Const_Hist_FieldCaptionForm');
    FieldEvent := RetornaLingua(Language, 'Const_Hist_FieldEvent');
    FieldObs := RetornaLingua(Language, 'Const_Hist_FieldObs');
    FieldTableName := RetornaLingua(Language, 'Const_Hist_FieldTableName');
  End;
End;

procedure TUCControlHistorico.ActionUserHistory(Sender: TObject);
Begin
  FrmHistorico := TFrmHistorico.Create(Self);
  TFrmHistorico(FrmHistorico).fControl := Self;
  TFrmHistorico(FrmHistorico).Position :=
    fUserControl.UserSettings.WindowsPosition;
  FrmHistorico.ShowModal;
  FreeAndNil(FrmHistorico);
End;

end.
