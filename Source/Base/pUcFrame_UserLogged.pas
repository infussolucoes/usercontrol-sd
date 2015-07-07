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
unit pUcFrame_UserLogged;

interface

{$I 'UserControl.inc'}

uses
{$IFDEF DELPHI5}
{$ELSE}
  Variants,
{$ENDIF}
  Buttons,
  Classes,
  Controls,
  DB,
  DBGrids,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  Grids,
  Menus,
  Messages,
  StdCtrls,
  SysUtils,
  Windows,

  IncUser_U,
  UCBase;

type
  TUCFrame_UsersLogged = class(TFrame)
    dsDados: TDataSource;
    DBGrid: TDBGrid;
    Panel3: TPanel;
    BitMsg: TBitBtn;
    BitRefresh: TBitBtn;
    procedure BitRefreshClick(Sender: TObject);
    procedure BitMsgClick(Sender: TObject);
  private
    DSUserLogados: TDataset;
    UCMes: TUCApplicationMessage;
  public
    FUserControl: TUserControl;
    procedure SetWindow;
    destructor Destroy; override;
  end;

implementation

uses
  UCMessages;

{$R *.dfm}

procedure TUCFrame_UsersLogged.SetWindow;
var
  SQLStmt: String;
  I: Integer;
  Form: TForm;
begin
  UCMes := nil;
  Form := Application.MainForm;
  for I := 0 to Form.ComponentCount - 1 do
    if (Form.Components[I] is TUCApplicationMessage) then
      UCMes := TUCApplicationMessage(Form.Components[I]);
  BitMsg.Visible := UCMes <> nil;

  with FUserControl do
  begin
    SQLStmt := 'SELECT U.' + TableUsers.FieldUserName + ' AS UserName,' +
      '       U.' + TableUsers.FieldUserId + ' AS id, ' + '       U.' +
      TableUsers.FieldLogin + ' AS Login,' + '       L.' +
      TableUsersLogged.FieldMachineName + ' AS MachineName,' + '       L.' +
      TableUsersLogged.FieldData + ' AS DATA ' + 'FROM ' +
      TableUsersLogged.TableName + ' L ' + '    INNER JOIN ' +
      TableUsers.TableName + ' U ON U.' + TableUsers.FieldUserId + ' = L.' +
      TableUsersLogged.FieldUserId + '    LEFT  JOIN ' + TableUsers.TableName +
      ' P ON P.' + TableUsers.FieldUserId + ' = U.' + TableUsers.FieldProfile +
      ' ' + 'WHERE L.' + TableUsersLogged.FieldApplicationID + ' = ' +
      QuotedStr(ApplicationID);

    DSUserLogados := DataConnector.UCGetSQLDataset(SQLStmt);

    with UserSettings.UsersLogged do
    begin
      Caption := LabelCaption;
      BitMsg.Caption := BtnMessage;
      BitRefresh.Caption := BtnRefresh;

      DBGrid.Columns[0].Title.Caption := ColName;
      DBGrid.Columns[1].Title.Caption := ColLogin;
      DBGrid.Columns[2].Title.Caption := ColComputer;
      DBGrid.Columns[3].Title.Caption := ColData;
    end;

  end;
  dsDados.Dataset := DSUserLogados;
end;

procedure TUCFrame_UsersLogged.BitRefreshClick(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    dsDados.Dataset.Close;
    dsDados.Dataset.Open;
  finally
    Screen.Cursor := crDefault;
  end;
end;

destructor TUCFrame_UsersLogged.Destroy;
begin
  FreeAndNil(DSUserLogados);
  FreeAndNil(UCMes);
  inherited;
end;

procedure TUCFrame_UsersLogged.BitMsgClick(Sender: TObject);
var
  Msg: String;
begin
  if Assigned(UCMes) then
    if InputQuery(FUserControl.UserSettings.UsersLogged.InputText,
      FUserControl.UserSettings.UsersLogged.InputCaption, Msg) = True then
      UCMes.SendAppMessage(dsDados.Dataset.FieldValues['id'],
        FUserControl.UserSettings.UsersLogged.MsgSystem, Msg);
end;

end.
