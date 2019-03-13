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

unit LoginWindow_U;

interface

{$I 'UserControl.inc'}

uses
  Variants,
  Buttons,
  Classes,
  Controls,
  DB,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  math,
  Messages,
  StdCtrls,
  SysUtils,
  {$IFDEF FPC}
  {$IFDEF WINDOWS}Windows,{$ELSE}LCLType,{$ENDIF}
  {$ELSE}
  Windows,
  {$ENDIF}
  ComCtrls,

  UCBase;

type
  TfrmLoginWindow = class(TForm)
    PTop: TPanel;
    ImgTop: TImage;
    PLeft: TPanel;
    imgLeft: TImage;
    PBottom: TPanel;
    ImgBottom: TImage;
    Panel1: TPanel;
    StatusBar: TStatusBar;
    PLogin: TPanel;
    LbUsuario: TLabel;
    LbSenha: TLabel;
    lbEsqueci: TLabel;
    EditUsuario: TEdit;
    EditSenha: TEdit;
    btOK: TBitBtn;
    BtCancela: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtCancelaClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditUsuarioChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure BotoesClickVisualizacao(Sender: TObject);
    procedure btOKClick(Sender: TObject);
  public
    FUserControl: TUserControl;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TfrmLoginWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmLoginWindow.BotoesClickVisualizacao(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmLoginWindow.BtCancelaClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmLoginWindow.btOKClick(Sender: TObject);
begin
  // Aqui nao faz nada.Mas não apague.
end;

procedure TfrmLoginWindow.FormShow(Sender: TObject);
var
  w, h: Integer;
begin
  w := Max(ImgTop.Width, imgLeft.Width + PLogin.Width);
  w := Max(w, ImgBottom.Width);
  h := Max(imgLeft.Height + ImgTop.Height + ImgBottom.Height,
    ImgTop.Height + PLogin.Height + ImgBottom.Height);

  Width := w;
  Height := h + 28;
  If FUserControl.Login.MaxLoginAttempts > 0 then
  Begin
    Height := Height + 19;
    StatusBar.Panels[0].Text := FUserControl.UserSettings.Login.LabelTentativa;
    StatusBar.Panels[2].Text := FUserControl.UserSettings.Login.LabelTentativas;
  End;

  // Topo
  PTop.Height := ImgTop.Height;
  ImgTop.AutoSize := False;
  ImgTop.Align := alClient;
  ImgTop.Center := True;

  // Centro
  PLeft.Width := imgLeft.Width;
  imgLeft.AutoSize := False;
  imgLeft.Align := alClient;
  imgLeft.Center := True;

  // Bottom
  PBottom.Height := ImgBottom.Height;
  ImgBottom.AutoSize := False;
  ImgBottom.Align := alClient;
  ImgBottom.Center := True;

  PTop.Visible := ImgTop.Picture <> nil;
  PLeft.Visible := imgLeft.Picture <> nil;
  PBottom.Visible := ImgBottom.Picture <> nil;

  if FUserControl.Login.GetLoginName = lnUserName then
    EditUsuario.Text := FUserControl.GetLocalUserName;
  if FUserControl.Login.GetLoginName = lnMachineName then
    EditUsuario.Text := FUserControl.GetLocalComputerName;

  Position := Self.FUserControl.UserSettings.WindowsPosition;
  EditUsuario.CharCase := Self.FUserControl.Login.CharCaseUser;
  EditSenha.CharCase := Self.FUserControl.Login.CharCasePass;
  EditUsuario.SetFocus;
end;

procedure TfrmLoginWindow.EditUsuarioChange(Sender: TObject);
begin
  lbEsqueci.Enabled := Length(EditUsuario.Text) > 0;
end;

procedure TfrmLoginWindow.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  Begin
    Key := #0;
    if TfrmLoginWindow(Sender).ActiveControl = EditSenha then
      btOK.Click
    else
    {$IFDEF WINDOWS}
      Perform(WM_NEXTDLGCTL, 0, 0);
    {$ELSE}
      SelectNext(activecontrol,True,True);
    {$ENDIF}
  End;
end;

end.
