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
  |* 12/03/2019: Giovani Da Cruz
  |*  - Criação e distribuição da Primeira Versao para Lazarus
  ******************************************************************************* }

unit UCAbout;

interface

{$I 'UserControl.inc'}

uses
  Messages,
  Buttons,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  {$IFNDEF FPC}
  jpeg,
  {$ENDIF}
  StdCtrls;

type
  TUCAboutForm = class(TForm)
    Panel1: TPanel;
    lblVersao: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label9: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MemoAgrd: TMemo;
    Image3: TImage;
    Label12: TLabel;
    BitBtn1: TBitBtn;
    pnlFundo: TPanel;
    pnlComponentes: TPanel;
    Label10: TLabel;
    Label13: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Label5: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Label4Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure WMNChitTest(var M: TWMNchitTest); message WM_NCHITTEST;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  {$IFDEF FPC}
  {$IFDEF WINDOWS}ShellAPI, Windows,{$ELSE}LCLType,{$ENDIF}
  {$ELSE}
  ShellAPI, Windows,
  {$ENDIF}

  UCBase;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TUCAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TUCAboutForm.Label4Click(Sender: TObject);
begin
  {$IFDEF FPC}
  {$IFDEF WINDOWS}
  ShellExecute(0, 'open', 'mailto:qmd@usercontrol.com.br', '', '', SW_SHOW);
  {$ENDIF}
  {$ELSE}
  ShellExecute(0, 'open', 'mailto:qmd@usercontrol.com.br', '', '', SW_SHOW);
  {$ENDIF}
end;

procedure TUCAboutForm.Label6Click(Sender: TObject);
begin
  {$IFDEF FPC}
  {$IFDEF WINDOWS}
  ShellExecute(0, 'open', 'http://infussolucoes.github.io/usercontrol-sd', '',
    '', SW_SHOW);
  {$ENDIF}
  {$ELSE}
  ShellExecute(0, 'open', 'http://infussolucoes.github.io/usercontrol-sd', '',
    '', SW_SHOW);
  {$ENDIF}
end;

procedure TUCAboutForm.FormCreate(Sender: TObject);
begin
  Self.BorderStyle := bsNone;
  lblVersao.Caption := 'Versão ' + UCVersion;
end;

procedure TUCAboutForm.WMNChitTest(var M: TWMNchitTest);
begin
  inherited;

  if (M.result = HTCLIENT) then
    M.result := HTCAPTION;
end;

end.
