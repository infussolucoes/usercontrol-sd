{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usu�rios }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{
Autor da vers�o Original: Rodrigo Alves Cordeiro

Colaboradores da vers�o original
Alexandre Oliveira Campioni - alexandre.rural@netsite.com.br
Bernard Grandmougin
Carlos Guerra
Daniel Wszelaki
Everton Ramos [BS2 Internet]
Francisco Due�as - fduenas@flashmail.com
Germ�n H. Cravero
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
{ Vers�o ShowDelphi Edition                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015   Giovani Da Cruz                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{ Voc� pode obter a �ltima vers�o desse arquivo na pagina do projeto           }
{ User Control ShowDelphi Edition                                              }
{ Componentes localizado em http://infussolucoes.github.io/usercontrol-sd/     }
{                                                                              }
{ Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la  }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{ Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{ Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto }
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
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
  |*  - Cria��o e distribui��o da Primeira Versao ShowDelphi
  ******************************************************************************* }

unit UserPermis_U;

interface

{$I 'UserControl.inc'}

uses

  Variants,
  Buttons,
  Classes,
  Controls,
  DB,
  DBCtrls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  Messages,
  Spin,
  StdCtrls,
  SysUtils,
  Windows,
  DBGrids,
  Grids,

  UCBase,

  ActnMan,
  ActnMenus,


  ComCtrls,

  ImgList,

  // Delphi XE 8 ou superior
  {$IFDEF DELPHI22_UP}
//      System.ImageList,
  {$ENDIF}

  Menus, System.ImageList;

type
  PTreeMenu = ^TTreeMenu;

  TTreeMenu = record
    Selecionado: Integer;
    MenuName: String;
  end;

  PTreeAction = ^TTreeAction;

  TTreeAction = record
    Grupo: Boolean;
    Selecionado: Integer;
    MenuName: String;
  end;

  PTreeControl = ^TTreeControl;

  TTreeControl = record
    Grupo: Boolean;
    Selecionado: Integer;
    CompName: String;
    FormName: String;
  end;

  TUserPermis = class(TForm)
    Panel1: TPanel;
    LbDescricao: TLabel;
    Image1: TImage;
    Panel3: TPanel;
    BtLibera: TBitBtn;
    BtBloqueia: TBitBtn;
    BtGrava: TBitBtn;
    lbUser: TLabel;
    ImageList1: TImageList;
    BtCancel: TBitBtn;
    PC: TPageControl;
    PageMenu: TTabSheet;
    PageAction: TTabSheet;
    TreeMenu: TTreeView;
    TreeAction: TTreeView;
    PageControls: TTabSheet;
    TreeControls: TTreeView;
    GroupBox1: TGroupBox;
    CBBloqueado: TCheckBox;
    CBLiberado: TCheckBox;
    procedure BtGravaClick(Sender: TObject);
    procedure TreeMenuClick(Sender: TObject);
    procedure BtCancelClick(Sender: TObject);
    procedure BtLiberaClick(Sender: TObject);
    procedure BtBloqueiaClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeActionClick(Sender: TObject);
    procedure TreeControlsClick(Sender: TObject);
    procedure TreeMenuKeyPress(Sender: TObject; var Key: char);
    procedure TreeMenuMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMenu: TMenu;
    FActions: TObject;
    FChangingTree: Boolean;
    FTempMPointer: PTreeMenu;
    FTempAPointer: PTreeAction;
    FTempCPointer: PTreeControl;
    FExtraRights: TUCExtraRights;
    FTempLista: TStringList;
    FListaAction: array of PTreeAction;
    FListaMenu: array of PTreeMenu;
    FListaControl: array of PTreeControl;
    { .$IFDEF UCACTMANAGER }
    FActionMainMenuBar: TActionMainMenuBar;
    procedure TrataItem(IT: TActionClientItem; node: TTreeNode); overload;
    { .$ENDIF }
    procedure TrataItem(IT: TMenuItem; node: TTreeNode); overload;
    procedure TreeMenuItem(marca: Boolean);
    procedure Atualiza(Selec: Boolean);
    procedure TreeActionItem(marca: Boolean);
    procedure UnCheckChild(node: TTreeNode);
    procedure TreeControlItem(marca: Boolean);
    procedure CarregaTreeviews;
  public
    FTempIdUser: Integer;
    FUserControl: TUserControl;
    DSPermiss: TDataset;
    DSPermissEX: TDataset;
    DSPerfil: TDataset;
    DSPerfilEX: TDataset;
  end;

var
  UserPermis: TUserPermis;

implementation

uses
  ActnList,
  pUCFrame_Profile;

{$R *.dfm}

procedure TUserPermis.BtGravaClick(Sender: TObject);
var
  Contador: Integer;
begin

  with FUserControl.TableRights do
  begin
    FUserControl.DataConnector.UCExecSQL('Delete from ' + TableName + ' Where '
      + FieldUserID + ' = ' + IntToStr(FTempIdUser) + ' and ' + FieldModule +
      ' = ' + QuotedStr(FUserControl.ApplicationID));
    FUserControl.DataConnector.UCExecSQL('Delete from ' + TableName +
      'EX Where ' + FieldUserID + ' = ' + IntToStr(FTempIdUser) + ' and ' +
      FieldModule + ' = ' + QuotedStr(FUserControl.ApplicationID));
  end;

  for Contador := 0 to TreeMenu.Items.Count - 1 do
    if PTreeMenu(TreeMenu.Items[Contador].Data).Selecionado = 1 then
      FUserControl.AddRight(FTempIdUser,
        PTreeMenu(TreeMenu.Items[Contador].Data).MenuName);

  for Contador := 0 to TreeAction.Items.Count - 1 do
    if PTreeAction(TreeAction.Items[Contador].Data).Selecionado = 1 then
      FUserControl.AddRight(FTempIdUser,
        PTreeAction(TreeAction.Items[Contador].Data).MenuName);

  // Extra Rights
  for Contador := 0 to Pred(TreeControls.Items.Count) do
    if PTreeControl(TreeControls.Items[Contador].Data).Selecionado = 1 then
      FUserControl.AddRightEX(FTempIdUser, FUserControl.ApplicationID,
        PTreeControl(TreeControls.Items[Contador].Data).FormName,
        PTreeControl(TreeControls.Items[Contador].Data).CompName);

  Close;
end;

procedure TUserPermis.TrataItem(IT: TMenuItem; node: TTreeNode);
var
  Contador: Integer;
  TempNode: TTreeNode;
begin
  for Contador := 0 to IT.Count - 1 do
    if IT.Items[Contador].Caption <> '-' then
      if IT.Items[Contador].Count > 0 then
      begin
        New(FTempMPointer);
        SetLength(FListaMenu, Length(FListaMenu) + 1);
        // Adicionado por Luiz 18/01/06
        FListaMenu[High(FListaMenu)] := FTempMPointer;
        // Adicionado por Luiz 18/01/06
        FTempMPointer.Selecionado := 0;
        FTempMPointer.MenuName := IT.Items[Contador].Name;
        TempNode := TreeMenu.Items.AddChildObject(node,
          StringReplace(IT.Items[Contador].Caption, '&', '', [rfReplaceAll]),
          FTempMPointer);
        TrataItem(IT.Items[Contador], TempNode);
      end
      else
      begin
        New(FTempMPointer);
        SetLength(FListaMenu, Length(FListaMenu) + 1);
        // Adicionado por Luiz 18/01/06
        FListaMenu[High(FListaMenu)] := FTempMPointer;
        // Adicionado por Luiz 18/01/06
        FTempMPointer.Selecionado := 0;
        FTempMPointer.MenuName := IT.Items[Contador].Name;
        TreeMenu.Items.AddChildObject(node,
          StringReplace(IT.Items[Contador].Caption, '&', '', [rfReplaceAll]),
          FTempMPointer);
      end;
end;

{ .$IFDEF UCACTMANAGER }
procedure TUserPermis.TrataItem(IT: TActionClientItem; node: TTreeNode);
var
  Contador: Integer;
  TempNode: TTreeNode;
begin
  for Contador := 0 to IT.Items.Count - 1 do
    if IT.Items[Contador].Caption <> '-' then
      if IT.Items[Contador].Items.Count > 0 then
      begin
        New(FTempMPointer);
        SetLength(FListaMenu, Length(FListaMenu) + 1);
        // Adicionado por Luiz 18/01/06
        FListaMenu[High(FListaMenu)] := FTempMPointer;
        // Adicionado por Luiz 18/01/06
        FTempMPointer.Selecionado := 0;
        FTempMPointer.MenuName := #1 + 'G' + IT.Items[Contador].Caption;
        TempNode := TreeMenu.Items.AddChildObject(node,
          StringReplace(IT.Items[Contador].Caption, '&', '', [rfReplaceAll]),
          FTempMPointer);
        TrataItem(IT.Items[Contador], TempNode);
      end
      else
      begin
        New(FTempMPointer);
        SetLength(FListaMenu, Length(FListaMenu) + 1);
        // Adicionado por Luiz 18/01/06
        FListaMenu[High(FListaMenu)] := FTempMPointer;
        // Adicionado por Luiz 18/01/06
        FTempMPointer.Selecionado := 0;
        FTempMPointer.MenuName := IT.Items[Contador].Action.Name;
        TreeMenu.Items.AddChildObject(node,
          StringReplace(IT.Items[Contador].Caption, '&', '', [rfReplaceAll]),
          FTempMPointer);
      end;
end;

{ .$ENDIF }

procedure TUserPermis.CarregaTreeviews;
var
  Contador: Integer;
  TempNode: TTreeNode;
  Temp: String;
  Temp2: String;
  Desc: String;
begin
  FChangingTree := False;
  PC.ActivePage := PageMenu;

  { Self.FMenu              := fUserControl.ControlRight.MainMenu;
    Self.FActionMainMenuBar := fUserControl.ControlRight.ActionMainMenuBar;
    if Assigned(fUserControl.ControlRight.ActionList) then
    Self.FActions := fUserControl.ControlRight.ActionList
    else
    Self.FActions := fUserControl.ControlRight.ActionManager; }

  Self.FMenu := FUserControl.ControlRight.MainMenu;
  Self.FActionMainMenuBar := FUserControl.ControlRight.ActionMainMenuBar;
  if Assigned(FUserControl.ControlRight.ActionList) then
    Self.FActions := FUserControl.ControlRight.ActionList
  else
    Self.FActions := FUserControl.ControlRight.ActionManager;
  Self.FExtraRights := FUserControl.ExtraRights;

  (* if (not Assigned(FMenu)) and (not Assigned(fUserControl.ControlRight.ActionList))
    {.$IFDEF UCACTMANAGER} and (not Assigned(fUserControl.ControlRight.ActionManager)) and
    (not Assigned(fUserControl.ControlRight.ActionMainMenuBar))
    {.$ENDIF} then
    begin
    if (Assigned(FMenu))
    {.$IFDEF UCACTMANAGER} and (not Assigned(fUserControl.ControlRight.ActionMainMenuBar))
    {.$ENDIF} then *)

  // TempNode := nil;
  if Assigned(FMenu) then
  begin
    TreeMenu.Items.Clear;
    for Contador := 0 to FMenu.Items.Count - 1 do
      if FMenu.Items[Contador].Count > 0 then
      begin
        New(FTempMPointer);
        SetLength(FListaMenu, Length(FListaMenu) + 1);
        // Adicionado por Luiz 18/01/06
        FListaMenu[High(FListaMenu)] := FTempMPointer;
        // Adicionado por Luiz 18/01/06
        FTempMPointer.Selecionado := 0;
        FTempMPointer.MenuName := FMenu.Items[Contador].Name;
        TempNode := TreeMenu.Items.AddObject(nil,
          StringReplace(FMenu.Items[Contador].Caption, '&', '', [rfReplaceAll]),
          FTempMPointer);
        TrataItem(FMenu.Items[Contador], TempNode);
      end
      else if FMenu.Items[Contador].Caption <> '-' then
      begin
        New(FTempMPointer);
        SetLength(FListaMenu, Length(FListaMenu) + 1);
        // Adicionado por Luiz 18/01/06
        FListaMenu[High(FListaMenu)] := FTempMPointer;
        // Adicionado por Luiz 18/01/06
        FTempMPointer.Selecionado := 0;
        FTempMPointer.MenuName := FMenu.Items[Contador].Name;
        TreeMenu.Items.AddObject(nil,
          StringReplace(FMenu.Items[Contador].Caption, '&', '', [rfReplaceAll]),
          FTempMPointer);
      end;
//    TreeAction.FullCollapse;
    TreeAction.FullExpand;  // Mauri

    TreeMenu.Perform(WM_VSCROLL, SB_TOP, 0);
  end;

  { .$IFDEF UCACTMANAGER }
  // TempNode := nil;
  if Assigned(FActionMainMenuBar) then
  begin
    TreeMenu.Items.Clear;
    for Contador := 0 to FActionMainMenuBar.ActionClient.Items.Count - 1 do
    begin
      Temp := IntToStr(Contador);
      if FActionMainMenuBar.ActionClient.Items[StrToInt(Temp)].Items.Count > 0
      then
      begin
        New(FTempMPointer);
        SetLength(FListaMenu, Length(FListaMenu) + 1);
        // Adicionado por Luiz 18/01/06
        FListaMenu[High(FListaMenu)] := FTempMPointer;
        // Adicionado por Luiz 18/01/06
        FTempMPointer.Selecionado := 0;
        FTempMPointer.MenuName := #1 + 'G' + FActionMainMenuBar.ActionClient.
          Items[StrToInt(Temp)].Caption;
        TempNode := TreeMenu.Items.AddObject(nil,
          StringReplace(FActionMainMenuBar.ActionClient.Items[StrToInt(Temp)
          ].Caption, '&', '', [rfReplaceAll]), FTempMPointer);
        TrataItem(FActionMainMenuBar.ActionClient.Items[StrToInt(Temp)],
          TempNode);
      end
      else
      begin
        if FActionMainMenuBar.ActionClient.Items[StrToInt(Temp)].Items.Count > 0
        then
        begin
          New(FTempMPointer);
          SetLength(FListaMenu, Length(FListaMenu) + 1);
          // Adicionado por Luiz 18/01/06
          FListaMenu[High(FListaMenu)] := FTempMPointer;
          // Adicionado por Luiz 18/01/06
          FTempMPointer.Selecionado := 0;
          FTempMPointer.MenuName := FActionMainMenuBar.ActionClient.Items
            [StrToInt(Temp)].Action.Name;
          TreeMenu.Items.AddObject(nil,
            StringReplace(FActionMainMenuBar.ActionClient.Items[StrToInt(Temp)
            ].Action.Name, '&', '', [rfReplaceAll]), FTempMPointer);
        end;
      end;
//      TreeAction.FullCollapse;
    TreeAction.FullExpand;  // Mauri

      TreeMenu.Perform(WM_VSCROLL, SB_TOP, 0);
    end;
  end;
  { .$ENDIF }

  (* if (Assigned(fUserControl.ControlRight.ActionList))
    {.$IFDEF UCACTMANAGER} or (Assigned(fUserControl.ControlRight.ActionManager))
    {.$ENDIF} then *)

  TempNode := nil;
  if Assigned(FActions) then
  begin
    TreeAction.Items.Clear;
    if Assigned(FTempLista) then
      FreeAndNil(FTempLista);
    FTempLista := TStringList.Create;
    for Contador := 0 to TActionList(FActions).ActionCount - 1 do
      FTempLista.Append(TActionList(FActions).Actions[Contador].Category + #1 +
        TActionList(FActions).Actions[Contador].Name + #2 +
        TAction(TActionList(FActions).Actions[Contador]).Caption);
    FTempLista.Sort;
    Temp := #1;
    for Contador := 0 to FTempLista.Count - 1 do
    begin
      if Temp <> Copy(FTempLista[Contador], 1, Pos(#1, FTempLista[Contador]) - 1)
      then
      begin
        New(FTempAPointer);
        SetLength(FListaAction, Length(FListaAction) + 1);
        // Adicionado por Luiz 18/01/06
        FListaAction[High(FListaAction)] := FTempAPointer;
        // Adicionado por Luiz 18/01/06
        FTempAPointer.Grupo := True;
        FTempAPointer.Selecionado := 0;
        FTempAPointer.MenuName := 'Grupo';
        TempNode := TreeAction.Items.AddObject(nil,
          StringReplace(Copy(FTempLista[Contador], 1,
          Pos(#1, FTempLista[Contador]) - 1), '&', '', [rfReplaceAll]),
          FTempAPointer);
        TempNode.ImageIndex := 2;
        TempNode.SelectedIndex := 2;
        Temp := Copy(FTempLista[Contador], 1,
          Pos(#1, FTempLista[Contador]) - 1);
      end;
      Temp2 := FTempLista[Contador];
      Delete(Temp2, 1, Pos(#1, Temp2));
      New(FTempAPointer);
      SetLength(FListaAction, Length(FListaAction) + 1);
      // Adicionado por Luiz 18/01/06
      FListaAction[High(FListaAction)] := FTempAPointer;
      // Adicionado por Luiz 18/01/06
      FTempAPointer.Grupo := False;
      FTempAPointer.Selecionado := 0;
      FTempAPointer.MenuName := Copy(Temp2, 1, Pos(#2, Temp2) - 1);
      Delete(Temp2, 1, Pos(#2, Temp2));
      TreeAction.Items.AddChildObject(TempNode, StringReplace(Temp2, '&', '',
        [rfReplaceAll]), FTempAPointer);
    end;
//    TreeAction.FullCollapse;
    TreeAction.FullExpand;  // Mauri

    TreeAction.Perform(WM_VSCROLL, SB_TOP, 0);
  end;

  // ExtraRights
  TempNode := nil;
  if Self.FExtraRights.Count > 0 then
  begin
    TreeControls.Items.Clear;
    if Assigned(FTempLista) then
      FreeAndNil(FTempLista);
    FTempLista := TStringList.Create;
    for Contador := 0 to Pred(FExtraRights.Count) do
      FTempLista.Append(FExtraRights[Contador].GroupName + #1 + FExtraRights
        [Contador].Caption + #2 + FExtraRights[Contador].FormName + #3 +
        FExtraRights[Contador].CompName);
    FTempLista.Sort;
    Temp := #1;
    for Contador := 0 to Pred(FTempLista.Count) do
    begin
      if Temp <> Copy(FTempLista[Contador], 1, Pos(#1, FTempLista[Contador]) - 1)
      then
      begin
        New(FTempCPointer);
        SetLength(FListaControl, Length(FListaControl) + 1);
        // Adicionado por Luiz 18/01/06
        FListaControl[High(FListaControl)] := FTempCPointer;
        // Adicionado por Luiz 18/01/06
        FTempCPointer.Grupo := True;
        FTempCPointer.Selecionado := 0;
        FTempCPointer.FormName := 'Grupo';
        FTempCPointer.CompName := 'Grupo';
        TempNode := TreeControls.Items.AddObject(nil,
          Copy(FTempLista[Contador], 1, Pos(#1, FTempLista[Contador]) - 1),
          FTempCPointer);
        TempNode.ImageIndex := 2;
        TempNode.SelectedIndex := 2;
        Temp := Copy(FTempLista[Contador], 1,
          Pos(#1, FTempLista[Contador]) - 1);
      end;
      Temp2 := FTempLista[Contador];
      Delete(Temp2, 1, Pos(#1, Temp2));
      New(FTempCPointer);
      SetLength(FListaControl, Length(FListaControl) + 1);
      // Adicionado por Luiz 18/01/06
      FListaControl[High(FListaControl)] := FTempCPointer;
      // Adicionado por Luiz 18/01/06
      FTempCPointer.Grupo := False;
      FTempCPointer.Selecionado := 0;
      Desc := Copy(Temp2, 1, Pos(#2, Temp2) - 1); // descricao do objeto
      Delete(Temp2, 1, Pos(#2, Temp2));

      FTempCPointer.FormName := Copy(Temp2, 1, Pos(#3, Temp2) - 1);
      Delete(Temp2, 1, Pos(#3, Temp2));
      FTempCPointer.CompName := Temp2;
      TreeControls.Items.AddChildObject(TempNode, Desc, FTempCPointer);
      FTempCPointer := nil;
    end;
    TreeAction.FullCollapse;
    TreeControls.Perform(WM_VSCROLL, SB_TOP, 0);
  end;

  PageMenu.TabVisible := Assigned(FMenu);

  PageAction.TabVisible := Assigned(FActions);

  PageControls.TabVisible := (Assigned(FExtraRights) and
    (FExtraRights.Count > 0));
end;

procedure TUserPermis.UnCheckChild(node: TTreeNode);
var
  child: TTreeNode;
begin
  PTreeMenu(node.Data).Selecionado := 0;
  node.ImageIndex := 0;
  node.SelectedIndex := 0;
  child := node.GetFirstChild;
  repeat
    if child.HasChildren then
      UnCheckChild(child)
    else
    begin
      PTreeMenu(child.Data).Selecionado := 0;
      child.ImageIndex := 0;
      child.SelectedIndex := 0;
    end;
    child := node.GetNextChild(child);
  until child = nil;
end;

procedure TUserPermis.TreeMenuItem(marca: Boolean);
var
  AbsIdx: Integer;
begin
  if marca then
    if PTreeMenu(TreeMenu.Selected.Data).Selecionado < 2 then
    begin
      if PTreeMenu(TreeMenu.Selected.Data).Selecionado = 0 then // marcar
      begin
        AbsIdx := TreeMenu.Selected.AbsoluteIndex;
        while AbsIdx > -1 do
        begin
          PTreeMenu(TreeMenu.Items.Item[AbsIdx].Data).Selecionado := 1;
          TreeMenu.Items.Item[AbsIdx].ImageIndex := 1;
          TreeMenu.Items.Item[AbsIdx].SelectedIndex := 1;
          if TreeMenu.Items.Item[AbsIdx].Parent <> nil then
          begin
            AbsIdx := TreeMenu.Items.Item[AbsIdx].Parent.AbsoluteIndex;
            if PTreeMenu(TreeMenu.Items.Item[AbsIdx].Data).Selecionado = 2 then
              AbsIdx := -1;
          end
          else
            AbsIdx := -1;
        end;
      end
      else if TreeMenu.Selected.HasChildren then
        UnCheckChild(TreeMenu.Selected)
      else
      begin
        PTreeMenu(TreeMenu.Selected.Data).Selecionado := 0;
        TreeMenu.Selected.ImageIndex := 0;
        TreeMenu.Selected.SelectedIndex := 0;
      end; // desmarcar
      TreeMenu.Repaint;
    end;
end;

procedure TUserPermis.TreeActionItem(marca: Boolean);
begin
  if not Assigned(FActions) then
    Exit;

  if PTreeAction(TreeAction.Selected.Data).Grupo then
    Exit;

  if marca then
  begin
    if PTreeAction(TreeAction.Selected.Data).Selecionado < 2 then
      if PTreeAction(TreeAction.Selected.Data).Selecionado = 0 then
        PTreeAction(TreeAction.Selected.Data).Selecionado := 1
      else
        PTreeAction(TreeAction.Selected.Data).Selecionado := 0;
    TreeAction.Selected.ImageIndex := PTreeAction(TreeAction.Selected.Data)
      .Selecionado;
    TreeAction.Selected.SelectedIndex := PTreeAction(TreeAction.Selected.Data)
      .Selecionado;
  end;
  TreeAction.Repaint;
end;

procedure TUserPermis.TreeControlItem(marca: Boolean);
begin
  if PTreeControl(TreeControls.Selected.Data).Grupo then
    Exit;
  if marca then
  begin
    if PTreeControl(TreeControls.Selected.Data).Selecionado < 2 then
      if PTreeControl(TreeControls.Selected.Data).Selecionado = 0 then
        PTreeControl(TreeControls.Selected.Data).Selecionado := 1
      else
        PTreeControl(TreeControls.Selected.Data).Selecionado := 0;
    TreeControls.Selected.ImageIndex := PTreeControl(TreeControls.Selected.Data)
      .Selecionado;
    TreeControls.Selected.SelectedIndex :=
      PTreeAction(TreeControls.Selected.Data).Selecionado;
  end;
  TreeControls.Repaint;
end;

procedure TUserPermis.TreeMenuClick(Sender: TObject);
begin
  if not FChangingTree then
    TreeMenuItem(True);
end;

procedure TUserPermis.BtCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TUserPermis.BtLiberaClick(Sender: TObject);
begin
  Atualiza(True);
end;

procedure TUserPermis.Atualiza(Selec: Boolean);
var
  Contador: Integer;
  Temp: Integer;
begin
  if Selec then
    Temp := 1
  else
    Temp := 0;

  if PC.ActivePage = PageMenu then
  begin
    for Contador := 0 to TreeMenu.Items.Count - 1 do
      if PTreeMenu(TreeMenu.Items[Contador].Data).Selecionado < 2 then
      begin
        PTreeMenu(TreeMenu.Items[Contador].Data).Selecionado := Temp;
        TreeMenu.Items[Contador].ImageIndex := Temp;
        TreeMenu.Items[Contador].SelectedIndex := Temp;
      end;
    TreeMenu.Repaint;
  end

  else if PC.ActivePage = PageAction then
  begin
    for Contador := 0 to TreeAction.Items.Count - 1 do
      if not PTreeAction(TreeAction.Items[Contador].Data).Grupo then
        if PTreeAction(TreeAction.Items[Contador].Data).Selecionado < 2 then
        begin
          PTreeAction(TreeAction.Items[Contador].Data).Selecionado := Temp;
          TreeAction.Items[Contador].ImageIndex := Temp;
          TreeAction.Items[Contador].SelectedIndex := Temp;
        end;
    TreeAction.Repaint;
  end

  else
  begin // tabContols
    for Contador := 0 to TreeControls.Items.Count - 1 do
      if not PTreeControl(TreeControls.Items[Contador].Data).Grupo then
        if PTreeControl(TreeControls.Items[Contador].Data).Selecionado < 2 then
        begin
          PTreeControl(TreeControls.Items[Contador].Data).Selecionado := Temp;
          TreeControls.Items[Contador].ImageIndex := Temp;
          TreeControls.Items[Contador].SelectedIndex := Temp;
        end;
    TreeControls.Repaint;
  end;
end;

procedure TUserPermis.BtBloqueiaClick(Sender: TObject);
begin
  Atualiza(False);
end;

procedure TUserPermis.FormShow(Sender: TObject);
var
  Contador: Integer;
  Selec: Integer;
begin
  // Adcionado por Luiz
  SetLength(FListaAction, 0);
  SetLength(FListaMenu, 0);
  SetLength(FListaControl, 0);

  // carrega itens do menu, actions e controles
  CarregaTreeviews;

  // Exibe Permissoes do Usuario
  for Contador := 0 to TreeAction.Items.Count - 1 do
  begin
    DSPermiss.First;
    if DSPermiss.Locate('ObjName', PTreeAction(TreeAction.Items[Contador].Data)
      .MenuName, []) then
      Selec := 1
    else
      Selec := 0;

    PTreeAction(TreeAction.Items[Contador].Data).Selecionado := Selec;
    if not PTreeAction(TreeAction.Items[Contador].Data).Grupo then
    begin
      TreeAction.Items[Contador].ImageIndex := Selec;
      TreeAction.Items[Contador].SelectedIndex := Selec;
    end;
  end;

  for Contador := 0 to TreeMenu.Items.Count - 1 do
  begin
    DSPermiss.First;
    if DSPermiss.Locate('ObjName', PTreeMenu(TreeMenu.Items[Contador].Data)
      .MenuName, []) then
      Selec := 1
    else
      Selec := 0;

    PTreeMenu(TreeMenu.Items[Contador].Data).Selecionado := Selec;
    TreeMenu.Items[Contador].ImageIndex := Selec;
    TreeMenu.Items[Contador].SelectedIndex := Selec;
  end;

  // Extra Rights
  for Contador := 0 to Pred(TreeControls.Items.Count) do
  begin
    DSPermissEX.First;
    if DSPermissEX.Locate('FormName;ObjName',
      VarArrayOf([PTreeControl(TreeControls.Items[Contador].Data).FormName,
      PTreeControl(TreeControls.Items[Contador].Data).CompName]), []) then
      Selec := 1
    else
      Selec := 0;

    PTreeControl(TreeControls.Items[Contador].Data).Selecionado := Selec;
    if not PTreeControl(TreeControls.Items[Contador].Data).Grupo then
    begin
      TreeControls.Items[Contador].ImageIndex := Selec;
      TreeControls.Items[Contador].SelectedIndex := Selec;
    end;
  end;

  // Exibe Permissoes do Perfil
  if DSPerfil.Active then
  begin
    for Contador := 0 to TreeAction.Items.Count - 1 do
    begin
      DSPerfil.First;
      if DSPerfil.Locate('ObjName', PTreeAction(TreeAction.Items[Contador].Data)
        .MenuName, []) then
      begin
        Selec := 2;
        PTreeAction(TreeAction.Items[Contador].Data).Selecionado := Selec;
        if not PTreeAction(TreeAction.Items[Contador].Data).Grupo then
        begin
          TreeAction.Items[Contador].ImageIndex := Selec;
          TreeAction.Items[Contador].SelectedIndex := Selec;
        end;
      end;
    end;

    for Contador := 0 to TreeMenu.Items.Count - 1 do
    begin
      DSPerfil.First;
      if DSPerfil.Locate('ObjName', PTreeMenu(TreeMenu.Items[Contador].Data)
        .MenuName, []) then
      begin
        Selec := 2;
        PTreeMenu(TreeMenu.Items[Contador].Data).Selecionado := Selec;
        TreeMenu.Items[Contador].ImageIndex := Selec;
        TreeMenu.Items[Contador].SelectedIndex := Selec;
      end;
    end;

    // Extra Rights
    for Contador := 0 to Pred(TreeControls.Items.Count) do
    begin
      DSPerfilEX.First;
      if DSPerfilEX.Locate('FormName;ObjName',
        VarArrayOf([PTreeControl(TreeControls.Items[Contador].Data).FormName,
        PTreeControl(TreeControls.Items[Contador].Data).CompName]), []) then
      begin
        Selec := 2;
        PTreeControl(TreeControls.Items[Contador].Data).Selecionado := Selec;
        if not PTreeControl(TreeControls.Items[Contador].Data).Grupo then
        begin
          TreeControls.Items[Contador].ImageIndex := Selec;
          TreeControls.Items[Contador].SelectedIndex := Selec;
        end;
      end;
    end;
  end;

  TreeAction.Repaint;
  TreeMenu.Repaint;
  FChangingTree := False;
  PC.ActivePageIndex := 1;
  PageAction.Caption := 'Menu Controle de Usuarios';
  PageMenu.Caption := 'Menu Controle de Usuarios';
end;

procedure TUserPermis.TreeActionClick(Sender: TObject);
begin
  if not FChangingTree then
    TreeActionItem(True);
end;

procedure TUserPermis.TreeControlsClick(Sender: TObject);
begin
  if not FChangingTree then
    TreeControlItem(True);
end;

procedure TUserPermis.TreeMenuKeyPress(Sender: TObject; var Key: char);
begin
  if Key = ' ' then
  begin
    TTreeView(Sender).OnClick(Sender);
    Key := #0;
  end;
end;

procedure TUserPermis.TreeMenuMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FChangingTree := False;
end;

procedure TUserPermis.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.Release;
end;

procedure TUserPermis.FormDestroy(Sender: TObject);
var
  Contador: Integer;
begin
  // Adicionado por Luiz 18/01/06
  if Assigned(DSPermiss) then
    FreeAndNil(DSPermiss);

  if Assigned(DSPermissEX) then
    FreeAndNil(DSPermissEX);

  if Assigned(DSPerfil) then
    FreeAndNil(DSPerfil);

  if Assigned(DSPerfilEX) then
    FreeAndNil(DSPerfilEX);

  if Assigned(FTempLista) then
    FreeAndNil(FTempLista);

  for Contador := 0 to High(FListaMenu) do
    Dispose(FListaMenu[Contador]);

  for Contador := 0 to High(FListaAction) do
    Dispose(FListaAction[Contador]);

  for Contador := 0 to High(FListaControl) do
    Dispose(FListaControl[Contador]);
end;

end.
