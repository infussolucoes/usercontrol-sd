object FrmCliente: TFrmCliente
  Left = 0
  Top = 0
  Width = 643
  Height = 333
  Caption = 'Clientes'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnIncluir: TButton
    Left = 72
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Incluir'
    TabOrder = 0
  end
  object BtnAlterar: TButton
    Left = 159
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Alterar'
    TabOrder = 1
  end
  object BtnExcluir: TButton
    Left = 248
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Excluir'
    TabOrder = 2
  end
  object UCControls1: TUCControls
    GroupName = 'Tela de Clientes'
    UserControl = FrmPrincipal.ucMyControl
    NotAllowed = naDisabled
    Left = 384
    Top = 144
  end
end
