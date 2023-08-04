object FrmCliente: TFrmCliente
  Left = 0
  Top = 0
  Caption = 'Cadastro de Clientes'
  ClientHeight = 267
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 288
    Top = 88
    Width = 303
    Height = 57
    Caption = 
      'Os bot'#245'es Incluir, alterar e excluir estar'#227'o vis'#237'veis ou n'#227'o con' +
      'forme as premiss'#245'es do usu'#225'rio.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    WordWrap = True
  end
  object BtnIncluir: TButton
    Left = 32
    Top = 40
    Width = 202
    Height = 41
    Caption = 'Incluir'
    TabOrder = 0
    OnClick = BtnIncluirClick
  end
  object BtnAlterar: TButton
    Left = 32
    Top = 103
    Width = 202
    Height = 41
    Caption = 'Alterar'
    TabOrder = 1
    OnClick = BtnIncluirClick
  end
  object BtnExcluir: TButton
    Left = 32
    Top = 168
    Width = 202
    Height = 41
    Caption = 'Excluir'
    TabOrder = 2
    OnClick = BtnIncluirClick
  end
  object UCControls1: TUCControls
    GroupName = 'Tela de Clientes'
    UserControl = FrmPrincipal.ucMyControl
    Components = ''
    NotAllowed = naDisabled
    Left = 552
    Top = 208
  end
end
