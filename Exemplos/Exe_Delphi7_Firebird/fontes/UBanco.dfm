object FrmBanco: TFrmBanco
  Left = 0
  Top = 0
  Width = 663
  Height = 399
  Caption = 'Bancos'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 655
    Height = 340
    Align = alClient
    DataSource = DSBanco
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 340
    Width = 655
    Height = 25
    DataSource = DSBanco
    Align = alBottom
    TabOrder = 1
  end
  object DSBanco: TDataSource
    DataSet = dmUC.QryBanco
    Left = 184
    Top = 104
  end
end
