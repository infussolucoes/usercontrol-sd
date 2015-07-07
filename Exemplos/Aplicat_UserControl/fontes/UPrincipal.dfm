object Form1: TForm1
  Left = 408
  Top = 246
  Width = 435
  Height = 175
  Caption = 'Tutorial UserControl'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object Cadastro1: TMenuItem
      Caption = 'Cadastro'
      object Bancos1: TMenuItem
        Caption = 'Bancos'
      end
      object Clientes1: TMenuItem
        Caption = 'Clientes'
      end
      object Cidades1: TMenuItem
        Caption = 'Cidades'
      end
      object Produtos1: TMenuItem
        Caption = 'Produtos'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Sair1: TMenuItem
        Caption = 'Sair'
      end
    end
    object Sistema1: TMenuItem
      Caption = 'Sistema'
      object CadastrodeUsurios1: TMenuItem
        Caption = 'Cadastro de Usu'#225'rios'
      end
      object CadastrodePerfil1: TMenuItem
        Caption = 'Cadastro de Perfil'
      end
      object RegistrodeLogins1: TMenuItem
        Caption = 'Registro de Login'#39's'
      end
      object rocarSenha1: TMenuItem
        Caption = 'Trocar Senha'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Executarlogon1: TMenuItem
        Caption = 'Conectar com outro usu'#225'rio...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Mensagens1: TMenuItem
        Caption = 'Mensagens'
      end
    end
  end
end
