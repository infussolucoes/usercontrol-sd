object AboutForm: TAboutForm
  Left = 296
  Top = 71
  BorderStyle = bsNone
  Caption = 'About User Control'
  ClientHeight = 617
  ClientWidth = 495
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlFundo: TPanel
    Left = 0
    Top = 0
    Width = 495
    Height = 617
    Align = alClient
    Caption = 'pnlFundo'
    Color = 6435072
    TabOrder = 0
    object pnlComponentes: TPanel
      Left = 1
      Top = 1
      Width = 493
      Height = 615
      Align = alClient
      AutoSize = True
      BevelOuter = bvNone
      Color = 6435072
      TabOrder = 0
      object Panel1: TPanel
        Left = 0
        Top = 2
        Width = 474
        Height = 104
        BevelOuter = bvNone
        Color = clWhite
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        DesignSize = (
          474
          104)
        object lblVersao: TLabel
          Left = 396
          Top = 81
          Width = 71
          Height = 23
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'Version'
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = 6435072
          Font.Height = -19
          Font.Name = 'Verdana'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = True
          Layout = tlCenter
        end
        object Label10: TLabel
          Left = 0
          Top = 8
          Width = 304
          Height = 25
          Alignment = taCenter
          Anchors = [akLeft, akTop, akRight]
          Caption = 'User Control ShowDelphi Edition'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -21
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label13: TLabel
          Left = 16
          Top = 45
          Width = 287
          Height = 13
          Caption = 'Baseado no User Control Package que foi desenvolvido por:'
        end
        object Label1: TLabel
          Left = 16
          Top = 61
          Width = 110
          Height = 13
          Caption = 'Rodrigo Alves Cordeiro'
        end
        object Label3: TLabel
          Left = 130
          Top = 61
          Width = 37
          Height = 13
          Caption = '( QmD )'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 16
          Top = 76
          Width = 143
          Height = 13
          Cursor = crHandPoint
          Caption = 'qmd@usercontrol.com.br'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = [fsUnderline]
          ParentFont = False
          OnClick = Label4Click
        end
        object Label8: TLabel
          Left = 168
          Top = 76
          Width = 68
          Height = 13
          Caption = 'icq: 15630894'
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 109
        Width = 474
        Height = 105
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 1
        object Image3: TImage
          Left = 424
          Top = 40
          Width = 30
          Height = 16
          Cursor = crHandPoint
          AutoSize = True
          Picture.Data = {
            0A544A504547496D61676502040000FFD8FFE000104A46494600010101004800
            480000FFDB0043000302020302020303030304030304050805050404050A0707
            06080C0A0C0C0B0A0B0B0D0E12100D0E110E0B0B1016101113141515150C0F17
            1816141812141514FFDB00430103040405040509050509140D0B0D1414141414
            1414141414141414141414141414141414141414141414141414141414141414
            14141414141414141414141414FFC00011080010001E03012200021101031101
            FFC4001F0000010501010101010100000000000000000102030405060708090A
            0BFFC400B5100002010303020403050504040000017D01020300041105122131
            410613516107227114328191A1082342B1C11552D1F02433627282090A161718
            191A25262728292A3435363738393A434445464748494A535455565758595A63
            6465666768696A737475767778797A838485868788898A92939495969798999A
            A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
            D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
            01010101010101010000000000000102030405060708090A0BFFC400B5110002
            0102040403040705040400010277000102031104052131061241510761711322
            328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
            292A35363738393A434445464748494A535455565758595A636465666768696A
            737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
            A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
            E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00EC22
            F12FC42F16F88A4B2D0D5AE56355DF2150B1C63FDA63C0FA753DB3567C57F193
            5DF84FE289BC3561A769FE33D7D2DD659E5D4B52B6B445C8DC2386DD9B7C8D8C
            60E7249C2A9AF21D3BF6BCB2F873E23BA8F4ED563B8B266FDED9CE85A22D800B
            0C1043600E41F4C835E77F1CBC5FE10F8C3E3DBBF16F877C4769692DEA45F68D
            3B5589A2963748C47942AACAEA4203C9041ED5F8EF0BE1DE3388254B889B8E0B
            924E2EF51273BC6CA6E0E2D2B7335AA8B764DBD11F3B4146182F6B85E6788E65
            7524ACA3AFC09DD3E9BEBD92DCFA7BC1BF10FC45F1B6C7C4571E1FB69BC39E23
            D12758AFBC3F7243C6598B00639485DBCA499461C63AF35F3CFED1BF117C69A7
            C96561A94F3595CC3236E85D0A10707F3FAD747F07FF00695F0CFC06F056A3A6
            0F11AF88F589D93ECD0A23C96968A32482EDB5893B8FCAA368C0E72493E2BF1F
            7E3643F15351B5BFB8D45EFEE9723EE61634E48551D00E4FE75857C3E2171262
            6860A2E597A6BD9B939B7F02BD9CB571E6BD9CAEFB4A4ACCAC452A32A54653BF
            B777E7492E4DDDAFD13B5B45A79267FFD9}
        end
        object Label12: TLabel
          Left = 408
          Top = 64
          Width = 59
          Height = 13
          Cursor = crHandPoint
          Caption = 'Curitiba - PR'
        end
        object Label2: TLabel
          Left = 405
          Top = 78
          Width = 65
          Height = 13
          Cursor = crHandPoint
          Caption = 'Guapor'#233' - RS'
        end
        object Label11: TLabel
          Left = 17
          Top = 27
          Width = 360
          Height = 39
          AutoSize = False
          Caption = 
            'A vers'#227'o ShowDelphi Edition '#233' mantida e aprimorada pela comunida' +
            'de ShowDelphi. Visite www.showdelphi.com.br'
          WordWrap = True
        end
        object Label14: TLabel
          Left = 17
          Top = 63
          Width = 360
          Height = 39
          AutoSize = False
          Caption = 'Patroc'#237'nio Master: Infus Solu'#231#245'es em Tecnologia'
          WordWrap = True
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 217
        Width = 474
        Height = 392
        BevelOuter = bvNone
        TabOrder = 2
        object Label9: TLabel
          Left = 16
          Top = 9
          Width = 305
          Height = 13
          Caption = 'Cr'#233'ditos e agradecimentos do User Control Package:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 147
          Top = 336
          Width = 253
          Height = 13
          Cursor = crHandPoint
          Caption = 'http://infussolucoes.github.io/usercontrol-sd'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          OnClick = Label6Click
        end
        object Label7: TLabel
          Left = 16
          Top = 336
          Width = 125
          Height = 13
          Caption = 'Site oficial da vers'#227'o:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
        end
        object Label5: TLabel
          Left = 16
          Top = 240
          Width = 333
          Height = 13
          Caption = 'Cr'#233'ditos e agradecimentos da vers'#227'o ShowDelphi Edition:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
        end
        object MemoAgrd: TMemo
          Left = 16
          Top = 24
          Width = 457
          Height = 201
          Ctl3D = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          Lines.Strings = (
            'Alexandre Oliveira Campioni - alexandre.rural@netsite.com.br'
            'Bernard Grandmougin'
            'Carlos Guerra'
            'Daniel Wszelaki'
            'Everton Ramos [BS2 Internet]'
            'Francisco Due'#241'as - fduenas@flashmail.com'
            'Germ'#225'n H. Cravero'
            'Luciano Almeida Pimenta [ClubeDelphi.net]'
            'Luiz Benevenuto - luiz@siffra.com'
            'Luiz Fernando Severnini'
            'Peter van Mierlo'
            'Rodolfo Ferezin Moreira - rodolfo.fm@bol.com.br'
            'Rodrigo Palhano (WertherOO)'
            'Ronald Marconi  '
            'Sergiy Sekela (Dr.Web)'
            'Stefan Nawrath'
            'Vicente Barros Leonel [ Fknyght ]')
          ParentCtl3D = False
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object BitBtn1: TBitBtn
          Left = 394
          Top = 360
          Width = 75
          Height = 25
          Kind = bkOK
          NumGlyphs = 2
          TabOrder = 0
        end
        object Memo1: TMemo
          Left = 17
          Top = 256
          Width = 457
          Height = 65
          Ctl3D = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          Lines.Strings = (
            'Giovani Da Cruz - giovani@infus.inf.br'
            'Infus Solu'#231#245'es em Tecnologia - www.infus.inf.br'
            'Cassiano Baltazar - cassianobs7@gmail.com')
          ParentCtl3D = False
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 2
        end
      end
    end
  end
end
