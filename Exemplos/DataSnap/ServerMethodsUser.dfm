object SMUserControl: TSMUserControl
  OldCreateOrder = False
  OnCreate = DSServerModuleCreate
  Height = 322
  Width = 522
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=D:\APLICATIVO_UC.FDB'
      'CharacterSet=iSO8859_1'
      'Port=3050'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=FB')
    Left = 88
    Top = 136
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 88
    Top = 32
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 232
    Top = 32
  end
  object QryAux: TFDQuery
    Connection = FDConnection1
    Left = 233
    Top = 144
  end
  object ProviderAux: TDataSetProvider
    DataSet = QryAux
    Options = [poReadOnly, poUseQuoteChar]
    UpdateMode = upWhereKeyOnly
    Left = 233
    Top = 208
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 376
    Top = 24
  end
end
