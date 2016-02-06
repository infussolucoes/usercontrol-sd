object dmUC: TdmUC
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 296
  Width = 447
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
  object QryBanco: TFDQuery
    Connection = FDConnection1
    UpdateOptions.UpdateTableName = 'BANCOS'
    SQL.Strings = (
      'select * from bancos')
    Left = 232
    Top = 128
  end
end
