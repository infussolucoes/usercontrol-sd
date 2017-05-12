object dmUC: TdmUC
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 296
  Width = 447
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=Teste')
    LoginPrompt = False
    Left = 88
    Top = 136
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
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 304
    Top = 176
  end
end
