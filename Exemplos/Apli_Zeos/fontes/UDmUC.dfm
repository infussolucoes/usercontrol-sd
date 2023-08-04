object dmUC: TdmUC
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 468
  Width = 447
  object QryBanco: TZQuery
    Connection = ZConnection1
    SQL.Strings = (
      'select * from bancos')
    Params = <>
    Options = [doCalcDefaults, doPreferPrepared]
    Left = 232
    Top = 248
  end
  object ZConnection1: TZConnection
    ControlsCodePage = cGET_ACP
    ClientCodepage = 'ISO8859_1'
    Catalog = ''
    Properties.Strings = (
      'codepage=ISO8859_1'
      'controls_cp=GET_ACP')
    TransactIsolationLevel = tiReadCommitted
    SQLHourGlass = True
    HostName = ''
    Port = 3050
    Database = ''
    User = 'SYSDBA'
    Password = 'masterkey'
    Protocol = 'firebird'
    Left = 96
    Top = 248
  end
end
