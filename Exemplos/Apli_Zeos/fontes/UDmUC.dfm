object dmUC: TdmUC
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 391
  Top = 251
  Height = 296
  Width = 447
  object con1: TZConnection
    ControlsCodePage = cCP_UTF8
    AutoEncodeStrings = False
    Properties.Strings = (
      'controls_cp=CP_UTF8')
    HostName = 'localhost'
    Port = 0
    User = 'SYSDBA'
    Password = 'masterkey'
    Protocol = 'firebird-2.5'
    LibraryLocation = 'C:\Program Files\Firebird\Firebird_2_5\bin\fbclient.dll'
    Left = 88
    Top = 136
  end
  object QryBanco: TZQuery
    Connection = con1
    SQL.Strings = (
      'select * from bancos')
    Params = <>
    Left = 200
    Top = 40
  end
end
