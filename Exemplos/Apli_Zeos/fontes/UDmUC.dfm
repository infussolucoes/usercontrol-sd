object dmUC: TdmUC
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 296
  Width = 447
  object con1: TZConnection
    ControlsCodePage = cCP_UTF16
    Catalog = ''
    Connected = True
    HostName = 'localhost'
    Port = 0
    Database = 
      'D:\Usr\Comp Tokyo\UserControl\usercontrol-sd\Exemplos\Apli_Zeos\' +
      'DBase\APLICATIVO_UC_FB.FDB'
    User = 'SYSDBA'
    Password = 'masterkey'
    Protocol = 'firebirdd-3.0'
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
