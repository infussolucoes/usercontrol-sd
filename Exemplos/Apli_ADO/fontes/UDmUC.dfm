object dmUC: TdmUC
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 296
  Width = 447
  object ADOConnection1: TADOConnection
    ConnectionString = 
      'Provider=MSDASQL.1;Persist Security Info=False;User ID=SYSDBA;Da' +
      'ta Source=Fire;Dbname=192.168.227.3:D:\Usr\Comp Tokyo\UserContro' +
      'l\usercontrol-sd\Exemplos\Apli_ADO\DBase\APLICATIVO_UC_FB.FDB;'
    LoginPrompt = False
    Mode = cmReadWrite
    Provider = 'MSDASQL.1'
    Left = 208
    Top = 144
  end
end
