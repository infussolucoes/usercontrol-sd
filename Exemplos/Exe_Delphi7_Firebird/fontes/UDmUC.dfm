object dmUC: TdmUC
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 296
  Width = 447
  object IBDatabase1: TIBDatabase
    DatabaseName = 'D:\APLICATIVO_UC.FDB'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey'
      'lc_ctype=ISO8859_1')
    LoginPrompt = False
    DefaultTransaction = IBTransaction1
    AllowStreamedConnected = False
    Left = 72
    Top = 80
  end
  object IBTransaction1: TIBTransaction
    DefaultDatabase = IBDatabase1
    Left = 72
    Top = 160
  end
  object QryBanco: TIBDataSet
    Database = IBDatabase1
    Transaction = IBTransaction1
    DeleteSQL.Strings = (
      'delete from bancos'
      'where'
      '  CODIGO_B = :OLD_CODIGO_B')
    InsertSQL.Strings = (
      'insert into bancos'
      '  (CODIGO_B, NOME, CREDITO_DISPON, LIMITE)'
      'values'
      '  (:CODIGO_B, :NOME, :CREDITO_DISPON, :LIMITE)')
    RefreshSQL.Strings = (
      'Select *'
      'from bancos '
      'where'
      '  CODIGO_B = :CODIGO_B')
    SelectSQL.Strings = (
      'select * from bancos')
    ModifySQL.Strings = (
      'update bancos'
      'set'
      '  CODIGO_B = :CODIGO_B,'
      '  NOME = :NOME,'
      '  CREDITO_DISPON = :CREDITO_DISPON,'
      '  LIMITE = :LIMITE'
      'where'
      '  CODIGO_B = :OLD_CODIGO_B')
    Left = 216
    Top = 96
  end
end
