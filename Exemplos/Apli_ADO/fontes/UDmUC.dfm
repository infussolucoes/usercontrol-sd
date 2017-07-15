object dmUC: TdmUC
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 586
  Top = 312
  Height = 296
  Width = 447
  object ADOConnection1: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=MSDASQL.1;Persist Security Info=False;User ID=SYSDBA;Da' +
      'ta Source=Firebird;Mode=ReadWrite'
    LoginPrompt = False
    Mode = cmReadWrite
    Left = 104
    Top = 56
  end
  object ADOQuery1: TADOQuery
    Connection = ADOConnection1
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'select *  from BANCOS')
    Left = 192
    Top = 96
  end
  object ADOTable1: TADOTable
    Connection = ADOConnection1
    Left = 88
    Top = 160
  end
end
