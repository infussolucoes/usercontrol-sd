object ServerMethodDM: TServerMethodDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 402
  Width = 528
  object Server_FDConnection: TFDConnection
    Params.Strings = (
      'Database=USERCONTROL.FDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=127.0.0.1'
      'Port=3050'
      'CharacterSet=WIN1252'
      'DriverID=FB')
    FetchOptions.AssignedValues = [evCursorKind]
    FetchOptions.CursorKind = ckDefault
    UpdateOptions.AssignedValues = [uvCountUpdatedRecords]
    ConnectedStoredUsage = []
    LoginPrompt = False
    Left = 144
    Top = 63
  end
  object RESTDWDriverFD1: TRESTDWDriverFD
    CommitRecords = 100
    Connection = Server_FDConnection
    Left = 144
    Top = 132
  end
  object RESTDWPoolerFD: TRESTDWPoolerDB
    RESTDriver = RESTDWDriverFD1
    Compression = True
    Encoding = esUtf8
    StrsTrim = False
    StrsEmpty2Null = False
    StrsTrim2Len = True
    Active = True
    PoolerOffMessage = 'RESTPooler not active.'
    ParamCreate = True
    Left = 144
    Top = 207
  end
end
