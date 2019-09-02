object dmUC: TdmUC
  OldCreateOrder = False
  Height = 296
  Width = 447
  object RESTDWDataBase1: TRESTDWDataBase
    Active = False
    Compression = True
    CriptOptions.Use = False
    CriptOptions.Key = 'RDWBASEKEY256'
    Login = 'usercontrol'
    Password = 'showdelphi'
    Proxy = False
    ProxyOptions.Port = 8888
    PoolerService = '127.0.0.1'
    PoolerPort = 8082
    PoolerName = 'TServerMethodDM.RESTDWPoolerFD'
    StateConnection.AutoCheck = False
    StateConnection.InTime = 1000
    RequestTimeOut = 10000
    EncodeStrings = True
    Encoding = esUtf8
    StrsTrim = False
    StrsEmpty2Null = False
    StrsTrim2Len = True
    ParamCreate = True
    FailOver = False
    FailOverConnections = <>
    FailOverReplaceDefaults = False
    ClientConnectionDefs.Active = False
    Left = 128
    Top = 104
  end
  object QryBanco: TRESTDWClientSQL
    Active = False
    Filtered = False
    FieldDefs = <
      item
        Name = 'CODIGO_B'
        DataType = ftInteger
      end
      item
        Name = 'NOME'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'CREDITO_DISPON'
        DataType = ftSingle
        Precision = 7
      end
      item
        Name = 'LIMITE'
        DataType = ftSingle
        Precision = 7
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    MasterCascadeDelete = True
    BinaryRequest = False
    Datapacks = -1
    DataCache = False
    Params = <>
    DataBase = RESTDWDataBase1
    SQL.Strings = (
      'SELECT * FROM BANCOS')
    UpdateTableName = 'BANCOS'
    CacheUpdateRecords = True
    AutoCommitData = True
    AutoRefreshAfterCommit = False
    RaiseErrors = False
    ActionCursor = crSQLWait
    ReflectChanges = False
    Left = 280
    Top = 104
  end
end
