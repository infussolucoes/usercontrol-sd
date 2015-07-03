unit UCDataConnector;

interface

{.$I 'UserControl.inc'}

uses
  Classes,
  DB;

type
  TUCDataConnector = class(TComponent)
  public
    procedure UCExecSQL(FSQL: String); virtual; abstract;
    function  UCGetSQLDataset(FSQL: String): TDataset; dynamic; abstract;
    function  UCFindTable(const Tablename: String): Boolean; virtual; abstract;
    function  UCFindDataConnection: Boolean; virtual; abstract;
    function  GetDBObjectName: String; virtual; abstract;
    function  GetTransObjectName: String; virtual; abstract;
  end;

implementation

end.

