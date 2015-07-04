unit UCADOConnReg;

interface

uses
  Classes,
  UCADOConn;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SWDelphi - UC Connectors', [TUCADOConn]);
end;

end.

