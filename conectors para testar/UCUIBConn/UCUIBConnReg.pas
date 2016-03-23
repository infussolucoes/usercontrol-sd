unit UCUIBConnReg;

interface

uses
  Classes;

procedure Register;

implementation

uses
  UCUIBConn;

procedure Register;
begin
  RegisterComponents('UC Connectors', [TUCUIBConn]);
end;

end.

