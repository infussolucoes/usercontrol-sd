unit UCBDEConnReg;

interface

uses
  Classes;

procedure Register;

implementation

uses
  UCBDEConn;

procedure Register;
begin
  RegisterComponents('UC Connectors', [TUCBDEConn]);
end;

end.
