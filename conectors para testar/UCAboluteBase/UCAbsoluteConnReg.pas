unit UCAbsoluteConnReg;

interface

uses Classes;

procedure Register;

implementation

uses UCAbsoluteConn;

procedure Register;
begin
  RegisterComponents('UC Connectors', [TUCAbsoluteConn]);
end;
end.
