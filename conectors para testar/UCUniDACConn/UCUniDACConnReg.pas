unit UCUniDACConnReg;

interface

uses Classes;

procedure Register;

implementation

uses UCUniDACConn;

procedure Register;
begin
  RegisterComponents('UC Connectors', [TUCUniDACConn]);
end;
end.
