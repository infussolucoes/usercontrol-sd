unit UCODACConnReg;

interface

uses Classes;

procedure Register;

implementation

uses UCODACConn;

procedure Register;
begin
  RegisterComponents('UC Connectors', [TUCODACConn]);
end;
end.
