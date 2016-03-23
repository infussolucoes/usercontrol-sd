unit UCMDOConnReg;

interface

uses Classes;

procedure Register;

implementation

uses UCMDOConn;

procedure Register;
begin
  RegisterComponents('UC Connectors', [TUCMDOConn]);
end;
end.
