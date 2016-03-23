unit UCFIBReg;

interface

uses Classes;

procedure Register;

implementation

uses UCFIBConn;

procedure Register;
begin
  RegisterComponents('UC Connectors', [TUCFIBConn]);
end;
end.
