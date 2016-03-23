unit UCDBISAMReg;

interface

uses Classes;

procedure Register;

implementation

uses UCDBISAMConn;

procedure Register;
begin
  RegisterComponents('UC Connectors', [TUCDBISAMConn]);
end;
end.
