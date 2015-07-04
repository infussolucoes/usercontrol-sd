unit UCFireDACConnReg;

interface

uses Classes;

procedure Register;

implementation

uses UCFireDACConn;

procedure Register;
begin
  RegisterComponents('SWDelphi - UC Connectors', [TUCFireDACConn]);
end;

end.
