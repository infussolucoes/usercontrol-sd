unit UCIBXConnReg;

interface

uses
  Classes;

procedure Register;

implementation

uses
  UCIBXConn;

procedure Register;
begin
  RegisterComponents('SWDelphi - UC Connectors', [TUCIBXConn]);
end;

end.
