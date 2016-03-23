unit UCAstaReg;

interface

uses Classes;

procedure Register;

implementation

uses UCAstaConn;

procedure Register;
begin
  RegisterComponents('UC Connectors', [TUCAstaConn]);
end;
end.
