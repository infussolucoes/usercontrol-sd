unit UCNexusDBConnReg;

interface

uses Classes, UCNexusDBConn;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('UC Connectors', [TUCNexusDBConnector]);
end;
end.
