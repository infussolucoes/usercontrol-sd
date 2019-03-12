{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pckSQLdbConn;

{$warn 5023 off : no warning about unused units}
interface

uses
  UCSQLdbConn, ucsqldbconnreg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ucsqldbconnreg', @ucsqldbconnreg.Register);
end;

initialization
  RegisterPackage('pckSQLdbConn', @Register);
end.
