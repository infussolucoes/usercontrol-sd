{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pckUserControlDesign;

{$warn 5023 off : no warning about unused units}
interface

uses
  UCEditorForm_U, UCIdle, UCReg, UCAbout, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UCReg', @UCReg.Register);
end;

initialization
  RegisterPackage('pckUserControlDesign', @Register);
end.
