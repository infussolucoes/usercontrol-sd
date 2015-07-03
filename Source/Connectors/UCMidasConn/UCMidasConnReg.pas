unit UCMidasConnReg;

interface

{$I 'UserControl.inc'}

uses
  Classes,
  DesignEditors,
  DesignIntf,
  TypInfo;

type
  TUCProviderNameProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  Midas,
  UCMidasConn,
  Variants;

procedure Register;
begin
  RegisterComponents('SWDelphi - UC Connectors', [TUCMidasConn]);
  RegisterPropertyEditor(TypeInfo(String), TUCMidasConn, 'ProviderName',
    TUCProviderNameProperty);
end;

{ TUCProviderNameProperty }

function TUCProviderNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TUCProviderNameProperty.GetValues(Proc: TGetStrProc);
var
  Componente: TComponent;
  Lista: variant;
  I: Integer;
begin
  Componente := TComponent(GetComponent(0));

  if not Assigned(Componente) then
    Exit;

  if not(Componente is TUCMidasConn) then
    Exit;

  if not Assigned(TUCMidasConn(Componente).Connection) then
    Exit;

  with TUCMidasConn(Componente) do
  begin

    try
      Lista := IAppServer(Connection.GetServer).AS_GetProviderNames;
    except
    end;

    if VarIsArray(Lista) and (VarArrayDimCount(Lista) = 1) then
      for I := VarArrayLowBound(Lista, 1) to VarArrayHighBound(Lista, 1) do
        Proc(Lista[I]);
  end;
end;

end.
