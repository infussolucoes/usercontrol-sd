{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usu�rios }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{
Autor da vers�o Original: Rodrigo Alves Cordeiro

Colaboradores da vers�o original
Alexandre Oliveira Campioni - alexandre.rural@netsite.com.br
Bernard Grandmougin
Carlos Guerra
Daniel Wszelaki
Everton Ramos [BS2 Internet]
Francisco Due�as - fduenas@flashmail.com
Germ�n H. Cravero
Luciano Almeida Pimenta [ClubeDelphi.net]
Luiz Benevenuto - luiz@siffra.com
Luiz Fernando Severnini
Peter van Mierlo
Rodolfo Ferezin Moreira - rodolfo.fm@bol.com.br
Rodrigo Palhano (WertherOO)
Ronald Marconi
Sergiy Sekela (Dr.Web)
Stefan Nawrath
Vicente Barros Leonel [ Fknyght ]

*******************************************************************************}
{ Vers�o ShowDelphi Edition                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015   Giovani Da Cruz                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{ Voc� pode obter a �ltima vers�o desse arquivo na pagina do projeto           }
{ User Control ShowDelphi Edition                                              }
{ Componentes localizado em http://infussolucoes.github.io/usercontrol-sd/     }
{                                                                              }
{ Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la  }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{ Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{ Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto }
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{                                                                              }
{ Comunidade Show Delphi - www.showdelphi.com.br                               }
{                                                                              }
{ Giovani Da Cruz  -  giovani@infus.inf.br  -  www.infus.inf.br                }
{                                                                              }
{ ****************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 01/07/2015: Giovani Da Cruz
  |*  - Cria��o e distribui��o da Primeira Versao ShowDelphi
  ******************************************************************************* }
unit UCDataInfo;

interface

uses
  Classes, UCSettings;

type
  TUCCriptografia = (cPadrao, cMD5);

  TUCTableUsers = class(TPersistent)
  private
    FEmail: String;
    FTypeRec: String;
    FUserID: String;
    FPrivileged: String;
    FUserName: String;
    FTable: String;
    FProfile: String;
    FLogin: String;
    FPassword: String;
    FKey: String;
    fDateExpired: String;
    fUserExpired: String;
    fFieldUserDaysSun: String;
    fFieldUserInative: String;
    FFieldImage: string;
    FUserSettings: TUCUserSettings;
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetFieldList: TStringList;
    function GetFieldType(const FieldName: string; const Criptografia: TUCCriptografia): string;
    property UserSettings: TUCUserSettings read FUserSettings write FUserSettings;
  published
    property FieldUserID: String read FUserID write FUserID;
    property FieldUserName: String read FUserName write FUserName;
    property FieldLogin: String read FLogin write FLogin;
    property FieldPassword: String read FPassword write FPassword;
    property FieldEmail: String read FEmail write FEmail;
    property FieldPrivileged: String read FPrivileged write FPrivileged;
    property FieldTypeRec: String read FTypeRec write FTypeRec;
    property FieldProfile: String read FProfile write FProfile;
    property FieldKey: String read FKey write FKey;
    property FieldDateExpired: String read fDateExpired write fDateExpired;
    { By Vicente Barros Leonel }
    property FieldUserExpired: String read fUserExpired write fUserExpired;
    { By vicente barros leonel }
    property FieldUserDaysSun: String read fFieldUserDaysSun write fFieldUserDaysSun;
    { By vicente barros leonel }
    property FieldUserInative: String read fFieldUserInative write fFieldUserInative;
    { By vicente barros leonel }
    property TableName: String read FTable write FTable;
    property FieldImage: string read FFieldImage write FFieldimage;
  end;

  TUCTableRights = class(TPersistent)
  private
    FUserID: String;
    FFormName: String;
    FModule: String;
    FTable: String;
    FComponentName: String;
    FKey: String;
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property FieldUserID: String read FUserID write FUserID;
    property FieldModule: String read FModule write FModule;
    property FieldComponentName: String read FComponentName
      write FComponentName;
    property FieldFormName: String read FFormName write FFormName;
    property FieldKey: String read FKey write FKey;
    property TableName: String read FTable write FTable;
  end;

  TUCTableUsersLogged = class(TPersistent)
  private
    FTableName: String;
    FData: String;
    FApplicationID: String;
    FUserID: String;
    FLogonID: String;
    FMachineName: String;
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { published declarations }
    property FieldLogonID: String read FLogonID write FLogonID;
    property FieldUserID: String read FUserID write FUserID;
    property FieldApplicationID: String read FApplicationID
      write FApplicationID;
    property FieldMachineName: String read FMachineName write FMachineName;
    property FieldData: String read FData write FData;
    property TableName: String read FTableName write FTableName;
  end;

  TUCTableHistorico = class(TPersistent)
  private
    FTable: String;
    FApplicationID: String;
    FUserID: String;
    fDateEvent: String;
    fFieldForm: String;
    fFieldEvent: String;
    fFieldObs: String;
    fCaptionForm: string;
    fEventTime: String;
    fFieldTableName: String;
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TableName: String read FTable write FTable; // nome da tabela
    property FieldApplicationID: String read FApplicationID
      write FApplicationID;
    property FieldUserID: String read FUserID write FUserID;
    property FieldEventDate: String read fDateEvent write fDateEvent;
    property FieldEventTime: String read fEventTime Write fEventTime;
    property FieldForm: String read fFieldForm write fFieldForm;
    property FieldCaptionForm: string read fCaptionForm write fCaptionForm;
    Property FieldEvent: String read fFieldEvent write fFieldEvent;
    property FieldObs: String read fFieldObs write fFieldObs;
    property FieldTableName: String read fFieldTableName write fFieldTableName;
    // grava o nome da tabela monitorada
  end;

implementation

{ TUCTableRights }

procedure TUCTableRights.Assign(Source: TPersistent);
begin
  if Source is TUCTableRights then
  begin
    Self.FieldUserID := TUCTableRights(Source).FieldUserID;
    Self.FieldModule := TUCTableRights(Source).FieldModule;
    Self.FieldComponentName := TUCTableRights(Source).FieldComponentName;
    Self.FieldFormName := TUCTableRights(Source).FieldFormName;
    Self.FieldKey := TUCTableRights(Source).FieldKey;
  end
  else
    inherited;
end;

constructor TUCTableRights.Create(AOwner: TComponent);
begin
  inherited Create;
end;

destructor TUCTableRights.Destroy;
begin

  inherited;
end;

{ TUCTableUsers }

procedure TUCTableUsers.Assign(Source: TPersistent);
begin
  if Source is TUCTableUsers then
  begin
    Self.FieldUserID := TUCTableUsers(Source).FieldUserID;
    Self.FieldUserName := TUCTableUsers(Source).FieldUserName;
    Self.FieldLogin := TUCTableUsers(Source).FieldLogin;
    Self.FieldPassword := TUCTableUsers(Source).FieldPassword;
    Self.FieldEmail := TUCTableUsers(Source).FieldEmail;
    Self.FieldPrivileged := TUCTableUsers(Source).FieldPrivileged;
    Self.FieldProfile := TUCTableUsers(Source).FieldProfile;
    Self.FieldKey := TUCTableUsers(Source).FieldKey;
    Self.FieldDateExpired := TUCTableUsers(Source).FieldDateExpired;
    { By Vicente Barros Leonel }
    Self.FieldUserExpired := TUCTableUsers(Source).FieldUserExpired;
    { By Vicente Barros Leonel }
    Self.FieldUserDaysSun := TUCTableUsers(Source).FieldUserDaysSun;
    { By vicente barros leonel }
    Self.FieldUserInative := TUCTableUsers(Source).FieldUserInative;
    { By vicente barros leonel }
    Self.TableName := TUCTableUsers(Source).TableName;
    Self.FieldImage := TUCTableUsers(Source).FieldImage;
  end
  else
    inherited;
end;

constructor TUCTableUsers.Create(AOwner: TComponent);
begin
  inherited Create;
end;

destructor TUCTableUsers.Destroy;
begin
  inherited;
end;

function TUCTableUsers.GetFieldList: TStringList;
begin
  Result := TStringList.Create;
  Result.Add(FieldUserID);
  Result.Add(FieldUserName);
  Result.Add(FieldLogin);
  Result.Add(FieldPassword);
  Result.Add(FieldEmail);
  Result.Add(FieldPrivileged);
  Result.Add(FieldTypeRec);
  Result.Add(FieldProfile);
  Result.Add(FieldKey);
  Result.Add(FieldDateExpired);
  Result.Add(FieldUserExpired);
  Result.Add(FieldUserDaysSun);
  Result.Add(FieldUserInative);
  Result.Add(FieldImage);
end;

function TUCTableUsers.GetFieldType(const FieldName: string; const Criptografia: TUCCriptografia): string;
var
  TipoCampo: string;
begin
  case Criptografia of
    cPadrao:
      TipoCampo := UserSettings.Type_VarChar + '(250)';
    cMD5:
      TipoCampo := UserSettings.Type_VarChar + '(32)';
  end;

  if FieldName = FieldUserID then
    Result := UserSettings.Type_Int;
  if FieldName = FieldUserName then
    Result := UserSettings.Type_VarChar;
  if FieldName = FieldLogin then
    Result := UserSettings.Type_VarChar;
  if FieldName = FieldPassword then
    Result := TipoCampo;
  if FieldName = FieldDateExpired then
    Result := UserSettings.Type_Char;
  if FieldName = FieldUserExpired then
    Result := UserSettings.Type_Int;
  if FieldName = FieldUserDaysSun then
    Result := UserSettings.Type_Int;
  if FieldName = FieldEmail then
    Result := UserSettings.Type_VarChar;
  if FieldName = FieldPrivileged then
    Result := UserSettings.Type_Int;
  if FieldName = FieldTypeRec then
    Result := UserSettings.Type_Char;
  if FieldName = FieldProfile then
    Result := UserSettings.Type_Int;
  if FieldName = FieldKey then
    Result := TipoCampo;
  if FieldName = FieldUserInative then
    Result := UserSettings.Type_Int;
  if FieldName = FieldImage then
    Result := UserSettings.Type_Blob; //Type_Memo;
end;

{ TUCTableUsersLogged }

procedure TUCTableUsersLogged.Assign(Source: TPersistent);
begin
  if Source is TUCTableUsersLogged then
  begin
    Self.FieldLogonID := TUCTableUsersLogged(Source).FieldLogonID;
    Self.FieldUserID := TUCTableUsersLogged(Source).FieldUserID;
    Self.FieldApplicationID := TUCTableUsersLogged(Source).FieldApplicationID;
    Self.FieldMachineName := TUCTableUsersLogged(Source).FieldMachineName;
    Self.FieldData := TUCTableUsersLogged(Source).FieldData;
    Self.TableName := TUCTableUsersLogged(Source).TableName;
  end
  else
    inherited;
end;

constructor TUCTableUsersLogged.Create(AOwner: TComponent);
begin

end;

destructor TUCTableUsersLogged.Destroy;
begin
  inherited;
end;

{ TUCTableHistorico }

procedure TUCTableHistorico.Assign(Source: TPersistent);
begin
  if Source is TUCTableHistorico then
  begin
    Self.FieldApplicationID := TUCTableHistorico(Source).FieldApplicationID;
    Self.FieldUserID := TUCTableHistorico(Source).FieldUserID;
    Self.FieldEventDate := TUCTableHistorico(Source).FieldEventDate;
    Self.TableName := TUCTableHistorico(Source).TableName;
    Self.FieldForm := TUCTableHistorico(Source).FieldForm;
    Self.FieldEvent := TUCTableHistorico(Source).FieldEvent;
    Self.FieldObs := TUCTableHistorico(Source).FieldObs;
    Self.FieldCaptionForm := TUCTableHistorico(Source).FieldCaptionForm;
    Self.FieldEventTime := TUCTableHistorico(Source).FieldEventTime;
    Self.FieldTableName := TUCTableHistorico(Source).FieldTableName;
  end
  else
    inherited;
end;

constructor TUCTableHistorico.Create(AOwner: TComponent);
begin
  inherited Create;
end;

destructor TUCTableHistorico.Destroy;
begin
  inherited;
end;

end.
