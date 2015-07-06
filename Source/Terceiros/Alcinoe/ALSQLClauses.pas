{ *************************************************************
  www:          http://sourceforge.net/projects/alcinoe/
  svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
  Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
  Sponsor(s):   Arkadia SA (http://www.arkadia.com)

  product:      SQL Parser Functions
  Version:      4.00

  Description:  SQL function to create easily sql string without
  take care if it's an update or insert sql Statement.
  just add the value in a tstringList like
  fieldname=value and at the end contruct the sql string

  Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

  This software is provided 'as-is', without any express
  or implied warranty.  In no event will the author be
  held liable for any  damages arising from the use of
  this software.

  Permission is granted to anyone to use this software
  for any purpose, including commercial applications,
  and to alter it and redistribute it freely, subject
  to the following restrictions:

  1. The origin of this software must not be
  misrepresented, you must not claim that you wrote
  the original software. If you use this software in
  a product, an acknowledgment in the product
  documentation would be appreciated but is not
  required.

  2. Altered source versions must be plainly marked as
  such, and must not be misrepresented as being the
  original software.

  3. This notice may not be removed or altered from any
  source distribution.

  4. You must register this software by sending a picture
  postcard to the author. Use a nice stamp and mention
  your name, street address, EMail address and any
  comment you like to say.

  Know bug :

  History :     20/04/2006: Add plan property
  26/06/2012: Add xe2 support

  Link :

  * Please send all your feedback to alcinoe@arkadia.com
  * If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
  * Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
  ************************************************************** }
unit ALSQLClauses;

interface

{$IF CompilerVersion >= 25} { Delphi XE4 }
{$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
  System.Contnrs,
{$ELSE}
  Contnrs,
{$IFEND}
  AlStringList,
  ALFbxClient;

Type

  { ------------------------------------------------------------------------ }
  TALSQLClauseUpdateKind = (alDelete, alUpdate, AlInsert, AlUpdateOrInsert);
  TALSQLClauseServerType = (alFirebird, AlSphinx, AlMySql);

  { --------------------------------- }
  TAlSelectSQLClause = Class(Tobject)
  public
    ServerType: TALSQLClauseServerType;
    First: integer;
    Skip: integer;
    Distinct: Boolean;
    Select: TALStrings;
    Where: TALStrings;
    From: TALStrings;
    Join: TALStrings;
    GroupBy: TALStrings;
    Having: TALStrings;
    Plan: ansiString; // dedicated to Firebird
    OrderBy: TALStrings;
    Customs: TALStrings;
    FBXClientSQLParams: TALFBXClientSQLParams; // dedicated to Firebird
    Constructor Create; Virtual;
    Destructor Destroy; override;
    Procedure clear; virtual;
    procedure Assign(Source: TAlSelectSQLClause); virtual;
    function SQLText: ansiString; virtual;
    function FBXClientSelectDataSQL(const ViewTag, RowTag: ansiString)
      : TALFBXClientSelectDataQUERY; overload;
    function FBXClientSelectDataSQL(const RowTag: ansiString)
      : TALFBXClientSelectDataQUERY; overload;
    function FBXClientSelectDataSQL: TALFBXClientSelectDataQUERY; overload;
  end;

  { -------------------------------------- }
  TAlSelectSQLClauses = class(TObjectList)
  private
  protected
    function GetItems(Index: integer): TAlSelectSQLClause;
    procedure SetItems(Index: integer; aSelectSQLClause: TAlSelectSQLClause);
  public
    function Add(aSelectSQLClause: TAlSelectSQLClause): integer;
    function Extract(Item: TAlSelectSQLClause): TAlSelectSQLClause;
    function Remove(aSelectSQLClause: TAlSelectSQLClause): integer;
    function IndexOf(aSelectSQLClause: TAlSelectSQLClause): integer;
    function First: TAlSelectSQLClause;
    function Last: TAlSelectSQLClause;
    procedure Insert(Index: integer; aSelectSQLClause: TAlSelectSQLClause);
    property Items[Index: integer]: TAlSelectSQLClause read GetItems
      write SetItems; default;
    function FBXClientSelectDataQUERIES(const ViewTag, RowTag: ansiString)
      : TALFBXClientSelectDataQUERIES; overload;
    function FBXClientSelectDataQUERIES(const RowTag: ansiString)
      : TALFBXClientSelectDataQUERIES; overload;
    function FBXClientSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
      overload;
  end;

  { --------------------------------- }
  TALUpdateSQLClause = Class(Tobject)
    ServerType: TALSQLClauseServerType;
    Kind: TALSQLClauseUpdateKind;
    Table: ansiString;
    Value: TALStrings;
    Where: TALStrings;
    Customs: TALStrings;
    FBXClientSQLParams: TALFBXClientSQLParams; // dedicated to Firebird
    Constructor Create; Virtual;
    Destructor Destroy; override;
    Procedure clear; virtual;
    procedure Assign(Source: TALUpdateSQLClause); virtual;
    function SQLText: ansiString; virtual;
    function FBXClientUpdateDataSQL(const ViewTag, RowTag: ansiString)
      : TALFBXClientUpdateDataQUERY; overload;
    function FBXClientUpdateDataSQL(const RowTag: ansiString)
      : TALFBXClientUpdateDataQUERY; overload;
    function FBXClientUpdateDataSQL: TALFBXClientUpdateDataQUERY; overload;
  end;

  { -------------------------------------- }
  TAlUpdateSQLClauses = class(TObjectList)
  private
  protected
    function GetItems(Index: integer): TALUpdateSQLClause;
    procedure SetItems(Index: integer; aUpdateSQLClause: TALUpdateSQLClause);
  public
    function Add(aUpdateSQLClause: TALUpdateSQLClause): integer;
    function Extract(Item: TALUpdateSQLClause): TALUpdateSQLClause;
    function Remove(aUpdateSQLClause: TALUpdateSQLClause): integer;
    function IndexOf(aUpdateSQLClause: TALUpdateSQLClause): integer;
    function First: TALUpdateSQLClause;
    function Last: TALUpdateSQLClause;
    procedure Insert(Index: integer; aUpdateSQLClause: TALUpdateSQLClause);
    property Items[Index: integer]: TALUpdateSQLClause read GetItems
      write SetItems; default;
    function FBXClientUpdateDataQUERIES(const ViewTag, RowTag: ansiString)
      : TALFBXClientUpdateDataQUERIES; overload;
    function FBXClientUpdateDataQUERIES(const RowTag: ansiString)
      : TALFBXClientUpdateDataQUERIES; overload;
    function FBXClientUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES;
      overload;
  end;

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
  System.Classes,
  System.Types,
{$ELSE}
  Classes,
  Types,
{$IFEND}
  AlString;

{ ************************************ }
constructor TAlSelectSQLClause.Create;
Begin
  ServerType := alFirebird;
  First := -1;
  Skip := -1;
  Distinct := False;
  Select := TALStringList.Create;
  Where := TALStringList.Create;
  From := TALStringList.Create;
  Join := TALStringList.Create;
  GroupBy := TALStringList.Create;
  Having := TALStringList.Create;
  Plan := '';
  OrderBy := TALStringList.Create;
  Customs := TALStringList.Create;
  setlength(FBXClientSQLParams, 0);
end;

{ ************************************ }
destructor TAlSelectSQLClause.Destroy;
begin
  Select.free;
  Where.free;
  From.free;
  Join.free;
  GroupBy.free;
  Having.free;
  OrderBy.free;
  Customs.free;
  inherited;
end;

{ ************************************************************** }
procedure TAlSelectSQLClause.Assign(Source: TAlSelectSQLClause);
begin
  ServerType := Source.ServerType;
  First := Source.First;
  Skip := Source.Skip;
  Distinct := Source.Distinct;
  Select.Assign(Source.Select);
  Where.Assign(Source.Where);
  From.Assign(Source.From);
  Join.Assign(Source.Join);
  GroupBy.Assign(Source.GroupBy);
  Having.Assign(Source.Having);
  Plan := Source.Plan;
  OrderBy.Assign(Source.OrderBy);
  Customs.Assign(Source.Customs);
  FBXClientSQLParams := Source.FBXClientSQLParams;
end;

{ ********************************************** }
function TAlSelectSQLClause.SQLText: ansiString;
Var
  Flag: Boolean;
  i: integer;
  S: ansiString;
Begin

  // start
  Result := 'Select ';

  // first + skip (if server type = ALFirebird)
  if ServerType = alFirebird then
  begin
    if First >= 0 then
      Result := Result + 'first ' + ALIntToStr(First) + ' ';
    if Skip >= 0 then
      Result := Result + 'skip ' + ALIntToStr(Skip) + ' ';
  end;

  // distinct
  If Distinct then
    Result := Result + 'distinct ';

  // Select
  Flag := False;
  For i := 0 to Select.Count - 1 do
  begin
    If ALTrim(Select[i]) <> '' then
    begin
      Flag := True;
      Result := Result + ALTrim(Select[i]) + ', ';
    end;
  end;
  IF not Flag then
    Result := Result + '*'
  else
    Delete(Result, length(Result) - 1, 2);

  // From
  Result := Result + ' From ';
  For i := 0 to From.Count - 1 do
    If ALTrim(From[i]) <> '' then
      Result := Result + ALTrim(From[i]) + ', ';
  Delete(Result, length(Result) - 1, 2);

  // join
  For i := 0 to Join.Count - 1 do
    If ALTrim(Join[i]) <> '' then
      Result := Result + ' ' + ALTrim(Join[i]);

  // Where
  If Where.Count > 0 then
  begin
    S := '';
    For i := 0 to Where.Count - 1 do
      If ALTrim(Where[i]) <> '' then
      begin
        if ServerType <> AlSphinx then
          S := S + '(' + ALTrim(Where[i]) + ') and '
        else
          S := S + ALTrim(Where[i]) + ' and ';
      end;
    If S <> '' then
    begin
      Delete(S, length(S) - 4, 5);
      Result := Result + ' Where ' + S;
    end;
  end;

  // group by
  If GroupBy.Count > 0 then
  begin
    S := '';
    For i := 0 to GroupBy.Count - 1 do
      If ALTrim(GroupBy[i]) <> '' then
        S := S + ALTrim(GroupBy[i]) + ' and ';
    If S <> '' then
    begin
      Delete(S, length(S) - 4, 5);
      Result := Result + ' Group by ' + S;
    end;
  end;

  // Having
  If Having.Count > 0 then
  begin
    S := '';
    For i := 0 to Having.Count - 1 do
      If ALTrim(Having[i]) <> '' then
        S := S + ALTrim(Having[i]) + ' and ';
    If S <> '' then
    begin
      Delete(S, length(S) - 4, 5);
      Result := Result + ' Having ' + S;
    end;
  end;

  // Plan
  if (ServerType = alFirebird) then
  begin
    If ALTrim(Plan) <> '' then
      Result := Result + ' ' + ALTrim(Plan);
  end;

  // order by
  If OrderBy.Count > 0 then
  begin
    S := '';
    For i := 0 to OrderBy.Count - 1 do
      If ALTrim(OrderBy[i]) <> '' then
        S := S + ALTrim(OrderBy[i]) + ', ';
    If S <> '' then
    begin
      Delete(S, length(S) - 1, 2);
      Result := Result + ' Order by ' + S;
    end;
  end;

  // first + skip (if server type = ALSphinx)
  if (ServerType in [AlSphinx, AlMySql]) then
  begin
    if (First >= 0) and (Skip >= 0) then
      Result := Result + ' Limit ' + ALIntToStr(Skip) + ', ' + ALIntToStr(First)
      // With two arguments, the first argument specifies the offset of the first row to return, and the second specifies the maximum number of rows to return
    else if (Skip >= 0) then
      Result := Result + ' Limit ' + ALIntToStr(Skip) + ', ' +
        ALIntToStr(Maxint)
      // To retrieve all rows from a certain offset up to the end of the result set, you can use some large number for the second parameter.
    else if (First >= 0) then
      Result := Result + ' Limit 0, ' + ALIntToStr(First)
  end;

End;

{ ***************************************************************************************************************** }
function TAlSelectSQLClause.FBXClientSelectDataSQL(const ViewTag,
  RowTag: ansiString): TALFBXClientSelectDataQUERY;
begin
  Result.SQL := SQLText;
  Result.Params := FBXClientSQLParams;
  Result.RowTag := RowTag;
  Result.ViewTag := ViewTag;
  Result.Skip := -1; // because it's added in the SQL
  Result.First := -1; // because it's added in the SQL
  Result.CacheThreshold := 0;
end;

{ ******************************************************************************************************** }
function TAlSelectSQLClause.FBXClientSelectDataSQL(const RowTag: ansiString)
  : TALFBXClientSelectDataQUERY;
begin
  Result := FBXClientSelectDataSQL('', RowTag);
end;

{ ****************************************************************************** }
function TAlSelectSQLClause.FBXClientSelectDataSQL: TALFBXClientSelectDataQUERY;
begin
  Result := FBXClientSelectDataSQL('', '');
end;

{ ********************************* }
procedure TAlSelectSQLClause.clear;
Begin
  ServerType := alFirebird;
  First := -1;
  Skip := -1;
  Distinct := False;
  Select.clear;
  Where.clear;
  From.clear;
  Join.clear;
  GroupBy.clear;
  Having.clear;
  Plan := '';
  OrderBy.clear;
  Customs.clear;
  setlength(FBXClientSQLParams, 0);
End;

{ ****************************************************************************** }
function TAlSelectSQLClauses.Add(aSelectSQLClause: TAlSelectSQLClause): integer;
begin
  Result := inherited Add(aSelectSQLClause);
end;

{ ********************************************************************************* }
function TAlSelectSQLClauses.Extract(Item: TAlSelectSQLClause)
  : TAlSelectSQLClause;
begin
  Result := TAlSelectSQLClause(inherited Extract(Item));
end;

{ ***************************************************** }
function TAlSelectSQLClauses.First: TAlSelectSQLClause;
begin
  Result := TAlSelectSQLClause(inherited First);
end;

{ ************************************************************************ }
function TAlSelectSQLClauses.GetItems(Index: integer): TAlSelectSQLClause;
begin
  Result := TAlSelectSQLClause(inherited Items[Index]);
end;

{ ********************************************************************************** }
function TAlSelectSQLClauses.IndexOf(aSelectSQLClause
  : TAlSelectSQLClause): integer;
begin
  Result := inherited IndexOf(aSelectSQLClause);
end;

{ ***************************************************************************************** }
procedure TAlSelectSQLClauses.Insert(Index: integer;
  aSelectSQLClause: TAlSelectSQLClause);
begin
  inherited Insert(Index, aSelectSQLClause);
end;

{ **************************************************** }
function TAlSelectSQLClauses.Last: TAlSelectSQLClause;
begin
  Result := TAlSelectSQLClause(inherited Last);
end;

{ ********************************************************************************* }
function TAlSelectSQLClauses.Remove(aSelectSQLClause
  : TAlSelectSQLClause): integer;
begin
  Result := inherited Remove(aSelectSQLClause);
end;

{ ******************************************************************************************* }
procedure TAlSelectSQLClauses.SetItems(Index: integer;
  aSelectSQLClause: TAlSelectSQLClause);
begin
  inherited Items[Index] := aSelectSQLClause;
end;

{ ************************************************************************************************************************ }
function TAlSelectSQLClauses.FBXClientSelectDataQUERIES(const ViewTag,
  RowTag: ansiString): TALFBXClientSelectDataQUERIES;
var
  i: integer;
begin
  setlength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := GetItems(i).FBXClientSelectDataSQL(ViewTag, RowTag);
end;

{ *************************************************************************************************************** }
function TAlSelectSQLClauses.FBXClientSelectDataQUERIES
  (const RowTag: ansiString): TALFBXClientSelectDataQUERIES;
begin
  Result := FBXClientSelectDataQUERIES('', RowTag);
end;

{ ************************************************************************************* }
function TAlSelectSQLClauses.FBXClientSelectDataQUERIES
  : TALFBXClientSelectDataQUERIES;
begin
  Result := FBXClientSelectDataQUERIES('', '');
end;

{ ************************************ }
constructor TALUpdateSQLClause.Create;
Begin
  ServerType := alFirebird;
  Kind := AlInsert;
  Table := '';
  Value := TALStringList.Create;
  Where := TALStringList.Create;
  Customs := TALStringList.Create;
  setlength(FBXClientSQLParams, 0);
end;

{ ************************************ }
destructor TALUpdateSQLClause.Destroy;
begin
  Value.free;
  Where.free;
  Customs.free;
  inherited;
end;

{ ************************************************************** }
procedure TALUpdateSQLClause.Assign(Source: TALUpdateSQLClause);
begin
  ServerType := Source.ServerType;
  Kind := Source.Kind;
  Table := Source.Table;
  Value.Assign(Source.Value);
  Where.Assign(Source.Where);
  Customs.Assign(Source.Customs);
  FBXClientSQLParams := Source.FBXClientSQLParams;
end;

{ ********************************************** }
function TALUpdateSQLClause.SQLText: ansiString;
var
  i: integer;
  S1, S2: ansiString;
Begin

  // empty result if Value.Count = 0
  if (Kind <> alDelete) and (Value.Count = 0) then
  begin
    Result := '';
    exit;
  end;

  // AlUpdate
  If Kind = alUpdate then
  Begin
    Result := '';
    for i := 0 to Value.Count - 1 do
      If ALTrim(Value[i]) <> '' then
        Result := Result + ALTrim(Value[i]) + ', ';
    Delete(Result, length(Result) - 1, 2);

    Result := 'Update ' + Table + ' Set ' + Result;

    If Where.Count > 0 then
    begin
      Result := Result + ' Where ';
      For i := 0 to Where.Count - 1 do
        If ALTrim(Where[i]) <> '' then
          Result := Result + '(' + ALTrim(Where[i]) + ') and ';
      Delete(Result, length(Result) - 4, 5);
    end;
  end

  // AlDelete
  else If Kind = alDelete then
  Begin
    Result := 'delete from ' + Table;

    If Where.Count > 0 then
    begin
      Result := Result + ' Where ';
      For i := 0 to Where.Count - 1 do
        If ALTrim(Where[i]) <> '' then
          Result := Result + '(' + ALTrim(Where[i]) + ') and ';
      Delete(Result, length(Result) - 4, 5);
    end;
  end

  // AlInsert, AlUpdateOrInsert
  else
  Begin
    S1 := '';
    S2 := '';

    for i := 0 to Value.Count - 1 do
      If ALTrim(Value[i]) <> '' then
      begin
        S1 := S1 + ALTrim(Value.Names[i]) + ', ';
        S2 := S2 + ALTrim(Value.ValueFromIndex[i]) + ', ';
      end;
    Delete(S1, length(S1) - 1, 2);
    Delete(S2, length(S2) - 1, 2);

    if Kind = AlInsert then
      Result := 'Insert into '
    else
      Result := 'Update or Insert into ';
    Result := Result + Table + ' (' + S1 + ') Values (' + S2 + ')';
  end;

end;

{ ***************************************************************************************************************** }
function TALUpdateSQLClause.FBXClientUpdateDataSQL(const ViewTag,
  RowTag: ansiString): TALFBXClientUpdateDataQUERY;
begin
  Result.SQL := SQLText;
  Result.Params := FBXClientSQLParams;
end;

{ ******************************************************************************************************** }
function TALUpdateSQLClause.FBXClientUpdateDataSQL(const RowTag: ansiString)
  : TALFBXClientUpdateDataQUERY;
begin
  Result := FBXClientUpdateDataSQL('', RowTag);
end;

{ ****************************************************************************** }
function TALUpdateSQLClause.FBXClientUpdateDataSQL: TALFBXClientUpdateDataQUERY;
begin
  Result := FBXClientUpdateDataSQL('', '');
end;

{ ********************************* }
procedure TALUpdateSQLClause.clear;
begin
  ServerType := alFirebird;
  Kind := AlInsert;
  Table := '';
  Value.clear;
  Where.clear;
  Customs.clear;
  setlength(FBXClientSQLParams, 0);
end;

{ ****************************************************************************** }
function TAlUpdateSQLClauses.Add(aUpdateSQLClause: TALUpdateSQLClause): integer;
begin
  Result := inherited Add(aUpdateSQLClause);
end;

{ ********************************************************************************* }
function TAlUpdateSQLClauses.Extract(Item: TALUpdateSQLClause)
  : TALUpdateSQLClause;
begin
  Result := TALUpdateSQLClause(inherited Extract(Item));
end;

{ ***************************************************** }
function TAlUpdateSQLClauses.First: TALUpdateSQLClause;
begin
  Result := TALUpdateSQLClause(inherited First);
end;

{ ************************************************************************ }
function TAlUpdateSQLClauses.GetItems(Index: integer): TALUpdateSQLClause;
begin
  Result := TALUpdateSQLClause(inherited Items[Index]);
end;

{ ********************************************************************************** }
function TAlUpdateSQLClauses.IndexOf(aUpdateSQLClause
  : TALUpdateSQLClause): integer;
begin
  Result := inherited IndexOf(aUpdateSQLClause);
end;

{ ***************************************************************************************** }
procedure TAlUpdateSQLClauses.Insert(Index: integer;
  aUpdateSQLClause: TALUpdateSQLClause);
begin
  inherited Insert(Index, aUpdateSQLClause);
end;

{ **************************************************** }
function TAlUpdateSQLClauses.Last: TALUpdateSQLClause;
begin
  Result := TALUpdateSQLClause(inherited Last);
end;

{ ********************************************************************************* }
function TAlUpdateSQLClauses.Remove(aUpdateSQLClause
  : TALUpdateSQLClause): integer;
begin
  Result := inherited Remove(aUpdateSQLClause);
end;

{ ******************************************************************************************* }
procedure TAlUpdateSQLClauses.SetItems(Index: integer;
  aUpdateSQLClause: TALUpdateSQLClause);
begin
  inherited Items[Index] := aUpdateSQLClause;
end;

{ ************************************************************************************************************************ }
function TAlUpdateSQLClauses.FBXClientUpdateDataQUERIES(const ViewTag,
  RowTag: ansiString): TALFBXClientUpdateDataQUERIES;
var
  i: integer;
begin
  setlength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := GetItems(i).FBXClientUpdateDataSQL(ViewTag, RowTag);
end;

{ *************************************************************************************************************** }
function TAlUpdateSQLClauses.FBXClientUpdateDataQUERIES
  (const RowTag: ansiString): TALFBXClientUpdateDataQUERIES;
begin
  Result := FBXClientUpdateDataQUERIES('', RowTag);
end;

{ ************************************************************************************* }
function TAlUpdateSQLClauses.FBXClientUpdateDataQUERIES
  : TALFBXClientUpdateDataQUERIES;
begin
  Result := FBXClientUpdateDataQUERIES('', '');
end;

end.
