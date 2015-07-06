{ *************************************************************
  www:          http://sourceforge.net/projects/alcinoe/
  svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
  Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
  Sponsor(s):   Arkadia SA (http://www.arkadia.com)

  product:      ALMultiPartBaseParser
  Version:      4.00

  Description:  MultiPart objects to encode or decode stream
  in mime multipart/xxx format.

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

  History :     26/06/2012: Add xe2 support

  Link :        http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cdosys/html/7a18a98b-3a18-45b2-83a9-28a8f4099970.asp
  http://www.ietf.org/rfc/rfc2646.txt
  http://www.w3.org/TR/REC-html40/interact/forms.html#h-17.1
  http://www.ietf.org/rfc/rfc1867.txt
  http://www.ietf.org/rfc/rfc2388.txt
  http://www.w3.org/MarkUp/html-spec/html-spec_8.html

  * Please send all your feedback to alcinoe@arkadia.com
  * If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
  * Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
  ************************************************************** }
unit ALMultiPartParser;

interface

{$IF CompilerVersion >= 25} { Delphi XE4 }
{$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
  System.Classes,
  System.Contnrs,
{$ELSE}
  Classes,
  Contnrs,
{$IFEND}
  ALStringList;

type

  { --Single multipart Object------------- }
  TALMultiPartBaseContent = class(TObject)
  private
    FContentType: AnsiString;
    FContentTransferEncoding: AnsiString;
    FContentDisposition: AnsiString;
    FContentID: AnsiString;
    FContentDescription: AnsiString;
    FDataStream: TStream;
    FCustomHeaders: TALStrings;
    Function GetRawHeaderText: AnsiString;
    procedure SetRawHeaderText(const aRawHeaderText: AnsiString);
    function GetDataString: AnsiString;
    procedure SetDataString(const aValue: AnsiString);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure LoadDataFromFile(aFileName: AnsiString); virtual;
    procedure LoadDataFromStream(aStream: TStream); virtual;
    procedure LoadDataFromFileBase64Encode(aFileName: AnsiString); virtual;
    procedure LoadDataFromStreamBase64Encode(aStream: TStream); virtual;
    procedure SaveDataToFile(aFileName: AnsiString); virtual;
    procedure SaveDataToStream(aStream: TStream); virtual;
    procedure SaveDataToFileBase64Decode(aFileName: AnsiString); virtual;
    procedure SaveDataToStreamBase64Decode(aStream: TStream); virtual;
    property ContentType: AnsiString read FContentType write FContentType;
    // Content-Type: text/plain; charset="utf-8"  or  Content-Type: image/bmp; name="Blue Lace 16.bmp"
    property ContentTransferEncoding: AnsiString read FContentTransferEncoding
      write FContentTransferEncoding; // Content-Transfer-Encoding: base64
    property ContentDisposition: AnsiString read FContentDisposition
      write FContentDisposition;
    // Content-Disposition: attachment; filename="Blue Lace 16.bmp"
    property ContentID: AnsiString read FContentID write FContentID;
    // Content-ID: <foo4%25foo1@bar.net>
    property ContentDescription: AnsiString read FContentDescription
      write FContentDescription; // Content-Description: some text
    property DataStream: TStream read FDataStream;
    property DataString: AnsiString read GetDataString Write SetDataString;
    property CustomHeaders: TALStrings read FCustomHeaders;
    property RawHeaderText: AnsiString read GetRawHeaderText
      write SetRawHeaderText;
  end;

  { --List Of multipart Objects---------------- }
  TALMultiPartBaseContents = class(TObjectList)
  private
  protected
    function GetItem(Index: Integer): TALMultiPartBaseContent;
    procedure SetItem(Index: Integer; AObject: TALMultiPartBaseContent);
  public
    Function Add: TALMultiPartBaseContent; overload;
    function Add(AObject: TALMultiPartBaseContent): Integer; overload;
    function Remove(AObject: TALMultiPartBaseContent): Integer;
    function IndexOf(AObject: TALMultiPartBaseContent): Integer;
    procedure Insert(Index: Integer; AObject: TALMultiPartBaseContent);
    property Items[Index: Integer]: TALMultiPartBaseContent read GetItem
      write SetItem; default;
  end;

  { --TAlMultiPartBaseStream------------------- }
  TAlMultiPartBaseStream = class(TMemoryStream)
  private
    FBoundary: AnsiString;
    function GenerateUniqueBoundary: AnsiString;
  public
    constructor Create; virtual;
    procedure AddContent(aContent: TALMultiPartBaseContent); virtual;
    procedure CloseBoundary; virtual;
    property Boundary: AnsiString read FBoundary write FBoundary;
  end;

  { --TALMultipartBaseEncoder------------- }
  TALMultipartBaseEncoder = class(TObject)
  private
    FDataStream: TAlMultiPartBaseStream;
  protected
    Function CreateDataStream: TAlMultiPartBaseStream; virtual; Abstract;
    function GetDataStream: TAlMultiPartBaseStream; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Encode(acontents: TALMultiPartBaseContents); overload;
    property DataStream: TAlMultiPartBaseStream read GetDataStream;
  end;

  { --TALMultipartBaseDecoder------------- }
  TALMultipartBaseDecoder = class(TObject)
  private
    FContents: TALMultiPartBaseContents;
  protected
    function GetContents: TALMultiPartBaseContents; virtual;
    Function CreateContent: TALMultiPartBaseContent; virtual; Abstract;
    Function CreateContents: TALMultiPartBaseContents; virtual; Abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Decode(aDataStream: TStream; aboundary: AnsiString);
      overload; Virtual;
    procedure Decode(aDataStr: AnsiString; aboundary: AnsiString);
      overload; Virtual;
  end;

  { Below a sample of multipart/alternative message :
    ********************************************
    Mime-Version: 1.0
    Content-Type: multipart/alternative;
    boundary="----=_NextPart_000_0189_01C81ED9.A3E50190"

    This is a multi-part message in MIME format.

    ------=_NextPart_000_0189_01C81ED9.A3E50190
    Content-Type: text/plain;
    charset="utf-8"
    Content-Transfer-Encoding: quoted-printable

    qsdqsd

    qsdqsdqsd sqdqsds
    ------=_NextPart_000_0189_01C81ED9.A3E50190
    Content-Type: text/html;
    charset="utf-8"
    Content-Transfer-Encoding: quoted-printable

    =EF=BB=BF<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
    <HTML><HEAD>
    <META http-equiv=3DContent-Type content=3D"text/html; charset=3Dutf-8">
    <META content=3D"MSHTML 6.00.6000.16544" name=3DGENERATOR>
    <STYLE></STYLE>
    </HEAD>
    <BODY>
    <DIV><FONT face=3DArial size=3D2>qsdqsd</FONT></DIV>
    <DIV><FONT face=3DArial size=3D2></FONT>&nbsp;</DIV>
    <DIV><FONT face=3DArial size=3D2>qsdqsdqsd=20
    <EM>sqdqsds</EM></FONT></DIV></BODY></HTML>

    ------=_NextPart_000_0189_01C81ED9.A3E50190--
    ******************************************** }
type

  { --Single multipart Object------------------------------------ }
  TALMultiPartAlternativeContent = class(TALMultiPartBaseContent)
  private
  public
  end;

  { --List Of multipart Objects------------------------------------ }
  TALMultiPartAlternativeContents = class(TALMultiPartBaseContents)
  private
  protected
    function GetItem(Index: Integer): TALMultiPartAlternativeContent;
    procedure SetItem(Index: Integer; AObject: TALMultiPartAlternativeContent);
  public
    Function Add: TALMultiPartAlternativeContent; overload;
    function Add(AObject: TALMultiPartAlternativeContent): Integer; overload;
    function Remove(AObject: TALMultiPartAlternativeContent): Integer;
    function IndexOf(AObject: TALMultiPartAlternativeContent): Integer;
    procedure Insert(Index: Integer; AObject: TALMultiPartAlternativeContent);
    property Items[Index: Integer]: TALMultiPartAlternativeContent read GetItem
      write SetItem; default;
  end;

  { --TAlMultiPartAlternativeStream---------------------------- }
  TAlMultiPartAlternativeStream = class(TAlMultiPartBaseStream)
  private
  public
    procedure AddContent(aContent: TALMultiPartAlternativeContent); reintroduce;
  end;

  { --TALMultipartAlternativeEncoder----------------------------- }
  TALMultipartAlternativeEncoder = class(TALMultipartBaseEncoder)
  private
  protected
    Function CreateDataStream: TAlMultiPartBaseStream; override;
    function GetDataStream: TAlMultiPartAlternativeStream; reintroduce;
  public
    property DataStream: TAlMultiPartAlternativeStream read GetDataStream;
  end;

  { --TALMultipartAlternativeDecoder----------------------------- }
  TALMultipartAlternativeDecoder = class(TALMultipartBaseDecoder)
  private
  protected
    function GetContents: TALMultiPartAlternativeContents; reintroduce;
    Function CreateContent: TALMultiPartBaseContent; override;
    Function CreateContents: TALMultiPartBaseContents; override;
  public
    property Contents: TALMultiPartAlternativeContents read GetContents;
  end;

  { Below a sample of multipart/form-data message :
    ********************************************
    Content-Type: multipart/form-data; boundary=---------------------------7d728842d0b36

    -----------------------------7d728842d0b36
    Content-Disposition: form-data; name="picture"; filename="C:\ud964D.tmp.jpg"
    Content-Type: image/pjpeg

    ÿØÿà ...
    -----------------------------7d728842d0b36
    Content-Disposition: form-data; name="field1"

    avalue
    -----------------------------7d728842d0b36
    Content-Disposition: form-data; name="field2"

    aValue
    -----------------------------7d728842d0b36--
    ******************************************** }

type

  { --Single multipart Object--------------------------------- }
  TALMultiPartFormDataContent = class(TALMultiPartBaseContent)
  private
    function GetFieldName: AnsiString;
    function GetFileName: AnsiString;
    procedure SetfieldName(const aValue: AnsiString);
    procedure SetfileName(const aValue: AnsiString);
  public
    procedure LoadDataFromFile(aFileName: AnsiString); override;
    procedure LoadDataFromStream(aStream: TStream); override;
    Property FieldName: AnsiString Read GetFieldName Write SetfieldName;
    Property FileName: AnsiString Read GetFileName Write SetfileName;
  end;

  { --List Of multipart Objects--------------------------------- }
  TALMultiPartFormDataContents = class(TALMultiPartBaseContents)
  private
  protected
    function GetItem(Index: Integer): TALMultiPartFormDataContent;
    procedure SetItem(Index: Integer; AObject: TALMultiPartFormDataContent);
  public
    Function Add: TALMultiPartFormDataContent; overload;
    function Add(AObject: TALMultiPartFormDataContent): Integer; overload;
    function Remove(AObject: TALMultiPartFormDataContent): Integer;
    function IndexOf(AObject: TALMultiPartFormDataContent): Integer;
    procedure Insert(Index: Integer; AObject: TALMultiPartFormDataContent);
    property Items[Index: Integer]: TALMultiPartFormDataContent read GetItem
      write SetItem; default;
  end;

  { --TAlMultiPartFormDataStream---------------------------- }
  TAlMultiPartFormDataStream = class(TAlMultiPartBaseStream)
  private
  public
    procedure AddField(const aFieldName, aFieldValue: AnsiString);
    procedure AddFile(const aFieldName, aFileName, aContentType: AnsiString;
      aFileData: TStream); overload;
    procedure AddFile(const aFieldName, aFileName: AnsiString); overload;
    procedure AddContent(aContent: TALMultiPartFormDataContent); reintroduce;
  end;

  { --TALMultipartFormDataEncoder----------------------------- }
  TALMultipartFormDataEncoder = class(TALMultipartBaseEncoder)
  private
  protected
    Function CreateDataStream: TAlMultiPartBaseStream; override;
    function GetDataStream: TAlMultiPartFormDataStream; reintroduce;
  public
    procedure Encode(aContentFields: TALStrings;
      aContentFiles: TALMultiPartFormDataContents);
    property DataStream: TAlMultiPartFormDataStream read GetDataStream;
  end;

  { --TALMultipartFormDataDecoder----------------------------- }
  TALMultipartFormDataDecoder = class(TALMultipartBaseDecoder)
  private
    FContentFiles: TALMultiPartFormDataContents;
    FContentFields: TALStrings;
    function GetContentFields: TALStrings;
    function GetContentFiles: TALMultiPartFormDataContents;
  protected
    function GetContents: TALMultiPartFormDataContents; reintroduce;
    Function CreateContent: TALMultiPartBaseContent; override;
    Function CreateContents: TALMultiPartBaseContents; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Decode(aDataStr, aboundary: AnsiString); overload; Override;
    procedure Decode(aDataStr, aboundary: AnsiString;
      aContentFields: TALStrings;
      aContentFiles: TALMultiPartFormDataContents); overload;
    property ContentFiles: TALMultiPartFormDataContents read GetContentFiles;
    property ContentFields: TALStrings read GetContentFields;
  end;

  { Below a sample of multipart/mixed message :
    ********************************************
    Mime-Version: 1.0
    Content-Type: multipart/mixed;
    boundary="----=_NextPart_000_0145_01C81ECF.27E4F8C0"

    This is a multi-part message in MIME format.

    ------=_NextPart_000_0145_01C81ECF.27E4F8C0
    Content-Type: text/plain;
    charset="utf-8"
    Content-Transfer-Encoding: base64

    YyB1biB0ZXN0ICEhIQ==

    ------=_NextPart_000_0145_01C81ECF.27E4F8C0
    Content-Type: image/bmp;
    name="Blue Lace 16.bmp"
    Content-Transfer-Encoding: base64
    Content-Disposition: attachment;
    filename="Blue Lace 16.bmp"

    Qk34BAAAAAAAAHYAAAAoAAAAMAAAADAAAAABAAQAAAAAAAAAAADDDgAAww4AAAAAAAAAAAAAAAAA
    AAAAgAAAgAAAAICAAIAAAACAAIAAgIAAAICAgADAwMAAAAD/AAD/AAAA//8A/wAAAP8A/wD//wAA
    RMRERERExExEREREzMRERAAA

    ------=_NextPart_000_0145_01C81ECF.27E4F8C0--
    ******************************************** }
type

  { --Single multipart Object------------------------------ }
  TALMultiPartMixedContent = class(TALMultiPartBaseContent)
  private
    function GetAttachment: Boolean;
  public
    procedure LoadDataFromFileAsAttachmentBase64Encode
      (aFileName: AnsiString); virtual;
    Property IsAttachment: Boolean read GetAttachment;
  end;

  { --List Of multipart Objects------------------------------ }
  TALMultiPartMixedContents = class(TALMultiPartBaseContents)
  private
  protected
    function GetItem(Index: Integer): TALMultiPartMixedContent;
    procedure SetItem(Index: Integer; AObject: TALMultiPartMixedContent);
  public
    Function Add: TALMultiPartMixedContent; overload;
    function Add(AObject: TALMultiPartMixedContent): Integer; overload;
    function Remove(AObject: TALMultiPartMixedContent): Integer;
    function IndexOf(AObject: TALMultiPartMixedContent): Integer;
    procedure Insert(Index: Integer; AObject: TALMultiPartMixedContent);
    property Items[Index: Integer]: TALMultiPartMixedContent read GetItem
      write SetItem; default;
  end;

  { --TAlMultiPartMixedStream---------------------------- }
  TAlMultiPartMixedStream = class(TAlMultiPartBaseStream)
  private
  public
    procedure AddInlineTextBase64Encode(aContentType, aText: AnsiString);
    procedure AddAttachmentBase64Encode(aFileName, aContentType: AnsiString;
      aFileData: TStream); overload;
    procedure AddAttachmentBase64Encode(aFileName: AnsiString); overload;
    procedure AddContent(aContent: TALMultiPartMixedContent); reintroduce;
  end;

  { --TALMultipartMixedEncoder----------------------------- }
  TALMultipartMixedEncoder = class(TALMultipartBaseEncoder)
  private
  protected
    Function CreateDataStream: TAlMultiPartBaseStream; override;
    function GetDataStream: TAlMultiPartMixedStream; reintroduce;
  public
    procedure Encode(aInlineText, aInlineTextContentType: AnsiString;
      aAttachments: TALMultiPartMixedContents); overload;
    property DataStream: TAlMultiPartMixedStream read GetDataStream;
  end;

  { --TALMultipartMixedDecoder----------------------------- }
  TALMultipartMixedDecoder = class(TALMultipartBaseDecoder)
  private
  protected
    function GetContents: TALMultiPartMixedContents; reintroduce;
    Function CreateContent: TALMultiPartBaseContent; override;
    Function CreateContents: TALMultiPartBaseContents; override;
  public
    property Contents: TALMultiPartMixedContents read GetContents;
  end;

  { --------------------------------------------------------------------------------------- }
Function ALMultipartExtractBoundaryFromContentType(aContentType: AnsiString)
  : AnsiString;
Function ALMultipartExtractSubValueFromHeaderLine(aHeaderLine: AnsiString;
  aName: AnsiString): AnsiString;
Function ALMultipartSetSubValueInHeaderLine(aHeaderLine: AnsiString;
  aName, aValue: AnsiString): AnsiString;

implementation

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
  System.SysUtils,
  System.Types, // to expand the inline function
{$ELSE}
  SysUtils,
  Types, // to expand the inline function
{$IFEND}
  ALString,
  ALMime;

{ ******************************************************************************************************** }
Function ALMultipartExtractSubValueFromHeaderLine(aHeaderLine: AnsiString;
  aName: AnsiString): AnsiString;

{ ------------------------------------------------------------ }
  function InternalRemoveQuoteStr(aStr: AnsiString): AnsiString;
  Begin
    Result := aStr;
    If (Length(Result) > 0) and (Result[1] in ['"', '''']) and
      (Result[1] = Result[Length(Result)]) then
      Result := AlCopyStr(Result, 2, Length(Result) - 2);
  end;

Var
  aLst: TALStringList;
  i: Integer;

begin
  Result := '';
  aName := AlLowerCase(aName);
  aLst := TALStringList.Create;
  Try
    aLst.Text := AlStringReplace(aHeaderLine, ';', #13#10, [RfReplaceAll]);
    For i := 0 to aLst.Count - 1 do
      If AlLowerCase(ALTrim(aLst.Names[i])) = aName then
      begin
        Result := InternalRemoveQuoteStr(aLst.ValueFromIndex[i]);
        Break;
      end;
  finally
    aLst.Free;
  end;
end;

{ ********************************************************************************************************** }
Function ALMultipartSetSubValueInHeaderLine(aHeaderLine: AnsiString;
  aName, aValue: AnsiString): AnsiString;
Var
  aLst: TALStringList;
  aLowerCaseName: AnsiString;
  aFlag: Boolean;
  i: Integer;
begin
  Result := '';
  aLowerCaseName := AlLowerCase(aName);
  aHeaderLine := AlStringReplace(aHeaderLine, #13#10, ' ', [RfReplaceAll]);
  aLst := TALStringList.Create;
  Try
    aFlag := False;
    aLst.Text := AlStringReplace(aHeaderLine, ';', #13#10, [RfReplaceAll]);
    For i := 0 to aLst.Count - 1 do
      If AlLowerCase(ALTrim(aLst.Names[i])) = aLowerCaseName then
      begin
        aLst.ValueFromIndex[i] := '"' + aValue + '"';
        aFlag := True;
        Break;
      end;

    For i := 0 to aLst.Count - 1 do
      Result := Result + '; ' + ALTrim(aLst[i]);

    if Not aFlag then
      Result := Result + '; ' + aName + '=' + '"' + aValue + '"';

    Delete(Result, 1, 2);
  finally
    aLst.Free;
  end;
end;

{ *************************************************************************************** }
Function ALMultipartExtractBoundaryFromContentType(aContentType: AnsiString)
  : AnsiString;
Begin
  Result := ALMultipartExtractSubValueFromHeaderLine(aContentType, 'boundary');
end;

{ ***************************************** }
constructor TALMultiPartBaseContent.Create;
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  FCustomHeaders := TALStringList.Create;
  FCustomHeaders.NameValueSeparator := ':';
  Clear;
end;

{ ***************************************** }
destructor TALMultiPartBaseContent.Destroy;
begin
  FDataStream.Free;
  FCustomHeaders.Free;
  inherited;
end;

{ ************************************** }
procedure TALMultiPartBaseContent.Clear;
begin
  FContentType := '';
  FContentTransferEncoding := '';
  FContentDisposition := '';
  FContentID := '';
  FContentDescription := '';
  FCustomHeaders.Clear;
  TMemoryStream(FDataStream).Clear;
end;

{ ************************************************************ }
function TALMultiPartBaseContent.GetRawHeaderText: AnsiString;
Var
  i: Integer;
begin
  Result := '';
  If ALTrim(FContentType) <> '' then
    Result := Result + 'Content-Type: ' + ALTrim(FContentType) + #13#10;
  If ALTrim(FContentTransferEncoding) <> '' then
    Result := Result + 'Content-Transfer-Encoding: ' +
      ALTrim(FContentTransferEncoding) + #13#10;
  If ALTrim(FContentDisposition) <> '' then
    Result := Result + 'Content-Disposition: ' +
      ALTrim(FContentDisposition) + #13#10;
  If ALTrim(FContentID) <> '' then
    Result := Result + 'Content-ID: ' + ALTrim(FContentID) + #13#10;
  If ALTrim(FContentDescription) <> '' then
    Result := Result + 'Content-Description: ' +
      ALTrim(FContentDescription) + #13#10;
  For i := 0 to FCustomHeaders.Count - 1 do
    if (ALTrim(FCustomHeaders.Names[i]) <> '') and
      (ALTrim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      Result := Result + FCustomHeaders.Names[i] + ': ' +
        ALTrim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
end;

{ *********************************************************************************** }
procedure TALMultiPartBaseContent.SetRawHeaderText(const aRawHeaderText
  : AnsiString);

Var
  aRawHeaderLst: TALStringList;

  { --------------------------------------------- }
  Function AlG001(aName: AnsiString): AnsiString;
  Var
    i: Integer;
    Str: AnsiString;
  Begin
    i := aRawHeaderLst.IndexOfName(aName);
    If i >= 0 then
    Begin
      Result := ALTrim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
      While True do
      begin
        If i >= aRawHeaderLst.Count then
          Break;
        Str := aRawHeaderLst[i];
        If (Str = '') or (not(Str[1] in [' ', #9])) then
          Break; // (1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := ALTrim(Result + ' ' + ALTrim(Str));
        aRawHeaderLst.Delete(i);
      end;
    end
    else
      Result := '';
  end;

Var
  Str1, Str2: AnsiString;
  j: Integer;

begin
  Clear;
  aRawHeaderLst := TALStringList.Create;
  try
    aRawHeaderLst.NameValueSeparator := ':';
    aRawHeaderLst.Text := aRawHeaderText;

    FContentType := AlG001('Content-Type');
    FContentTransferEncoding := AlG001('Content-Transfer-Encoding');
    FContentDisposition := AlG001('Content-Disposition');
    FContentID := AlG001('Content-ID');
    FContentDescription := AlG001('Content-Description');

    FCustomHeaders.Clear;
    j := 0;
    while j <= aRawHeaderLst.Count - 1 do
    begin
      Str1 := ALTrim(aRawHeaderLst.Names[j]);
      If (ALTrim(Str1) <> '') and (not(Str1[1] in [' ', #9])) then
      begin
        Str1 := ALTrim(Str1) + ': ' + ALTrim(aRawHeaderLst.ValueFromIndex[j]);
        inc(j);
        While True do
        begin
          If j >= aRawHeaderLst.Count then
            Break;
          Str2 := aRawHeaderLst[j];
          If (Str2 = '') or (not(Str2[1] in [' ', #9])) then
            Break; // (1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := ALTrim(Str1 + ' ' + ALTrim(Str2));
          inc(j);
        end;
        FCustomHeaders.Add(Str1);
      end
      else
        inc(j);
    end;

  finally
    aRawHeaderLst.Free;
  end;
end;

{ ********************************************************* }
function TALMultiPartBaseContent.GetDataString: AnsiString;
begin
  FDataStream.Position := 0;
  if FDataStream.Size = 0 then
    Result := ''
  else
  begin
    SetLength(Result, FDataStream.Size);
    FDataStream.ReadBuffer(Result[1], FDataStream.Size);
  end;
  FDataStream.Position := 0;
end;

{ ************************************************************************ }
procedure TALMultiPartBaseContent.SetDataString(const aValue: AnsiString);
begin
  TMemoryStream(FDataStream).Clear;
  if Length(aValue) > 0 then
    FDataStream.WriteBuffer(aValue[1], Length(aValue));
  FDataStream.Position := 0;
end;

{ ************************************************************************ }
procedure TALMultiPartBaseContent.LoadDataFromFile(aFileName: AnsiString);
begin
  TMemoryStream(FDataStream).LoadFromFile(String(aFileName));
  ContentType := ALGetDefaultMIMEContentTypeFromExt
    (ALExtractFileExt(aFileName));
  ContentTransferEncoding := 'binary';
end;

{ ************************************************************************************ }
procedure TALMultiPartBaseContent.LoadDataFromFileBase64Encode
  (aFileName: AnsiString);
Var
  Buffer: AnsiString;
begin
  TMemoryStream(FDataStream).Clear;
  Buffer := ALMimeBase64EncodeString(AlGetStringFromFile(aFileName));
  FDataStream.WriteBuffer(Buffer[1], Length(Buffer));
  FDataStream.Position := 0;
  ContentType := ALGetDefaultMIMEContentTypeFromExt
    (ALExtractFileExt(aFileName));
  ContentTransferEncoding := 'base64';
end;

{ ********************************************************************* }
procedure TALMultiPartBaseContent.LoadDataFromStream(aStream: TStream);
Begin
  TMemoryStream(FDataStream).LoadFromStream(aStream);
  ContentTransferEncoding := 'binary';
end;

{ ********************************************************************************* }
procedure TALMultiPartBaseContent.LoadDataFromStreamBase64Encode
  (aStream: TStream);
Var
  Buffer: AnsiString;
Begin
  aStream.Position := 0;
  SetLength(Buffer, aStream.Size);
  aStream.ReadBuffer(Buffer[1], aStream.Size);
  Buffer := ALMimeBase64EncodeString(Buffer);
  TMemoryStream(FDataStream).Clear;
  FDataStream.WriteBuffer(Buffer[1], Length(Buffer));
  FDataStream.Position := 0;
  ContentTransferEncoding := 'base64';
end;

{ ********************************************************************** }
procedure TALMultiPartBaseContent.SaveDataToFile(aFileName: AnsiString);
begin
  TMemoryStream(FDataStream).SaveToFile(String(aFileName));
end;

{ ******************************************************************* }
procedure TALMultiPartBaseContent.SaveDataToStream(aStream: TStream);
begin
  TMemoryStream(FDataStream).SaveToStream(aStream);
end;

{ ********************************************************************************** }
procedure TALMultiPartBaseContent.SaveDataToFileBase64Decode
  (aFileName: AnsiString);
Var
  Buffer: AnsiString;
begin
  FDataStream.Position := 0;
  SetLength(Buffer, FDataStream.Size);
  FDataStream.ReadBuffer(Buffer[1], FDataStream.Size);
  AlSaveStringToFile(ALMimeBase64DecodeString(Buffer), aFileName);
end;

{ ******************************************************************************* }
procedure TALMultiPartBaseContent.SaveDataToStreamBase64Decode
  (aStream: TStream);
Var
  Buffer: AnsiString;
begin
  FDataStream.Position := 0;
  SetLength(Buffer, FDataStream.Size);
  FDataStream.ReadBuffer(Buffer[1], FDataStream.Size);
  Buffer := ALMimeBase64DecodeString(Buffer);
  aStream.WriteBuffer(Buffer[1], Length(Buffer));
end;

{ ******************************************************************************* }
function TALMultiPartBaseContents.Add(AObject: TALMultiPartBaseContent)
  : Integer;
begin
  Result := inherited Add(AObject);
end;

{ ************************************************************* }
function TALMultiPartBaseContents.Add: TALMultiPartBaseContent;
begin
  Result := TALMultiPartBaseContent.Create;
  Try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

{ ********************************************************************************* }
function TALMultiPartBaseContents.GetItem(Index: Integer)
  : TALMultiPartBaseContent;
begin
  Result := TALMultiPartBaseContent(inherited Items[Index]);
end;

{ *********************************************************************************** }
function TALMultiPartBaseContents.IndexOf
  (AObject: TALMultiPartBaseContent): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

{ ****************************************************************************************** }
procedure TALMultiPartBaseContents.Insert(Index: Integer;
  AObject: TALMultiPartBaseContent);
begin
  inherited Insert(Index, AObject);
end;

{ ********************************************************************************** }
function TALMultiPartBaseContents.Remove
  (AObject: TALMultiPartBaseContent): Integer;
begin
  Result := inherited Remove(AObject);
end;

{ ******************************************************************************************* }
procedure TALMultiPartBaseContents.SetItem(Index: Integer;
  AObject: TALMultiPartBaseContent);
begin
  inherited Items[Index] := AObject;
end;

{ **************************************** }
constructor TAlMultiPartBaseStream.Create;
begin
  inherited;
  FBoundary := GenerateUniqueBoundary;
end;

{ ***************************************************************************** }
procedure TAlMultiPartBaseStream.AddContent(aContent: TALMultiPartBaseContent);
var
  sFormFieldInfo: AnsiString;
begin
  sFormFieldInfo := #13#10 + '--' + Boundary + #13#10 +
    aContent.RawHeaderText + #13#10;

  Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
  CopyFrom(aContent.DataStream, 0);
end;

{ ***************************************************************** }
function TAlMultiPartBaseStream.GenerateUniqueBoundary: AnsiString;
begin
  Result := '---------------------------' + ALFormatDateTime('mmddyyhhnnsszzz',
    Now, ALDefaultFormatSettings);
end;

{ ********************************************* }
procedure TAlMultiPartBaseStream.CloseBoundary;
var
  sFormFieldInfo: AnsiString;
begin
  sFormFieldInfo := #13#10 + '--' + Boundary + '--' + #13#10;
  Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
end;

{ ***************************************** }
constructor TALMultipartBaseEncoder.Create;
begin
  inherited;
  FDataStream := CreateDataStream;
end;

{ ***************************************** }
destructor TALMultipartBaseEncoder.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

{ ********************************************************************* }
function TALMultipartBaseEncoder.GetDataStream: TAlMultiPartBaseStream;
begin
  Result := FDataStream;
end;

{ **************************************************************************** }
procedure TALMultipartBaseEncoder.Encode(acontents: TALMultiPartBaseContents);
Var
  i: Integer;
begin
  with FDataStream do
  begin
    Clear;
    For i := 0 to acontents.Count - 1 do
      AddContent(acontents[i]);
    CloseBoundary;
  end;
end;

{ ***************************************** }
constructor TALMultipartBaseDecoder.Create;
begin
  Inherited;
  FContents := CreateContents;
  FContents.OwnsObjects := True;
end;

{ ***************************************** }
destructor TALMultipartBaseDecoder.Destroy;
begin
  FContents.Free;
  inherited;
end;

{ ********************************************************************* }
function TALMultipartBaseDecoder.GetContents: TALMultiPartBaseContents;
begin
  Result := FContents;
end;

{ ************************************************************************************ }
procedure TALMultipartBaseDecoder.Decode(aDataStream: TStream;
  aboundary: AnsiString);
var
  sBuffer: AnsiString;
begin
  aDataStream.Position := 0;
  SetLength(sBuffer, aDataStream.Size);
  aDataStream.Read(Pointer(sBuffer)^, aDataStream.Size);
  Decode(sBuffer, aboundary);
end;

{ ************************************************************************ }
procedure TALMultipartBaseDecoder.Decode(aDataStr, aboundary: AnsiString);
var
  aLnBoundary: Integer;
  aContent: TALMultiPartBaseContent;
  aFlag: Boolean;
  P1, P2, P3: Integer;
begin

  { clear the fContent }
  FContents.Clear;

  { init LnBoundary }
  aLnBoundary := Length(aboundary);

  { Find the first Boundary }
  P1 := AlPos('--' + aboundary + #13#10, aDataStr);
  aFlag := P1 > 0;
  Dec(P1, 2);

  { start the loop on all Boundary }
  While aFlag Do
  begin
    aContent := CreateContent;
    With aContent do
    begin

      { Add the Content to the Contents }
      FContents.Add(aContent);

      { move P1 to the start of the header }
      P1 := P1 + aLnBoundary + 6;

      { Find the next Boundary }
      P3 := AlPosEx(#13#10 + '--' + aboundary + #13#10, aDataStr, P1);
      if P3 <= 0 then
      Begin
        aFlag := False;
        P3 := AlPosEx(#13#10 + '--' + aboundary + '--', aDataStr, P1);
        if P3 <= 0 then
          raise Exception.Create('Wrong MultiPart Content');
      end;

      { the the next 2 breakline that show the end of the header }
      P2 := AlPosEx(#13#10#13#10, aDataStr, P1);
      IF (P2 <= 0) or (P2 >= P3) then
        raise Exception.Create('Wrong MultiPart Content');
      RawHeaderText := AlCopyStr(aDataStr, P1, P2 - 1);

      { write the body to the content }
      P2 := P2 + 4;
      DataStream.Write(aDataStr[P2], P3 - P2);
      DataStream.Position := 0;

      { move P1 to the newt boundary }
      P1 := P3;
    end;
  end;

end;

{ ********************************************************************************************* }
function TALMultiPartAlternativeContents.Add
  (AObject: TALMultiPartAlternativeContent): Integer;
begin
  Result := inherited Add(AObject);
end;

{ *************************************************************************** }
function TALMultiPartAlternativeContents.Add: TALMultiPartAlternativeContent;
begin
  Result := TALMultiPartAlternativeContent.Create;
  Try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

{ *********************************************************************************************** }
function TALMultiPartAlternativeContents.GetItem(Index: Integer)
  : TALMultiPartAlternativeContent;
begin
  Result := TALMultiPartAlternativeContent(inherited Items[Index]);
end;

{ ************************************************************************************************* }
function TALMultiPartAlternativeContents.IndexOf
  (AObject: TALMultiPartAlternativeContent): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

{ ******************************************************************************************************** }
procedure TALMultiPartAlternativeContents.Insert(Index: Integer;
  AObject: TALMultiPartAlternativeContent);
begin
  inherited Insert(Index, AObject);
end;

{ ************************************************************************************************ }
function TALMultiPartAlternativeContents.Remove
  (AObject: TALMultiPartAlternativeContent): Integer;
begin
  Result := inherited Remove(AObject);
end;

{ ********************************************************************************************************* }
procedure TALMultiPartAlternativeContents.SetItem(Index: Integer;
  AObject: TALMultiPartAlternativeContent);
begin
  inherited Items[Index] := AObject;
end;

{ ******************************************************************************************* }
procedure TAlMultiPartAlternativeStream.AddContent
  (aContent: TALMultiPartAlternativeContent);
Var
  sFormFieldInfo: AnsiString;
begin
  If Position = 0 then
  Begin
    sFormFieldInfo := #13#10 +
      'This is a multi-part message in MIME format.' + #13#10;
    Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
  end;

  Inherited AddContent(aContent);
end;

{ ******************************************************************************* }
function TALMultipartAlternativeEncoder.CreateDataStream
  : TAlMultiPartBaseStream;
begin
  Result := TAlMultiPartAlternativeStream.Create;
end;

{ *********************************************************************************** }
function TALMultipartAlternativeEncoder.GetDataStream
  : TAlMultiPartAlternativeStream;
begin
  Result := TAlMultiPartAlternativeStream(inherited GetDataStream);
end;

{ ***************************************************************************** }
function TALMultipartAlternativeDecoder.CreateContent: TALMultiPartBaseContent;
begin
  Result := TALMultiPartAlternativeContent.Create;
end;

{ ******************************************************************************* }
function TALMultipartAlternativeDecoder.CreateContents
  : TALMultiPartBaseContents;
begin
  Result := TALMultiPartAlternativeContents.Create(True);
end;

{ *********************************************************************************** }
function TALMultipartAlternativeDecoder.GetContents
  : TALMultiPartAlternativeContents;
begin
  Result := TALMultiPartAlternativeContents(inherited GetContents);
end;

{ ************************************************************ }
function TALMultiPartFormDataContent.GetFieldName: AnsiString;
begin
  Result := ALMultipartExtractSubValueFromHeaderLine
    (ContentDisposition, 'name');
end;

{ *********************************************************** }
function TALMultiPartFormDataContent.GetFileName: AnsiString;
begin
  Result := ALMultipartExtractSubValueFromHeaderLine(ContentDisposition,
    'filename');
end;

{ **************************************************************************** }
procedure TALMultiPartFormDataContent.LoadDataFromFile(aFileName: AnsiString);
begin
  inherited LoadDataFromFile(aFileName);
  ContentTransferEncoding := '';
end;

{ ************************************************************************* }
procedure TALMultiPartFormDataContent.LoadDataFromStream(aStream: TStream);
begin
  inherited LoadDataFromStream(aStream);
  ContentTransferEncoding := '';
end;

{ ************************************************************************** }
procedure TALMultiPartFormDataContent.SetfieldName(const aValue: AnsiString);
begin
  ContentDisposition := ALMultipartSetSubValueInHeaderLine(ContentDisposition,
    'name', aValue);
end;

{ ************************************************************************** }
procedure TALMultiPartFormDataContent.SetfileName(const aValue: AnsiString);
begin
  ContentDisposition := ALMultipartSetSubValueInHeaderLine(ContentDisposition,
    'filename', aValue);
end;

{ *************************************************************************************** }
function TALMultiPartFormDataContents.Add
  (AObject: TALMultiPartFormDataContent): Integer;
begin
  Result := inherited Add(AObject);
end;

{ ********************************************************************* }
function TALMultiPartFormDataContents.Add: TALMultiPartFormDataContent;
begin
  Result := TALMultiPartFormDataContent.Create;
  Try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

{ ***************************************************************************************** }
function TALMultiPartFormDataContents.GetItem(Index: Integer)
  : TALMultiPartFormDataContent;
begin
  Result := TALMultiPartFormDataContent(inherited Items[Index]);
end;

{ ******************************************************************************************* }
function TALMultiPartFormDataContents.IndexOf
  (AObject: TALMultiPartFormDataContent): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

{ ************************************************************************************************** }
procedure TALMultiPartFormDataContents.Insert(Index: Integer;
  AObject: TALMultiPartFormDataContent);
begin
  inherited Insert(Index, AObject);
end;

{ ****************************************************************************************** }
function TALMultiPartFormDataContents.Remove
  (AObject: TALMultiPartFormDataContent): Integer;
begin
  Result := inherited Remove(AObject);
end;

{ *************************************************************************************************** }
procedure TALMultiPartFormDataContents.SetItem(Index: Integer;
  AObject: TALMultiPartFormDataContent);
begin
  inherited Items[Index] := AObject;
end;

{ ************************************************************ }
procedure TAlMultiPartFormDataStream.AddFile(const aFieldName, aFileName,
  aContentType: AnsiString; aFileData: TStream);
Var
  aContent: TALMultiPartFormDataContent;
begin
  aContent := TALMultiPartFormDataContent.Create;
  Try
    aContent.LoadDataFromStream(aFileData);
    aContent.ContentType := aContentType;
    aContent.ContentDisposition := 'form-data; name="' + aFieldName +
      '"; filename="' + aFileName + '"';
    AddContent(aContent);
  Finally
    aContent.Free;
  end;
end;

{ ************************************************************ }
procedure TAlMultiPartFormDataStream.AddFile(const aFieldName,
  aFileName: AnsiString);
Var
  aContent: TALMultiPartFormDataContent;
begin
  aContent := TALMultiPartFormDataContent.Create;
  Try
    aContent.LoadDataFromFile(aFileName);
    aContent.ContentDisposition := 'form-data; name="' + aFieldName +
      '"; filename="' + aFileName + '"';
    AddContent(aContent);
  Finally
    aContent.Free;
  end;
end;

{ *************************************************************************************** }
procedure TAlMultiPartFormDataStream.AddField(const aFieldName,
  aFieldValue: AnsiString);
Var
  aContent: TALMultiPartFormDataContent;
  aStringStream: TALStringStream;
begin
  aStringStream := TALStringStream.Create(aFieldValue);
  aContent := TALMultiPartFormDataContent.Create;
  Try
    aContent.LoadDataFromStream(aStringStream);
    aContent.ContentDisposition := 'form-data; name="' + aFieldName + '"';
    AddContent(aContent);
  Finally
    aContent.Free;
    aStringStream.Free;
  end;
end;

{ ************************************************************************************* }
procedure TAlMultiPartFormDataStream.AddContent
  (aContent: TALMultiPartFormDataContent);
begin
  Inherited AddContent(aContent);
end;

{ **************************************************************************** }
function TALMultipartFormDataEncoder.CreateDataStream: TAlMultiPartBaseStream;
begin
  Result := TAlMultiPartFormDataStream.Create;
end;

{ ***************************************************************************** }
function TALMultipartFormDataEncoder.GetDataStream: TAlMultiPartFormDataStream;
begin
  Result := TAlMultiPartFormDataStream(inherited GetDataStream);
end;

{ ******************************************************************************************************************** }
procedure TALMultipartFormDataEncoder.Encode(aContentFields: TALStrings;
  aContentFiles: TALMultiPartFormDataContents);
Var
  i: Integer;
begin
  with TAlMultiPartFormDataStream(DataStream) do
  begin
    Clear;
    If assigned(aContentFiles) then
      For i := 0 to aContentFiles.Count - 1 do
        AddContent(aContentFiles[i]);
    If assigned(aContentFields) then
      With aContentFields do
        For i := 0 to Count - 1 do
          AddField(Names[i], ValueFromIndex[i]);
    CloseBoundary;
  end;
end;

{ ********************************************* }
constructor TALMultipartFormDataDecoder.Create;
begin
  inherited;
  FContentFiles := TALMultiPartFormDataContents.Create(False);
  FContentFields := TALStringList.Create;
end;

{ ********************************************* }
destructor TALMultipartFormDataDecoder.Destroy;
begin
  FContentFiles.Free;
  FContentFields.Free;
  inherited;
end;

{ **************************************************************************** }
procedure TALMultipartFormDataDecoder.Decode(aDataStr, aboundary: AnsiString);
Var
  acontents: TALMultiPartFormDataContents;
  i: Integer;
begin
  // Update the Fcontent
  inherited Decode(aDataStr, aboundary);

  // clear the FContentFiles and FContentFields
  FContentFiles.Clear;
  FContentFields.Clear;

  // loop on all contents
  acontents := GetContents;
  For i := 0 to acontents.Count - 1 do
  begin
    If (acontents[i].FileName <> '') then
      FContentFiles.Add(acontents[i])
      // if Filename or contentType set them assume its File
    else
      FContentFields.Add(acontents[i].FieldName + '=' + acontents[i]
        .DataString); // it's a field value
  end;
end;

{ *************************************************************************** }
procedure TALMultipartFormDataDecoder.Decode(aDataStr, aboundary: AnsiString;
  aContentFields: TALStrings; aContentFiles: TALMultiPartFormDataContents);
Var
  acontents: TALMultiPartFormDataContents;
begin
  // Update the Fcontent
  inherited Decode(aDataStr, aboundary);

  // clear the aContentFiles and aContentFields
  aContentFields.Clear;
  aContentFiles.Clear;

  // loop on all contents
  acontents := GetContents;
  While acontents.Count > 0 do
  begin
    If (acontents[0].FileName <> '') then
      aContentFiles.Add(acontents.Extract(acontents[0]))
      // if Filename or contentType set them assume its File
    else
    begin
      aContentFields.Add(acontents[0].FieldName + '=' + acontents[0]
        .DataString); // it's a field value
      acontents.Delete(0);
    end;
  end;
end;

{ ************************************************************************** }
function TALMultipartFormDataDecoder.CreateContent: TALMultiPartBaseContent;
begin
  Result := TALMultiPartFormDataContent.Create;
end;

{ **************************************************************************** }
function TALMultipartFormDataDecoder.CreateContents: TALMultiPartBaseContents;
begin
  Result := TALMultiPartFormDataContents.Create(True);
end;

{ ***************************************************************************** }
function TALMultipartFormDataDecoder.GetContents: TALMultiPartFormDataContents;
begin
  Result := TALMultiPartFormDataContents(inherited GetContents);
end;

{ **************************************************************** }
function TALMultipartFormDataDecoder.GetContentFields: TALStrings;
begin
  Result := FContentFields;
end;

{ ********************************************************************************* }
function TALMultipartFormDataDecoder.GetContentFiles
  : TALMultiPartFormDataContents;
begin
  Result := FContentFiles;
end;

{ ******************************************************* }
function TALMultiPartMixedContent.GetAttachment: Boolean;
Var
  Lst: TALStringList;
  i: Integer;
begin
  Result := False;
  Lst := TALStringList.Create;
  Try
    Lst.Text := AlStringReplace(ContentDisposition, ';', #13#10,
      [RfReplaceAll]);
    For i := 0 to Lst.Count - 1 do
      If AlLowerCase(ALTrim(Lst[i])) = 'attachment' then
      begin
        Result := True;
        Break;
      end;
  finally
    Lst.Free;
  end;
end;

{ ************************************************************************************************* }
procedure TALMultiPartMixedContent.LoadDataFromFileAsAttachmentBase64Encode
  (aFileName: AnsiString);
begin
  LoadDataFromFileBase64Encode(aFileName);
  aFileName := ALExtractFileName(aFileName);
  ContentType := ContentType + '; name="' + aFileName + '"';
  ContentDisposition := 'attachment; filename="' + aFileName + '"';
end;

{ ********************************************************************************** }
function TALMultiPartMixedContents.Add
  (AObject: TALMultiPartMixedContent): Integer;
begin
  Result := inherited Add(AObject);
end;

{ *************************************************************** }
function TALMultiPartMixedContents.Add: TALMultiPartMixedContent;
begin
  Result := TALMultiPartMixedContent.Create;
  Try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

{ *********************************************************************************** }
function TALMultiPartMixedContents.GetItem(Index: Integer)
  : TALMultiPartMixedContent;
begin
  Result := TALMultiPartMixedContent(inherited Items[Index]);
end;

{ ************************************************************************************* }
function TALMultiPartMixedContents.IndexOf
  (AObject: TALMultiPartMixedContent): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

{ ******************************************************************************************** }
procedure TALMultiPartMixedContents.Insert(Index: Integer;
  AObject: TALMultiPartMixedContent);
begin
  inherited Insert(Index, AObject);
end;

{ ************************************************************************************ }
function TALMultiPartMixedContents.Remove
  (AObject: TALMultiPartMixedContent): Integer;
begin
  Result := inherited Remove(AObject);
end;

{ ********************************************************************************************* }
procedure TALMultiPartMixedContents.SetItem(Index: Integer;
  AObject: TALMultiPartMixedContent);
begin
  inherited Items[Index] := AObject;
end;

{ ******************************************************************************************************************* }
procedure TAlMultiPartMixedStream.AddAttachmentBase64Encode(aFileName,
  aContentType: AnsiString; aFileData: TStream);
Var
  aContent: TALMultiPartMixedContent;
begin
  aContent := TALMultiPartMixedContent.Create;
  Try
    aContent.LoadDataFromStreamBase64Encode(aFileData);
    aFileName := ALExtractFileName(aFileName);
    aContent.ContentType := aContentType + '; name="' + aFileName + '"';
    aContent.ContentDisposition := 'attachment; filename="' + aFileName + '"';
    AddContent(aContent);
  Finally
    aContent.Free;
  end;
end;

{ ********************************************************************************* }
procedure TAlMultiPartMixedStream.AddAttachmentBase64Encode
  (aFileName: AnsiString);
Var
  aContent: TALMultiPartMixedContent;
begin
  aContent := TALMultiPartMixedContent.Create;
  Try
    aContent.LoadDataFromFileAsAttachmentBase64Encode(aFileName);
    AddContent(aContent);
  Finally
    aContent.Free;
  end;
end;

{ ******************************************************************************************* }
procedure TAlMultiPartMixedStream.AddInlineTextBase64Encode(aContentType,
  aText: AnsiString);
Var
  aContent: TALMultiPartMixedContent;
  aStringStream: TALStringStream;
begin
  aContent := TALMultiPartMixedContent.Create;
  aStringStream := TALStringStream.Create(aText);
  Try
    aContent.LoadDataFromStreamBase64Encode(aStringStream);
    aContent.ContentType := aContentType;
    AddContent(aContent);
  Finally
    aContent.Free;
    aStringStream.Free;
  end;
end;

{ ******************************************************************************* }
procedure TAlMultiPartMixedStream.AddContent
  (aContent: TALMultiPartMixedContent);
Var
  sFormFieldInfo: AnsiString;
begin
  If Position = 0 then
  Begin
    sFormFieldInfo := #13#10 +
      'This is a multi-part message in MIME format.' + #13#10;
    Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
  end;

  Inherited AddContent(aContent);
end;

{ ************************************************************************* }
function TALMultipartMixedEncoder.CreateDataStream: TAlMultiPartBaseStream;
begin
  Result := TAlMultiPartMixedStream.Create;
end;

{ *********************************************************************** }
function TALMultipartMixedEncoder.GetDataStream: TAlMultiPartMixedStream;
begin
  Result := TAlMultiPartMixedStream(inherited GetDataStream);
end;

{ **************************************************** }
procedure TALMultipartMixedEncoder.Encode(aInlineText, aInlineTextContentType
  : AnsiString; aAttachments: TALMultiPartMixedContents);
Var
  i: Integer;
begin
  with TAlMultiPartMixedStream(DataStream) do
  begin
    Clear;
    AddInlineTextBase64Encode(aInlineTextContentType, aInlineText);
    If assigned(aAttachments) then
      For i := 0 to aAttachments.Count - 1 do
        AddContent(aAttachments[i]);
    CloseBoundary;
  end;
end;

{ *********************************************************************** }
function TALMultipartMixedDecoder.CreateContent: TALMultiPartBaseContent;
begin
  Result := TALMultiPartMixedContent.Create;
end;

{ ************************************************************************* }
function TALMultipartMixedDecoder.CreateContents: TALMultiPartBaseContents;
begin
  Result := TALMultiPartMixedContents.Create(True);
end;

{ *********************************************************************** }
function TALMultipartMixedDecoder.GetContents: TALMultiPartMixedContents;
begin
  Result := TALMultiPartMixedContents(inherited GetContents);
end;

end.
