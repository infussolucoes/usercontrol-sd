{*************************************************************
Author:       Stéphane Vander Clock (SVanderClock@Arkadia.com)

Contributor   François PIETTE (http://www.overbyte.be)
              Paul TOTH (tothpaul@free.fr - http://tothpaul.free.fr)

EMail:        http://www.arkadia.com
              SVanderClock@Arkadia.com

product:      TALSMTPClient
Version:      3.05

Description:  TALsmtpClient class implements the SMTP protocol (RFC-821)
              Support file attachement using MIME format (RFC-1521, RFC-2045)
              Support authentification (RFC-2104)

Legal issues: Copyright (C) 2005 by Stéphane Vander Clock

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

History :

Link :        http://linuxgazette.net/issue45/stumpel.html
              http://www.overbyte.be
              http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winsock/winsock/socket_options.asp
              http://www.fehcom.de/qmail/smtpauth.html
              http://www.freesoft.org/CIE/RFC/821/
              http://www.expita.com/header1.html
              http://cr.yp.to/immhf.html

Please send all your feedback to SVanderClock@Arkadia.com
**************************************************************}

{$I 'UserControl.inc'}

unit UCALSMTPClient;

interface

uses
  Classes,
  Dialogs,
  sysutils,
  windows,
  WinSock;

type
  {Note: when changing TVendor, also change VendorStr array below}
  TALCPUVendor             = (
    cvUnknown,
    cvAMD,
    cvCentaur,
    cvCyrix,
    cvIntel,
    cvTransmeta,
    cvNexGen,
    cvRise,
    cvUMC,
    cvNSC,
    cvSiS
    );
{Note: when changing TInstruction, also change InstructionSupportStr below
   * - instruction(s) not supported in Delphi 7 assembler}
  TALCPUInstructions       = (
    isFPU,     {80x87}
    isTSC,     {RDTSC}
    isCX8,     {CMPXCHG8B}
    isSEP,     {SYSENTER/SYSEXIT}
    isCMOV,    {CMOVcc, and if isFPU, FCMOVcc/FCOMI}
    isMMX,     {MMX}
    isFXSR,    {FXSAVE/FXRSTOR}
    isSSE,     {SSE}
    isSSE2,    {SSE2}
    isSSE3,    {SSE3*}
    isMONITOR, {MONITOR/MWAIT*}
    isCX16,    {CMPXCHG16B*}
    isX64,     {AMD AMD64* or Intel EM64T*}
    isExMMX,   {MMX+ - AMD only}
    isEx3DNow, {3DNow!+ - AMD only}
    is3DNow    {3DNow! - AMD only}
    );
  TALCPUInstructionSupport = set of TALCPUInstructions;

  TALCPUInfo = record
    Vendor:             TALCPUVendor;
    Signature:          cardinal;
    EffFamily:          byte;      {ExtendedFamily + Family}
    EffModel:           byte;      {(ExtendedModel shl 4) + Model}
    CodeL1CacheSize,               {KB or micro-ops for Pentium 4}
    DataL1CacheSize,               {KB}
    L2CacheSize,                   {KB}
    L3CacheSize:        Word;      {KB}
    InstructionSupport: TALCPUInstructionSupport;
  end;
  TALHandleTagfunct = function(const TagString: String; TagParams: TStrings; ExtData: pointer; var Handled: Boolean): String;
  TALMoveProc = procedure(const Source; var Dest; Count: Integer);
  TALCharPosFunct = function(Ch: char; const Str: ansistring): Integer;
  TALPosFunct = function(const SubStr: ansistring; const Str: ansistring): Integer;


  PALMimeBase64Byte4 = ^TALMimeBase64Byte4;

  TALMimeBase64Byte4 = packed record
    b1: byte;
    b2: byte;
    b3: byte;
    b4: byte;
  end;
  PALMimeBase64Byte3 = ^TALMimeBase64Byte3;

  TALMimeBase64Byte3 = packed record
    b1: byte;
    b2: byte;
    b3: byte;
  end;

  {-----------------------}
  TAlSmtpClientAuthType = (
    AlsmtpClientAuthNone,
    alsmtpClientAuthPlain,
    AlsmtpClientAuthLogin,
    AlsmtpClientAuthCramMD5,
    AlsmtpClientAuthCramSha1,
    AlsmtpClientAuthAutoSelect
    );

  {------------------------------------------------------}
  TAlSmtpClientAuthTypeSet = set of TAlSmtpClientAuthType;

  {--------------------------------------}
  TALSMTPClientHeader = class(TPersistent)
  private
    fSendTo:        String;
    fSender:        String;
    fMessageID:     String;
    fbcc:           String;
    fContentTransferEncoding: String;
    fComments:      String;
    fMIMEVersion:   String;
    fPriority:      String;
    fReplyTo:       String;
    fSubject:       String;
    fFrom:          String;
    fDate:          String;
    fDispositionNotificationTo: String;
    fReferences:    String;
    fcc:            String;
    fContentType:   String;
    FCustomHeaders: TStrings;
    function GetRawHeaderText: String;
    procedure SetRawHeaderText(const aRawHeaderText: String);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
  published
    property From: String Read fFrom Write fFrom; {From: John Doe <jdoe@machine.example> - Author(s) or person(s) taking responsibility for the message 4.4.1; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1}
    property Sender: String Read fSender Write fSender; {Sender: Michael Jones <mjones@machine.example> - The person or agent submitting the message to the network, if other than shown by the From header RFC 822: 4.4.2; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1}
    property SendTo: String Read fSendTo Write fSendTo; {To: Mary Smith <mary@example.net> - Primary recipient(s) RFC 822: 4.5.1; RFC 1123: 5.2.15-16, 5.3.7;}
    property cc: String Read fcc Write fcc; {cc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net> - Secondary, informational recipient(s) RFC 822: 4.5.2; RFC 1123: 5.2.15-16, 5.3.7;}
    property bcc: String Read fbcc Write fbcc; {bcc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net> - Recipient(s) not to be disclosed to other recipients ("blind carbon copy") RFC 822: 4.5.3; RFC 1123: 5.2.15-16, 5.3.7;}
    property ReplyTo: String Read fReplyTo Write fReplyTo; {Reply-To: "Mary Smith: Personal Account" <smith@home.example> - Suggested E-mail address for replies RFC 822: 4.4.3; RFC 1036: 2.2.1}
    property Subject: String Read fSubject Write fSubject; {Subject: Saying Hello - Text that provides a summary, or indicates the nature, of the message RFC 822: 4.7.1; RFC 1036: 2.1.4}
    property MessageID: String Read fMessageID Write fMessageID; {Message-ID: <1234@local.machine.example> -  Unique ID for the message RFC 822: 4.6.1; RFC 1036: 2.1.5}
    property References: String Read fReferences Write fReferences; {References: <1234@local.machine.example> <3456@example.net> - In E-mail: reference to other related messages; in Usenet: reference to replied-to-articles RFC 822: 4.6.3; RFC 1036: 2.2.5}
    property Comments: String Read fComments Write fComments; {Comments: Authenticated sender is gboyd@netcom.com - Text comments added to the message RFC 822: 4.7.2}
    property Date: String Read fDate Write fDate; {Date: Fri, 21 Nov 1997 09:55:06 -0600 - The time when the message was written (or submitted) RFC 822: 5.1; RFC 1123: 5.2.14; RFC 1036: 2.1.2}
    property ContentType: String Read fContentType Write fContentType; {Content-Type: text/plain; charset="iso-8859-1" - Data type and format of content RFC 1049 (historic); RFC 1123: 5.2.13; RFC 2045: 5; RFC 1766: 4.1}
    property ContentTransferEncoding: String Read fContentTransferEncoding Write fContentTransferEncoding; {Content-Transfer-Encoding: 8bit - Coding method used in a MIME message body RFC 2045: 6;}
    property MIMEVersion: String Read fMIMEVersion Write fMIMEVersion; {MIME-Version: 1.0 - specifies the version of MIME that the message format complies with RFC 2045: 4}
    property Priority: String Read fPriority Write fPriority; {Priority: normal - Priority for message delivery ("normal" / "non-urgent" / "urgent") RFC 2156}
    property DispositionNotificationTo: String Read fDispositionNotificationTo Write fDispositionNotificationTo; {Disposition-Notification-To: boss@nil.test - Requests for notification when the message is received, and specifies the address for them RFC 2298}
    property CustomHeaders: TStrings Read FCustomHeaders;
    property RawHeaderText: String Read GetRawHeaderText Write SetRawHeaderText;
  end;

  TOnStatus = procedure(Status: String) of object;

  {----------------------------}
  TAlSmtpClient = class(TObject)
  private
    FWSAData:            TWSAData;
    Fconnected:          Boolean;
    FSocketDescriptor:   Integer;
    FAuthTypesSupported: TAlSmtpClientAuthTypeSet;
    Ftimeout:            Integer;
    fOnStatus:           TOnStatus;
    procedure Settimeout(const Value: Integer);
  protected
    procedure CheckError(Error: Boolean);
    function SendCmd(aCmd: String; OkResponses: array of Word): String; virtual;
    function GetResponse(OkResponses: array of Word): String;
    function SocketWrite(var Buffer; Count: longint): longint; virtual;
    function SocketRead(var Buffer; Count: longint): longint; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Connect(aHost: String; APort: Integer): String; virtual;
    function Helo: String; virtual;
    function Ehlo: String; virtual;
    function Auth(AUserName, APassword: String; aAuthType: TalSmtpClientAuthType): String; virtual;
    function Vrfy(aUserName: String): String; virtual;
    function MailFrom(aFromName: String): String; virtual;
    function RcptTo(aRcptNameLst: TStrings): String; virtual;
    function Data(aMailData: String): String; overload; virtual;
    function Data(aHeader, aBody: String): String; overload; virtual;
    function Data(aHeader: TALSMTPClientHeader; aBody: String): String; overload; virtual;
    function Quit: String; virtual;
    function Rset: String; virtual;
    procedure SendMail(aHost: String; APort: Integer; aFromName: String; aRcptNameLst: TStrings; AUserName, APassword: String; aAuthType: TalSmtpClientAuthType; aMailData: String); overload; virtual;
    procedure SendMail(aHost: String; APort: Integer; aFromName: String; aRcptNameLst: TStrings; AUserName, APassword: String; aAuthType: TalSmtpClientAuthType; aHeader, aBody: String); overload; virtual;
    procedure Disconnect; virtual;
    function GetAuthTypeFromEhloResponse(EhloResponse: String): TAlSmtpClientAuthTypeSet; virtual;
    property Connected: Boolean Read FConnected;
    property Timeout: Integer Read Ftimeout Write Settimeout default 60000;
    property OnStatus: TOnStatus Read fOnStatus Write fOnStatus;
  end;

{----------------------------------------------------------------------------------------}
function AlSMTPClientParseEmail(FriendlyEmail: String; var FriendlyName: String): String;
function AlSMTPClientGenerateMessageID: String;

var
  ALMove:    TALMoveProc;
  ALCharPos: TALCharPosFunct;
  ALPos:     TALPosFunct;


implementation

uses
  HTTPAPP;


 //***********************************************************************
 //AlFcnWinSock  Partial Unit
 //***********************************************************************
{$WARNINGS OFF}
function ALgetLocalHostName: String;
var
  Buffer  :  pAnsichar;
  WSAData : TWSAData;
begin
  WSAData.wVersion := 0;
  WSAStartup(MAKEWORD(2, 2), WSAData);
  try

    if gethostname(Buffer, SizeOf(Buffer)) <> 0 then
      raise Exception.Create('Winsock GetHostName failed');
    Result := StrPas(Buffer);
  finally
    if WSAData.wVersion = 2 then
      WSACleanup;
  end;
end;

function ALHostToIP(HostName: String; var Ip: String): Boolean;
var
  WSAData: TWSAData;
  hostEnt: PHostEnt;
  addr: PAnsiChar;
begin
  WSAData.wVersion := 0;
  WSAStartup(MAKEWORD(2, 2), WSAData);
  try
    hostEnt := gethostbyname(PAnsiChar(hostName));
    if Assigned(hostEnt) then
    begin
      if Assigned(hostEnt^.h_addr_list) then
      begin
        addr := hostEnt^.h_addr_list^;
        if Assigned(addr) then
        begin
          IP     := Format('%d.%d.%d.%d', [byte(addr[0]),
            byte(addr[1]), byte(addr[2]), byte(addr[3])]);
          Result := True;
        end
        else
          Result := False;
      end
      else
        Result := False;
    end
    else
      Result := False;

  finally
    if WSAData.wVersion = 2 then
      WSACleanup;
  end;
end;
{$WARNINGS ON}
 //***********************************************************************
 //AlFcnRFC  Partial Unit
 //***********************************************************************
const
  CAlRfc822DaysOfWeek: array[1..7] of String = (
    'Sun',
    'Mon',
    'Tue',
    'Wed',
    'Thu',
    'Fri',
    'Sat'
    );
  CALRfc822MonthNames: array[1..12] of String = (
    'Jan',
    'Feb',
    'Mar',
    'Apr',
    'May',
    'Jun',
    'Jul',
    'Aug',
    'Sep',
    'Oct',
    'Nov',
    'Dec'
    );


function ALGMTDateTimeToRfc822Str(const aValue: TDateTime): String;
var
  aDay, aMonth, aYear: Word;
begin
  DecodeDate(
    aValue,
    aYear,
    aMonth,
    aDay
    );

  Result := Format(
    '%s, %.2d %s %.4d %s %s',
    [
    CAlRfc822DaysOfWeek[DayOfWeek(aValue)],
    aDay,
    CAlRfc822MonthNames[aMonth],
    aYear,
    FormatDateTime('hh":"nn":"ss', aValue),
    'GMT'
    ]
    );
end;


function ALDateTimeToRfc822Str(const aValue: TDateTime): String;

  {--------------------------------------------}
  function InternalCalcTimeZoneBias: TDateTime;
  const
    Time_Zone_ID_DayLight = 2;
  var
    TZI:       TTimeZoneInformation;
    TZIResult: Integer;
    aBias:     Integer;
  begin
    TZIResult := GetTimeZoneInformation(TZI);
    if TZIResult = -1 then
      Result := 0
    else
    begin
      if TZIResult = Time_Zone_ID_DayLight then
        aBias := TZI.Bias + TZI.DayLightBias
      else
        aBias := TZI.Bias + TZI.StandardBias;
      Result := EncodeTime(Abs(aBias) div 60, Abs(aBias) mod 60, 0, 0);
      if aBias < 0 then
        Result := -Result;
    end;
  end;

begin
  Result := ALGMTDateTimeToRfc822Str(aValue + InternalCalcTimeZoneBias);
end;


 //***********************************************************************
 //AlFcnMime  Partial Unit
 //***********************************************************************
const
  cALMime_Base64_Encode_Table: array[0..63] of byte = (
    065, 066, 067, 068, 069, 070, 071, 072, //  00 - 07
    073, 074, 075, 076, 077, 078, 079, 080, //  08 - 15
    081, 082, 083, 084, 085, 086, 087, 088, //  16 - 23
    089, 090, 097, 098, 099, 100, 101, 102, //  24 - 31
    103, 104, 105, 106, 107, 108, 109, 110, //  32 - 39
    111, 112, 113, 114, 115, 116, 117, 118, //  40 - 47
    119, 120, 121, 122, 048, 049, 050, 051, //  48 - 55
    052, 053, 054, 055, 056, 057, 043, 047  // 56 - 63
    );
  cALMime_Base64_Pad_Char                           = byte('=');

function ALMimeBase64EncodedSizeNoCRLF(const InputSize: cardinal): cardinal;
begin
  Result := (InputSize + 2) div 3 * 4;
end;

{$WARNINGS OFF}
procedure ALMimeBase64EncodeNoCRLF(const InputBuffer; const InputByteCount: cardinal; out OutputBuffer);
var
  B, InnerLimit, OuterLimit: cardinal;
  InPtr:                     PALMimeBase64Byte3;
  OutPtr:                    PALMimeBase64Byte4;
begin
  if InputByteCount = 0 then
    Exit;

  InPtr  := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := cardinal(InPtr);
  Inc(InnerLimit, OuterLimit);

  { Last line loop. }
  while cardinal(InPtr) < InnerLimit do
  begin
    { Read 3 bytes from InputBuffer. }
    B := InPtr^.b1;
    B := B shl 8;
    B := B or InPtr^.b2;
    B := B shl 8;
    B := B or InPtr^.b3;
    Inc(InPtr);
    { Write 4 bytes to OutputBuffer (in reverse order). }
    OutPtr^.b4 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
    B          := B shr 6;
    OutPtr^.b3 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
    B          := B shr 6;
    OutPtr^.b2 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
    B          := B shr 6;
    OutPtr^.b1 := CALMIME_Base64_ENCODE_TABLE[B];
    Inc(OutPtr);
  end;

  { End of data & padding. }
  case InputByteCount - OuterLimit of
    1:
    begin
      B         := InPtr^.b1;
      B         := B shl 4;
      OutPtr.b2 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
      B         := B shr 6;
      OutPtr.b1 := CALMIME_Base64_ENCODE_TABLE[B];
      OutPtr.b3 := CALMIME_Base64_PAD_CHAR; { Pad remaining 2 bytes. }
      OutPtr.b4 := CALMIME_Base64_PAD_CHAR;
    end;
    2:
    begin
      B         := InPtr^.b1;
      B         := B shl 8;
      B         := B or InPtr^.b2;
      B         := B shl 2;
      OutPtr.b3 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
      B         := B shr 6;
      OutPtr.b2 := CALMIME_Base64_ENCODE_TABLE[B and $3F];
      B         := B shr 6;
      OutPtr.b1 := CALMIME_Base64_ENCODE_TABLE[B];
      OutPtr.b4 := CALMIME_Base64_PAD_CHAR; { Pad remaining byte. }
    end;
  end;
end;

function ALMimeBase64EncodeStringNoCRLF(const S: ansistring): ansistring;
var
  L: cardinal;
begin
  if Pointer(S) <> nil then
  begin
    L := PCardinal(cardinal(S) - 4)^;
    SetLength(Result, ALMimeBase64EncodedSizeNoCRLF(L));
    ALMimeBase64EncodeNoCRLF(Pointer(S)^, L, Pointer(Result)^);
  end
  else
    Result := '';
end;
{$WARNINGS ON}

 //***********************************************************************
 //ALFcnMisc  Partial Unit
 //***********************************************************************

function ALMakeKeyStrByGUID: String;
var
  aGUID: TGUID;
begin
  CreateGUID(aGUID);
  Result := GUIDToString(aGUID);
  Delete(Result, 1, 1);
  Delete(Result, Length(Result), 1);
end;


 //***********************************************************************
 //ALFCNString  Partial Unit
 //***********************************************************************


{-------------------------------}
const
  CALMOVE_SMALLMOVESIZE = 36;

{-------------------------------------------------}
var
  VALMove_AnsiUpcase: packed array[char] of char;
//    VALMove_PrefetchLimit: Integer;

 //////////////////////////////////////////////////////////////////////////////////////////////////////
 //////////ALPosEx from FastCode AINSIStringReplace John O'Harrow (john@almcrest.demon.co.uk)//////////
 //////////////////////////////////////////////////////////////////////////////////////////////////////

 {*********************************}
 {Fast Equivalent of Delphi 7 PosEx} {$WARNINGS OFF}
function ALPosEx(const SubStr, S: String; Offset: cardinal = 1): Integer;
asm
  push    ebx
  push    esi
  push    edx              {@Str}
  test    eax, eax
  jz      @@NotFound       {Exit if SubStr = ''}
  test    edx, edx
  jz      @@NotFound       {Exit if Str = ''}
  mov     esi, ecx
  mov     ecx, [edx-4]     {Length(Str)}
  mov     ebx, [eax-4]     {Length(SubStr)}
  add     ecx, edx
  sub     ecx, ebx         {Max Start Pos for Full Match}
  lea     edx, [edx+esi-1] {Set Start Position}
  cmp     edx, ecx
  jg      @@NotFound       {StartPos > Max Start Pos}
  cmp     ebx, 1           {Length(SubStr)}
  jle     @@SingleChar     {Length(SubStr) <= 1}
  push    edi
  push    ebp
  lea     edi, [ebx-2]     {Length(SubStr) - 2}
  mov     esi, eax
  movzx   ebx, [eax]         {Search Character}
  @@Loop:                    {Compare 2 Characters per Loop}
  cmp     bl, [edx]
  jne     @@NotChar1
  mov     ebp, edi         {Remainder}
  @@Char1Loop:
  movzx   eax, word ptr [esi+ebp]
  cmp     ax, [edx+ebp]
  jne     @@NotChar1
  sub     ebp, 2
  jnc     @@Char1Loop
  pop     ebp
  pop     edi
  jmp     @@SetResult
  @@NotChar1:
  cmp     bl, [edx+1]
  jne     @@NotChar2
  mov     ebp, edi         {Remainder}
  @@Char2Loop:
  movzx   eax, word ptr [esi+ebp]
  cmp     ax, [edx+ebp+1]
  jne     @@NotChar2
  sub     ebp, 2
  jnc     @@Char2Loop
  pop     ebp
  pop     edi
  jmp     @@CheckResult
  @@NotChar2:
  add     edx, 2
  cmp     edx, ecx         {Next Start Position <= Max Start Position}
  jle     @@Loop
  pop     ebp
  pop     edi
  jmp     @@NotFound
  @@SingleChar:
  jl      @@NotFound       {Needed for Zero-Length Non-NIL Strings}
  movzx   eax, [eax]       {Search Character}
  @@CharLoop:
  cmp     al, [edx]
  je      @@SetResult
  cmp     al, [edx+1]
  je      @@CheckResult
  add     edx, 2
  cmp     edx, ecx
  jle     @@CharLoop
  @@NotFound:
  xor     eax, eax
  pop     edx
  pop     esi
  pop     ebx
  ret
  @@CheckResult:             {Check within String}
  cmp     edx, ecx
  jge     @@NotFound
  add     edx, 1
  @@SetResult:
  pop     ecx              {@Str}
  pop     esi
  pop     ebx
  neg     ecx
  lea     eax, [edx+ecx+1]
end;

 {***********************************}
 {Non Case Sensitive version of PosEx}
function ALPosExIgnoreCase(const SubStr, S: String; Offset: cardinal = 1): Integer;
asm
  push    ebx
  push    esi
  push    edx              {@Str}
  test    eax, eax
  jz      @@NotFound       {Exit if SubStr = ''}
  test    edx, edx
  jz      @@NotFound       {Exit if Str = ''}
  mov     esi, ecx
  mov     ecx, [edx-4]     {Length(Str)}
  mov     ebx, [eax-4]     {Length(SubStr)}
  add     ecx, edx
  sub     ecx, ebx         {Max Start Pos for Full Match}
  lea     edx, [edx+esi-1] {Set Start Position}
  cmp     edx, ecx
  jg      @@NotFound       {StartPos > Max Start Pos}
  cmp     ebx, 1           {Length(SubStr)}
  jle     @@SingleChar     {Length(SubStr) <= 1}
  push    edi
  push    ebp
  lea     edi, [ebx-2]     {Length(SubStr) - 2}
  mov     esi, eax
  push    edi                      {Save Remainder to Check = Length(SubStr) - 2}
  push    ecx                      {Save Max Start Position}
  lea     edi, VALMove_AnsiUpcase  {Uppercase Lookup Table}
  movzx   ebx, [eax]               {Search Character = 1st Char of SubStr}
  movzx   ebx, [edi+ebx]           {Convert to Uppercase}
  @@Loop:                          {Loop Comparing 2 Characters per Loop}
  movzx   eax, [edx]               {Get Next Character}
  movzx   eax, [edi+eax]           {Convert to Uppercase}
  cmp     eax, ebx
  jne     @@NotChar1
  mov     ebp, [esp+4]     {Remainder to Check}
  @@Char1Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  sub     ebp, 2
  jnc     @@Char1Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@SetResult
  @@NotChar1:
  movzx   eax, [edx+1]     {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     bl, al
  jne     @@NotChar2
  mov     ebp, [esp+4]     {Remainder to Check}
  @@Char2Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+2]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  sub     ebp, 2
  jnc     @@Char2Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@CheckResult    {Check Match is within String Data}
  @@NotChar2:
  add     edx, 2
  cmp     edx, [esp]       {Compate to Max Start Position}
  jle     @@Loop           {Loop until Start Position > Max Start Position}
  pop     ecx              {Dump Start Position}
  pop     edi              {Dump Remainder to Check}
  pop     ebp
  pop     edi
  jmp     @@NotFound
  @@SingleChar:
  jl      @@NotFound       {Needed for Zero-Length Non-NIL Strings}
  lea     esi, VALMove_AnsiUpcase
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [esi+ebx]   {Convert to Uppercase}
  @@CharLoop:
  movzx   eax, [edx]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@SetResult
  movzx   eax, [edx+1]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@CheckResult
  add     edx, 2
  cmp     edx, ecx
  jle     @@CharLoop
  @@NotFound:
  xor     eax, eax
  pop     edx
  pop     esi
  pop     ebx
  ret
  @@CheckResult:             {Check Match is within String Data}
  cmp     edx, ecx
  jge     @@NotFound
  add     edx, 1             {OK - Adjust Result}
  @@SetResult:               {Set Result Position}
  pop     ecx                {@Str}
  pop     esi
  pop     ebx
  neg     ecx
  lea     eax, [edx+ecx+1]
end;


 ///////////////////////////////////////////////////////////////////////////////////////////////
 //////////AlFastStringReplace from FastCode John O'Harrow (john@almcrest.demon.co.uk)//////////
 ///////////////////////////////////////////////////////////////////////////////////////////////

 {****************************************}
 {Non-Overlapping Move for Positive Counts}
procedure ALStringReplaceMoveEx(const Source; var Dest; Count: Integer);
const
  SMALLMOVESIZE = 16;
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large
  lea     eax, [eax+ecx]
  lea     edx, [edx+ecx]
  jmp     dword ptr [@@FwdJumpTable+ecx*4]
  @Large:
  fild    qword ptr [eax]
  fistp   qword ptr [edx]
  add     eax, ecx
  add     ecx, edx
  add     edx, 7+8
  neg     ecx
  and     edx, -8
  add     ecx, edx
  sub     edx, ecx {Writes Now QWORD Aligned}
  @FwdLoop:
  fild    qword ptr [eax+ecx-8]
  fistp   qword ptr [edx+ecx-8]
  add     ecx, 8
  jle     @FwdLoop
  neg     ecx
  add     ecx, 8
  jmp     dword ptr [@@FwdJumpTable+ecx*4]
  nop {Align Jump Table}
  @@FwdJumpTable:
  dd      @@Done {Removes need to test for zero size Move}
  dd      @@Fwd01,@@Fwd02,@@Fwd03,@@Fwd04,@@Fwd05,@@Fwd06,@@Fwd07,@@Fwd08
  dd      @@Fwd09,@@Fwd10,@@Fwd11,@@Fwd12,@@Fwd13,@@Fwd14,@@Fwd15,@@Fwd16
  @@Fwd16:
  mov     ecx,[eax-16]
  mov[edx-16],ecx
  @@Fwd12:
  mov     ecx,[eax-12]
  mov[edx-12],ecx
  @@Fwd08:
  mov     ecx,[eax-8]
  mov[edx-8],ecx
  @@Fwd04:
  mov     ecx,[eax-4]
  mov[edx-4],ecx
  ret
  @@Fwd15:
  mov     ecx,[eax-15]
  mov[edx-15],ecx
  @@Fwd11:
  mov     ecx,[eax-11]
  mov[edx-11],ecx
  @@Fwd07:
  mov     ecx,[eax-7]
  mov[edx-7],ecx
  @@Fwd03:
  movzx   ecx, word ptr [eax-3]
  mov[edx-3],cx
  movzx   ecx, byte ptr [eax-1]
  mov[edx-1],cl
  ret
  @@Fwd14:
  mov     ecx,[eax-14]
  mov[edx-14],ecx
  @@Fwd10:
  mov     ecx,[eax-10]
  mov[edx-10],ecx
  @@Fwd06:
  mov     ecx,[eax-6]
  mov[edx-6],ecx
  @@Fwd02:
  movzx   ecx, word ptr [eax-2]
  mov[edx-2],cx
  ret
  @@Fwd13:
  mov     ecx,[eax-13]
  mov[edx-13],ecx
  @@Fwd09:
  mov     ecx,[eax-9]
  mov[edx-9],ecx
  @@Fwd05:
  mov     ecx,[eax-5]
  mov[edx-5],ecx
  @@Fwd01:
  movzx   ecx, byte ptr [eax-1]
  mov[edx-1],cl
  @@Done:
end; {MoveEx}

 {**************************************************************************}
 {Replace all occurance of Old (Ignoring Case) with New in Non-Null String S}
{$IFDEF DELPHI12}
procedure ALCharReplaceIC(var S: ansistring; const Old, New: pAnsichar);
{$ELSE}
procedure ALCharReplaceIC(var S: ansistring; const Old, New: char);
{$ENDIF}
asm
  push  ebx
  push  edi
  push  esi
  mov   eax, [eax]         {@S}
  mov   ebx, ecx           {bl = New}
  lea   edi, VALMove_AnsiUpcase
  and   edx, $FF           {edx = Old}
  mov   ecx, [eax-4]       {Length(S)}
  movzx edx, [edx+edi]     {edx = Uppercase(Old)}
  lea   esi, [eax+ecx]
  neg   ecx
  @@Loop:
  movzx eax, [esi+ecx]     {Next Char}
  movzx eax, [eax+edi]     {Convert to Uppercase}
  cmp   eax, edx           {Compare Char}
  jne   @@Next
  mov[esi+ecx], bl      {Replace Char}
  @@Next:
  add   ecx, 1
  jnz   @@Loop
  pop   esi
  pop   edi
  pop   ebx
end;

 {***************************************************************************}
 {Replace all occurance of Old (Case Sensitive) with New in Non-Null String S}
{$IFDEF DELPHI12}
procedure ALCharReplaceCS(var S: ansistring; const Old, New: pAnsichar);
{$ELSE}
procedure ALCharReplaceCS(var S: ansistring; const Old, New: char);
{$ENDIF}
asm
  push  ebx
  mov   eax, [eax]    {@S}
  mov   ebx, ecx      {bl = New, dl = Old}
  mov   ecx, [eax-4]  {Length(S)}
  add   eax, ecx
  neg   ecx
  @@Loop:
  cmp   dl, [eax+ecx] {Compare Next Char}
  jne   @@Next
  mov[eax+ecx], bl {Replace Char}
  @@Next:
  add   ecx, 1
  jnz   @@Loop
  pop   ebx
end;

 {***************************************************************************************}
 {from John O'Harrow (john@almcrest.demon.co.uk) - original name: StringReplaceJOH_IA32_4}
function ALStringReplace(const S, OldPattern, NewPattern: ansistring; Flags: TReplaceFlags): ansistring;
type
  TPosEx   = function(const SubStr, S: String; Offset: cardinal = 1): Integer;
  {$IFDEF DELPHI12}
    TCharRep = procedure(var S: ansistring; const Old, New: pAnsichar);
  {$ELSE}
    TCharRep = procedure(var S: ansistring; const Old, New: char);
  {$ENDIF}
const
  StaticBufferSize = 16;
  PosExFunction: array[Boolean] of TPosEx = (ALPosEx, ALPosExIgnoreCase);
  CharReplace: array[Boolean] of TCharRep = (ALCharReplaceCS, ALCharReplaceIC);
var
  SrcLen, OldLen, NewLen, Found, Count, Start, Match, BufSize, BufMax: Integer;
  StaticBuffer:  array[0..StaticBufferSize - 1] of Integer;
  Buffer:        PIntegerArray;
  PSrc, PRes: PAnsiChar;
  IgnoreCase:   Boolean;
begin
{$IFDEF ALStringReplace_AllowLengthShortcut}
  SrcLen := 0;
  if (S <> '') then
    SrcLen := PCardinal(Cardinal(S)-4)^;
  OldLen := 0;
  if (OldPattern <> '') then
    OldLen := PCardinal(Cardinal(OldPattern)-4)^;
  NewLen := 0;
  if (NewPattern <> '') then
    NewLen := PCardinal(Cardinal(NewPattern)-4)^;
{$ELSE}
  SrcLen := Length(S);
  OldLen := Length(OldPattern);
  NewLen := Length(NewPattern);
{$ENDIF}
  if (OldLen = 0) or (SrcLen < OldLen) then
  begin
    if SrcLen = 0 then
      Result := '' {Needed for Non-Nil Zero Length Strings}
    else
      Result := S;
  end
  else
  begin
    IgnoreCase := rfIgnoreCase in Flags;
    if rfReplaceAll in Flags then
    begin
      if (OldLen = 1) and (NewLen = 1) then
      begin
        SetLength(Result, SrcLen);
        ALStringReplaceMoveEx(Pointer(S)^, Pointer(Result)^, SrcLen);
        CharReplace[IgnoreCase](Result, Char(OldPattern[1]), Char(NewPattern[1]));
        Exit;
      end;
      Found := PosExFunction[IgnoreCase](OldPattern, S, 1);
      if Found <> 0 then
      begin
        Buffer    := @StaticBuffer;
        BufMax    := StaticBufferSize;
        BufSize   := 1;
        Buffer[0] := Found;
        repeat
          Inc(Found, OldLen);
          Found := PosExFunction[IgnoreCase](OldPattern, S, Found);
          if Found > 0 then
          begin
            if BufSize = BufMax then
            begin                                {Create or Expand Dynamic Buffer}
              BufMax := BufMax + (BufMax shr 1); {Grow by 50%}
              if Buffer = @StaticBuffer then
              begin {Create Dynamic Buffer}
                GetMem(Buffer, BufMax * SizeOf(integer));
                ALStringReplaceMoveEx(StaticBuffer, Buffer^, SizeOf(StaticBuffer));
              end
              else {Expand Dynamic Buffer}
                ReallocMem(Buffer, BufMax * SizeOf(integer));
            end;
            Buffer[BufSize] := Found;
            Inc(BufSize);
          end
        until Found = 0;
        SetLength(Result, SrcLen + (BufSize * (NewLen - OldLen)));
        PSrc  := Pointer(S);
        PRes  := Pointer(Result);
        Start := 1;
        Match := 0;
        repeat
          Found := Buffer[Match];
          Count := Found - Start;
          Start := Found + OldLen;
          if Count > 0 then
          begin
            ALStringReplaceMoveEx(PSrc^, PRes^, Count);
            Inc(PRes, Count);
          end;
          Inc(PSrc, Count + OldLen);
          ALStringReplaceMoveEx(Pointer(NewPattern)^, PRes^, NewLen);
          Inc(PRes, NewLen);
          Inc(Match);
        until Match = BufSize;
        Dec(SrcLen, Start);
        if SrcLen >= 0 then
          ALStringReplaceMoveEx(PSrc^, PRes^, SrcLen + 1);
        if BufMax <> StaticBufferSize then
          FreeMem(Buffer); {Free Dynamic Buffwe if Created}
      end
      else {No Matches Found}
        Result := S;
    end
    else
    begin {Replace First Occurance Only}
      Found := PosExFunction[IgnoreCase](OldPattern, S, 1);
      if Found <> 0 then
      begin {Match Found}
        SetLength(Result, SrcLen - OldLen + NewLen);
        Dec(Found);
        PSrc := Pointer(S);
        PRes := Pointer(Result);
        if NewLen = OldLen then
        begin
          ALStringReplaceMoveEx(PSrc^, PRes^, SrcLen);
          Inc(PRes, Found);
          ALStringReplaceMoveEx(Pointer(NewPattern)^, PRes^, NewLen);
        end
        else
        begin
          ALStringReplaceMoveEx(PSrc^, PRes^, Found);
          Inc(PRes, Found);
          Inc(PSrc, Found + OldLen);
          ALStringReplaceMoveEx(Pointer(NewPattern)^, PRes^, NewLen);
          Inc(PRes, NewLen);
          ALStringReplaceMoveEx(PSrc^, PRes^, SrcLen - Found - OldLen);
        end;
      end
      else {No Matches Found}
        Result := S;
    end;
  end;
end;


 ////////////////////////////////////////////////////////////////
 //////////ALUpperCase from FastCode Aleksandr Sharahov//////////
 ////////////////////////////////////////////////////////////////

{********************************************}
function AlUpperCase(const s: String): String;
asm
  push  ebx
  push  esi
  push  edi
  mov   esi, eax          // s
  mov   eax, edx
  test  esi, esi
  jz    @nil
  mov   ebx, [esi-4]      // Length(s)
  mov   edx, ebx
  mov   edi, eax          // @Result
  add   ebx, -1
  jl    @nil
  and   ebx, -4
  call  System.@LStrSetLength
  mov   eax, [ebx+esi]
  mov   edi, [edi]        // Result

  @loop: mov   ecx, eax
  or    eax, $80808080    // $E1..$FA
  mov   edx, eax
  sub   eax, $7B7B7B7B    // $66..$7F
  xor   edx, ecx          // $80
  or    eax, $80808080    // $E6..$FF
  sub   eax, $66666666    // $80..$99
  and   eax, edx          // $80
  shr   eax, 2            // $20
  xor   eax, ecx          // Upper
  mov[ebx+edi], eax
  mov   eax, [ebx+esi-4]
  sub   ebx, 4
  jge   @loop

  pop   edi
  pop   esi
  pop   ebx
  ret

  @nil:  pop   edi
  pop   esi
  pop   ebx
  jmp    System.@LStrClr   // Result:=''
end;

{********************************************}
function ALLowerCase(const s: String): String;
asm
  push  ebx
  push  esi
  push  edi
  mov   esi, eax          // s
  mov   eax, edx
  test  esi, esi
  jz    @nil
  mov   ebx, [esi-4]      // Length(s)
  mov   edx, ebx
  mov   edi, eax          // @Result
  add   ebx, -1
  jl    @nil
  and   ebx, -4
  call  System.@LStrSetLength
  mov   eax, [ebx+esi]
  mov   edi, [edi]        // Result

  @loop: mov   ecx, eax
  or    eax, $80808080    // $C1..$DA
  mov   edx, eax
  sub   eax, $5B5B5B5B    // $66..$7F
  xor   edx, ecx          // $80
  or    eax, $80808080    // $E6..$FF
  sub   eax, $66666666    // $80..$99
  and   eax, edx          // $80
  shr   eax, 2            // $20
  xor   eax, ecx          // Lower
  mov[ebx+edi], eax
  mov   eax, [ebx+esi-4]
  sub   ebx, 4
  jge   @loop

  pop   edi
  pop   esi
  pop   ebx
  ret

  @nil:  pop   edi
  pop   esi
  pop   ebx
  jmp    System.@LStrClr   // Result:=''
end;


 ///////////////////////////
 //////////Alcinoe//////////
 ///////////////////////////

{********************************************************************************}
function ALCopyStr(const aSourceString: String; aStart, aLength: Integer): String;
var
  SourceStringLength: Integer;
begin
  SourceStringLength := Length(aSourceString);
  if (aStart < 1) then
    aStart := 1;

  if (SourceStringLength = 0) or
    (aLength < 1) or
    (aStart > SourceStringLength) then
  begin
    Result := '';
    Exit;
  end;

  if aLength > SourceStringLength - (aStart - 1) then
    aLength := SourceStringLength - (aStart - 1);

  SetLength(Result, aLength);
  ALMove(aSourceString[aStart], Result[1], aLength);
end;

{************}
var
  Ch: char;


//***********************************************************************


function AlSMTPClientParseEmail(FriendlyEmail: String; var FriendlyName: String): String;
var
  I, J:  Integer;
  Flag:  Boolean;
  Delim: char;
begin
  Result       := '';
  FriendlyName := '';
  Flag         := (ALCharPos('<', FriendlyEmail) > 0);
  { Skip spaces }
  I            := 1;
  while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] = ' ') do
    Inc(I);
  if I > Length(FriendlyEmail) then
    Exit;
  { Check if quoted string }
  if FriendlyEmail[I] in ['"', ''''] then
  begin
    Delim := FriendlyEmail[I];
    { Skip opening quote }
    Inc(I);
    { Go to closing quote }
    J := I;
    while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> Delim) do
      Inc(I);
    FriendlyName := AlCopyStr(FriendlyEmail, J, I - J);
    Inc(I);
    if Flag then
    begin
      { Go to less-than sign }
      while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> '<') do
        Inc(I);
      Inc(I);
      J := I;
      while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> '>') do
        Inc(I);
      Result := AlCopyStr(FriendlyEmail, J, I - J);
    end
    else
      Result := Trim(AlCopyStr(FriendlyEmail, I, Length(FriendlyEmail)));
  end
  else if Flag then
  begin
    { Go to less-than sign }
    J := I;
    while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> '<') do
      Inc(I);
    FriendlyName := Trim(AlCopyStr(FriendlyEmail, J, I - J));
    Inc(I);
    { Go to greater-than sign }
    J := I;
    while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> '>') do
      Inc(I);
    Result := AlCopyStr(FriendlyEmail, J, I - J);
  end
  else
  begin
    { No <..>, goto next space }
    J := I;
    while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> ' ') do
      Inc(I);
    FriendlyName := Trim(AlCopyStr(FriendlyEmail, J, I - J));
    Result       := Trim(AlCopyStr(FriendlyEmail, I + 1, Length(FriendlyEmail)));
  end;
  if (Result = '') and (AlCharPos('@', FriendlyName) > 0) then
  begin
    Result       := FriendlyName;
    FriendlyName := '';
  end;
end;

{*********************************************}
function AlSMTPClientGenerateMessageID: String;
begin
  Result := AlStringReplace(ALMakeKeyStrByGUID, '-', '', [rfReplaceAll]) + '@' + AlGetLocalHostName;
end;


 {***********************************}
 {Perform Forward Move of 0..36 Bytes}
 {On Entry, ECX = Count, EAX = Source+Count, EDX = Dest+Count.  Destroys ECX}
procedure ALSmallForwardMove;
asm
  jmp     dword ptr [@@FwdJumpTable+ecx*4]
  nop {Align Jump Table}
  @@FwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Fwd01, @@Fwd02, @@Fwd03, @@Fwd04, @@Fwd05, @@Fwd06, @@Fwd07, @@Fwd08
  dd      @@Fwd09, @@Fwd10, @@Fwd11, @@Fwd12, @@Fwd13, @@Fwd14, @@Fwd15, @@Fwd16
  dd      @@Fwd17, @@Fwd18, @@Fwd19, @@Fwd20, @@Fwd21, @@Fwd22, @@Fwd23, @@Fwd24
  dd      @@Fwd25, @@Fwd26, @@Fwd27, @@Fwd28, @@Fwd29, @@Fwd30, @@Fwd31, @@Fwd32
  dd      @@Fwd33, @@Fwd34, @@Fwd35, @@Fwd36
  @@Fwd36:
  mov     ecx, [eax-36]
  mov[edx-36], ecx
  @@Fwd32:
  mov     ecx, [eax-32]
  mov[edx-32], ecx
  @@Fwd28:
  mov     ecx, [eax-28]
  mov[edx-28], ecx
  @@Fwd24:
  mov     ecx, [eax-24]
  mov[edx-24], ecx
  @@Fwd20:
  mov     ecx, [eax-20]
  mov[edx-20], ecx
  @@Fwd16:
  mov     ecx, [eax-16]
  mov[edx-16], ecx
  @@Fwd12:
  mov     ecx, [eax-12]
  mov[edx-12], ecx
  @@Fwd08:
  mov     ecx, [eax-8]
  mov[edx-8], ecx
  @@Fwd04:
  mov     ecx, [eax-4]
  mov[edx-4], ecx
  ret
  nop
  @@Fwd35:
  mov     ecx, [eax-35]
  mov[edx-35], ecx
  @@Fwd31:
  mov     ecx, [eax-31]
  mov[edx-31], ecx
  @@Fwd27:
  mov     ecx, [eax-27]
  mov[edx-27], ecx
  @@Fwd23:
  mov     ecx, [eax-23]
  mov[edx-23], ecx
  @@Fwd19:
  mov     ecx, [eax-19]
  mov[edx-19], ecx
  @@Fwd15:
  mov     ecx, [eax-15]
  mov[edx-15], ecx
  @@Fwd11:
  mov     ecx, [eax-11]
  mov[edx-11], ecx
  @@Fwd07:
  mov     ecx, [eax-7]
  mov[edx-7], ecx
  mov     ecx, [eax-4]
  mov[edx-4], ecx
  ret
  nop
  @@Fwd03:
  movzx   ecx,  word ptr [eax-3]
  mov[edx-3], cx
  movzx   ecx,  byte ptr [eax-1]
  mov[edx-1], cl
  ret
  @@Fwd34:
  mov     ecx, [eax-34]
  mov[edx-34], ecx
  @@Fwd30:
  mov     ecx, [eax-30]
  mov[edx-30], ecx
  @@Fwd26:
  mov     ecx, [eax-26]
  mov[edx-26], ecx
  @@Fwd22:
  mov     ecx, [eax-22]
  mov[edx-22], ecx
  @@Fwd18:
  mov     ecx, [eax-18]
  mov[edx-18], ecx
  @@Fwd14:
  mov     ecx, [eax-14]
  mov[edx-14], ecx
  @@Fwd10:
  mov     ecx, [eax-10]
  mov[edx-10], ecx
  @@Fwd06:
  mov     ecx, [eax-6]
  mov[edx-6], ecx
  @@Fwd02:
  movzx   ecx,  word ptr [eax-2]
  mov[edx-2], cx
  ret
  nop
  nop
  nop
  @@Fwd33:
  mov     ecx, [eax-33]
  mov[edx-33], ecx
  @@Fwd29:
  mov     ecx, [eax-29]
  mov[edx-29], ecx
  @@Fwd25:
  mov     ecx, [eax-25]
  mov[edx-25], ecx
  @@Fwd21:
  mov     ecx, [eax-21]
  mov[edx-21], ecx
  @@Fwd17:
  mov     ecx, [eax-17]
  mov[edx-17], ecx
  @@Fwd13:
  mov     ecx, [eax-13]
  mov[edx-13], ecx
  @@Fwd09:
  mov     ecx, [eax-9]
  mov[edx-9], ecx
  @@Fwd05:
  mov     ecx, [eax-5]
  mov[edx-5], ecx
  @@Fwd01:
  movzx   ecx,  byte ptr [eax-1]
  mov[edx-1], cl
  ret
  @@Done:
end;

 {************************************}
 {Perform Backward Move of 0..36 Bytes}
 {On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX}
procedure ALSmallBackwardMove;
asm
  jmp     dword ptr [@@BwdJumpTable+ecx*4]
  nop {Align Jump Table}
  @@BwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Bwd01, @@Bwd02, @@Bwd03, @@Bwd04, @@Bwd05, @@Bwd06, @@Bwd07, @@Bwd08
  dd      @@Bwd09, @@Bwd10, @@Bwd11, @@Bwd12, @@Bwd13, @@Bwd14, @@Bwd15, @@Bwd16
  dd      @@Bwd17, @@Bwd18, @@Bwd19, @@Bwd20, @@Bwd21, @@Bwd22, @@Bwd23, @@Bwd24
  dd      @@Bwd25, @@Bwd26, @@Bwd27, @@Bwd28, @@Bwd29, @@Bwd30, @@Bwd31, @@Bwd32
  dd      @@Bwd33, @@Bwd34, @@Bwd35, @@Bwd36
  @@Bwd36:
  mov     ecx, [eax+32]
  mov[edx+32], ecx
  @@Bwd32:
  mov     ecx, [eax+28]
  mov[edx+28], ecx
  @@Bwd28:
  mov     ecx, [eax+24]
  mov[edx+24], ecx
  @@Bwd24:
  mov     ecx, [eax+20]
  mov[edx+20], ecx
  @@Bwd20:
  mov     ecx, [eax+16]
  mov[edx+16], ecx
  @@Bwd16:
  mov     ecx, [eax+12]
  mov[edx+12], ecx
  @@Bwd12:
  mov     ecx, [eax+8]
  mov[edx+8], ecx
  @@Bwd08:
  mov     ecx, [eax+4]
  mov[edx+4], ecx
  @@Bwd04:
  mov     ecx, [eax]
  mov[edx], ecx
  ret
  nop
  nop
  nop
  @@Bwd35:
  mov     ecx, [eax+31]
  mov[edx+31], ecx
  @@Bwd31:
  mov     ecx, [eax+27]
  mov[edx+27], ecx
  @@Bwd27:
  mov     ecx, [eax+23]
  mov[edx+23], ecx
  @@Bwd23:
  mov     ecx, [eax+19]
  mov[edx+19], ecx
  @@Bwd19:
  mov     ecx, [eax+15]
  mov[edx+15], ecx
  @@Bwd15:
  mov     ecx, [eax+11]
  mov[edx+11], ecx
  @@Bwd11:
  mov     ecx, [eax+7]
  mov[edx+7], ecx
  @@Bwd07:
  mov     ecx, [eax+3]
  mov[edx+3], ecx
  mov     ecx, [eax]
  mov[edx], ecx
  ret
  nop
  nop
  nop
  @@Bwd03:
  movzx   ecx,  word ptr [eax+1]
  mov[edx+1], cx
  movzx   ecx,  byte ptr [eax]
  mov[edx], cl
  ret
  nop
  nop
  @@Bwd34:
  mov     ecx, [eax+30]
  mov[edx+30], ecx
  @@Bwd30:
  mov     ecx, [eax+26]
  mov[edx+26], ecx
  @@Bwd26:
  mov     ecx, [eax+22]
  mov[edx+22], ecx
  @@Bwd22:
  mov     ecx, [eax+18]
  mov[edx+18], ecx
  @@Bwd18:
  mov     ecx, [eax+14]
  mov[edx+14], ecx
  @@Bwd14:
  mov     ecx, [eax+10]
  mov[edx+10], ecx
  @@Bwd10:
  mov     ecx, [eax+6]
  mov[edx+6], ecx
  @@Bwd06:
  mov     ecx, [eax+2]
  mov[edx+2], ecx
  @@Bwd02:
  movzx   ecx,  word ptr [eax]
  mov[edx], cx
  ret
  nop
  @@Bwd33:
  mov     ecx, [eax+29]
  mov[edx+29], ecx
  @@Bwd29:
  mov     ecx, [eax+25]
  mov[edx+25], ecx
  @@Bwd25:
  mov     ecx, [eax+21]
  mov[edx+21], ecx
  @@Bwd21:
  mov     ecx, [eax+17]
  mov[edx+17], ecx
  @@Bwd17:
  mov     ecx, [eax+13]
  mov[edx+13], ecx
  @@Bwd13:
  mov     ecx, [eax+9]
  mov[edx+9], ecx
  @@Bwd09:
  mov     ecx, [eax+5]
  mov[edx+5], ecx
  @@Bwd05:
  mov     ecx, [eax+1]
  mov[edx+1], ecx
  @@Bwd01:
  movzx   ecx,  byte ptr[eax]
  mov[edx], cl
  ret
  nop
  nop
  @@Done:
end;

var
  VALMove_PrefetchLimit: Integer;

 {***********************************************************}
 {Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure ALAlignedFwdMoveSSE(const Source; var Dest; Count: Integer);
const
  Prefetch = 512;
asm
  push    ebx
  mov     ebx, eax                {ebx = Source}
  mov     eax, ecx                {EAX = Count}
  and     eax, -128               {EAX = No of Bytes to Block Move}
  add     ebx, eax
  add     edx, eax
  shr     eax, 3                  {EAX = No of QWORD's to Block Move}
  neg     eax
  cmp     eax, VALMove_PrefetchLimit   {Count > Limit - Use Prefetch}
  jl      @Large
  @Small:
  test    ebx, 15                 {Check if Both Source/Dest are Aligned}
  jnz     @SmallUnaligned
  @SmallAligned:                    {Both Source and Dest 16-Byte Aligned}

  nop                             {Align Loops}
  nop
  nop

  @SmallAlignedLoop:
  movaps  xmm0, [ebx+8*eax]
  movaps  xmm1, [ebx+8*eax+16]
  movaps  xmm2, [ebx+8*eax+32]
  movaps  xmm3, [ebx+8*eax+48]
  movaps[edx+8*eax], xmm0
  movaps[edx+8*eax+16], xmm1
  movaps[edx+8*eax+32], xmm2
  movaps[edx+8*eax+48], xmm3
  movaps  xmm4, [ebx+8*eax+64]
  movaps  xmm5, [ebx+8*eax+80]
  movaps  xmm6, [ebx+8*eax+96]
  movaps  xmm7, [ebx+8*eax+112]
  movaps[edx+8*eax+64], xmm4
  movaps[edx+8*eax+80], xmm5
  movaps[edx+8*eax+96], xmm6
  movaps[edx+8*eax+112], xmm7
  add     eax, 16
  js      @SmallAlignedLoop
  jmp     @Remainder

  @SmallUnaligned:               {Source Not 16-Byte Aligned}
  @SmallUnalignedLoop:
  movups  xmm0, [ebx+8*eax]
  movups  xmm1, [ebx+8*eax+16]
  movups  xmm2, [ebx+8*eax+32]
  movups  xmm3, [ebx+8*eax+48]
  movaps[edx+8*eax], xmm0
  movaps[edx+8*eax+16], xmm1
  movaps[edx+8*eax+32], xmm2
  movaps[edx+8*eax+48], xmm3
  movups  xmm4, [ebx+8*eax+64]
  movups  xmm5, [ebx+8*eax+80]
  movups  xmm6, [ebx+8*eax+96]
  movups  xmm7, [ebx+8*eax+112]
  movaps[edx+8*eax+64], xmm4
  movaps[edx+8*eax+80], xmm5
  movaps[edx+8*eax+96], xmm6
  movaps[edx+8*eax+112], xmm7
  add     eax, 16
  js      @SmallUnalignedLoop
  jmp     @Remainder

  @Large:
  test    ebx, 15              {Check if Both Source/Dest Aligned}
  jnz     @LargeUnaligned
  @LargeAligned:                 {Both Source and Dest 16-Byte Aligned}
  @LargeAlignedLoop:
  prefetchnta[ebx+8*eax+Prefetch]
  prefetchnta[ebx+8*eax+Prefetch+64]
  movaps  xmm0, [ebx+8*eax]
  movaps  xmm1, [ebx+8*eax+16]
  movaps  xmm2, [ebx+8*eax+32]
  movaps  xmm3, [ebx+8*eax+48]
  movntps[edx+8*eax], xmm0
  movntps[edx+8*eax+16], xmm1
  movntps[edx+8*eax+32], xmm2
  movntps[edx+8*eax+48], xmm3
  movaps  xmm4, [ebx+8*eax+64]
  movaps  xmm5, [ebx+8*eax+80]
  movaps  xmm6, [ebx+8*eax+96]
  movaps  xmm7, [ebx+8*eax+112]
  movntps[edx+8*eax+64], xmm4
  movntps[edx+8*eax+80], xmm5
  movntps[edx+8*eax+96], xmm6
  movntps[edx+8*eax+112], xmm7
  add     eax, 16
  js      @LargeAlignedLoop
  sfence
  jmp     @Remainder

  @LargeUnaligned:              {Source Not 16-Byte Aligned}
  @LargeUnalignedLoop:
  prefetchnta[ebx+8*eax+Prefetch]
  prefetchnta[ebx+8*eax+Prefetch+64]
  movups  xmm0, [ebx+8*eax]
  movups  xmm1, [ebx+8*eax+16]
  movups  xmm2, [ebx+8*eax+32]
  movups  xmm3, [ebx+8*eax+48]
  movntps[edx+8*eax], xmm0
  movntps[edx+8*eax+16], xmm1
  movntps[edx+8*eax+32], xmm2
  movntps[edx+8*eax+48], xmm3
  movups  xmm4, [ebx+8*eax+64]
  movups  xmm5, [ebx+8*eax+80]
  movups  xmm6, [ebx+8*eax+96]
  movups  xmm7, [ebx+8*eax+112]
  movntps[edx+8*eax+64], xmm4
  movntps[edx+8*eax+80], xmm5
  movntps[edx+8*eax+96], xmm6
  movntps[edx+8*eax+112], xmm7
  add     eax, 16
  js      @LargeUnalignedLoop
  sfence

  @Remainder:
  and     ecx, $7F {ECX = Remainder (0..112 - Multiple of 16)}
  jz      @Done
  add     ebx, ecx
  add     edx, ecx
  neg     ecx
  @RemainderLoop:
  movups  xmm0, [ebx+ecx]
  movaps[edx+ecx], xmm0
  add     ecx, 16
  jnz     @RemainderLoop
  @Done:
  pop     ebx
end;

 {****************************************************************************}
 {Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure ALForwards_SSE;
const
  LARGESIZE = 2048;
asm
  cmp     ecx, LARGESIZE
  jge     @FwdLargeMove
  cmp     ecx, CALMOVE_SMALLMOVESIZE+32
  movups  xmm0, [eax]
  jg      @FwdMoveSSE
  movups  xmm1, [eax+16]
  movups[edx], xmm0
  movups[edx+16], xmm1
  add     eax, ecx
  add     edx, ecx
  sub     ecx, 32
  jmp     ALSmallForwardMove
  @FwdMoveSSE:
  push    ebx
  mov     ebx, edx
  {Align Writes}
  add     eax, ecx
  add     ecx, edx
  add     edx, 15
  and     edx, -16
  sub     ecx, edx
  add     edx, ecx
  {Now Aligned}
  sub     ecx, 32
  neg     ecx
  @FwdLoopSSE:
  movups  xmm1, [eax+ecx-32]
  movups  xmm2, [eax+ecx-16]
  movaps[edx+ecx-32], xmm1
  movaps[edx+ecx-16], xmm2
  add     ecx, 32
  jle     @FwdLoopSSE
  movups[ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     ALSmallForwardMove
  @FwdLargeMove:
  push    ebx
  mov     ebx, ecx
  test    edx, 15
  jz      @FwdLargeAligned
  {16 byte Align Destination}
  mov     ecx, edx
  add     ecx, 15
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  {Destination now 16 Byte Aligned}
  call    ALSmallForwardMove
  mov     ecx, ebx
  @FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    ALAlignedFwdMoveSSE
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     ALSmallForwardMove
end;

 {****************************************************************************}
 {Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure ALBackwards_SSE;
asm
  cmp     ecx, CALMOVE_SMALLMOVESIZE+32
  jg      @BwdMoveSSE
  sub     ecx, 32
  movups  xmm1, [eax+ecx]
  movups  xmm2, [eax+ecx+16]
  movups[edx+ecx], xmm1
  movups[edx+ecx+16], xmm2
  jmp     ALSmallBackwardMove
  @BwdMoveSSE:
  push    ebx
  movups  xmm0, [eax+ecx-16] {Last 16 Bytes}
                             {Align Writes}
  lea     ebx, [edx+ecx]
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  {Now Aligned}
  sub     ecx, 32
  @BwdLoop:
  movups  xmm1, [eax+ecx]
  movups  xmm2, [eax+ecx+16]
  movaps[edx+ecx], xmm1
  movaps[edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @BwdLoop
  movups[edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     ALSmallBackwardMove
end;

 {******************************}
 {Move using SSE Instruction Set}
procedure ALMove_SSE(const Source; var Dest; Count: Integer);
asm
  cmp     ecx, CALMOVE_SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     ALSmallForwardMove
  @SmallCheck:
  jne     ALSmallBackwardMove
  ret {For Compatibility with Delphi's move for Source = Dest}
  @Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      ALForwards_SSE
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     ALForwards_SSE
  jmp     ALBackwards_SSE {Source/Dest Overlap}
  @Done:
end;

 {****************************************************************************}
 {Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure ALForwards_IA32;
asm
  push    ebx
  mov     ebx, edx
  fild    qword ptr [eax]
  add     eax, ecx {QWORD Align Writes}
  add     ecx, edx
  add     edx, 7
  and     edx, -8
  sub     ecx, edx
  add     edx, ecx {Now QWORD Aligned}
  sub     ecx, 16
  neg     ecx
  @FwdLoop:
  fild    qword ptr [eax+ecx-16]
  fistp   qword ptr [edx+ecx-16]
  fild    qword ptr [eax+ecx-8]
  fistp   qword ptr [edx+ecx-8]
  add     ecx, 16
  jle     @FwdLoop
  fistp   qword ptr [ebx]
  neg     ecx
  add     ecx, 16
  pop     ebx
  jmp     ALSmallForwardMove
end;

 {****************************************************************************}
 {Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure ALForwards_MMX;
const
  LARGESIZE = 1024;
asm
  cmp     ecx, LARGESIZE
  jge     @FwdLargeMove
  cmp     ecx, 72 {Size at which using MMX becomes worthwhile}
  jl      ALForwards_IA32
  push    ebx
  mov     ebx, edx
  movq    mm0, [eax] {First 8 Bytes}
                     {QWORD Align Writes}
  add     eax, ecx
  add     ecx, edx
  add     edx, 7
  and     edx, -8
  sub     ecx, edx
  add     edx, ecx
  {Now QWORD Aligned}
  sub     ecx, 32
  neg     ecx
  @FwdLoopMMX:
  movq    mm1, [eax+ecx-32]
  movq    mm2, [eax+ecx-24]
  movq    mm3, [eax+ecx-16]
  movq    mm4, [eax+ecx- 8]
  movq[edx+ecx-32], mm1
  movq[edx+ecx-24], mm2
  movq[edx+ecx-16], mm3
  movq[edx+ecx- 8], mm4
  add     ecx, 32
  jle     @FwdLoopMMX
  movq[ebx], mm0 {First 8 Bytes}
  emms
  pop     ebx
  neg     ecx
  add     ecx, 32
  jmp     ALSmallForwardMove
  @FwdLargeMove:
  push    ebx
  mov     ebx, ecx
  test    edx, 15
  jz      @FwdAligned
  {16 byte Align Destination}
  mov     ecx, edx
  add     ecx, 15
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  {Destination now 16 Byte Aligned}
  call    ALSmallForwardMove
  @FwdAligned:
  mov     ecx, ebx
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    esi
  push    edi
  mov     esi, eax          {ESI = Source}
  mov     edi, edx          {EDI = Dest}
  mov     eax, ecx          {EAX = Count}
  and     eax, -64          {EAX = No of Bytes to Blocks Moves}
  and     ecx, $3F          {ECX = Remaining Bytes to Move (0..63)}
  add     esi, eax
  add     edi, eax
  shr     eax, 3            {EAX = No of QWORD's to Block Move}
  neg     eax
  @MMXcopyloop:
  movq    mm0, [esi+eax*8   ]
  movq    mm1, [esi+eax*8+ 8]
  movq    mm2, [esi+eax*8+16]
  movq    mm3, [esi+eax*8+24]
  movq    mm4, [esi+eax*8+32]
  movq    mm5, [esi+eax*8+40]
  movq    mm6, [esi+eax*8+48]
  movq    mm7, [esi+eax*8+56]
  movq[edi+eax*8   ], mm0
  movq[edi+eax*8+ 8], mm1
  movq[edi+eax*8+16], mm2
  movq[edi+eax*8+24], mm3
  movq[edi+eax*8+32], mm4
  movq[edi+eax*8+40], mm5
  movq[edi+eax*8+48], mm6
  movq[edi+eax*8+56], mm7
  add     eax, 8
  jnz     @MMXcopyloop
  emms                   {Empty MMX State}
  add     ecx, ebx
  shr     ecx, 2
  rep     movsd
  mov     ecx, ebx
  and     ecx, 3
  rep     movsb
  pop     edi
  pop     esi
  pop     ebx
end;

 {****************************************************************************}
 {Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure ALBackwards_IA32;
asm
  push    ebx
  fild    qword ptr [eax+ecx-8]
  lea     ebx, [edx+ecx] {QWORD Align Writes}
  and     ebx, 7
  sub     ecx, ebx
  add     ebx, ecx {Now QWORD Aligned, EBX = Original Length}
  sub     ecx, 16
  @BwdLoop:
  fild    qword ptr [eax+ecx]
  fild    qword ptr [eax+ecx+8]
  fistp   qword ptr [edx+ecx+8]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 16
  jge     @BwdLoop
  fistp   qword ptr [edx+ebx-8]
  add     ecx, 16
  pop     ebx
  jmp     ALSmallBackwardMove
end;

 {****************************************************************************}
 {Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure ALBackwards_MMX;
asm
  cmp     ecx, 72 {Size at which using MMX becomes worthwhile}
  jl      ALBackwards_IA32
  push    ebx
  movq    mm0, [eax+ecx-8] {Get Last QWORD}
                           {QWORD Align Writes}
  lea     ebx, [edx+ecx]
  and     ebx, 7
  sub     ecx, ebx
  add     ebx, ecx
  {Now QWORD Aligned}
  sub     ecx, 32
  @BwdLoopMMX:
  movq    mm1, [eax+ecx   ]
  movq    mm2, [eax+ecx+ 8]
  movq    mm3, [eax+ecx+16]
  movq    mm4, [eax+ecx+24]
  movq[edx+ecx+24], mm4
  movq[edx+ecx+16], mm3
  movq[edx+ecx+ 8], mm2
  movq[edx+ecx   ], mm1
  sub     ecx, 32
  jge     @BwdLoopMMX
  movq[edx+ebx-8],  mm0 {Last QWORD}
  emms
  add     ecx, 32
  pop     ebx
  jmp     ALSmallBackwardMove
end;

 {******************************}
 {Move using MMX Instruction Set}
procedure ALMove_MMX(const Source; var Dest; Count: Integer);
asm
  cmp     ecx, CALMOVE_SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     ALSmallForwardMove
  @SmallCheck:
  jne     ALSmallBackwardMove
  ret {For Compatibility with Delphi's move for Source = Dest}
  @Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      ALForwards_MMX
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     ALForwards_MMX
  jmp     ALBackwards_MMX {Source/Dest Overlap}
  @Done:
end;

procedure ALMove_IA32(const Source; var Dest; Count: Integer);
asm
  cmp     ecx, CALMOVE_SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     ALSmallForwardMove
  @SmallCheck:
  jne     ALSmallBackwardMove
  ret {For Compatibility with Delphi's move for Source = Dest}
  @Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      ALForwards_IA32
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     ALForwards_IA32
  jmp     ALBackwards_IA32 {Source/Dest Overlap}
  @Done:
end;

 //***************************************************************************
 //**** ALCPUID Partial Unit
 //***************************************************************************
var
  VALCPUInfo: TALCPUinfo;

function ALGetCPUInfo: TALCPUinfo;
begin
  Result := VALCPUInfo;
end;

function ALPos_IA32(const SubStr: ansistring; const Str: ansistring): Integer;
asm
  test      eax, eax
  jz        @NotFoundExit    {Exit if SubStr = ''}
  test      edx, edx
  jz        @NotFound        {Exit if Str = ''}
  mov       ecx, [edx-4]     {Length(Str)}
  cmp[eax-4], 1              {Length SubStr = 1?}
  je        @SingleChar      {Yes - Exit via CharPos}
  jl        @NotFound        {Exit if Length(SubStr) < 1}
  sub       ecx, [eax-4]     {Subtract Length(SubStr), -ve handled by
CharPos}
  add       ecx, 1           {Number of Chars to Check for 1st Char}
  push      esi              {Save Registers}
  push      edi
  push      ebx
  push      ebp
  mov       ebx, [eax]       {BL = 1st Char of SubStr}
  mov       esi, eax         {Start Address of SubStr}
  mov       edi, ecx         {Initial Remainder Count}
  mov       ebp, edx         {Start Address of Str}
  @StrLoop:
  mov       eax, ebx         {AL  = 1st char of SubStr for next Search}
  mov       ecx, edi         {Remaining Length}
  push      edx              {Save Start Position}
  call      @CharPos         {Search for 1st Character}
  pop       edx              {Restore Start Position}
  jz        @StrExit         {Exit with Zero Result if 1st Char Not Found}
  mov       ecx, [esi-4]     {Length SubStr}
  add       edx, eax         {Update Start Position for Next Loop}
  sub       edi, eax         {Update Remaining Length for Next Loop}
  sub       ecx, 1           {Remaining Characters to Compare}
  @StrCheck:
  mov       al, [edx+ecx-1]  {Compare Next Char of SubStr and Str}
  cmp       al, [esi+ecx]
  jne       @StrLoop         {Different - Return to First Character Search}
  sub       ecx, 1
  jg        @StrCheck        {Check each Remaining Character}
  mov       eax, edx         {All Characters Matched - Calculate Result}
  sub       eax, ebp
  @StrExit:
  pop       ebp              {Restore Registers}
  pop       ebx
  pop       edi
  pop       esi
  ret
  @NotFound:
  xor       eax, eax         {Return 0}
  @NotFoundExit:
  ret
  @SingleChar:
  mov       al, [eax]        {Search Character}
                             {Return Position of Character AL within a String of Length ECX starting}
                             {at Address EDX.  If Found, Return Index in EAX and Clear Zero Flag,   }
                             {otherwise Return 0 in EAX and Set Zero Flag.  Changes EAX, ECX and EDX}
  @CharPos:
  push      ecx              {Save Length}
  neg       ecx
  cmp       ecx, -4
  jle       @NotSmall        {Length >= 4}
  or        ecx, ecx
  jge       @CharNotFound    {Exit if Length <= 0}
  cmp       al, [edx]        {Check 1st Char}
  je        @Found
  add       ecx, 1
  jz        @CharNotFound
  cmp       al, [edx+1]      {Check 2nd Char}
  je        @Found
  add       ecx, 1
  jz        @CharNotFound
  cmp       al, [edx+2]      {Check 3rd Char}
  je        @Found
  jmp       @CharNotFound
  @NotSmall:
  sub       edx, ecx         {End of String}
  @Loop:
  cmp       al, [edx+ecx]    {Compare Next 4 Characters}
  je        @Found
  cmp       al, [edx+ecx+1]
  je        @Found2
  cmp       al, [edx+ecx+2]
  je        @Found3
  cmp       al, [edx+ecx+3]
  je        @Found4
  add       ecx, 4           {Next Character Position}
  and       ecx, -4          {Prevent Read Past Last Character}
  jnz       @Loop            {Loop until all Characters Compared}
  @CharNotFound:
  pop       ecx              {Restore Stack}
  xor       eax, eax         {Set Result to 0 and Set Zero Flag}
  ret                        {Finished}
  @Found4:
  add       ecx, 1
  @Found3:
  add       ecx, 1
  @Found2:
  add       ecx, 1
  @Found:
  add       ecx, 1
  pop       eax
  add       eax, ecx         {Set Result and Clear Zero Flag}
end;

{***************************************************************************}
function ALPos_MMX(const SubStr: ansistring; const Str: ansistring): Integer;
asm
  test      eax, eax
  jz        @NotFoundExit    {Exit if SurStr = ''}
  test      edx, edx
  jz        @NotFound        {Exit if Str = ''}
  mov       ecx, [edx-4]     {Length(Str)}
  cmp[eax-4], 1              {Length SubStr = 1?}
  je        @SingleChar      {Yes - Exit via CharPos}
  jl        @NotFound        {Exit if Length(SubStr) < 1}
  sub       ecx, [eax-4]     {Subtract Length(SubStr), -ve handled by
CharPos}
  add       ecx, 1           {Number of Chars to Check for 1st Char}
  push      esi              {Save Registers}
  push      edi
  push      ebx
  push      ebp
  mov       esi, eax         {Start Address of SubStr}
  mov       edi, ecx         {Initial Remainder Count}
  mov       eax, [eax]       {AL = 1st Char of SubStr}
  mov       ebp, edx         {Start Address of Str}
  mov       ebx, eax         {Maintain 1st Search Char in BL}
  @StrLoop:
  mov       eax, ebx         {AL  = 1st char of SubStr}
  mov       ecx, edi         {Remaining Length}
  push      edx              {Save Start Position}
  call      @CharPos         {Search for 1st Character}
  pop       edx              {Restore Start Position}
  test      eax, eax         {Result = 0?}
  jz        @StrExit         {Exit if 1st Character Not Found}
  mov       ecx, [esi-4]     {Length SubStr}
  add       edx, eax         {Update Start Position for Next Loop}
  sub       edi, eax         {Update Remaining Length for Next Loop}
  sub       ecx, 1           {Remaining Characters to Compare}
  @StrCheck:
  mov       al, [edx+ecx-1]  {Compare Next Char of SubStr and Str}
  cmp       al, [esi+ecx]
  jne       @StrLoop         {Different - Return to First Character Search}
  sub       ecx, 1
  jnz       @StrCheck        {Check each Remaining Character}
  mov       eax, edx         {All Characters Matched - Calculate Result}
  sub       eax, ebp
  @StrExit:
  pop       ebp              {Restore Registers}
  pop       ebx
  pop       edi
  pop       esi
  ret
  @NotFound:
  xor       eax, eax         {Return 0}
  @NotFoundExit:
  ret
  @SingleChar:
  mov       al, [eax]        {Search Character}
  @CharPos:
  CMP       ECX, 8
  JG        @@NotSmall
  @@Small:
  or        ecx, ecx
  jle       @@NotFound       {Exit if Length <= 0}
  CMP       AL, [EDX]
  JZ        @Found1
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+1]
  JZ        @Found2
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+2]
  JZ        @Found3
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+3]
  JZ        @Found4
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+4]
  JZ        @Found5
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+5]
  JZ        @Found6
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+6]
  JZ        @Found7
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+7]
  JZ        @Found8
  @@NotFound:
  xor       EAX, EAX
  RET
  @Found1:
  MOV       EAX, 1
  RET
  @Found2:
  MOV       EAX, 2
  RET
  @Found3:
  MOV       EAX, 3
  RET
  @Found4:
  MOV       EAX, 4
  RET
  @Found5:
  MOV       EAX, 5
  RET
  @Found6:
  MOV       EAX, 6
  RET
  @Found7:
  MOV       EAX, 7
  RET
  @Found8:
  MOV       EAX, 8
  RET

  @@NotSmall:                  {Length(Str) > 8}
  MOV       AH, AL
  ADD       EDX, ECX
  MOVD      MM0, EAX
  PUNPCKLWD MM0, MM0
  PUNPCKLDQ MM0, MM0
  PUSH      ECX              {Save Length}
  NEG       ECX
  @@First8:
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare All 8 Bytes}
  PACKSSWB  MM1, MM1         {Pack Result into 4 Bytes}
  MOVD      EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
  JGE       @@Last8
  @@Align:                     {Align to Previous 8 Byte Boundary}
  LEA       EAX, [EDX+ECX]
  and       EAX, 7           {EAX -> 0 or 4}
  SUB       ECX, EAX
  @@Loop:
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare All 8 Bytes}
  PACKSSWB  MM1, MM1         {Pack Result into 4 Bytes}
  MOVD      EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
{$IFNDEF NoUnroll}
  JGE       @@Last8
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare All 8 Bytes}
  PACKSSWB  MM1, MM1         {Pack Result into 4 Bytes}
  MOVD      EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
{$ENDIF}
  JL        @@Loop
  @@Last8:
  MOVQ      MM1, [EDX-8]     {Position for Last 8 Used Characters}
  POP       EDX              {Original Length}
  PCMPEQB   MM1, MM0         {Compare All 8 Bytes}
  PACKSSWB  MM1, MM1         {Pack Result into 4 Bytes}
  MOVD      EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched2       {Exit on Match at any Position}
  EMMS
  RET                          {Finished - Not Found}
  @@Matched:                   {Set Result from 1st Match in EDX}
  POP       EDX                {Original Length}
  ADD       EDX, ECX
  @@Matched2:
  EMMS
  SUB       EDX, 8           {Adjust for Extra ADD ECX,8 in Loop}
  TEST      AL, AL
  JNZ       @@MatchDone      {Match at Position 1 or 2}
  TEST      AH, AH
  JNZ       @@Match1         {Match at Position 3 or 4}
  shr       EAX, 16
  TEST      AL, AL
  JNZ       @@Match2         {Match at Position 5 or 6}
  shr       EAX, 8
  ADD       EDX, 6
  JMP       @@MatchDone
  @@Match2:
  ADD       EDX, 4
  JMP       @@MatchDone
  @@Match1:
  shr       EAX, 8           {AL <- AH}
  ADD       EDX, 2
  @@MatchDone:
  xor       EAX, 2
  and       EAX, 3           {EAX <- 1 or 2}
  ADD       EAX, EDX

end;

{***************************************************************************}
function ALPos_SSE(const SubStr: ansistring; const Str: ansistring): Integer;
asm
  test      eax, eax
  jz        @NotFoundExit    {Exit if SurStr = ''}
  test      edx, edx
  jz        @NotFound        {Exit if Str = ''}
  mov       ecx, [edx-4]     {Length(Str)}
  cmp[eax-4], 1              {Length SubStr = 1?}
  je        @SingleChar      {Yes - Exit via CharPos}
  jl        @NotFound        {Exit if Length(SubStr) < 1}
  sub       ecx, [eax-4]     {Subtract Length(SubStr), -ve handled by
CharPos}
  add       ecx, 1           {Number of Chars to Check for 1st Char}
  push      esi              {Save Registers}
  push      edi
  push      ebx
  push      ebp
  mov       esi, eax         {Start Address of SubStr}
  mov       edi, ecx         {Initial Remainder Count}
  mov       eax, [eax]       {AL = 1st Char of SubStr}
  mov       ebp, edx         {Start Address of Str}
  mov       ebx, eax         {Maintain 1st Search Char in BL}
  @StrLoop:
  mov       eax, ebx         {AL  = 1st char of SubStr}
  mov       ecx, edi         {Remaining Length}
  push      edx              {Save Start Position}
  call      @CharPos         {Search for 1st Character}
  pop       edx              {Restore Start Position}
  test      eax, eax         {Result = 0?}
  jz        @StrExit         {Exit if 1st Character Not Found}
  mov       ecx, [esi-4]     {Length SubStr}
  add       edx, eax         {Update Start Position for Next Loop}
  sub       edi, eax         {Update Remaining Length for Next Loop}
  sub       ecx, 1           {Remaining Characters to Compare}
  @StrCheck:
  mov       al, [edx+ecx-1]  {Compare Next Char of SubStr and Str}
  cmp       al, [esi+ecx]
  jne       @StrLoop         {Different - Return to First Character Search}
  sub       ecx, 1
  jnz       @StrCheck        {Check each Remaining Character}
  mov       eax, edx         {All Characters Matched - Calculate Result}
  sub       eax, ebp
  @StrExit:
  pop       ebp              {Restore Registers}
  pop       ebx
  pop       edi
  pop       esi
  ret
  @NotFound:
  xor       eax, eax         {Return 0}
  @NotFoundExit:
  ret
  @SingleChar:
  mov       al, [eax]        {Search Character}
  @CharPos:
  CMP       ECX, 8
  JG        @@NotSmall
  @@Small:
  or        ecx, ecx
  jle       @@NotFound       {Exit if Length <= 0}
  CMP       AL, [EDX]
  JZ        @Found1
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+1]
  JZ        @Found2
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+2]
  JZ        @Found3
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+3]
  JZ        @Found4
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+4]
  JZ        @Found5
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+5]
  JZ        @Found6
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+6]
  JZ        @Found7
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+7]
  JZ        @Found8
  @@NotFound:
  xor       EAX, EAX
  RET
  @Found1:
  MOV       EAX, 1
  RET
  @Found2:
  MOV       EAX, 2
  RET
  @Found3:
  MOV       EAX, 3
  RET
  @Found4:
  MOV       EAX, 4
  RET
  @Found5:
  MOV       EAX, 5
  RET
  @Found6:
  MOV       EAX, 6
  RET
  @Found7:
  MOV       EAX, 7
  RET
  @Found8:
  MOV       EAX, 8
  RET
  @@NotSmall:
  MOV       AH, AL
  ADD       EDX, ECX
  MOVD      MM0, EAX
  PSHUFW    MM0, MM0, 0
  PUSH      ECX
  NEG       ECX
  @@First8:
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare Next 8 Bytes}
  PMOVMSKB  EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
  JGE       @@Last8
  @@Align:
  LEA       EAX, [EDX+ECX]
  and       EAX, 7
  SUB       ECX, EAX
  @@Loop:                      {Loop Unrolled 2X}
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare Next 8 Bytes}
  PMOVMSKB  EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
{$IFNDEF NoUnroll}
  JGE       @@Last8
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare Next 8 Bytes}
  PMOVMSKB  EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
{$ENDIF}
  JL        @@loop
  @@Last8:
  PCMPEQB   MM0, [EDX-8]
  POP       ECX              {Original Length}
  PMOVMSKB  EAX, MM0
  TEST      EAX, EAX
  JNZ       @@Matched2
  EMMS
  RET                          {Finished}
  @@Matched:                   {Set Result from 1st Match in EcX}
  POP       EDX                {Original Length}
  ADD       ECX, EDX
  @@Matched2:
  EMMS
  BSF       EDX, EAX
  LEA       EAX, [EDX+ECX-7]
end;

{****************************************************************************}
function ALPos_SSE2(const SubStr: ansistring; const Str: ansistring): Integer;
asm
  test      eax, eax
  jz        @NotFoundExit    {Exit if SurStr = ''}
  test      edx, edx
  jz        @NotFound        {Exit if Str = ''}
  mov       ecx, [edx-4]     {Length(Str)}
  cmp[eax-4], 1              {Length SubStr = 1?}
  je        @SingleChar      {Yes - Exit via CharPos}
  jl        @NotFound        {Exit if Length(SubStr) < 1}
  sub       ecx, [eax-4]     {Subtract Length(SubStr)}
  jl        @NotFound        {Exit if Length(SubStr) > Length(Str)}
  add       ecx, 1           {Number of Chars to Check for 1st Char}
  push      esi              {Save Registers}
  push      edi
  push      ebx
  push      ebp
  mov       esi, eax         {Start Address of SubStr}
  mov       edi, ecx         {Initial Remainder Count}
  mov       eax, [eax]       {AL = 1st Char of SubStr}
  mov       ebp, edx         {Start Address of Str}
  mov       ebx, eax         {Maintain 1st Search Char in BL}
  @StrLoop:
  mov       eax, ebx         {AL  = 1st char of SubStr}
  mov       ecx, edi         {Remaining Length}
  push      edx              {Save Start Position}
  call      @CharPos         {Search for 1st Character}
  pop       edx              {Restore Start Position}
  test      eax, eax         {Result = 0?}
  jz        @StrExit         {Exit if 1st Character Not Found}
  mov       ecx, [esi-4]     {Length SubStr}
  add       edx, eax         {Update Start Position for Next Loop}
  sub       edi, eax         {Update Remaining Length for Next Loop}
  sub       ecx, 1           {Remaining Characters to Compare}
  @StrCheck:
  mov       al, [edx+ecx-1]  {Compare Next Char of SubStr and Str}
  cmp       al, [esi+ecx]
  jne       @StrLoop         {Different - Return to First Character Search}
  sub       ecx, 1
  jnz       @StrCheck        {Check each Remaining Character}
  mov       eax, edx         {All Characters Matched - Calculate Result}
  sub       eax, ebp
  @StrExit:
  pop       ebp              {Restore Registers}
  pop       ebx
  pop       edi
  pop       esi
  ret
  @NotFound:
  xor       eax, eax         {Return 0}
  @NotFoundExit:
  ret
  @SingleChar:
  mov       al, [eax]        {Search Character}
  @CharPos:
  PUSH      EBX
  MOV       EBX, EAX
  CMP       ECX, 16
  JL        @@Small
  @@NotSmall:
  MOV       AH, AL           {Fill each Byte of XMM1 with AL}
  MOVD      XMM1, EAX
  PSHUFLW   XMM1, XMM1, 0
  PSHUFD    XMM1, XMM1, 0
  @@First16:
  MOVUPS    XMM0, [EDX]      {Unaligned}
  PCMPEQB   XMM0, XMM1       {Compare First 16 Characters}
  PMOVMSKB  EAX, XMM0
  TEST      EAX, EAX
  JNZ       @@FoundStart     {Exit on any Match}
  CMP       ECX, 32
  JL        @@Medium         {If Length(Str) < 32, Check Remainder}
  @@Align:
  SUB       ECX, 16          {Align Block Reads}
  PUSH      ECX
  MOV       EAX, EDX
  NEG       EAX
  and       EAX, 15
  ADD       EDX, ECX
  NEG       ECX
  ADD       ECX, EAX
  @@Loop:
  MOVAPS    XMM0, [EDX+ECX]  {Aligned}
  PCMPEQB   XMM0, XMM1       {Compare Next 16 Characters}
  PMOVMSKB  EAX, XMM0
  TEST      EAX, EAX
  JNZ       @@Found          {Exit on any Match}
  ADD       ECX, 16
  JLE       @@Loop
  @Remainder:
  POP       EAX              {Check Remaining Characters}
  ADD       EDX, 16
  ADD       EAX, ECX         {Count from Last Loop End Position}
  JMP       DWORD PTR [@@JumpTable2-ECX*4]

  @@NullString:
  xor       EAX, EAX         {Result = 0}
  RET

  @@FoundStart:
  BSF       EAX, EAX         {Get Set Bit}
  POP       EBX
  ADD       EAX, 1           {Set Result}
  RET

  @@Found:
  POP       EDX
  BSF       EAX, EAX         {Get Set Bit}
  ADD       EDX, ECX
  POP       EBX
  LEA       EAX, [EAX+EDX+1] {Set Result}
  RET

  @@Medium:
  ADD       EDX, ECX         {End of String}
  MOV       EAX, 16          {Count from 16}
  JMP       DWORD PTR [@@JumpTable1-64-ECX*4]

  @@Small:
  ADD       EDX, ECX         {End of String}
  xor       EAX, EAX         {Count from 0}
  JMP       DWORD PTR [@@JumpTable1-ECX*4]

  nop;
  nop;
  nop              {Aligb Jump Tables}

  @@JumpTable1:
  DD        @@NotFound, @@01, @@02, @@03, @@04, @@05, @@06, @@07
  DD        @@08, @@09, @@10, @@11, @@12, @@13, @@14, @@15, @@16

  @@JumpTable2:
  DD        @@16, @@15, @@14, @@13, @@12, @@11, @@10, @@09, @@08
  DD        @@07, @@06, @@05, @@04, @@03, @@02, @@01, @@NotFound

  @@16:
  ADD       EAX, 1
  CMP       BL, [EDX-16]
  JE        @@Done
  @@15:
  ADD       EAX, 1
  CMP       BL, [EDX-15]
  JE        @@Done
  @@14:
  ADD       EAX, 1
  CMP       BL, [EDX-14]
  JE        @@Done
  @@13:
  ADD       EAX, 1
  CMP       BL, [EDX-13]
  JE        @@Done
  @@12:
  ADD       EAX, 1
  CMP       BL, [EDX-12]
  JE        @@Done
  @@11:
  ADD       EAX, 1
  CMP       BL, [EDX-11]
  JE        @@Done
  @@10:
  ADD       EAX, 1
  CMP       BL, [EDX-10]
  JE        @@Done
  @@09:
  ADD       EAX, 1
  CMP       BL, [EDX-9]
  JE        @@Done
  @@08:
  ADD       EAX, 1
  CMP       BL, [EDX-8]
  JE        @@Done
  @@07:
  ADD       EAX, 1
  CMP       BL, [EDX-7]
  JE        @@Done
  @@06:
  ADD       EAX, 1
  CMP       BL, [EDX-6]
  JE        @@Done
  @@05:
  ADD       EAX, 1
  CMP       BL, [EDX-5]
  JE        @@Done
  @@04:
  ADD       EAX, 1
  CMP       BL, [EDX-4]
  JE        @@Done
  @@03:
  ADD       EAX, 1
  CMP       BL, [EDX-3]
  JE        @@Done
  @@02:
  ADD       EAX, 1
  CMP       BL, [EDX-2]
  JE        @@Done
  @@01:
  ADD       EAX, 1
  CMP       BL, [EDX-1]
  JE        @@Done
  @@NotFound:
  xor       EAX, EAX
  @@Done:
  POP       EBX
end;


 {**********************************}
 {Called Once by Unit Initialisation}
procedure ALInitFastPosFunct;
var
  aCpuInfo: TALCpuinfo;
begin
  aCpuInfo := AlGetCpuInfo;

  if (isSSE2 in aCpuInfo.InstructionSupport) then
    ALPos := AlPos_SSE2 {Processor Supports SSE}
  else if (isSSE in aCpuInfo.InstructionSupport) then
    ALPos := AlPos_SSE  {Processor Supports SSE}
  else if (isMMX in aCpuInfo.InstructionSupport) then
    ALPos := AlPos_MMX  {Processor Supports MMX}
  else
    ALPos := ALPos_IA32;                                               {Processor does not Support MMX or SSE}
end;

//***************************************************************************
procedure ALInitFastMovProc;
var
  aCpuInfo: TALCpuinfo;
begin
  aCpuInfo := AlGetCpuInfo;

  if (isSSE in aCpuInfo.InstructionSupport) then
    ALMove := AlMove_SSE  {Processor Supports SSE}
  else if (isMMX in aCpuInfo.InstructionSupport) then
    ALMove := AlMove_MMX  {Processor Supports MMX}
  else
    ALMove := ALMove_IA32;                                               {Processor does not Support MMX or SSE}
end;


 /////////////////////////////////////////
 ////////// TALSMTPClientHeader //////////
 /////////////////////////////////////////

{********************************************************}
procedure TALSMTPClientHeader.AssignTo(Dest: TPersistent);
begin
  if Dest is TALSMTPClientHeader then
    with Dest as TALSMTPClientHeader do
    begin
      fSendTo                    := self.fSendTo;
      fSender                    := self.fSender;
      fMessageID                 := self.fMessageID;
      fbcc                       := self.fbcc;
      fContentTransferEncoding   := self.fContentTransferEncoding;
      fComments                  := self.fComments;
      fMIMEVersion               := self.fMIMEVersion;
      fPriority                  := self.fPriority;
      fReplyTo                   := self.fReplyTo;
      fSubject                   := self.fSubject;
      fFrom                      := self.fFrom;
      fDate                      := self.fDate;
      fDispositionNotificationTo := self.fDispositionNotificationTo;
      fReferences                := self.fReferences;
      fcc                        := self.fcc;
      fContentType               := self.fContentType;
      FCustomHeaders.Assign(FCustomHeaders);
    end
  else
    inherited AssignTo(Dest);
end;

{**********************************}
procedure TALSMTPClientHeader.Clear;
begin
  fSendTo                    := '';
  fSender                    := '';
  fMessageID                 := '';
  fbcc                       := '';
  fContentTransferEncoding   := '';
  fComments                  := '';
  fMIMEVersion               := '';
  fPriority                  := '';
  fReplyTo                   := '';
  fSubject                   := '';
  fFrom                      := '';
  fDate                      := '';
  fDispositionNotificationTo := '';
  fReferences                := '';
  fcc                        := '';
  fContentType               := '';
  FCustomHeaders.Clear;
end;

{*************************************}
constructor TALSMTPClientHeader.Create;
begin
  inherited Create;
  FCustomHeaders                    := TStringList.Create;
  FCustomHeaders.NameValueSeparator := ':';
  Clear;
  fMessageID   := 'AUTO';
  fMIMEVersion := '1.0';
  fDate        := 'NOW';
  fContentType := 'text/plain';
end;

{*************************************}
destructor TALSMTPClientHeader.Destroy;
begin
  FCustomHeaders.Free;
  inherited;
end;

{****************************************************}
function TALSMTPClientHeader.GetRawHeaderText: String;
var
  i:   Integer;
  Str: String;
begin
  Result := '';
  if Trim(fFrom) <> '' then
    Result := Result + 'From: ' + trim(fFrom) + #13#10;
  if Trim(fSender) <> '' then
    Result := Result + 'Sender: ' + trim(fSender) + #13#10;
  if Trim(fSendTo) <> '' then
    Result := Result + 'To: ' + trim(fSendTo) + #13#10;
  if Trim(fcc) <> '' then
    Result := Result + 'cc: ' + trim(fcc) + #13#10;
  if Trim(fbcc) <> '' then
    Result := Result + 'bcc: ' + trim(fbcc) + #13#10;
  if Trim(fReplyTo) <> '' then
    Result := Result + 'Reply-To: ' + trim(fReplyTo) + #13#10;
  if Trim(fSubject) <> '' then
    Result := Result + 'Subject: ' + trim(fSubject) + #13#10;
  Str := fMessageID;
  if Trim(str) <> '' then
  begin
    if sametext(Str, 'AUTO') then
      Str := '<' + AlSMTPClientGenerateMessageID + '>';
    Result := Result + 'Message-ID: ' + trim(str) + #13#10;
  end;
  if Trim(fReferences) <> '' then
    Result := Result + 'References: ' + trim(fReferences) + #13#10;
  if Trim(fComments) <> '' then
    Result := Result + 'Comments: ' + trim(fComments) + #13#10;
  Str := fDate;
  if Trim(str) <> '' then
  begin
    if sametext(Str, 'NOW') then
      Str := ALDateTimeToRfc822Str(Now);
    Result := Result + 'Date: ' + trim(str) + #13#10;
  end;
  if Trim(fContentType) <> '' then
    Result := Result + 'Content-Type: ' + trim(fContentType) + #13#10;
  if Trim(fContentTransferEncoding) <> '' then
    Result := Result + 'Content-Transfer-Encoding: ' + trim(fContentTransferEncoding) + #13#10;
  if Trim(fMIMEVersion) <> '' then
    Result := Result + 'MIME-Version: ' + trim(fMIMEVersion) + #13#10;
  if Trim(fPriority) <> '' then
    Result := Result + 'Priority: ' + trim(fPriority) + #13#10;
  if Trim(fDispositionNotificationTo) <> '' then
    Result := Result + 'Disposition-Notification-To: ' + trim(fDispositionNotificationTo) + #13#10;
  for i := 0 to FCustomHeaders.Count - 1 do
    if (trim(FCustomHeaders.names[i]) <> '') and (trim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      Result := Result + FCustomHeaders.names[i] + ': ' + trim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
end;

{***************************************************************************}
procedure TALSMTPClientHeader.SetRawHeaderText(const aRawHeaderText: String);
var
  aRawHeaderLst: TStringList;

  {-------------------------------------}
  function AlG001(aName: String): String;
  var
    i:   Integer;
    Str: String;
  begin
    I := aRawHeaderLst.IndexOfName(aName);
    if I >= 0 then
    begin
      Result := Trim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
      while True do
      begin
        if i >= aRawHeaderLst.Count then
          break;
        str := aRawHeaderLst[i];
        if (str = '') or
          (not (str[1] in [' ', #9])) then
          break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := trim(Result + ' ' + trim(str));
        aRawHeaderLst.Delete(i);
      end;
    end
    else
      Result := '';
  end;

var
  Str1, Str2: String;
  j:          Integer;
begin
  aRawHeaderLst := TStringList.Create;
  try
    aRawHeaderLst.NameValueSeparator := ':';
    aRawHeaderLst.Text               := aRawHeaderText;

    fFrom                      := Alg001('From');
    fSender                    := Alg001('Sender');
    fSendTo                    := Alg001('To');
    fcc                        := Alg001('cc');
    fbcc                       := Alg001('bcc');
    fReplyTo                   := Alg001('Reply-To');
    fSubject                   := Alg001('Subject');
    fMessageID                 := Alg001('Message-ID');
    fReferences                := Alg001('References');
    fComments                  := Alg001('Comments');
    fDate                      := Alg001('Date');
    fContentType               := Alg001('Content-Type');
    fContentTransferEncoding   := Alg001('Content-Transfer-Encoding');
    fMIMEVersion               := Alg001('MIME-Version');
    fPriority                  := Alg001('Priority');
    fDispositionNotificationTo := Alg001('Disposition-Notification-To');

    FCustomHeaders.Clear;
    J := 0;
    while j <= aRawHeaderLst.Count - 1 do
    begin
      Str1 := trim(aRawHeaderLst.Names[j]);
      if (trim(str1) <> '') and (not (str1[1] in [' ', #9])) then
      begin
        Str1 := trim(Str1) + ': ' + trim(aRawHeaderLst.ValueFromIndex[j]);
        Inc(j);
        while True do
        begin
          if j >= aRawHeaderLst.Count then
            break;
          str2 := aRawHeaderLst[j];
          if (str2 = '') or
            (not (str2[1] in [' ', #9])) then
            break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := trim(Str1 + ' ' + trim(str2));
          Inc(j);
        end;
        FCustomHeaders.Add(Str1);
      end
      else
        Inc(j);
    end;

  finally
    aRawHeaderLst.Free;
  end;
end;


 ///////////////////////////////////
 ////////// TAlSmtpClient //////////
 ///////////////////////////////////

{*******************************}
constructor TAlSmtpClient.Create;
begin
  FWSAData.wVersion   := 0;
  Fconnected          := False;
  FSocketDescriptor   := INVALID_SOCKET;
  FAuthTypesSupported := [];
  Ftimeout            := 60000;
  Randomize;
end;

{*******************************}
destructor TAlSmtpClient.Destroy;
begin
  if Fconnected then
    Disconnect;
  inherited;
end;

{*************************************************}
procedure TAlSmtpClient.CheckError(Error: Boolean);
var
  ErrCode: Integer;
  S:       String;
begin
  ErrCode := WSAGetLastError;
  if Error and (ErrCode <> 0) then
  begin
    case ErrCode of
      WSAEINTR: S           := 'Interrupted function call';
      WSAEACCES: S          := 'Permission denied';
      WSAEFAULT: S          := 'Bad address';
      WSAEINVAL: S          := 'Invalid argument';
      WSAEMFILE: S          := 'Too many open files';
      WSAEWOULDBLOCK: S     := 'Resource temporarily unavailable';
      WSAEINPROGRESS: S     := 'Operation now in progress';
      WSAEALREADY: S        := 'Operation already in progress';
      WSAENOTSOCK: S        := 'Socket operation on nonsocket';
      WSAEDESTADDRREQ: S    := 'Destination address required';
      WSAEMSGSIZE: S        := 'Message too long';
      WSAEPROTOTYPE: S      := 'Protocol wrong type for socket';
      WSAENOPROTOOPT: S     := 'Bad protocol option';
      WSAEPROTONOSUPPORT: S := 'Protocol not supported';
      WSAESOCKTNOSUPPORT: S := 'Socket type not supported';
      WSAEOPNOTSUPP: S      := 'Operation not supported';
      WSAEPFNOSUPPORT: S    := 'Protocol family not supported';
      WSAEAFNOSUPPORT: S    := 'Address family not supported by protocol family';
      WSAEADDRINUSE: S      := 'Address already in use';
      WSAEADDRNOTAVAIL: S   := 'Cannot assign requested address';
      WSAENETDOWN: S        := 'Network is down';
      WSAENETUNREACH: S     := 'Network is unreachable';
      WSAENETRESET: S       := 'Network dropped connection on reset';
      WSAECONNABORTED: S    := 'Software caused connection abort';
      WSAECONNRESET: S      := 'Connection reset by peer';
      WSAENOBUFS: S         := 'No buffer space available';
      WSAEISCONN: S         := 'Socket is already connected';
      WSAENOTCONN: S        := 'Socket is not connected';
      WSAESHUTDOWN: S       := 'Cannot send after socket shutdown';
      WSAETIMEDOUT: S       := 'Connection timed out';
      WSAECONNREFUSED: S    := 'Connection refused';
      WSAEHOSTDOWN: S       := 'Host is down';
      WSAEHOSTUNREACH: S    := 'No route to host';
      WSAEPROCLIM: S        := 'Too many processes';
      WSASYSNOTREADY: S     := 'Network subsystem is unavailable';
      WSAVERNOTSUPPORTED: S := 'Winsock.dll version out of range';
      WSANOTINITIALISED: S  := 'Successful WSAStartup not yet performed';
      WSAEDISCON: S         := 'Graceful shutdown in progress';
      WSAHOST_NOT_FOUND: S  := 'Host not found';
      WSATRY_AGAIN: S       := 'Nonauthoritative host not found';
      WSANO_RECOVERY: S     := 'This is a nonrecoverable error';
      WSANO_DATA: S         := 'Valid name, no data record of requested type';
      else
      begin
        SetLength(S, 256);
        FormatMessage(
          FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_FROM_HMODULE,
          Pointer(GetModuleHandle('wsock32.dll')),
          ErrCode,
          0,
          PChar(S),
          Length(S),
          nil
          );
        SetLength(S, StrLen(PChar(S)));
        while (Length(S) > 0) and (S[Length(S)] in [#10, #13]) do
          SetLength(S, Length(S) - 1);
      end;
    end;
    raise Exception.Create(Format('%s (Error code:%s)', [S, IntToStr(ErrCode)]));      { Do not localize }
  end;
end;

{********************************************************************}
function TAlSmtpClient.Connect(aHost: String; APort: Integer): String;

  {---------------------------------------------}
  procedure CallServer(Server: String; Port: Word);
  var
    SockAddr: Sockaddr_in;
    IP:       String;
  begin
    FSocketDescriptor := Socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
    CheckError(FSocketDescriptor = INVALID_SOCKET);
    FillChar(SockAddr, SizeOf(SockAddr), 0);
    SockAddr.sin_family      := AF_INET;
    SockAddr.sin_port        := swap(Port);
    SockAddr.sin_addr.S_addr := inet_addr( PAnsiChar(Server) );
    if SockAddr.sin_addr.S_addr = INADDR_NONE then
    begin
      checkError(ALHostToIP(Server, IP));
      SockAddr.sin_addr.S_addr := inet_addr(pAnsiChar(IP));
    end;
    CheckError(WinSock.Connect(FSocketDescriptor, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR);
  end;

begin
  if FConnected then
    raise Exception.Create('SMTP component already connected');

  try

    WSAStartup(MAKEWORD(2, 2), FWSAData);
    CallServer(aHost, aPort);
    CheckError(setsockopt(FSocketDescriptor, SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@FTimeOut), SizeOf(integer)) = SOCKET_ERROR);
    CheckError(setsockopt(FSocketDescriptor, SOL_SOCKET, SO_SNDTIMEO, PAnsiChar(@FTimeOut), SizeOf(integer)) = SOCKET_ERROR);
    Result              := GetResponse([220]);
    FAuthTypesSupported := [];
    Fconnected          := True;

  except
    Disconnect;
    raise;
  end;

end;

{*********************************}
procedure TAlSmtpClient.Disconnect;
begin
  if Fconnected then
  begin
    ShutDown(FSocketDescriptor, SD_BOTH);
    CloseSocket(FSocketDescriptor);
    FSocketDescriptor := INVALID_SOCKET;
    if FWSAData.wVersion = 2 then
      WSACleanup;
    FWSAData.wVersion := 0;
    Fconnected        := False;
    if Assigned(OnStatus) then
      OnStatus('Disconnect');
  end;
end;

{********************}
{EhloResponse is like:
 250-ec-is.net Hello your_name, ravi de vous rencontrer
 250-VRFY
 250-ETRN
 250-AUTH=LOGIN
 250-AUTH LOGIN CRAM-MD5
 250-8BITMIME
 250 SIZE 0}
function TAlSmtpClient.GetAuthTypeFromEhloResponse(EhloResponse: String): TAlSmtpClientAuthTypeSet;
var
  k, J:       Integer;
  Str1, Str2: String;
  Lst:        TStringList;
begin
  Result := [];
  Lst    := TStringList.Create;
  try
    Lst.Text := AlUpperCase(Trim(EhloResponse));
    for j := 0 to Lst.Count - 1 do
    begin
      Str1 := trim(Lst[J]);          //250-AUTH=LOGIN
      Delete(Str1, 1, 4);            //AUTH=LOGIN
      Str2 := AlCopyStr(Str1, 1, 5); //AUTH=
      if (str2 = 'AUTH ') or (Str2 = 'AUTH=') then
      begin
        Str1 := AlCopyStr(Str1, 6, maxint);                      //LOGIN
        Str1 := AlStringReplace(Str1, '=', ' ', [rfReplaceAll]); //LOGIN
        while (str1 <> '') do
        begin
          K := AlCharPos(' ', Str1);
          if K <= 0 then
          begin
            Str2 := trim(Str1);
            Str1 := '';
          end
          else
          begin
            Str2 := Trim(AlCopyStr(Str1, 1, k - 1));
            Delete(Str1, 1, k);
          end;

          if Str2 = ('PLAIN') then
            Result := Result + [AlsmtpClientAuthPlain]
          else if Str2 = ('LOGIN') then
            Result := Result + [AlsmtpClientAuthLogin]
          else if Str2 = ('CRAM-MD5') then
            Result := Result + [AlsmtpClientAuthCramMD5]
          else if Str2 = ('CRAM-SHA1') then
            Result := Result + [AlsmtpClientAuthCramSHA1];

        end;
      end;
    end;
  finally
    Lst.Free;
  end;
end;

{****************************************************************************************}
{This command is used to identify the sender-SMTP to the receiver-SMTP. The argument field
 contains the host name of the sender-SMTP. The receiver-SMTP identifies itself to the
 sender-SMTP in the connection greeting reply, and in the response to this command.
 This command and an OK reply to it confirm that both the sender-SMTP and the receiver-SMTP
 are in the initial state, that is, there is no transaction in progress and all state tables
 and buffers are cleared.}
function TAlSmtpClient.Helo: String;
begin
  Result := SendCmd('HELO ' + AlGetLocalHostName, [250]);
end;

{**********************************}
function TAlSmtpClient.Ehlo: String;
begin
  Result              := SendCmd('EHLO ' + AlGetLocalHostName, [250]);
  FAuthTypesSupported := GetAuthTypeFromEhloResponse(Result);
end;

{****************************************************************************}
{This command is used to initiate a mail transaction in which the mail data is
 delivered to one or more mailboxes. The argument field contains a reverse-path.
 The reverse-path consists of an optional list of hosts and the sender mailbox. When
 the list of hosts is present, it is a "reverse" source route and indicates that the
 mail was relayed through each host on the list (the first host in the list was the
 most recent relay). This list is used as a source route to return non-delivery notices
 to the sender. As each relay host adds itself to the beginning of the list, it must
 use its name as known in the IPCE to which it is relaying the mail rather than the IPCE
 from which the mail came (if they are different). In some types of error reporting
 messages (for example, undeliverable mail notifications) the reverse-path may be null.
 This command clears the reverse-path buffer, the forward-path buffer, and the mail data
 buffer; and inserts the reverse-path information from this command into the reverse-path buffer.}
function TAlSmtpClient.MailFrom(aFromName: String): String;
begin
  aFromName := trim(aFromName);
  if aFromName = '' then
    raise Exception.Create('From name is empty');

  if AlPos(#13#10, aFromName) > 0 then
    raise Exception.Create('From name is invalid');

  Result := SendCmd('MAIL From:<' + aFromName + '>', [250]);
end;

{**************************************************************************************************}
function TAlSmtpClient.Auth(AUserName, APassword: String; aAuthType: TalSmtpClientAuthType): String;

  {-----------------------------------}
  function InternalDoAuthPlain: String;
  var
    aAuthPlain: String;
  begin
    if aUserName = '' then
      raise Exception.Create('UserName is empty');

    if aPassword = '' then
      raise Exception.Create('Password is empty');

    aAuthPlain := ALMimeBase64EncodeStringNoCRLF(aUserName + #0 + aUserName + #0 + aPassword);
    Result     := SendCmd('AUTH PLAIN ' + aAuthPlain, [235]);
  end;

  {-----------------------------------}
  function InternalDoAuthLogin: String;
  begin
    if aUserName = '' then
      raise Exception.Create('UserName is empty');

    if aPassword = '' then
      raise Exception.Create('Password is empty');

    SendCmd('AUTH LOGIN', [334]);
    SendCmd(ALMimeBase64EncodeStringNoCRLF(aUsername), [334]);
    Result := SendCmd(ALMimeBase64EncodeStringNoCRLF(aPassword), [235]);
  end;

var
  tmpAuthType: TAlSmtpClientAuthType;
begin

  if aAuthType = AlsmtpClientAuthAutoSelect then
  begin
    if AlsmtpClientAuthPlain in FAuthTypesSupported then
      tmpAuthType := AlsmtpClientAuthPlain
    else if AlsmtpClientAuthLogin in FAuthTypesSupported then
      tmpAuthType := AlsmtpClientAuthLogin
    else if AlsmtpClientAuthCramMD5 in FAuthTypesSupported then
      tmpAuthType := AlsmtpClientAuthCramMD5
    else if AlsmtpClientAuthCramSHA1 in FAuthTypesSupported then
      tmpAuthType := AlsmtpClientAuthCramSHA1
    else
      tmpAuthType := AlsmtpClientAuthNone;
  end
  else
    tmpAuthType := aAuthType;

  if Assigned(OnStatus) then
    OnStatus('Authentication');

  case tmpAuthType of
    alsmtpClientAuthPlain: Result := InternalDoAuthPlain;
    alsmtpClientAuthLogin: Result := InternalDoAuthLogin;
    alsmtpClientAuthCramMD5: raise Exception.Create('CRAM-MD5 Authentication is not supported yet!');
    alsmtpClientAuthCramSHA1: raise Exception.Create('CRAM-SHA1 Authentication is not supported yet!');
    else
      raise Exception.Create('No Authentication scheme found');
  end;

end;

{*************************************************************************}
{This command is used to identify an individual recipient of the mail data;
 multiple recipients are specified by multiple use of this command.}
function TAlSmtpClient.RcptTo(aRcptNameLst: TStrings): String;
var
  i:              Integer;
  aRcptNameValue: String;
begin
  Result := '';
  if aRcptNameLst.Count <= 0 then
    raise Exception.Create('RcptName list is empty');

  for i := 0 to aRcptNameLst.Count - 1 do
  begin
    aRcptNameValue := trim(aRcptNameLst[i]);
    if (aRcptNameValue = '') or (AlPos(#13#10, aRcptNameValue) > 0) then
      raise Exception.Create('Bad entry in RcptName list');

    Result := Result + SendCmd('RCPT To:<' + aRcptNameValue + '>', [250, 251]) + #13#10;
  end;
  if Result <> '' then
    Delete(Result, Length(Result) - 1, 2);
end;

{********************************************************************************}
{The receiver treats the lines following the command as mail data from the sender.
 This command causes the mail data from this command to be appended to the mail data buffer.
 The mail data may contain any of the 128 ASCII character codes.
 The mail data is terminated by a line containing only a period, that is the character sequence "<CRLF>.<CRLF>".
 This is the end of mail data indication. The end of mail data indication requires that the receiver must now process
 the stored mail transaction information. This processing consumes the information in the reverse-path buffer,
 the forward-path buffer, and the mail data buffer, and on the completion of this command these buffers are cleared.
 If the processing is successful the receiver must send an OK reply. If the processing fails completely
 the receiver must send a failure reply. When the receiver-SMTP accepts a message either for relaying or for
 final delivery it inserts at the beginning of the mail data a time stamp line. The time stamp line indicates the
 identity of the host that sent the message, and the identity of the host that received the message (and is inserting this
 time stamp), and the date and time the message was received. Relayed messages will have multiple time stamp lines.
 When the receiver-SMTP makes the "final delivery" of a message it inserts at the beginning of the mail data a return path
 line. The return path line preserves the information in the <reverse-path> from the MAIL command. Here, final delivery
 means the message leaves the SMTP world. Normally, this would mean it has been delivered to the destination user, but
 in some cases it may be further processed and transmitted by another mail system.
 It is possible for the mailbox in the return path be different from the actual sender's mailbox, for example,
 if error responses are to be delivered a special error handling mailbox rather than the message senders.
 The preceding two paragraphs imply that the final mail data will begin with a return path line, followed
 by one or more time stamp lines. These lines will be followed by the mail data header and body [2].
 Special mention is needed of the response and further action required when the processing following the end of mail
 data indication is partially successful. This could arise if after accepting several recipients and the mail data,
 the receiver-SMTP finds that the mail data can be successfully delivered to some of the recipients, but it cannot
 be to others (for example, due to mailbox space allocation problems). In such a situation, the response to the DATA
 command must be an OK reply. But, the receiver-SMTP must compose and send an "undeliverable mail" notification
 message to the originator of the message. Either a single notification which lists all of the recipients that failed
 to get the message, or separate notification messages must be sent for each failed recipient. All undeliverable mail
 notification messages are sent using the MAIL command (even if they result from processing a SEND, SOML, or SAML command).}
function TAlSmtpClient.Data(aMailData: String): String;
var
  I: Integer;
begin
  SendCmd('DATA', [354]);

  i := 2;
  while i <= Length(aMailData) do
  begin
    if (aMailData[i] = '.') and (aMailData[i - 1] = #10) and (aMailData[i - 2] = #13) then
      Insert('.', aMailData, i);
    Inc(i);
  end;

  Result := SendCmd(aMailData + #13#10 + '.', [250]);

end;

{**********************************************************}
function TAlSmtpClient.Data(aHeader, aBody: String): String;
begin
  Result := Data(Trim(aHeader) + #13#10#13#10 + aBody);
end;

{******************************************************************************}
function TAlSmtpClient.Data(aHeader: TALSMTPClientHeader; aBody: String): String;
begin
  Result := Data(aHeader.GetRawHeaderText, aBody);
end;

{**************************************************************}
{This command specifies that the receiver must send an OK reply,
 and then close the transmission channel. The receiver should not
 close the transmission channel until it receives and replies to
 a QUIT command (even if there was an error). The sender should not
 close the transmission channel until it send a QUIT command and
 receives the reply (even if there was an error response to a previous
 command). If the connection is closed prematurely the receiver should
 act as if a RSET command had been received (canceling any pending
 transaction, but not undoing any previously completed transaction),
 the sender should act as if the command or transaction in progress had
 received a temporary error (4xx).}
function TAlSmtpClient.Quit: String;
begin
  Result := SendCmd('QUIT', [221]);
  Disconnect;
end;

{**********************************}
{This command asks the receiver to confirm that the argument identifies a user.
 If it is a user name, the full name of the user (if known) and the fully
 specified mailbox are returned. This command has no effect on any of the
 reverse-path buffer, the forward-path buffer, or the mail data buffer.}
function TAlSmtpClient.Vrfy(aUserName: String): String;
begin
  Result := SendCmd('VRFY ' + aUserName, [250]);
end;

{*************************************************************}
{This command specifies that the current mail transaction is to
 be aborted. Any stored sender, recipients, and mail data must be
 discarded, and all buffers and state tables cleared. The receiver
 must send an OK reply.}
function TAlSmtpClient.Rset: String;
begin
  Result := SendCmd('RSET', [250]);
end;

{*********************************************}
procedure TAlSmtpClient.SendMail(aHost: String; APort: Integer; aFromName: String; aRcptNameLst: TStrings; AUserName, APassword: String; aAuthType: TalSmtpClientAuthType; aMailData: String);
begin
  if Fconnected then
    Disconnect;

  connect(aHost, APort);
  try

    if aAuthType = AlsmtpClientAuthAutoSelect then
      ehlo
    else
      Helo;
    if aAuthType <> AlsmtpClientAuthNone then
      Auth(AUserName, APassword, aAuthType);
    mailFrom(aFromName);
    RcptTo(aRcptNameLst);
    Data(aMailData);
    Quit;

  finally
    Disconnect;
  end;
end;

{*********************************************}
procedure TAlSmtpClient.SendMail(aHost: String; APort: Integer; aFromName: String; aRcptNameLst: TStrings; AUserName, APassword: String; aAuthType: TalSmtpClientAuthType; aHeader, aBody: String);
begin
  if Fconnected then
    Disconnect;

  if Assigned(OnStatus) then
    OnStatus('Wait for connection');

  connect(aHost, APort);
  try
    if aAuthType = AlsmtpClientAuthAutoSelect then
      ehlo
    else
      Helo;
    if aAuthType <> AlsmtpClientAuthNone then
      Auth(AUserName, APassword, aAuthType);

    if Assigned(OnStatus) then
      OnStatus('Send email');

    mailFrom(aFromName);
    RcptTo(aRcptNameLst);
    Data(aHeader, aBody);
    Quit;
  finally
    Disconnect;
  end;
end;

{***********************************************************}

{*******************************************************************************}
{commands consist of a command code followed by an argument field. Command codes
 are four alphabetic characters. Upper and lower case alphabetic characters are
 to be treated identically. Thus, any of the following may represent the mail command:
            MAIL    Mail    mail    MaIl    mAIl
 This also applies to any symbols representing parameter values, such as "TO" or "to"
 for the forward-path. Command codes and the argument fields are separated by one or
 more spaces. However, within the reverse-path and forward-path arguments case is
 important. In particular, in some hosts the user "smith" is different from the user
 "Smith". The argument field consists of a variable length character string ending
 with the character sequence <CRLF>. The receiver is to take no action until
 this sequence is received. Square brackets denote an optional argument field.
 If the option is not taken, the appropriate default is implied.
 The following are the SMTP commands:
            HELO <SP> <domain> <CRLF>
            MAIL <SP> FROM:<reverse-path> <CRLF>
            RCPT <SP> TO:<forward-path> <CRLF>
            DATA <CRLF>
            RSET <CRLF>
            SEND <SP> FROM:<reverse-path> <CRLF>
            SOML <SP> FROM:<reverse-path> <CRLF>
            SAML <SP> FROM:<reverse-path> <CRLF>
            VRFY <SP> <string> <CRLF>
            EXPN <SP> <string> <CRLF>
            HELP [<SP> <string>] <CRLF>
            NOOP <CRLF>
            QUIT <CRLF>
            TURN <CRLF>}
function TAlSmtpClient.SendCmd(aCmd: String; OkResponses: array of Word): String;
var
  P:        PChar;
  L:        Integer;
  ByteSent: Integer;
begin
  if (length(aCmd) <= 1) or
    (aCmd[length(aCmd)] <> #10) or
    (aCmd[length(aCmd) - 1] <> #13) then
    aCmd := aCmd + #13#10;

  p := @aCmd[1]; // pchar
  l := length(aCmd);
  while l > 0 do
  begin
    ByteSent := SocketWrite(p^, l);
    if ByteSent <= 0 then
      raise Exception.Create('Connection close gracefully!');

    Inc(p, ByteSent);
    Dec(l, ByteSent);
  end;

  Result := GetResponse(OkResponses);
end;

{*********************************************************************}
{An SMTP reply consists of a three digit number (transmitted as three
 alphanumeric characters) followed by some text. The number is intended
 for use by automata to determine what state to enter next; the text is
 meant for the human user. It is intended that the three digits contain
 enough encoded information that the sender-SMTP need not examine the
 text and may either discard it or pass it on to the user, as appropriate.
 In particular, the text may be receiver-dependent and context dependent,
 so there are likely to be varying texts for each reply code. Formally,
 a reply is defined to be the sequence:
 a three-digit code, <SP>, one line of text, and <CRLF>, or a multiline reply.
 Only the EXPN and HELP commands are expected to result in multiline replies
 in normal circumstances, however multiline replies are allowed for any
 command.}
function TAlSmtpClient.GetResponse(OkResponses: array of Word): String;

  {----------------------------------------------}
  function Internalstpblk(PValue: PChar): PChar;
  begin
    Result := PValue;
    {$IFDEF DELPHI12}
      while CharinSet(Result^,[' ', #9, #10, #13]) do
    {$ELSE}
      while Result^ in [' ', #9, #10, #13] do
    {$ENDIF}
      Inc(Result);
  end;

  {---------------------------------------------------------------------}
  function InternalGetInteger(Data: PChar; var Number: Integer): PChar;
  var
    bSign: Boolean;
  begin
    Number := 0;
    Result := InternalStpBlk(Data);
    if (Result = nil) then
      Exit;
    { Remember the sign }
    {$IFDEF DELPHI12}
      If CharInSet(Result^,['-', '+']) then
    {$ELSE}
      if Result^ in ['-', '+'] then
    {$ENDIF}
    begin
      bSign := (Result^ = '-');
      Inc(Result);
    end
    else
      bSign := False;
    { Convert any number }
    {$IFDEF DELPHI12}
      while ( CharInSet(Result^,[#0]) ) and ( CharInSet(Result^,['0'..'9']) ) do
    {$ELSE}
      while (Result^ <> #0) and (Result^ in ['0'..'9']) do
    {$ENDIF}
    begin
      Number := Number * 10 + Ord(Result^) - Ord('0');
      Inc(Result);
    end;
    { Correct for sign }
    if bSign then
      Number := -Number;
  end;

var
  aBuffStr:       String;
  aBuffStrLength: Integer;
  aResponse:      String;
  aStatusCode:    Integer;
  aGoodResponse:  Boolean;
  ALst:           TStringList;
  P:              PChar;
  i, j:           Integer;
begin
  Result := '';
  while True do
  begin

    {Read the response from the socket - end of the response is show by <CRLF>}
    aResponse := '';
    while True do
    begin
      Setlength(aBuffStr, 512); //The maximum total length of a reply line including the reply code and the <CRLF> is 512 characters. (http://www.freesoft.org/CIE/RFC/821/24.htm)
      aBuffStrLength := SocketRead(aBuffStr[1], length(aBuffStr));
      aResponse      := AResponse + AlCopyStr(aBuffStr, 1, aBuffStrLength);
      if aResponse = '' then
        raise Exception.Create('Connection close gracefully!');

      if (aBuffStrLength > 1) and
        (aBuffStr[aBuffStrLength] = #10) and
        (aBuffStr[aBuffStrLength - 1] = #13) then
        Break;
    end;
    Result := Result + aResponse;

    {The format for multiline replies requires that every line, except the last,
     begin with the reply code, followed immediately by a hyphen, "-" (also known as minus),
     followed by text. The last line will begin with the reply code, followed immediately
     by <SP>, optionally some text, and <CRLF>.}
    ALst := TStringList.Create;
    try
      Alst.Text := aResponse;
      if Alst.Count = 0 then
        raise Exception.Create('Emtpy response');

      for j := 0 to Alst.Count - 1 do
      begin
        aResponse     := Alst[j];
        p             := InternalGetInteger(@aResponse[1], aStatusCode);
        aGoodResponse := False;
        for I := 0 to High(OkResponses) do
          if OkResponses[I] = aStatusCode then
          begin
            aGoodResponse := True;
            Break;
          end;

        if not aGoodResponse then
          raise Exception.Create(aResponse);

        if p^ <> '-' then
        begin
          if J <> Alst.Count - 1 then
            raise Exception.Create(aResponse);
          Exit;
        end;
      end;
    finally
      ALst.Free;
    end;

  end;
end;

{**********************************************************************}
function TAlSmtpClient.SocketWrite(var Buffer; Count: longint): longint;
begin
  Result := Send(FSocketDescriptor, Buffer, Count, 0);
  CheckError(Result = SOCKET_ERROR);
end;

{*********************************************************************}
function TAlSmtpClient.SocketRead(var Buffer; Count: longint): longint;
begin
  Result := Recv(FSocketDescriptor, Buffer, Count, 0);
  CheckError(Result = SOCKET_ERROR);
end;

{*******************************************************}
procedure TAlSmtpClient.Settimeout(const Value: Integer);
begin
  if Value <> Ftimeout then
  begin
    CheckError(setsockopt(FSocketDescriptor, SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@FTimeOut), SizeOf(integer)) = SOCKET_ERROR);
    CheckError(setsockopt(FSocketDescriptor, SOL_SOCKET, SO_SNDTIMEO, PAnsiChar(@FTimeOut), SizeOf(integer)) = SOCKET_ERROR);
    Ftimeout := Value;
  end;
end;

initialization
  for Ch := #0 to #255 do
    VALMove_AnsiUpcase[Ch] := Ch;
  CharUpperBuff(@VALMove_AnsiUpcase, 256);
  ALInitFastMovProc;
  ALInitFastPosFunct;
end.

{$WARNINGS ON}

