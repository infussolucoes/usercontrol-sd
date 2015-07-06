{ *****************************************************************
  www:          http://sourceforge.net/projects/alcinoe/
  Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
  Sponsor(s):   Arkadia SA (http://www.arkadia.com)

  product:      ALPhpRunner
  Version:      4.00

  Description:  ALPHPRunnerEngine is a simple but useful component for
  easily use php (any version) as a scripting language
  in Delphi applications. ALPhpRunnerEngine allows to
  execute the PHP scripts within the Delphi program without
  a WebServer. ALPHPRunnerEngine use the CGI/FastCGI
  interface (php-cgi.exe) of PHP to communicate with PHP engine.

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

  Note :        If you use fastCGI (Socket) interface, you will need to start
  php-cgi separatly:

  php-cgi.exe -b host:port
  php-cgi.exe -b 127.0.0.1:8002

  Security
  --------
  Be sure to run the php binary as an appropriate userid
  Also, firewall out the port that PHP is listening on. In addition,
  you can set the environment variable FCGI_WEB_SERVER_ADDRS to
  control who can connect to the FastCGI.
  Set it to a comma separated list of IP addresses, e.g.:

  export FCGI_WEB_SERVER_ADDRS=199.170.183.28,199.170.183.71

  Tuning
  ------
  There are a few tuning parameters that can be tweaked to control
  the performance of FastCGI PHP. The following are environment
  variables that can be set before running the PHP binary:

  PHP_FCGI_CHILDREN  (default value: 0)
  !!! NOT WORK ON WINDOWS !!!

  This controls how many child processes the PHP process spawns. When the
  fastcgi starts, it creates a number of child processes which handle one
  page request at a time. Value 0 means that PHP willnot start additional
  processes and main process will handle FastCGI requests by itself. Note that
  this process may die (because of PHP_FCGI_MAX_REQUESTS) and it willnot
  respawned automatic. Values 1 and above force PHP start additioanl processes
  those will handle requests. The main process will restart children in case of
  their death. So by default, you will be able to handle 1 concurrent PHP page
  requests. Further requests will be queued. Increasing this number will allow
  for better concurrency, especially if you have pages that take a significant
  time to create, or supply a lot of data (e.g. downloading huge files via PHP).
  On the other hand, having more processes running will use more RAM, and letting
  too many PHP pages be generated concurrently will mean that each request will
  be slow. We recommend a value of 8 for a fairly busy site. If you have many,
  long-running PHP scripts, then you may need to increase this further.

  PHP_FCGI_MAX_REQUESTS (default value: 500)
  !!! set MaxRequestCount of TALPhpRunnerEngine < PHP_FCGI_MAX_REQUESTS !!!

  This controls how many requests each child process will handle before
  exitting. When one process exits, another will be created. This tuning is
  necessary because several PHP functions are known to have memory leaks. If the
  PHP processes were left around forever, they would be become very inefficient.

  Know bug :

  History :     29/01/2007: correct status missed in ALPhpRunnerECBServerSupportFunction
  Add ALL_HTTP in servervariables object
  30/01/2007: Add fconnectioncount to not unload bug when action is processing
  10/10/2009: rename TALPhpRunnerEngine in TALPhpIsapiRunnerEngine
  and add also TALPhpFastCgiRunnerEngine
  26/06/2012: Add xe2 support. Also Retiring the TALPhpIsapiRunnerEngine
  http://www.iis-aid.com/articles/iis_aid_news/php_isapi_module_to_be_retired

  Link :

  * Please send all your feedback to alcinoe@arkadia.com
  * If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
  * Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
  ************************************************************** }
unit ALPhpRunner;

interface

{$IF CompilerVersion >= 25} { Delphi XE4 }
{$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
  Winapi.Windows,
  Winapi.WinSock2,
  System.Classes,
  System.Contnrs,
  System.SyncObjs,
{$ELSE}
  Windows,
  WinSock,
  Classes,
  Contnrs,
  SyncObjs,
{$IFEND}
  ALHttpClient,
  ALStringList;

{ ###############################################################################
  Below the list of some server variables.
  It's important to init them correctly
  before to call the execute methode of
  the ALPhpRunnerEngine because they
  give to the engine all the neccessary
  params

  URL                    (URL=/scripts/rooter.php)
  PATH_INFO              (PATH_INFO=/scripts/rooter.php)
  PATH_TRANSLATED        (PATH_TRANSLATED=C:\InetPub\scripts\rooter.php)
  SCRIPT_NAME            (SCRIPT_NAME=/scripts/rooter.php)
  SCRIPT_FILENAME        (SCRIPT_FILENAME=C:\InetPub\scripts\rooter.php)   => init by php to PATH_TRANSLATED if empty
  DOCUMENT_ROOT      	   (DOCUMENT_ROOT=C:\InetPub\)                       => Server variable introduced in PHP
  if set, It will be use to calculate
  Path_Translated (from SCRIPT_NAME first, and
  if not set them from PATH_INFO)

  REQUEST_METHOD         (REQUEST_METHOD=GET)
  SERVER_PROTOCOL        (SERVER_PROTOCOL=HTTP/1.1)
  QUERY_STRING           (QUERY_STRING=goto=newpost&t=1)                   => Don't forget that Query_string need to be
  url_encoded
  HTTP_CACHE_CONTROL     (HTTP_CACHE_CONTROL=)
  HTTP_DATE              (HTTP_DATE=)
  HTTP_ACCEPT            (HTTP_ACCEPT=*/*)
  HTTP_FROM              (HTTP_FROM=)
  HTTP_HOST              (HTTP_HOST=127.0.0.1)
  HTTP_IF_MODIFIED_SINCE (HTTP_IF_MODIFIED_SINCE=)
  HTTP_REFERER           (HTTP_REFERER=http://www.yahoo.fr)
  HTTP_USER_AGENT        (HTTP_USER_AGENT=Mozilla/4.0)
  HTTP_CONTENT_ENCODING  (HTTP_CONTENT_ENCODING=)
  CONTENT_TYPE           (CONTENT_TYPE=)
  CONTENT_LENGTH         (CONTENT_LENGTH=0)                                => set automatiquely with the size of the
  contentstream provided
  HTTP_CONTENT_VERSION   (HTTP_CONTENT_VERSION=)
  HTTP_DERIVED_FROM      (HTTP_DERIVED_FROM=)
  HTTP_EXPIRES	  			 (HTTP_EXPIRES=)
  HTTP_TITLE						 (HTTP_TITLE=)
  REMOTE_ADDR            (REMOTE_ADDR=127.0.0.1)
  REMOTE_HOST            (REMOTE_HOST=127.0.0.1)
  SERVER_PORT            (SERVER_PORT=80)
  HTTP_CONNECTION        (HTTP_CONNECTION=Keep-Alive)
  HTTP_COOKIE						 (HTTP_COOKIE=cookie1=value1; cookie2=Value2)
  HTTP_AUTHORIZATION     (HTTP_AUTHORIZATION=)

  SERVER_SOFTWARE      	 (SERVER_SOFTWARE=Microsoft-IIS/5.1)
  SERVER_NAME            (SERVER_NAME=127.0.0.1)
  AUTH_TYPE              (AUTH_TYPE=)
  REMOTE_USER            (REMOTE_USER=)
  REMOTE_IDENT           (REMOTE_IDENT=)


  Below the list of all server variable found in the
  code of Php5Isapi.dll

  ALL_HTTP
  APPL_MD_PATH
  APPL_PHYSICAL_PATH
  AUTH_PASSWORD
  AUTH_TYPE
  AUTH_USER
  CERT_COOKIE
  CERT_FLAGS
  CERT_ISSUER
  CERT_KEYSIZE
  CERT_SECRETKEYSIZE
  CERT_SERIALNUMBER
  CERT_SERVER_ISSUER
  CERT_SERVER_SUBJECT
  CERT_SUBJECT
  CONTENT_LENGTH
  CONTENT_TYPE
  DOCUMENT_ROOT
  HTTP_AUTHORIZATION
  HTTP_COOKIE
  HTTPS
  HTTPS_KEYSIZE
  HTTPS_SECRETKEYSIZE
  HTTPS_SERVER_ISSUER
  HTTPS_SERVER_SUBJECT
  INSTANCE_ID
  INSTANCE_META_PATH
  LOGON_USER
  ORIG_PATH_INFO
  ORIG_PATH_TRANSLATED
  PATH_INFO
  PATH_TRANSLATED
  PHP_AUTH_PW
  PHP_AUTH_USER
  PHP_SELF
  QUERY_STRING
  REMOTE_ADDR
  REMOTE_HOST
  REMOTE_USER
  REQUEST_METHOD
  REQUEST_URI
  SCRIPT_FILENAME
  SCRIPT_NAME
  SERVER_NAME
  SERVER_PORT
  SERVER_PORT_SECURE
  SERVER_PROTOCOL
  SERVER_SIGNATURE
  SERVER_SOFTWARE
  SSL_CLIENT_C
  SSL_CLIENT_CN
  SSL_CLIENT_DN
  SSL_CLIENT_EMAIL
  SSL_CLIENT_I_C
  SSL_CLIENT_I_CN
  SSL_CLIENT_I_DN
  SSL_CLIENT_I_EMAIL
  SSL_CLIENT_I_L
  SSL_CLIENT_I_O
  SSL_CLIENT_I_OU
  SSL_CLIENT_I_ST
  SSL_CLIENT_L
  SSL_CLIENT_O
  SSL_CLIENT_OU
  SSL_CLIENT_ST
  URL
  ############################################################################### }

{$IF CompilerVersion < 18.5}

Type
  TStartupInfoA = TStartupInfo;
{$IFEND}

type

  { --------------------------------- }
  TALPhpRunnerEngine = class(Tobject)
  private
  protected
  public
    procedure Execute(ServerVariables: TALStrings;
      RequestContentStream: Tstream; ResponseContentStream: Tstream;
      ResponseHeader: TALHTTPResponseHeader); overload; virtual; abstract;
    function Execute(ServerVariables: TALStrings; RequestContentStream: Tstream)
      : AnsiString; overload; virtual;
    procedure Execute(ServerVariables: TALStrings;
      RequestContentString: AnsiString; ResponseContentStream: Tstream;
      ResponseHeader: TALHTTPResponseHeader); overload; virtual;
    function Execute(ServerVariables: TALStrings;
      RequestContentString: AnsiString): AnsiString; overload; virtual;
    procedure ExecutePostUrlEncoded(ServerVariables: TALStrings;
      PostDataStrings: TALStrings; ResponseContentStream: Tstream;
      ResponseHeader: TALHTTPResponseHeader;
      Const EncodeParams: Boolean = True); overload; virtual;
    function ExecutePostUrlEncoded(ServerVariables: TALStrings;
      PostDataStrings: TALStrings; Const EncodeParams: Boolean = True)
      : AnsiString; overload; virtual;
  end;

  { --------------------------------------------------- }
  TALPhpFastCgiRunnerEngine = class(TALPhpRunnerEngine)
  private
  protected
    procedure CheckError(Error: Boolean); virtual; abstract;
    Function IOWrite({$IF CompilerVersion >= 23}const {$ELSE}var
      {$IFEND} Buf; len: Integer): Integer; virtual; abstract;
    Function IORead(var Buf; len: Integer): Integer; virtual; abstract;
    Procedure SendRequest(aRequest: AnsiString); virtual;
    function ReadResponse: AnsiString; virtual;
  public
    procedure Execute(ServerVariables: TALStrings;
      RequestContentStream: Tstream; ResponseContentStream: Tstream;
      ResponseHeader: TALHTTPResponseHeader); override;
  end;

  { ---------------------------------------------------------------- }
  TALPhpSocketFastCgiRunnerEngine = class(TALPhpFastCgiRunnerEngine)
  private
    Fconnected: Boolean;
    FSocketDescriptor: TSocket;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    fKeepAlive: Boolean;
    fTCPNoDelay: Boolean;
    procedure SetSendTimeout(const Value: Integer);
    procedure SetReceiveTimeout(const Value: Integer);
    procedure SetKeepAlive(const Value: Boolean);
    procedure SetTCPNoDelay(const Value: Boolean);
  protected
    procedure CheckError(Error: Boolean); override;
    Function IOWrite({$IF CompilerVersion >= 23}const {$ELSE}var
      {$IFEND} Buf; len: Integer): Integer; override;
    Function IORead(var Buf; len: Integer): Integer; override;
  public
    constructor Create; overload; virtual;
    constructor Create(const aHost: AnsiString; const APort: Integer);
      overload; virtual;
    destructor Destroy; override;
    Procedure Connect(const aHost: AnsiString; const APort: Integer); virtual;
    Procedure Disconnect; virtual;
    property Connected: Boolean read Fconnected;
    property SendTimeout: Integer read FSendTimeout write SetSendTimeout;
    property ReceiveTimeout: Integer read FReceiveTimeout
      write SetReceiveTimeout;
    property KeepAlive: Boolean read fKeepAlive write SetKeepAlive;
    property TcpNoDelay: Boolean read fTCPNoDelay write fTCPNoDelay;
  end;

  { ------------------------------------------------------------------- }
  TALPhpNamedPipeFastCgiRunnerEngine = class(TALPhpFastCgiRunnerEngine)
  private
    FServerPipe: Thandle;
    fServerterminationEvent: Thandle;
    fServerProcessInformation: TProcessInformation;
    FClientPipe: Thandle;
    FPipePath: AnsiString;
    Fconnected: Boolean;
    FRequestCount: Integer;
    FMaxRequestCount: Integer;
    fPhpInterpreterFileName: AnsiString;
    Ftimeout: Integer;
  protected
    procedure CheckError(Error: Boolean); override;
    Function IOWrite({$IF CompilerVersion >= 23}const {$ELSE}var
      {$IFEND} Buf; len: Integer): Integer; override;
    Function IORead(var Buf; len: Integer): Integer; override;
    Property RequestCount: Integer read FRequestCount;
  public
    constructor Create; overload; virtual;
    constructor Create(aPhpInterpreterFilename: AnsiString); overload; virtual;
    destructor Destroy; override;
    Procedure Connect(aPhpInterpreterFilename: AnsiString); virtual;
    Procedure Disconnect; virtual;
    procedure Execute(ServerVariables: TALStrings;
      RequestContentStream: Tstream; ResponseContentStream: Tstream;
      ResponseHeader: TALHTTPResponseHeader); override;
    property Connected: Boolean read Fconnected;
    Property Timeout: Integer read Ftimeout write Ftimeout default 60000;
    Property MaxRequestCount: Integer read FMaxRequestCount
      write FMaxRequestCount Default 450;
  end;

  { ------------------------------------------------------- }
  TALPhpNamedPipeFastCgiManager = class(TALPhpRunnerEngine)
  private
    fCriticalSection: TCriticalSection;
    fPhpInterpreterFileName: AnsiString;
    FWorkingPhpRunnerEngineCount: Integer;
    FAvailablePhpRunnerengineLst: TobjectList;
    fProcessPoolSize: Integer;
    fIsDestroying: Boolean;
    FMaxRequestCount: Integer;
    Ftimeout: Integer;
  protected
    Function AcquirePHPRunnerEngine: TALPhpNamedPipeFastCgiRunnerEngine;
    Procedure ReleasePHPRunnerEngine(aPHPRunnerEngine
      : TALPhpNamedPipeFastCgiRunnerEngine);
  public
    constructor Create; overload; virtual;
    constructor Create(aPhpInterpreter: AnsiString); overload; virtual;
    destructor Destroy; override;
    procedure Execute(ServerVariables: TALStrings;
      RequestContentStream: Tstream; ResponseContentStream: Tstream;
      ResponseHeader: TALHTTPResponseHeader); override;
    Property PhpInterpreter: AnsiString read fPhpInterpreterFileName
      write fPhpInterpreterFileName;
    Property ProcessPoolSize: Integer read fProcessPoolSize
      write fProcessPoolSize default 8;
    Property MaxRequestCount: Integer read FMaxRequestCount
      write FMaxRequestCount Default 450;
    Property Timeout: Integer read Ftimeout write Ftimeout default 60000;
  end;

  { ----------------------------------------------- }
  TALPhpCgiRunnerEngine = class(TALPhpRunnerEngine)
  private
    fPhpInterpreterFileName: AnsiString;
  protected
  public
    constructor Create; overload; virtual;
    constructor Create(aPhpInterpreter: AnsiString); overload; virtual;
    procedure Execute(ServerVariables: TALStrings;
      RequestContentStream: Tstream; ResponseContentStream: Tstream;
      ResponseHeader: TALHTTPResponseHeader); override;
    property PhpInterpreter: AnsiString read fPhpInterpreterFileName
      write fPhpInterpreterFileName;
  end;

implementation

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
  System.sysutils,
{$ELSE}
  sysutils,
{$IFEND}
  ALWinSock,
  ALString,
  AlExecute,
  ALWindows,
  AlCGI;

{ ***************************************************************************** }
procedure TALPhpRunnerEngine.ExecutePostUrlEncoded(ServerVariables: TALStrings;
  PostDataStrings: TALStrings; ResponseContentStream: Tstream;
  ResponseHeader: TALHTTPResponseHeader; Const EncodeParams: Boolean = True);
Var
  aURLEncodedContentStream: TALStringStream;
  I: Integer;
begin
  aURLEncodedContentStream := TALStringStream.Create('');
  try

    if EncodeParams then
      ALHTTPEncodeParamNameValues(PostDataStrings);
    With PostDataStrings do
      for I := 0 to Count - 1 do
        If I < Count - 1 then
          aURLEncodedContentStream.WriteString(Strings[I] + '&')
        else
          aURLEncodedContentStream.WriteString(Strings[I]);

    ServerVariables.Values['REQUEST_METHOD'] := 'POST';
    ServerVariables.Values['CONTENT_TYPE'] :=
      'application/x-www-form-urlencoded';

    Execute(ServerVariables, aURLEncodedContentStream, ResponseContentStream,
      ResponseHeader);
  finally
    aURLEncodedContentStream.free;
  end;
end;

{ ************************************************************************************************************* }
function TALPhpRunnerEngine.Execute(ServerVariables: TALStrings;
  RequestContentString: AnsiString): AnsiString;
var
  ResponseContentStream: TALStringStream;
  ResponseHeader: TALHTTPResponseHeader;
  RequestContentStream: TALStringStream;
begin
  RequestContentStream := TALStringStream.Create(RequestContentString);
  ResponseContentStream := TALStringStream.Create('');
  ResponseHeader := TALHTTPResponseHeader.Create;
  Try
    Execute(ServerVariables, RequestContentStream, ResponseContentStream,
      ResponseHeader);
    Result := ResponseContentStream.DataString;
  finally
    ResponseContentStream.free;
    ResponseHeader.free;
    RequestContentStream.free;
  end;
end;

{ *************************************************************** }
procedure TALPhpRunnerEngine.Execute(ServerVariables: TALStrings;
  RequestContentString: AnsiString; ResponseContentStream: Tstream;
  ResponseHeader: TALHTTPResponseHeader);
var
  RequestContentStream: TALStringStream;
begin
  RequestContentStream := TALStringStream.Create(RequestContentString);
  Try
    Execute(ServerVariables, RequestContentStream, ResponseContentStream,
      ResponseHeader);
  finally
    RequestContentStream.free;
  end;
end;

{ ********************************************************************************************************** }
function TALPhpRunnerEngine.Execute(ServerVariables: TALStrings;
  RequestContentStream: Tstream): AnsiString;
var
  ResponseContentStream: TALStringStream;
  ResponseHeader: TALHTTPResponseHeader;
begin
  ResponseContentStream := TALStringStream.Create('');
  ResponseHeader := TALHTTPResponseHeader.Create;
  Try
    Execute(ServerVariables, RequestContentStream, ResponseContentStream,
      ResponseHeader);
    Result := ResponseContentStream.DataString;
  finally
    ResponseContentStream.free;
    ResponseHeader.free;
  end;
end;

{ **************************************************************************** }
function TALPhpRunnerEngine.ExecutePostUrlEncoded(ServerVariables: TALStrings;
  PostDataStrings: TALStrings; Const EncodeParams: Boolean = True): AnsiString;
var
  ResponseContentStream: TALStringStream;
  ResponseHeader: TALHTTPResponseHeader;
begin
  ResponseContentStream := TALStringStream.Create('');
  ResponseHeader := TALHTTPResponseHeader.Create;
  Try
    ExecutePostUrlEncoded(ServerVariables, PostDataStrings,
      ResponseContentStream, ResponseHeader);
    Result := ResponseContentStream.DataString;
  finally
    ResponseContentStream.free;
    ResponseHeader.free;
  end;
end;

{ ******************************************************************** }
procedure TALPhpFastCgiRunnerEngine.SendRequest(aRequest: AnsiString);
Var
  P: PAnsiChar;
  L: Integer;
  ByteSent: Integer;
begin
  P := @aRequest[1]; // pchar
  L := length(aRequest);
  while L > 0 do
  begin
    ByteSent := IOWrite(P^, L);
    if ByteSent <= 0 then
      raise Exception.Create('Connection close gracefully!');
    inc(P, ByteSent);
    dec(L, ByteSent);
  end;
end;

{ ********************************************************** }
function TALPhpFastCgiRunnerEngine.ReadResponse: AnsiString;

{ ------------------------------------------------------------ }
  Procedure InternalRead(var aStr: AnsiString; aCount: Longint);
  var
    aBuffStr: AnsiString;
    aBuffStrLength: Integer;
  Begin
    if aCount <= 0 then
      exit;
    Setlength(aBuffStr, 8192); // use a 8 ko buffer
    // we can also use IOCtlSocket(Socket, FIONREAD, @Tam) Use to determine the amount of
    // data pending in the network's input buffer that can be read from socket

    while aCount > 0 do
    begin
      aBuffStrLength := IORead(aBuffStr[1], length(aBuffStr));
      If aBuffStrLength <= 0 then
        raise Exception.Create('Connection close gracefully!');
      aStr := aStr + AlCopyStr(aBuffStr, 1, aBuffStrLength);
      dec(aCount, aBuffStrLength);
    end;
  End;

Var
  ErrMsg: AnsiString;
  CurrMsgStr: AnsiString;
  CurrMsgContentlength: Integer;
  CurrMsgPaddingLength: Integer;
begin
  { init the result and local var }
  Result := '';
  ErrMsg := '';
  CurrMsgStr := '';

  { loop throught all message }
  While True do
  begin

    // msg is of the form :
    // version#type#requestIdB1#requestIdB0#contentLengthB1#contentLengthB0#paddingLength#reserved#contentData[contentLength]#paddingData[paddingLength];
    // [1]  [2]    [3]           [4]          [5]              [6]            [7]        [8]           [9]

    { first read enalf of the message to know his size }
    while length(CurrMsgStr) < 7 do
      InternalRead(CurrMsgStr, 1);

    { first read enalf of the message to know the size }
    CurrMsgContentlength := (byte(CurrMsgStr[5]) shl 8) + byte(CurrMsgStr[6]);
    // contentLengthB1             contentLengthB0
    CurrMsgPaddingLength := byte(CurrMsgStr[7]);
    // paddingLength

    { put the full message in CurrMsgStr }
    InternalRead(CurrMsgStr, CurrMsgContentlength + CurrMsgPaddingLength + 8 -
      length(CurrMsgStr));

    { if message = FCGI_END_REQUEST }
    if CurrMsgStr[2] = #3 then
    Begin

      // The contentData component of a FCGI_END_REQUEST record has the form:
      // appStatusB3#appStatusB2#appStatusB1#appStatusB0#protocolStatus#reserved[3]
      // [9]           [10]        [11]        [12]          [13]          [14]

      if ErrMsg <> '' then
        raise Exception.Create(String(ErrMsg))
      else if (length(CurrMsgStr) < 13) or
        (CurrMsgStr[13] <> #0 { FCGI_REQUEST_COMPLETE } ) then
        raise Exception.Create
          ('The Php has encountered an error while processing the request!')
      else
        exit; // ok, everything is ok so exit;

    End

    { else if message = FCGI_STDOUT }
    else if CurrMsgStr[2] = #6 then
    begin
      Result := Result + AlCopyStr(CurrMsgStr, 9, CurrMsgContentlength);
      CurrMsgStr := AlCopyStr(CurrMsgStr, 9 + CurrMsgContentlength +
        CurrMsgPaddingLength, Maxint);
    end

    { else if message = FCGI_STDERR }
    else if CurrMsgStr[2] = #7 then
    begin
      ErrMsg := ErrMsg + AlCopyStr(CurrMsgStr, 9, CurrMsgContentlength);
      CurrMsgStr := AlCopyStr(CurrMsgStr, 9 + CurrMsgContentlength +
        CurrMsgPaddingLength, Maxint);
    end;

    // else not possible, not in the fcgi spec, so skip the message,

  end;

end;

{ ********************************************************************** }
procedure TALPhpFastCgiRunnerEngine.Execute(ServerVariables: TALStrings;
  RequestContentStream: Tstream; ResponseContentStream: Tstream;
  ResponseHeader: TALHTTPResponseHeader);

{ ---------------------------------------------------------------------- }
{ i not understand why in FCGI_PARAMS we need to specify te contentlength
  to max 65535 and in name value pair we can specify a length up to 17 Mo!
  anyway a content length of 65535 for the server variable seam to be suffisant }
  procedure InternalAddParam(var aStr: AnsiString; aName, aValue: AnsiString);
  var
    I, J: Integer;
    len: array [0 .. 1] of Integer;
    Format: array [0 .. 1] of Integer;
    Tam: word;
  begin

    { ---------- }
    len[0] := length(aName);
    if len[0] <= 127 then
      Format[0] := 1
    else
      Format[0] := 4;
    { ---------- }
    len[1] := length(aValue);
    if len[1] <= 127 then
      Format[1] := 1
    else
      Format[1] := 4;
    { ---------- }
    Tam := len[0] + Format[0] + len[1] + Format[1];
    aStr := aStr + #1 + #4 + #0 + #1 + AnsiChar(hi(Tam)) + AnsiChar(lo(Tam)
      ) + #0 + #0;
    // +FCGI_VERSION_1 +FCGI_PARAMS +requestIdB1 +requestIdB0 +contentLengthB1   +contentLengthB0   +paddingLength +reserved
    J := length(aStr);
    Setlength(aStr, J + Tam);
    inc(J);
    for I := 0 to 1 do
    begin
      if Format[I] = 1 then
        aStr[J] := AnsiChar(len[I])
      else
      begin
        aStr[J] := AnsiChar(((len[I] shr 24) and $FF) + $80);
        aStr[J + 1] := AnsiChar((len[I] shr 16) and $FF);
        aStr[J + 2] := AnsiChar((len[I] shr 8) and $FF);
        aStr[J + 3] := AnsiChar(len[I] and $FF);
      end;
      inc(J, Format[I]);
    end;
    ALMove(aName[1], aStr[J], len[0]);
    ALMove(aValue[1], aStr[J + len[0]], len[1]);

    // the content data of the name value pair look like :
    // nameLengthB0#valueLengthB0#nameData[nameLength]#valueData[valueLength]
    // nameLengthB0#valueLengthB3#valueLengthB2#valueLengthB1#valueLengthB0#nameData[nameLength]#valueData[valueLength]
    // nameLengthB3#nameLengthB2#nameLengthB1#nameLengthB0#valueLengthB0#nameData[nameLength]#valueData[valueLength]
    // nameLengthB3#nameLengthB2#nameLengthB1#nameLengthB0#valueLengthB3#valueLengthB2#valueLengthB1#valueLengthB0#nameData[nameLength]#valueData[valueLength]

  end;

{ ---------------------------------------------- }
  function InternalAddServerVariables: AnsiString;
  var
    aValue: AnsiString;
    I: Integer;
  begin

    { build result }
    Result := '';
    for I := 0 to ServerVariables.Count - 1 do
    begin
      aValue := ServerVariables.ValueFromIndex[I];
      if aValue <> '' then
        InternalAddParam(Result, ServerVariables.Names[I], aValue);
    end;

    { finalize Result with an empty FCGI_PARAMS }
    Result := Result + #1 + #4 + #0 + #1 + #0 + #0 + #0 + #0;
    // FCGI_VERSION_1 +FCGI_PARAMS +requestIdB1 +requestIdB0 +contentLengthB1 +contentLengthB0 +paddingLength +reserved

  end;

var
  aResponseStr: AnsiString;
  aFormatedRequestStr: AnsiString;
  Tam: word;
  P1: Integer;
  S1: AnsiString;
begin

  { init aFormatedRequestStr from aRequestStr }
  aFormatedRequestStr := '';
  if assigned(RequestContentStream) then
  begin
    P1 := 1;
    Setlength(S1, 8184);
    RequestContentStream.Position := 0;
    while P1 <= RequestContentStream.Size do
    begin
      Tam := RequestContentStream.Read(S1[1], 8184);
      // ok i decide to plit the message in 8ko, because php send me in FCGI_STDOUT message split in 8ko (including 8 bytes of header)
      inc(P1, Tam);
      aFormatedRequestStr := aFormatedRequestStr + #1 + #5 + #0 + #1 +
        AnsiChar(hi(Tam)) + AnsiChar(lo(Tam)) + #0 + #0 + AlCopyStr(S1, 1, Tam);
      // FCGI_VERSION_1 +FCGI_STDIN +requestIdB1 +requestIdB0 +contentLengthB1   +contentLengthB0   +paddingLength +reserved +contentData[contentLength]
    end;

    { For securty issue... if content_length badly set then cpu can go to 100% }
    ServerVariables.Values['CONTENT_LENGTH'] :=
      ALIntToStr(RequestContentStream.Size);

  end

  { For securty issue... if content_length badly set then cpu can go to 100% }
  else
    ServerVariables.Values['CONTENT_LENGTH'] := '0';

  { finalize the aFormatedRequestStr with an empty FCGI_STDIN }
  aFormatedRequestStr := aFormatedRequestStr + #1 + #5 + #0 + #1 + #0 + #0
    + #0 + #0;
  // FCGI_VERSION_1 +FCGI_STDIN +requestIdB1 +requestIdB0 +contentLengthB1 +contentLengthB0 +paddingLength +reserved

  SendRequest(#1 + #1 + #0 + #1 + #0 + #8 + #0 + #0 + #0 + #1 + #1 + #0 + #0 +
    #0 + #0 + #0 +
    // FCGI_VERSION_1 +FCGI_BEGIN_REQUEST +requestIdB1 +requestIdB0 +contentLengthB1 +contentLengthB0 +paddingLength +reserved +roleB1 +FCGI_RESPONDER +FCGI_KEEP_CONN +reserved +reserved +reserved +reserved +reserved
    // contentData[contentLength]-----------------------------------------------------
    InternalAddServerVariables + aFormatedRequestStr);

  { ---------- }
  aResponseStr := ReadResponse;
  P1 := AlPos(#13#10#13#10, aResponseStr);
  if P1 <= 0 then
    raise Exception.Create
      ('The Php has encountered an error while processing the request!');
  ResponseHeader.RawHeaderText := AlCopyStr(aResponseStr, 1, P1 - 1);
  if P1 + 4 <= length(aResponseStr) then
    ResponseContentStream.Write(aResponseStr[P1 + 4],
      length(aResponseStr) - P1 - 3);

end;

{ ************************************************************************************************ }
constructor TALPhpSocketFastCgiRunnerEngine.Create(const aHost: AnsiString;
  const APort: Integer);
Begin
  Create;
  Connect(aHost, APort);
End;

{ ************************************************* }
constructor TALPhpSocketFastCgiRunnerEngine.Create;
var
  aWSAData: TWSAData;
begin
  CheckError(WSAStartup(MAKEWORD(2, 2), aWSAData) <> 0);
  Fconnected := False;
  FSocketDescriptor := INVALID_SOCKET;
  FSendTimeout := 60000; // 60 seconds
  FReceiveTimeout := 60000; // 60 seconds
  fKeepAlive := True;
  fTCPNoDelay := True;
end;

{ ************************************************* }
destructor TALPhpSocketFastCgiRunnerEngine.Destroy;
begin
  If Fconnected then
    Disconnect;
  WSACleanup;
  inherited;
end;

{ *********************************************************************************************** }
procedure TALPhpSocketFastCgiRunnerEngine.Connect(const aHost: AnsiString;
  const APort: Integer);

{ -------------------------------------------------------------- }
  procedure _CallServer(const Server: AnsiString; const Port: word);
  var
    SockAddr: Sockaddr_in;
    IP: AnsiString;
  begin
    FSocketDescriptor := Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    CheckError(FSocketDescriptor = INVALID_SOCKET);
    FillChar(SockAddr, SizeOf(SockAddr), 0);
    SockAddr.sin_family := AF_INET;
    SockAddr.sin_port := swap(Port);
    SockAddr.sin_addr.S_addr := inet_addr(PAnsiChar(Server));
{$IF CompilerVersion >= 23} { Delphi XE2 }
    If SockAddr.sin_addr.S_addr = INADDR_NONE then
    begin
{$ELSE}
    If SockAddr.sin_addr.S_addr = Integer(INADDR_NONE) then
    begin
{$IFEND}
      CheckError(ALHostToIP(Server, IP));
      SockAddr.sin_addr.S_addr := inet_addr(PAnsiChar(IP));
    end;
{$IF CompilerVersion >= 23} { Delphi XE2 }
    CheckError(Winapi.WinSock2.Connect(FSocketDescriptor, TSockAddr(SockAddr),
      SizeOf(SockAddr)) = SOCKET_ERROR);
{$ELSE}
    CheckError(WinSock.Connect(FSocketDescriptor, SockAddr, SizeOf(SockAddr))
      = SOCKET_ERROR);
{$IFEND}
  end;

begin

  if Fconnected then
    raise Exception.Create('Already connected');

  Try

    _CallServer(aHost, APort);
    Fconnected := True;
    SetSendTimeout(FSendTimeout);
    SetReceiveTimeout(FReceiveTimeout);
    SetKeepAlive(fKeepAlive);
    SetTCPNoDelay(fTCPNoDelay);

  Except
    Disconnect;
    raise;
  end;

end;

{ *************************************************** }
procedure TALPhpSocketFastCgiRunnerEngine.Disconnect;
begin
  If Fconnected then
  begin
    ShutDown(FSocketDescriptor, SD_BOTH);
    CloseSocket(FSocketDescriptor);
    FSocketDescriptor := INVALID_SOCKET;
    Fconnected := False;
  end;
end;

{ ******************************************************************* }
procedure TALPhpSocketFastCgiRunnerEngine.CheckError(Error: Boolean);
begin
  if Error then
    RaiseLastOSError;
end;

{ ***************************************************************************** }
procedure TALPhpSocketFastCgiRunnerEngine.SetSendTimeout(const Value: Integer);
begin
  FSendTimeout := Value;
  if Fconnected then
    CheckError(setsockopt(FSocketDescriptor, SOL_SOCKET, SO_SNDTIMEO,
      PAnsiChar(@FSendTimeout), SizeOf(FSendTimeout)) = SOCKET_ERROR);
end;

{ ******************************************************************************** }
procedure TALPhpSocketFastCgiRunnerEngine.SetReceiveTimeout
  (const Value: Integer);
begin
  FReceiveTimeout := Value;
  if Fconnected then
    CheckError(setsockopt(FSocketDescriptor, SOL_SOCKET, SO_RCVTIMEO,
      PAnsiChar(@FReceiveTimeout), SizeOf(FReceiveTimeout)) = SOCKET_ERROR);
end;

{ ******************************************************************************************************************* }
// http://blogs.technet.com/b/nettracer/archive/2010/06/03/things-that-you-may-want-to-know-about-tcp-keepalives.aspx
procedure TALPhpSocketFastCgiRunnerEngine.SetKeepAlive(const Value: Boolean);
var
  aIntBool: Integer;
begin
  fKeepAlive := Value;
  if Fconnected then
  begin
    // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
    // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
    // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
    if fKeepAlive then
      aIntBool := 1
    else
      aIntBool := 0;
    CheckError(setsockopt(FSocketDescriptor, SOL_SOCKET, SO_KEEPALIVE,
      PAnsiChar(@aIntBool), SizeOf(aIntBool)) = SOCKET_ERROR);
  end;
end;

{ *************************************************************************************************************************************************************************************************************** }
// https://access.redhat.com/site/documentation/en-US/Red_Hat_Enterprise_MRG/1.1/html/Realtime_Tuning_Guide/sect-Realtime_Tuning_Guide-Application_Tuning_and_Deployment-TCP_NODELAY_and_Small_Buffer_Writes.html
procedure TALPhpSocketFastCgiRunnerEngine.SetTCPNoDelay(const Value: Boolean);
var
  aIntBool: Integer;
begin
  fTCPNoDelay := Value;
  if Fconnected then
  begin
    // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
    // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
    // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
    if fTCPNoDelay then
      aIntBool := 1
    else
      aIntBool := 0;
    CheckError(setsockopt(FSocketDescriptor, SOL_SOCKET, TCP_NODELAY,
      PAnsiChar(@aIntBool), SizeOf(aIntBool)) = SOCKET_ERROR);
  end;
end;

{ ****************************************************************************** }
function TALPhpSocketFastCgiRunnerEngine.IORead(var Buf; len: Integer): Integer;
begin
  Result := Recv(FSocketDescriptor, Buf, len, 0);
  CheckError(Result = SOCKET_ERROR);
end;

{ ****************************************************************************************************************************** }
function TALPhpSocketFastCgiRunnerEngine.IOWrite
  ({$IF CompilerVersion >= 23}const {$ELSE}var
  {$IFEND} Buf; len: Integer): Integer;
begin
  Result := Send(FSocketDescriptor, Buf, len, 0);
  CheckError(Result = SOCKET_ERROR);
end;

{ ***************************************************************************************** }
constructor TALPhpNamedPipeFastCgiRunnerEngine.Create(aPhpInterpreterFilename
  : AnsiString);
begin
  Create;
  Connect(aPhpInterpreterFilename);
end;

{ **************************************************** }
constructor TALPhpNamedPipeFastCgiRunnerEngine.Create;
begin
  FServerPipe := INVALID_HANDLE_VALUE;
  FClientPipe := INVALID_HANDLE_VALUE;
  fServerterminationEvent := INVALID_HANDLE_VALUE;
  fServerProcessInformation.hProcess := INVALID_HANDLE_VALUE;
  fServerProcessInformation.hThread := INVALID_HANDLE_VALUE;
  fServerProcessInformation.dwProcessId := 0;
  fServerProcessInformation.dwThreadId := 0;
  FRequestCount := 0;
  FMaxRequestCount := 450;
  fPhpInterpreterFileName := 'php-cgi.exe';
  Fconnected := False;
  Ftimeout := 60000;
end;

{ **************************************************** }
destructor TALPhpNamedPipeFastCgiRunnerEngine.Destroy;
begin
  if Connected then
    Disconnect;
  inherited;
end;

{ **************************************************************************************** }
procedure TALPhpNamedPipeFastCgiRunnerEngine.Connect(aPhpInterpreterFilename
  : AnsiString);
Var
  aStartupInfo: TStartupInfoA;
  aEnvironment: AnsiString;
begin
  if Fconnected then
    raise Exception.Create('Already connected');

  // create the pipepath here because if we do it in the oncreate the
  // fpipepath can survive few seconds after the disconnection, making
  // some trouble in the next execute loop (pipe has been ended)
  FPipePath := '\\.\pipe\ALPhpFastCGIRunner-' +
    ALNewGUIDString(True { WithoutBracket } );

  // create the server pipe
  FServerPipe := CreateNamedPipeA(PAnsiChar(FPipePath), // lpName
    PIPE_ACCESS_DUPLEX, // dwOpenMode
    PIPE_TYPE_BYTE or PIPE_WAIT or PIPE_READMODE_BYTE, // dwPipeMode
    PIPE_UNLIMITED_INSTANCES, // nMaxInstances
    4096, // nOutBufferSize
    4096, // nInBufferSize
    0, // nDefaultTimeOut
    NiL); // lpSecurityAttributes
  CheckError(FServerPipe = INVALID_HANDLE_VALUE);
  try

    // Make FServerPipe inheritable.
    CheckError(not SetHandleInformation(FServerPipe, HANDLE_FLAG_INHERIT,
      HANDLE_FLAG_INHERIT));

    // create the termination event
    fServerterminationEvent := CreateEvent(NiL, // lpEventAttributes
      True, // bManualReset
      False, // bInitialState
      NiL); // lpName
    CheckError(fServerterminationEvent = INVALID_HANDLE_VALUE);
    Try

      CheckError(not SetHandleInformation(fServerterminationEvent,
        HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT));
      aEnvironment := AlGetEnvironmentString + '_FCGI_SHUTDOWN_EVENT_' + '=' +
        ALIntToStr(fServerterminationEvent) + #0#0;

      // Set up the start up info struct.
      ZeroMemory(@aStartupInfo, SizeOf(TStartupInfo));
      aStartupInfo.cb := SizeOf(TStartupInfo);
      aStartupInfo.lpReserved := nil;
      aStartupInfo.lpReserved2 := nil;
      aStartupInfo.cbReserved2 := 0;
      aStartupInfo.lpDesktop := nil;
      aStartupInfo.dwFlags := STARTF_USESTDHANDLES;
      // FastCGI on NT will set the listener pipe HANDLE in the stdin of
      // the new process.  The fact that there is a stdin and NULL handles
      // for stdout and stderr tells the FastCGI process that this is a
      // FastCGI process and not a CGI process.
      aStartupInfo.hStdInput := FServerPipe;
      aStartupInfo.hStdOutput := INVALID_HANDLE_VALUE;
      aStartupInfo.hStdError := INVALID_HANDLE_VALUE;

      // Make the listener socket inheritable.
      CheckError(not SetHandleInformation(aStartupInfo.hStdInput,
        HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT));

      // Launch the process that you want to redirect.
      CheckError(Not CreateProcessA(PAnsiChar(aPhpInterpreterFilename),
        // pointer to name of executable module
        nil, // pointer to command line string
        nil, // pointer to process security attributes
        NiL, // pointer to thread security attributes
        True, // handle inheritance flag
        CREATE_NO_WINDOW, // creation flags
        PAnsiChar(aEnvironment), // pointer to new environment block
        nil, // pointer to current directory name
        aStartupInfo, // pointer to STARTUPINFO
        fServerProcessInformation)); // pointer to PROCESS_INFORMATION

      CheckError(not WaitNamedPipeA(PAnsiChar(FPipePath), Ftimeout));
      FClientPipe := CreateFileA(PAnsiChar(FPipePath), // lpFileName
        GENERIC_WRITE or GENERIC_READ, // dwDesiredAccess
        FILE_SHARE_READ or FILE_SHARE_WRITE, // dwShareMode
        nil, // lpSecurityAttributes
        OPEN_EXISTING, // dwCreationDisposition
        0, // dwFlagsAndAttributes
        0); // hTemplateFile
      CheckError(FClientPipe = INVALID_HANDLE_VALUE);

    Except
      CloseHandle(fServerterminationEvent);
      fServerterminationEvent := INVALID_HANDLE_VALUE;
      Raise;
    End;

  Except
    CloseHandle(FServerPipe);
    FServerPipe := INVALID_HANDLE_VALUE;
    raise;
  end;
  Fconnected := True;
  FRequestCount := 0;
  fPhpInterpreterFileName := aPhpInterpreterFilename;
end;

{ ****************************************************** }
procedure TALPhpNamedPipeFastCgiRunnerEngine.Disconnect;
var
  lpExitCode: DWORD;
begin
  If Fconnected then
  begin

    // Send The Signal Shutdown, but it's seam than
    // php-cgi not handle it, it's simply set a flag in_shutdown
    // to 1 and not close the application
    SetEvent(fServerterminationEvent);

    // Force terminate the server is still active
    GetExitCodeProcess(fServerProcessInformation.hProcess, lpExitCode);
    if (lpExitCode = STILL_ACTIVE) then
      TerminateProcess(fServerProcessInformation.hProcess, 1);

    // close all the handle
    CloseHandle(fServerProcessInformation.hProcess);
    fServerProcessInformation.hProcess := INVALID_HANDLE_VALUE;
    CloseHandle(fServerProcessInformation.hThread);
    fServerProcessInformation.hThread := INVALID_HANDLE_VALUE;
    fServerProcessInformation.dwProcessId := 0;
    fServerProcessInformation.dwThreadId := 0;
    CloseHandle(FClientPipe);
    FClientPipe := INVALID_HANDLE_VALUE;
    CloseHandle(FServerPipe);
    FServerPipe := INVALID_HANDLE_VALUE;
    CloseHandle(fServerterminationEvent);
    fServerterminationEvent := INVALID_HANDLE_VALUE;

    // set connected to false
    Fconnected := False;
    FRequestCount := 0;

  end;
end;

{ ********************************************************************** }
procedure TALPhpNamedPipeFastCgiRunnerEngine.CheckError(Error: Boolean);
begin
  if Error then
    RaiseLastOSError
end;

{ ********************************************************************************* }
function TALPhpNamedPipeFastCgiRunnerEngine.IORead(var Buf;
  len: Integer): Integer;
Var
  lpNumberOfBytesRead: DWORD;
  StartTickCount: Int64;
begin
  // Ok i don't found any other way than this loop to do a timeout !
  // timeout are neccessary if the php-cgi.exe dead suddenly for exemple
  // in the way without timout the readfile will never return freezing the application
  StartTickCount := ALGetTickCount64;
  Repeat
    CheckError(not PeekNamedPipe(FClientPipe, // handle to pipe to copy from
      nil, // pointer to data buffer
      0, // size, in bytes, of data buffer
      nil, // pointer to number of bytes read
      @lpNumberOfBytesRead, // pointer to total number of bytes available
      nil)); // pointer to unread bytes in this message
    if lpNumberOfBytesRead > 0 then
    begin
      CheckError(not ReadFile(FClientPipe, Buf, len, lpNumberOfBytesRead, nil));
      Result := lpNumberOfBytesRead;
      break;
    end
    else
      Result := 0;
    sleep(1); // this is neccessary to not use 100% CPU usage
  Until ALGetTickCount64 - StartTickCount > Ftimeout;
end;

{ ********************************************************************************************************************************* }
function TALPhpNamedPipeFastCgiRunnerEngine.IOWrite
  ({$IF CompilerVersion >= 23}const {$ELSE}var
  {$IFEND} Buf; len: Integer): Integer;
Var
  lpNumberOfBytesWritten: DWORD;
begin
  CheckError(not WriteFile(FClientPipe, Buf, len, lpNumberOfBytesWritten, nil));
  Result := lpNumberOfBytesWritten;
end;

{ ******************************************************************************* }
procedure TALPhpNamedPipeFastCgiRunnerEngine.Execute(ServerVariables
  : TALStrings; RequestContentStream: Tstream; ResponseContentStream: Tstream;
  ResponseHeader: TALHTTPResponseHeader);
begin
  if (FMaxRequestCount > 0) and (FRequestCount >= FMaxRequestCount) then
  begin
    Disconnect;
    Connect(fPhpInterpreterFileName);
  end;
  inc(FRequestCount);

  inherited Execute(ServerVariables, RequestContentStream,
    ResponseContentStream, ResponseHeader);
end;

{ *********************************************** }
constructor TALPhpNamedPipeFastCgiManager.Create;
begin
  fCriticalSection := TCriticalSection.Create;
  fPhpInterpreterFileName := 'php-cgi.exe';
  fIsDestroying := False;
  FWorkingPhpRunnerEngineCount := 0;
  FAvailablePhpRunnerengineLst := TobjectList.Create(False);
  fProcessPoolSize := 8;
  FMaxRequestCount := 450;
  Ftimeout := 60000;
end;

{ **************************************************************************** }
constructor TALPhpNamedPipeFastCgiManager.Create(aPhpInterpreter: AnsiString);
begin
  Create;
  fPhpInterpreterFileName := aPhpInterpreter;
end;

{ *********************************************** }
destructor TALPhpNamedPipeFastCgiManager.Destroy;
Var
  I: Integer;
begin

  { we do this to forbid any new thread to create a new transaction }
  fCriticalSection.Acquire;
  Try
    fIsDestroying := True;
  finally
    fCriticalSection.Release;
  end;

  { wait that all transaction are finished }
  while True do
  begin
    fCriticalSection.Acquire;
    Try
      if FWorkingPhpRunnerEngineCount <= 0 then
        break;
    finally
      fCriticalSection.Release;
    end;
    sleep(1); // to not use 100% CPU Usage
  end;

  { free all object }
  for I := 0 to FAvailablePhpRunnerengineLst.Count - 1 do
    FAvailablePhpRunnerengineLst[I].free;
  FAvailablePhpRunnerengineLst.free;
  fCriticalSection.free;

  inherited;
end;

{ ************************************************************************************************ }
function TALPhpNamedPipeFastCgiManager.AcquirePHPRunnerEngine
  : TALPhpNamedPipeFastCgiRunnerEngine;
begin
  fCriticalSection.Acquire;
  Try

    // for a stupid warning (D2007)
    Result := nil;

    // raise an exception if the object is destroying
    if fIsDestroying then
      raise Exception.Create('Manager is destroying!');

    // Extract one engine
    If FAvailablePhpRunnerengineLst.Count > 0 then
    begin
      Result := TALPhpNamedPipeFastCgiRunnerEngine
        (FAvailablePhpRunnerengineLst
        [(FAvailablePhpRunnerengineLst.Count - 1)]);
      FAvailablePhpRunnerengineLst.Delete
        (FAvailablePhpRunnerengineLst.Count - 1);
    end
    else
    begin
      Result := TALPhpNamedPipeFastCgiRunnerEngine.Create
        (fPhpInterpreterFileName);
      Result.MaxRequestCount := FMaxRequestCount;
      Result.Timeout := Ftimeout;
    end;

    // increase the number of Working PHPRunnerEngine
    inc(FWorkingPhpRunnerEngineCount);

  finally
    fCriticalSection.Release;
  end;
end;

{ ******************************************************************************************************************* }
Procedure TALPhpNamedPipeFastCgiManager.ReleasePHPRunnerEngine(aPHPRunnerEngine
  : TALPhpNamedPipeFastCgiRunnerEngine);
begin
  fCriticalSection.Acquire;
  Try

    // decrease the number of Working PHPRunnerEngine
    dec(FWorkingPhpRunnerEngineCount);
    if assigned(aPHPRunnerEngine) then
    begin
      If (FAvailablePhpRunnerengineLst.Count < fProcessPoolSize) then
        FAvailablePhpRunnerengineLst.Add(aPHPRunnerEngine)
      else
        aPHPRunnerEngine.free;
    end;

  finally
    fCriticalSection.Release;
  end;
end;

{ ************************************************************************** }
procedure TALPhpNamedPipeFastCgiManager.Execute(ServerVariables: TALStrings;
  RequestContentStream: Tstream; ResponseContentStream: Tstream;
  ResponseHeader: TALHTTPResponseHeader);
Var
  aPHPRunnerEngine: TALPhpNamedPipeFastCgiRunnerEngine;
begin
  aPHPRunnerEngine := AcquirePHPRunnerEngine;
  try

    try

      aPHPRunnerEngine.Execute(ServerVariables, RequestContentStream,
        ResponseContentStream, ResponseHeader);

    Except
      freeandnil(aPHPRunnerEngine);
      raise;
    end;

  finally
    ReleasePHPRunnerEngine(aPHPRunnerEngine)
  end;
end;

{ ******************************************************************** }
constructor TALPhpCgiRunnerEngine.Create(aPhpInterpreter: AnsiString);
begin
  fPhpInterpreterFileName := aPhpInterpreter;
end;

{ *************************************** }
constructor TALPhpCgiRunnerEngine.Create;
begin
  Create('php-cgi.exe');
end;

{ ****************************************************************** }
procedure TALPhpCgiRunnerEngine.Execute(ServerVariables: TALStrings;
  RequestContentStream: Tstream; ResponseContentStream: Tstream;
  ResponseHeader: TALHTTPResponseHeader);
begin
  AlCGIExec(fPhpInterpreterFileName, ServerVariables, RequestContentStream,
    ResponseContentStream, ResponseHeader);
end;

end.
