{ *************************************************************
  www:          http://sourceforge.net/projects/alcinoe/
  svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
  Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
  Sponsor(s):   Arkadia SA (http://www.arkadia.com)

  product:      Alcinoe Internet Messages Utilities
  Version:      4.00

  Description:  This unit contains utilities to manipulate
  Internet Messages Headers (EMAIL or NEWSNET messages)

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

  History :     05/02/2008: add soft line break in ALDecodeQuotedPrintableString
  26/06/2012: Add xe2 support

  Link :        http://www.faqs.org/rfcs/rfc2047.html

  * Please send all your feedback to alcinoe@arkadia.com
  * If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
  * Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
  ************************************************************** }
unit ALInternetMessages;

interface

{$IF CompilerVersion >= 25} { Delphi XE4 }
{$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
  system.Classes,
{$ELSE}
  Classes,
{$IFEND}
  AlStringList;

Type

  { ----------------------------- }
  TALEMailHeader = Class(Tobject)
  Private
    fSendTo: AnsiString;
    fSender: AnsiString;
    fMessageID: AnsiString;
    fbcc: AnsiString;
    fContentTransferEncoding: AnsiString;
    fComments: AnsiString;
    fMIMEVersion: AnsiString;
    fPriority: AnsiString;
    fReplyTo: AnsiString;
    fSubject: AnsiString;
    fFrom: AnsiString;
    fDate: AnsiString;
    fDispositionNotificationTo: AnsiString;
    fReferences: AnsiString;
    fcc: AnsiString;
    fContentType: AnsiString;
    FCustomHeaders: TALStrings;
    Function GetRawHeaderText: AnsiString;
    procedure SetRawHeaderText(const aRawHeaderText: AnsiString);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property From: AnsiString read fFrom write fFrom;
    { From: John Doe <jdoe@machine.example> - Author(s) or person(s) taking responsibility for the message 4.4.1; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1 }
    property Sender: AnsiString read fSender write fSender;
    { Sender: Michael Jones <mjones@machine.example> - The person or agent submitting the message to the network, if other than shown by the From header RFC 822: 4.4.2; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1 }
    property SendTo: AnsiString read fSendTo write fSendTo;
    { To: Mary Smith <mary@example.net> - Primary recipient(s) RFC 822: 4.5.1; RFC 1123: 5.2.15-16, 5.3.7; }
    property cc: AnsiString read fcc write fcc;
    { cc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net> - Secondary, informational recipient(s) RFC 822: 4.5.2; RFC 1123: 5.2.15-16, 5.3.7; }
    property bcc: AnsiString read fbcc write fbcc;
    { bcc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net> - Recipient(s) not to be disclosed to other recipients ("blind carbon copy") RFC 822: 4.5.3; RFC 1123: 5.2.15-16, 5.3.7; }
    property ReplyTo: AnsiString read fReplyTo write fReplyTo;
    { Reply-To: "Mary Smith: Personal Account" <smith@home.example> - Suggested E-mail address for replies RFC 822: 4.4.3; RFC 1036: 2.2.1 }
    property Subject: AnsiString read fSubject write fSubject;
    { Subject: Saying Hello - Text that provides a summary, or indicates the nature, of the message RFC 822: 4.7.1; RFC 1036: 2.1.4 }
    property MessageID: AnsiString read fMessageID write fMessageID;
    { Message-ID: <1234@local.machine.example> -	Unique ID for the message RFC 822: 4.6.1; RFC 1036: 2.1.5 }
    property References: AnsiString read fReferences write fReferences;
    { References: <1234@local.machine.example> <3456@example.net> - In E-mail: reference to other related messages; in Usenet: reference to replied-to-articles RFC 822: 4.6.3; RFC 1036: 2.2.5 }
    property Comments: AnsiString read fComments write fComments;
    { Comments: Authenticated sender is gboyd@netcom.com - Text comments added to the message RFC 822: 4.7.2 }
    property Date: AnsiString read fDate write fDate;
    { Date: Fri, 21 Nov 1997 09:55:06 -0600 - The time when the message was written (or submitted) RFC 822: 5.1; RFC 1123: 5.2.14; RFC 1036: 2.1.2 }
    property ContentType: AnsiString read fContentType write fContentType;
    { Content-Type: text/plain; charset="iso-8859-1" - Data type and format of content RFC 1049 (historic); RFC 1123: 5.2.13; RFC 2045: 5; RFC 1766: 4.1 }
    property ContentTransferEncoding: AnsiString read fContentTransferEncoding
      write fContentTransferEncoding;
    { Content-Transfer-Encoding: 8bit - Coding method used in a MIME message body RFC 2045: 6; }
    property MIMEVersion: AnsiString read fMIMEVersion write fMIMEVersion;
    { MIME-Version: 1.0 - specifies the version of MIME that the message format complies with RFC 2045: 4 }
    property Priority: AnsiString read fPriority write fPriority;
    { Priority: normal - Priority for message delivery ("normal" / "non-urgent" / "urgent") RFC 2156 }
    property DispositionNotificationTo: AnsiString
      read fDispositionNotificationTo write fDispositionNotificationTo;
    { Disposition-Notification-To: boss@nil.test - Requests for notification when the message is received, and specifies the address for them RFC 2298 }
    property CustomHeaders: TALStrings read FCustomHeaders;
    Property RawHeaderText: AnsiString read GetRawHeaderText
      write SetRawHeaderText;
  end;

  { ----------------------------------- }
  TALNewsArticleHeader = Class(Tobject)
  Private
    FCustomHeaders: TALStrings;
    fExpires: AnsiString;
    fMessageID: AnsiString;
    fReplyTo: AnsiString;
    fOrganization: AnsiString;
    fDateReceived: AnsiString;
    fNNTPPostingHost: AnsiString;
    fContentTransferEncoding: AnsiString;
    fComments: AnsiString;
    fMIMEVersion: AnsiString;
    fSender: AnsiString;
    fNewsgroups: AnsiString;
    fReferences: AnsiString;
    fPostingVersion: AnsiString;
    fRelayVersion: AnsiString;
    fDate: AnsiString;
    fNNTPPostingDate: AnsiString;
    fPath: AnsiString;
    fDistribution: AnsiString;
    fContentType: AnsiString;
    fFollowupTo: AnsiString;
    fSubject: AnsiString;
    fControl: AnsiString;
    fFrom: AnsiString;
    Function GetRawHeaderText: AnsiString;
    procedure SetRawHeaderText(const aRawHeaderText: AnsiString);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property RelayVersion: AnsiString read fRelayVersion write fRelayVersion;
    { Relay-Version: Version 1.7 PSU-NETNEWS 5/20/88; site MAINE.BITNET }
    property PostingVersion: AnsiString read fPostingVersion
      write fPostingVersion;
    { Posting-Version: Version 1.7 PSU-NETNEWS 5/20/88; site MAINE.BITNET }
    property From: AnsiString read fFrom write fFrom;
    { From: John Doe <jdoe@machine.example> - Author(s) or person(s) taking responsibility for the message 4.4.1; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1 }
    property Date: AnsiString read fDate write fDate;
    { Date: Fri, 21 Nov 1997 09:55:06 -0600 - The time when the message was written (or submitted) RFC 822: 5.1; RFC 1123: 5.2.14; RFC 1036: 2.1.2 }
    property Newsgroups: AnsiString read fNewsgroups write fNewsgroups;
    { Newsgroups: net.sport.football }
    property Subject: AnsiString read fSubject write fSubject;
    { Subject: Saying Hello - Text that provides a summary, or indicates the nature, of the message RFC 822: 4.7.1; RFC 1036: 2.1.4 }
    property MessageID: AnsiString read fMessageID write fMessageID;
    { Message-ID: <1234@local.machine.example> -	Unique ID for the message RFC 822: 4.6.1; RFC 1036: 2.1.5 }
    property Path: AnsiString read fPath write fPath;
    { Path: psuvm!cunyvm!maine.bitnet!michael }
    property ReplyTo: AnsiString read fReplyTo write fReplyTo;
    { Reply-To: "Mary Smith: Personal Account" <smith@home.example> - Suggested E-mail address for replies RFC 822: 4.4.3; RFC 1036: 2.2.1 }
    property Sender: AnsiString read fSender write fSender;
    { Sender: Michael Jones <mjones@machine.example> - The person or agent submitting the message to the network, if other than shown by the From header RFC 822: 4.4.2; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1 }
    property FollowupTo: AnsiString read fFollowupTo write fFollowupTo;
    { Followup-To: uk.legal,uk.misc }
    property DateReceived: AnsiString read fDateReceived write fDateReceived;
    { Date-Received: Fri, 21 Nov 1997 09:55:06 -0600 }
    property Expires: AnsiString read fExpires write fExpires;
    { Expires: Fri, 21 Nov 1997 09:55:06 -0600 }
    property References: AnsiString read fReferences write fReferences;
    { References: <1234@local.machine.example> <3456@example.net> - In E-mail: reference to other related messages; in Usenet: reference to replied-to-articles RFC 822: 4.6.3; RFC 1036: 2.2.5 }
    property Control: AnsiString read fControl write fControl;
    { Control: cancel <xb8700A@twits.site.com> }
    property Distribution: AnsiString read fDistribution write fDistribution;
    { Distribution: nj.all }
    property Organization: AnsiString read fOrganization write fOrganization;
    { Organization: A poorly-installed InterNetNews site }
    property Comments: AnsiString read fComments write fComments;
    { Comments: Authenticated sender is gboyd@netcom.com - Text comments added to the message RFC 822: 4.7.2 }
    property ContentType: AnsiString read fContentType write fContentType;
    { Content-Type: text/plain; charset="iso-8859-1" - Data type and format of content RFC 1049 (historic); RFC 1123: 5.2.13; RFC 2045: 5; RFC 1766: 4.1 }
    property ContentTransferEncoding: AnsiString read fContentTransferEncoding
      write fContentTransferEncoding;
    { Content-Transfer-Encoding: 8bit - Coding method used in a MIME message body RFC 2045: 6; }
    property MIMEVersion: AnsiString read fMIMEVersion write fMIMEVersion;
    { MIME-Version: 1.0 - specifies the version of MIME that the message format complies with RFC 2045: 4 }
    property NNTPPostingHost: AnsiString read fNNTPPostingHost
      write fNNTPPostingHost;
    { NNTP-Posting-Host: stc92-3-82-245-250-13.fbx.proxad.net }
    property NNTPPostingDate: AnsiString read fNNTPPostingDate
      write fNNTPPostingDate;
    { NNTP-Posting-Date: Sun, 30 Sep 2007 20:28:56 +0000 (UTC) }
    property CustomHeaders: TALStrings read FCustomHeaders;
    Property RawHeaderText: AnsiString read GetRawHeaderText
      write SetRawHeaderText;
  end;

function AlParseEmailAddress(FriendlyEmail: AnsiString;
  var RealName: AnsiString; Const decodeRealName: Boolean = True): AnsiString;
function AlExtractEmailAddress(const FriendlyEmail: AnsiString): AnsiString;
Function ALMakeFriendlyEmailAddress(const aRealName, aEmail: AnsiString)
  : AnsiString;
Function ALEncodeRealName4FriendlyEmailAddress(const aRealName: AnsiString)
  : AnsiString;
Function AlGenerateInternetMessageID: AnsiString; overload;
Function AlGenerateInternetMessageID(ahostname: AnsiString)
  : AnsiString; overload;
Function ALDecodeQuotedPrintableString(src: AnsiString): AnsiString;
Function AlDecodeInternetMessageHeaderInUTF8(const aHeaderStr: AnsiString;
  aDefaultCodePage: Integer): AnsiString;
function AlIsValidEmail(const Value: AnsiString): Boolean;

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
  system.Sysutils,
{$ELSE}
  Sysutils,
{$IFEND}
  ALHttpClient,
  ALMime,
  ALWinsock,
  ALString;

{ *************************************************************************** }
{ FriendlyEmail                  RealName       Result }
{ ----------------------------   ------------   -------------- }
{ "my name" <name@domain.com>    my name        name@domain.com }
{ myname <name@domain.com>       myname         name@domain.com }
{ name@domain.com                empty          name@domain.com }
{ <name@domain.com>              empty          name@domain.com }
{ }
{ name@domain.com (myname)       myname         name@domain.com }
{ (myname) name@domain.com       myname         name@domain.com }
{ <name@domain.com> "my name"    my name        name@domain.com }
function AlParseEmailAddress(FriendlyEmail: AnsiString;
  var RealName: AnsiString; Const decodeRealName: Boolean = True): AnsiString;

var
  P1, P2, P3, P4, P5: Integer;
  S1: AnsiString;
  ln: Integer;

begin

  { ---------- }
  FriendlyEmail := ALTrim(FriendlyEmail);

  { ---------- }
  Result := '';
  RealName := '';

  { ---------- }
  ln := Length(FriendlyEmail);

  { ---------- }
  if ALMatchesMask(FriendlyEmail, '* <*>') then
    P1 := ln - 1
    // toto <toto@toto.com> | "toto" <toto@toto.com> | (toto) <toto@toto.com>
  else if ALMatchesMask(FriendlyEmail, '<*> *') then
    P1 := 2 // <toto@toto.com> toto.com | <toto@toto.com> "toto" | <toto@toto.com> (toto)
  else if ALMatchesMask(FriendlyEmail, '<*>') then
    P1 := 2 // <toto@toto.com>
  else if ALMatchesMask(FriendlyEmail, '* (*)') then
    P1 := 1 // toto@toto.com (toto)
  else
  Begin
    RealName := '';
    Result := FriendlyEmail;
    Exit;
  end;

  { ---------- }
  Result := FriendlyEmail[P1];

  { ---------- }
  P2 := P1 - 1;
  While (P2 > 0) and (FriendlyEmail[P2] in ['a' .. 'z', 'A' .. 'Z', '0' .. '9',
    '_', '-', '.', '@']) do
  begin
    Result := FriendlyEmail[P2] + Result;
    dec(P2);
  end;
  While (P2 > 0) and (FriendlyEmail[P2] in [' ', #9]) do
    dec(P2);

  P3 := P1 + 1;
  While (P3 <= ln) and (FriendlyEmail[P3] in ['a' .. 'z', 'A' .. 'Z',
    '0' .. '9', '_', '-', '.', '@']) do
  begin
    Result := Result + FriendlyEmail[P3];
    inc(P3);
  end;
  While (P3 <= ln) and (FriendlyEmail[P3] in [' ', #9]) do
    inc(P3);

  { ---------- }
  If (P2 > 0) and (P3 <= ln) then
  begin
    If (((FriendlyEmail[P2] = '<') and (FriendlyEmail[P3] = '>'))) then
    begin
      dec(P2);
      inc(P3);
    end
    else
    begin
      S1 := Result;

      P4 := P2;
      While (P4 > 0) and (FriendlyEmail[P4] <> '<') do
        if (FriendlyEmail[P4] = '>') then
        begin
          P4 := 0;
          Break;
        end
        else
        begin
          S1 := FriendlyEmail[P4] + S1;
          dec(P4);
        end;

      P5 := P3;
      While (P5 <= ln) and (FriendlyEmail[P5] <> '>') do
        if (FriendlyEmail[P5] = '<') then
        begin
          P5 := ln + 1;
          Break;
        end
        else
        begin
          S1 := S1 + FriendlyEmail[P5];
          inc(P5);
        end;

      If (P4 > 0) and (P5 <= ln) then
      begin
        P2 := P4 - 1;
        P3 := P5 + 1;
        Result := S1;
      end;
    end;
  end;

  { ---------- }
  RealName := ALTrim(AlCopyStr(FriendlyEmail, 1, P2) + AlCopyStr(FriendlyEmail,
    P3, Maxint));

  { ---------- }
  If (RealName = '') then
  begin
    If (Result <> '') then
      Exit;
    RealName := ALTrim(FriendlyEmail);
  end;

  { ---------- }
  ln := Length(RealName);
  if (decodeRealName) and (ln >= 2) and (RealName[1] = '"') and
    (RealName[ln] = '"') then
  begin
    RealName := AlCopyStr(RealName, 2, ln - 2);
    RealName := alStringReplace(RealName, '\\', #1,
      [rfIgnoreCase, RfReplaceAll]);
    RealName := alStringReplace(RealName, '\', '',
      [rfIgnoreCase, RfReplaceAll]);
    RealName := alStringReplace(RealName, #1, '\',
      [rfIgnoreCase, RfReplaceAll]);
  end
  else if (ln >= 2) and (((RealName[1] = '(') and (RealName[ln] = ')')) or
    ((RealName[1] = '<') and (RealName[ln] = '>')) or
    ((RealName[1] = '"') and (RealName[ln] = '"'))) then
    RealName := AlCopyStr(RealName, 2, ln - 2)

end;

{ ************************************************************************** }
function AlExtractEmailAddress(const FriendlyEmail: AnsiString): AnsiString;
Var
  aRealName: AnsiString;
Begin
  Result := AlParseEmailAddress(FriendlyEmail, aRealName, False);
end;

{ ************************************************************************** }
{ Return a new string with backslashes in str replaced by two backslashes and
  double quotes replaced by backslash-double quote. }
Function ALEncodeRealName4FriendlyEmailAddress(const aRealName: AnsiString)
  : AnsiString;
var
  i, l, x: Integer;
  Buf, P: PAnsiChar;
  ch: AnsiChar;
begin
  Result := '';
  l := Length(aRealName);
  if l = 0 then
    Exit;
  GetMem(Buf, (l * 2) + 2); // to be on the *very* safe side
  try
    ch := '"';
    P := Buf;
    ALMove(ch, P^, 1);
    inc(P, 1);
    for i := 1 to l do
    begin
      x := Ord(aRealName[i]);
      case x of
        34:
          begin // quot "
            ALStrMove('/"', P, 2);
            inc(P, 2);
          end;
        47:
          begin // backslash /
            ALStrMove('//', P, 2);
            inc(P, 2);
          end;
      else
        Begin
          P^ := AnsiChar(x);
          inc(P);
        end;
      end;
    end;
    ALMove(ch, P^, 1);
    inc(P, 1);
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;

{ ******************************************************************************** }
{ this takes a 2-tuple of the form (realname, email_address) and returns the string
  value suitable for a To: or Cc: header. }
Function ALMakeFriendlyEmailAddress(const aRealName, aEmail: AnsiString)
  : AnsiString;
begin
  if aRealName <> '' then
    Result := ALEncodeRealName4FriendlyEmailAddress(aRealName) + ' <' +
      aEmail + '>'
  else
    Result := aEmail;
end;

{ *********************************************** }
Function AlGenerateInternetMessageID: AnsiString;
Begin
  Result := alStringReplace(ALNewGUIDString(True { WithoutBracket } ), '-', '',
    [RfReplaceAll]) + '@' + ALTrim(AlGetLocalHostName);
end;

{ ********************************************************************** }
Function AlGenerateInternetMessageID(ahostname: AnsiString): AnsiString;
Begin
  ahostname := ALTrim(ahostname);
  If ahostname <> '' then
    Result := alStringReplace(ALNewGUIDString(True { WithoutBracket } ), '-',
      '', [RfReplaceAll]) + '@' + ahostname
  else
    Result := AlGenerateInternetMessageID;
end;

{ ****************************************************************** }
Function ALDecodeQuotedPrintableString(src: AnsiString): AnsiString;
var
  CurrentSrcPos, CurrentResultPos: Integer;
  Entity: AnsiString;
  SrcLength: Integer;
  in1: Integer;

  { -------------------------------------- }
  procedure CopyCurrentSrcPosCharToResult;
  Begin
    Result[CurrentResultPos] := src[CurrentSrcPos];
    inc(CurrentResultPos);
    inc(CurrentSrcPos);
  end;

{ --------------------------------------------------------------------- }
  procedure CopyCharToResult(aChar: AnsiChar; NewCurrentSrcPos: Integer);
  Begin
    Result[CurrentResultPos] := aChar;
    inc(CurrentResultPos);
    CurrentSrcPos := NewCurrentSrcPos;
  end;

begin
  { remove soft line break }
  src := alStringReplace(src, '='#13#10, '', [RfReplaceAll]);

  { init var }
  CurrentSrcPos := 1;
  CurrentResultPos := 1;
  SrcLength := Length(src);
  Entity := '';
  SetLength(Result, SrcLength);

  { start loop }
  while (CurrentSrcPos <= SrcLength) do
  begin

    { Encoded entity detected }
    If src[CurrentSrcPos] = '=' then
    begin

      { Encoded entity is valid in length }
      If (CurrentSrcPos + 2 <= SrcLength) then
      Begin

        Entity := AlCopyStr(src, CurrentSrcPos + 1, 2);

        If ALTryStrToInt(alUpperCase('$' + Entity), in1) and (in1 <= 255) and
          (in1 >= 0) then
          CopyCharToResult(AnsiChar(in1), CurrentSrcPos + 3)
        else
          CopyCurrentSrcPosCharToResult;

      end
      else
        CopyCurrentSrcPosCharToResult;

    end
    else If src[CurrentSrcPos] = '_' then
      CopyCharToResult(' ', CurrentSrcPos + 1)
    else
      CopyCurrentSrcPosCharToResult;

  end;

  SetLength(Result, CurrentResultPos - 1);
end;

{ **************************************************************************************************************** }
Function AlDecodeInternetMessageHeaderInUTF8(const aHeaderStr: AnsiString;
  aDefaultCodePage: Integer): AnsiString;

Var
  P1, P2, P3, P4: Integer;
  i: Integer;
  aCharSet: AnsiString;
  aEncoding: AnsiString;
  aencodedText: AnsiString;
  aCodePage: Integer;
  LstEncodedWord: TALStringList;

Begin
  // encoded-word = "=?" charset "?" encoding "?" encoded-text "?="
  // =?iso-8859-1?q?=20this=20is=20some=20text?=
  Result := '';
  LstEncodedWord := TALStringList.Create;
  Try

    P1 := 1;
    While True do
    begin
      P2 := AlPosEx('=?', aHeaderStr, P1);
      If P2 <= 0 then
        Break;
      P3 := AlPosEx('?', aHeaderStr, P2 + 2);
      if P3 > 0 then
        P3 := AlPosEx('?', aHeaderStr, P3 + 1);
      if P3 > 0 then
        P3 := AlPosEx('?', aHeaderStr, P3 + 1);
      If (P3 <= 0) or (P3 = Length(aHeaderStr)) or (aHeaderStr[P3 + 1] <> '=')
      then
      begin
        LstEncodedWord.Add(AlCopyStr(aHeaderStr, P1, P2 + 2 - P1));
        P1 := P1 + 2;
      end
      else
      begin
        LstEncodedWord.Add(AlCopyStr(aHeaderStr, P1, P2 - P1));
        LstEncodedWord.Add(AlCopyStr(aHeaderStr, P2, P3 + 2 - P2));
        P1 := P3 + 2;
      end;
    end;
    LstEncodedWord.Add(AlCopyStr(aHeaderStr, P1, Maxint));

    For i := 0 to LstEncodedWord.Count - 1 do
    begin
      aencodedText := LstEncodedWord[i];
      aCodePage := aDefaultCodePage;
      LstEncodedWord.Objects[i] := Pointer(0);
      If aencodedText <> '' then
      begin
        P1 := AlPos('=?', aencodedText);
        If P1 = 1 then
        begin
          P2 := AlPosEx('?', aencodedText, P1 + 2);
          If (P2 > 0) then
          Begin
            P3 := AlPosEx('?', aencodedText, P2 + 1);
            If P3 > 0 then
            begin
              P4 := AlPosEx('?=', aencodedText, P3 + 1);
              If P4 > 0 then
              Begin
                aEncoding := alLowerCase(AlCopyStr(aencodedText, P2 + 1,
                  P3 - P2 - 1)); // q
                If (aEncoding = 'q') or (aEncoding = 'b') then
                begin
                  aCharSet := AlCopyStr(aencodedText, P1 + 2, P2 - P1 - 2);
                  // iso-8859-1
                  aCodePage := ALGetCodePageFromCharSetName(aCharSet); // 28591
                  aencodedText := AlCopyStr(aencodedText, P3 + 1, P4 - P3 - 1);
                  // this=20is=20some=20text
                  If (aEncoding = 'b') then
                    aencodedText := ALMimeBase64DecodeString(aencodedText)
                  else
                    aencodedText := ALDecodeQuotedPrintableString(aencodedText);
                  LstEncodedWord.Objects[i] := Pointer(1);
                end;
              end;
            end;
          end;
        end;
        If aCodePage <> 65001 then
          aencodedText := ALUTF8Encode(aencodedText, aCodePage);
        LstEncodedWord[i] := aencodedText;
        If (i >= 2) and (LstEncodedWord.Objects[i] = Pointer(1)) and
          (LstEncodedWord.Objects[i - 2] = Pointer(1)) and
          (ALTrim(LstEncodedWord[i - 1]) = '') then
          LstEncodedWord[i - 1] := '';
      end;
    end;
    For i := 0 to LstEncodedWord.Count - 1 do
      Result := Result + LstEncodedWord[i];

  finally
    LstEncodedWord.Free;
  end;
end;

{ ***************************** }
procedure TALEMailHeader.Clear;
begin
  fSendTo := '';
  fSender := '';
  fMessageID := '';
  fbcc := '';
  fContentTransferEncoding := '';
  fComments := '';
  fMIMEVersion := '';
  fPriority := '';
  fReplyTo := '';
  fSubject := '';
  fFrom := '';
  fDate := '';
  fDispositionNotificationTo := '';
  fReferences := '';
  fcc := '';
  fContentType := '';
  FCustomHeaders.Clear;
end;

{ ******************************** }
constructor TALEMailHeader.Create;
begin
  inherited Create;
  FCustomHeaders := TALStringList.Create;
  FCustomHeaders.NameValueSeparator := ':';
  Clear;
  fMessageID := 'AUTO';
  fMIMEVersion := '1.0';
  fDate := 'NOW';
  fContentType := 'text/plain';
end;

{ ******************************** }
destructor TALEMailHeader.Destroy;
begin
  FCustomHeaders.Free;
  inherited;
end;

{ *************************************************** }
function TALEMailHeader.GetRawHeaderText: AnsiString;
Var
  i: Integer;
  Str: AnsiString;
begin
  Result := '';
  If ALTrim(fFrom) <> '' then
    Result := Result + 'From: ' + ALTrim(fFrom) + #13#10;
  If ALTrim(fSender) <> '' then
    Result := Result + 'Sender: ' + ALTrim(fSender) + #13#10;
  If ALTrim(fSendTo) <> '' then
    Result := Result + 'To: ' + ALTrim(fSendTo) + #13#10;
  If ALTrim(fcc) <> '' then
    Result := Result + 'cc: ' + ALTrim(fcc) + #13#10;
  If ALTrim(fbcc) <> '' then
    Result := Result + 'bcc: ' + ALTrim(fbcc) + #13#10;
  If ALTrim(fReplyTo) <> '' then
    Result := Result + 'Reply-To: ' + ALTrim(fReplyTo) + #13#10;
  If ALTrim(fSubject) <> '' then
    Result := Result + 'Subject: ' + ALTrim(fSubject) + #13#10;
  Str := fMessageID;
  If ALTrim(Str) <> '' then
  begin
    If ALSameText(Str, 'AUTO') then
      Str := '<' + AlGenerateInternetMessageID + '>';
    Result := Result + 'Message-ID: ' + ALTrim(Str) + #13#10;
  end;
  If ALTrim(fReferences) <> '' then
    Result := Result + 'References: ' + ALTrim(fReferences) + #13#10;
  If ALTrim(fComments) <> '' then
    Result := Result + 'Comments: ' + ALTrim(fComments) + #13#10;
  Str := fDate;
  If ALTrim(Str) <> '' then
  begin
    If ALSameText(Str, 'NOW') then
      Str := ALDateTimeToRfc822Str(Now);
    Result := Result + 'Date: ' + ALTrim(Str) + #13#10;
  end;
  If ALTrim(fContentType) <> '' then
    Result := Result + 'Content-Type: ' + ALTrim(fContentType) + #13#10;
  If ALTrim(fContentTransferEncoding) <> '' then
    Result := Result + 'Content-Transfer-Encoding: ' +
      ALTrim(fContentTransferEncoding) + #13#10;
  If ALTrim(fMIMEVersion) <> '' then
    Result := Result + 'MIME-Version: ' + ALTrim(fMIMEVersion) + #13#10;
  If ALTrim(fPriority) <> '' then
    Result := Result + 'Priority: ' + ALTrim(fPriority) + #13#10;
  If ALTrim(fDispositionNotificationTo) <> '' then
    Result := Result + 'Disposition-Notification-To: ' +
      ALTrim(fDispositionNotificationTo) + #13#10;
  For i := 0 to FCustomHeaders.Count - 1 do
    if (ALTrim(FCustomHeaders.names[i]) <> '') and
      (ALTrim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      Result := Result + FCustomHeaders.names[i] + ': ' +
        ALTrim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
end;

{ ************************************************************************** }
procedure TALEMailHeader.SetRawHeaderText(const aRawHeaderText: AnsiString);
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

    fFrom := AlG001('From');
    fSender := AlG001('Sender');
    fSendTo := AlG001('To');
    fcc := AlG001('cc');
    fbcc := AlG001('bcc');
    fReplyTo := AlG001('Reply-To');
    fSubject := AlG001('Subject');
    fMessageID := AlG001('Message-ID');
    fReferences := AlG001('References');
    fComments := AlG001('Comments');
    fDate := AlG001('Date');
    fContentType := AlG001('Content-Type');
    fContentTransferEncoding := AlG001('Content-Transfer-Encoding');
    fMIMEVersion := AlG001('MIME-Version');
    fPriority := AlG001('Priority');
    fDispositionNotificationTo := AlG001('Disposition-Notification-To');

    FCustomHeaders.Clear;
    j := 0;
    while j <= aRawHeaderLst.Count - 1 do
    begin
      Str1 := ALTrim(aRawHeaderLst.names[j]);
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

{ *********************************** }
procedure TALNewsArticleHeader.Clear;
begin
  fExpires := '';
  fMessageID := '';
  fReplyTo := '';
  fOrganization := '';
  fDateReceived := '';
  fNNTPPostingHost := '';
  fContentTransferEncoding := '';
  fComments := '';
  fMIMEVersion := '';
  fSender := '';
  fNewsgroups := '';
  fReferences := '';
  fPostingVersion := '';
  fRelayVersion := '';
  fDate := '';
  fNNTPPostingDate := '';
  fPath := '';
  fDistribution := '';
  fContentType := '';
  fFollowupTo := '';
  fSubject := '';
  fControl := '';
  fFrom := '';
  FCustomHeaders.Clear;
end;

{ ************************************** }
constructor TALNewsArticleHeader.Create;
begin
  inherited Create;
  FCustomHeaders := TALStringList.Create;
  FCustomHeaders.NameValueSeparator := ':';
  Clear;
  fMessageID := 'AUTO';
  fMIMEVersion := '1.0';
  fDate := 'NOW';
  fContentType := 'text/plain';
end;

{ ************************************** }
destructor TALNewsArticleHeader.Destroy;
begin
  FCustomHeaders.Free;
  inherited;
end;

{ ********************************************************* }
function TALNewsArticleHeader.GetRawHeaderText: AnsiString;
Var
  i: Integer;
  Str: AnsiString;
begin
  Result := '';
  If ALTrim(fRelayVersion) <> '' then
    Result := Result + 'Relay-Version: ' + ALTrim(fRelayVersion) + #13#10;
  If ALTrim(fPostingVersion) <> '' then
    Result := Result + 'Posting-Version: ' + ALTrim(fPostingVersion) + #13#10;
  If ALTrim(fFrom) <> '' then
    Result := Result + 'From: ' + ALTrim(fFrom) + #13#10;
  Str := fDate;
  If ALTrim(Str) <> '' then
  begin
    If ALSameText(Str, 'NOW') then
      Str := ALDateTimeToRfc822Str(Now);
    Result := Result + 'Date: ' + ALTrim(Str) + #13#10;
  end;
  If ALTrim(fNewsgroups) <> '' then
    Result := Result + 'Newsgroups: ' + ALTrim(fNewsgroups) + #13#10;
  If ALTrim(fSubject) <> '' then
    Result := Result + 'Subject: ' + ALTrim(fSubject) + #13#10;
  Str := fMessageID;
  If ALTrim(Str) <> '' then
  begin
    If ALSameText(Str, 'AUTO') then
      Str := '<' + AlGenerateInternetMessageID + '>';
    Result := Result + 'Message-ID: ' + ALTrim(Str) + #13#10;
  end;
  If ALTrim(fPath) <> '' then
    Result := Result + 'Path: ' + ALTrim(fPath) + #13#10;
  If ALTrim(fReplyTo) <> '' then
    Result := Result + 'Reply-To: ' + ALTrim(fReplyTo) + #13#10;
  If ALTrim(fSender) <> '' then
    Result := Result + 'Sender: ' + ALTrim(fSender) + #13#10;
  If ALTrim(fFollowupTo) <> '' then
    Result := Result + 'Followup-To: ' + ALTrim(fFollowupTo) + #13#10;
  If ALTrim(fDateReceived) <> '' then
    Result := Result + 'Date-Received: ' + ALTrim(fDateReceived) + #13#10;
  If ALTrim(fExpires) <> '' then
    Result := Result + 'Expires: ' + ALTrim(fExpires) + #13#10;
  If ALTrim(fReferences) <> '' then
    Result := Result + 'References: ' + ALTrim(fReferences) + #13#10;
  If ALTrim(fControl) <> '' then
    Result := Result + 'Control: ' + ALTrim(fControl) + #13#10;
  If ALTrim(fDistribution) <> '' then
    Result := Result + 'Distribution: ' + ALTrim(fDistribution) + #13#10;
  If ALTrim(fOrganization) <> '' then
    Result := Result + 'Organization: ' + ALTrim(fOrganization) + #13#10;
  If ALTrim(fComments) <> '' then
    Result := Result + 'Comments: ' + ALTrim(fComments) + #13#10;
  If ALTrim(fContentType) <> '' then
    Result := Result + 'Content-Type: ' + ALTrim(fContentType) + #13#10;
  If ALTrim(fContentTransferEncoding) <> '' then
    Result := Result + 'Content-Transfer-Encoding: ' +
      ALTrim(fContentTransferEncoding) + #13#10;
  If ALTrim(fMIMEVersion) <> '' then
    Result := Result + 'MIME-Version: ' + ALTrim(fMIMEVersion) + #13#10;
  If ALTrim(fNNTPPostingHost) <> '' then
    Result := Result + 'NNTP-Posting-Host: ' + ALTrim(fNNTPPostingHost)
      + #13#10;
  If ALTrim(fNNTPPostingDate) <> '' then
    Result := Result + 'NNTP-Posting-Date: ' + ALTrim(fNNTPPostingDate)
      + #13#10;
  For i := 0 to FCustomHeaders.Count - 1 do
    if (ALTrim(FCustomHeaders.names[i]) <> '') and
      (ALTrim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      Result := Result + FCustomHeaders.names[i] + ': ' +
        ALTrim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
end;

{ ******************************************************************************** }
procedure TALNewsArticleHeader.SetRawHeaderText(const aRawHeaderText
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

    fRelayVersion := AlG001('Relay-Version');
    fPostingVersion := AlG001('Posting-Version');
    fFrom := AlG001('From');
    fDate := AlG001('Date');
    fNewsgroups := AlG001('Newsgroups');
    fSubject := AlG001('Subject');
    fMessageID := AlG001('Message-ID');
    fPath := AlG001('Path');
    fReplyTo := AlG001('Reply-To');
    fSender := AlG001('Sender');
    fFollowupTo := AlG001('Followup-To');
    fDateReceived := AlG001('Date-Received');
    fExpires := AlG001('Expires');
    fReferences := AlG001('References');
    fControl := AlG001('Control');
    fDistribution := AlG001('Distribution');
    fOrganization := AlG001('Organization');
    fComments := AlG001('Comments');
    fContentType := AlG001('Content-Type');
    fContentTransferEncoding := AlG001('Content-Transfer-Encoding');
    fMIMEVersion := AlG001('MIME-Version');
    fNNTPPostingHost := AlG001('NNTP-Posting-Host');
    fNNTPPostingDate := AlG001('NNTP-Posting-Date');

    FCustomHeaders.Clear;
    j := 0;
    while j <= aRawHeaderLst.Count - 1 do
    begin
      Str1 := ALTrim(aRawHeaderLst.names[j]);
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

{ ******************************************************** }
function AlIsValidEmail(const Value: AnsiString): Boolean;

{ ------------------------------------------------------ }
  function CheckAllowedName(const s: AnsiString): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to Length(s) do
    begin
      // illegal char in s -> no valid address
      if not(s[i] in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_', '-', '.', '+'])
      then
        Exit;
    end;
    Result := True;
  end;

{ ---------------------------------------------------------- }
  function CheckAllowedHostname(const s: AnsiString): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to Length(s) do
    begin
      // illegal char in s -> no valid address
      if not(s[i] in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', '.']) then
        Exit;
    end;
    Result := True;
  end;

{ ----------------------------------------------------- }
  function CheckAllowedExt(const s: AnsiString): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to Length(s) do
    begin
      // illegal char in s -> no valid address
      if not(s[i] in ['a' .. 'z', 'A' .. 'Z']) then
        Exit;
    end;
    Result := True;
  end;

var
  i, j: Integer;
  namePart, serverPart, extPart: AnsiString;
begin
  Result := False;

  // Value can not be < 6 char (ex: a@b.fr)
  if Length(Value) < 6 then
    Exit;

  // must have the '@' char inside
  i := AlPos('@', Value);
  if (i <= 1) or (i > Length(Value) - 4) then
    Exit;

  // can not have @. or .@
  if (Value[i - 1] = '.') or (Value[i + 1] = '.') then
    Exit;

  // can not have 2 ..
  If (AlPos('..', Value) > 0) then
    Exit;

  // extract namePart and serverPart
  namePart := AlCopyStr(Value, 1, i - 1);
  serverPart := AlCopyStr(Value, i + 1, Length(Value));

  // Extension (.fr, .com, etc..) must be betwen 2 to 6 char
  i := AlPos('.', serverPart);
  j := 0;
  While i > 0 do
  begin
    j := i;
    i := AlPosEx('.', serverPart, i + 1);
  end;
  if (j <= 1) then
    Exit; // no dot at all so exit !
  extPart := AlCopyStr(serverPart, j + 1, Maxint);
  serverPart := AlCopyStr(serverPart, 1, j - 1);
  If not(Length(extPart) in [2 .. 6]) then
    Exit;

  Result := CheckAllowedName(namePart) and CheckAllowedHostname(serverPart) and
    CheckAllowedExt(extPart);
end;

end.
