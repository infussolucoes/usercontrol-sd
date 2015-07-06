{ *************************************************************
  www:          http://sourceforge.net/projects/alcinoe/
  svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
  Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
  Sponsor(s):   Arkadia SA (http://www.arkadia.com)

  product:      ALCalendar
  Version:      4.00

  Description:  Functions to draw a calendar on a canvas

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

  History :     31/03/2007: rename the function in AL*
  12/01/2008: renove call to Controls.pas
  28/05/2008: Update it in WideString/Utf8
  26/06/2012: Add xe2 support

  Link :

  * Please send all your feedback to alcinoe@arkadia.com
  * If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
  * Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
  ************************************************************** }
unit ALCalendar;

interface

{$IF CompilerVersion >= 25} { Delphi XE4 }
{$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
  System.types,
  Vcl.Graphics,
{$ELSE}
  types,
  Graphics,
{$IFEND}
  ALString;

Type

  { --------------------------- }
  TALDrawCalendarStyle = Record
    FontColor: TColor;
    FontHeight: Integer;
    FontName: TFontName;
    FontStyle: TFontStyles;
    BackGroundColor: TColor;
    PaddingLeft: Integer;
    PaddingRight: Integer;
    PaddingTop: Integer;
    PaddingBottom: Integer;
  end;

  { ------------------------- }
  TALDrawCalendarDay = Record
    Rect: Trect;
    Style: TALDrawCalendarStyle;
  end;

  TALDrawCalendarDays = array of TALDrawCalendarDay;

  { ------------------------------------- }
Function ALDrawCalendar(Canvas: Tcanvas;
  // ----
  MonthHeaderStyle, DayHeaderStyle, DayDisabledStyle: TALDrawCalendarStyle;
  // ----
  BackGroundColor,
  // ----
  MonthsHeaderBorderColorBottom, MonthsHeaderBorderColorLo,
  MonthsHeaderBorderColorHi, DaysHeaderBorderColorBottom: TColor;
  // ----
  Year: Integer; FormatSettings: TALFormatSettings;
  // LongMonthNames and LongDayNames must be in UTF8
  // ----
  Var CalendarWidth: Integer; Var CalendarHeight: Integer;
  // ----
  Var Days: TALDrawCalendarDays;
  // ----
  Const DayHeaderOn2Char: Boolean = False;
  // ----
  Const MonthBoxPaddingLeftAndRight: Integer = 7;
  Const MonthBoxPaddingTop: Integer = 5): Boolean;

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
  Winapi.windows,
  System.SysUtils,
  System.dateutils;
{$ELSE}
  windows,
  SysUtils,
  dateutils;
{$IFEND}

{ *********************************************************************************** }
Function ALDrawCalendarGetTextSizeW(aCanvas: Tcanvas;
  const Text: WideString): TSize;
begin
  Result.cX := 0;
  Result.cY := 0;
  GetTextExtentPoint32w(aCanvas.Handle, PWideChar(Text), Length(Text), Result);
end;

{ **************************************************************************************** }
procedure ALDrawCalendarTextOutW(aCanvas: Tcanvas; X, Y: Integer;
  const Text: WideString);
begin
  TextOutW(aCanvas.Handle, X, Y, PWideChar(Text), Length(Text));
end;

{ ************************************************************************************************** }
Procedure ALDrawCalendarBox(Text: WideString; BackColor, FontColor: TColor;
  Rect: Trect; C: Tcanvas);
Var
  Wt, Ht, xt, yt: Integer;
  R: Trect;
  aSize: TSize;
Begin
  R := Rect;
  inflateRect(R, -1, -1);
  R.Bottom := R.Bottom + 1;
  R.Right := R.Right + 1;
  C.Brush.Color := BackColor;
  C.FillRect(R);

  C.Font.Color := FontColor;
  aSize := ALDrawCalendarGetTextSizeW(C, Text);
  Wt := aSize.cX;
  Ht := aSize.cY;
  xt := Rect.left + ((Rect.Right - Rect.left - Wt) div 2);
  yt := Rect.top + ((Rect.Bottom - Rect.top - Ht) div 2);
  ALDrawCalendarTextOutW(C, xt, yt, Text);
end;

{ ******************************************************************** }
Procedure ALDrawCalendarHeader(Month1, Month2, Month3, Year: WideString;
  R: Trect; C: Tcanvas; W1Calendar, HHeader1: Integer;
  HeaderColor, BorderColorBottom, BorderColorLo, BorderColorHi: TColor);
Var
  xt, yt, Wt, Ht: Integer;
Begin

  { --init canvas-------------- }
  C.Brush.Color := HeaderColor;
  C.FillRect(R);

  { --Bottom border---------------- }
  C.Pen.Color := BorderColorBottom;
  C.MoveTo(R.left, R.Bottom);
  C.LineTo(R.Right, R.Bottom);

  { --Separator1--------------------------- }
  C.MoveTo(R.left + W1Calendar, R.top + 1);
  C.LineTo(R.left + W1Calendar, R.top + HHeader1 - 1);
  C.Pen.Color := BorderColorHi;
  C.MoveTo(R.left + W1Calendar + 1, R.top + 1);
  C.LineTo(R.left + W1Calendar + 1, R.top + HHeader1 - 1);

  { --Separator2--------------- }
  C.Pen.Color := BorderColorLo;
  C.MoveTo(R.left + 2 * W1Calendar + 2, R.top + 1);
  C.LineTo(R.left + 2 * W1Calendar + 2, R.top + HHeader1 - 1);
  C.Pen.Color := BorderColorHi;
  C.MoveTo(R.left + 2 * W1Calendar + 3, R.top + 1);
  C.LineTo(R.left + 2 * W1Calendar + 3, R.top + HHeader1 - 1);

  { --Month1---------------------------------- }
  Ht := ALDrawCalendarGetTextSizeW(C, '^_').cY;
  Wt := ALDrawCalendarGetTextSizeW(C, Month1 + ' ' + Year).cX;
  xt := R.left + ((W1Calendar - Wt) div 2);
  yt := R.top + ((HHeader1 - Ht) div 2);
  ALDrawCalendarTextOutW(C, xt, yt, Month1 + ' ' + Year);

  { --Month2-------------------------------------------------- }
  Wt := ALDrawCalendarGetTextSizeW(C, Month2 + ' ' + Year).cX;
  xt := R.left + W1Calendar + 2 + ((W1Calendar - Wt) div 2);
  ALDrawCalendarTextOutW(C, xt, yt, Month2 + ' ' + Year);

  { --Month3-------------------------------------------------- }
  Wt := ALDrawCalendarGetTextSizeW(C, Month3 + ' ' + Year).cX;
  xt := R.left + 2 * (W1Calendar + 2) + ((W1Calendar - Wt) div 2);
  ALDrawCalendarTextOutW(C, xt, yt, Month3 + ' ' + Year);

end;

{ ******************************************************************************** }
Procedure ALDrawCalendarHeaderDay(Day1, Day2, Day3, Day4, Day5, Day6,
  Day7: WideString; Wblank, Yheader, wbox, hbox: Integer; C: Tcanvas;
  BackColor, FontColor, BorderColorLo: TColor);
Var
  R: Trect;
Begin
  R := Rect(Wblank, Yheader, Wblank + wbox, Yheader + hbox);
  ALDrawCalendarBox(Day1, BackColor, FontColor, R, C);
  R := Rect(R.Right, Yheader, R.Right + wbox, Yheader + hbox);
  ALDrawCalendarBox(Day2, BackColor, FontColor, R, C);
  R := Rect(R.Right, Yheader, R.Right + wbox, Yheader + hbox);
  ALDrawCalendarBox(Day3, BackColor, FontColor, R, C);
  R := Rect(R.Right, Yheader, R.Right + wbox, Yheader + hbox);
  ALDrawCalendarBox(Day4, BackColor, FontColor, R, C);
  R := Rect(R.Right, Yheader, R.Right + wbox, Yheader + hbox);
  ALDrawCalendarBox(Day5, BackColor, FontColor, R, C);
  R := Rect(R.Right, Yheader, R.Right + wbox, Yheader + hbox);
  ALDrawCalendarBox(Day6, BackColor, FontColor, R, C);
  R := Rect(R.Right, Yheader, R.Right + wbox, Yheader + hbox);
  ALDrawCalendarBox(Day7, BackColor, FontColor, R, C);

  C.Pen.Color := BorderColorLo;
  C.MoveTo(Wblank, Yheader + hbox - 1);
  C.LineTo(Wblank + 7 * wbox, Yheader + hbox - 1);
end;

{ ****************************************************************************************************************** }
Procedure ALCalculateDayBoxRect(Year, Wblank, HeaderHeight, wbox, hbox, wborder,
  W1Calendar, HMonthPadding: Integer; Var Days: TALDrawCalendarDays;
  Var YHeader2, YHeader3, YHeader4, Bottom: Integer);
Var
  Dt: Tdatetime;
  xSamedi, xDimanche, xLundi, xMardi, xMercredi, xJeudi, xVendredi: Integer;
  Month: Integer;
  i: Integer;
  YDay: Integer;

  { -------------------------------------- }
  Procedure UpdateXday(_Wblank: Integer);
  Begin
    xSamedi := _Wblank;
    xDimanche := _Wblank + wbox;
    xLundi := _Wblank + wbox * 2;
    xMardi := _Wblank + wbox * 3;
    xMercredi := _Wblank + wbox * 4;
    xJeudi := _Wblank + wbox * 5;
    xVendredi := _Wblank + wbox * 6;
  end;

Begin
  setlength(Days, daysInAyear(Year));
  Dt := encodeDate(Year, 1, 1);
  YDay := HeaderHeight;
  UpdateXday(Wblank);
  YHeader2 := 0;
  YHeader3 := 0;
  YHeader4 := 0;
  Month := 1;

  For i := low(Days) to high(Days) do
  begin

    case DayOfTheWeek(Dt) of
      DayMonday:
        Days[i].Rect := Rect(xLundi, YDay, xLundi + wbox, YDay + hbox);
      DayTuesday:
        Days[i].Rect := Rect(xMardi, YDay, xMardi + wbox, YDay + hbox);
      DayWednesday:
        Days[i].Rect := Rect(xMercredi, YDay, xMercredi + wbox, YDay + hbox);
      DayThursday:
        Days[i].Rect := Rect(xJeudi, YDay, xJeudi + wbox, YDay + hbox);
      DayFriday:
        begin
          Days[i].Rect := Rect(xVendredi, YDay, xVendredi + wbox, YDay + hbox);
          inc(YDay, hbox);
        end;
      DaySaturday:
        Days[i].Rect := Rect(xSamedi, YDay, xSamedi + wbox, YDay + hbox);
      DaySunday:
        Days[i].Rect := Rect(xDimanche, YDay, xDimanche + wbox, YDay + hbox);
    end;
    Dt := Dt + 1;

    If Month <> MonthOF(Dt) then
    begin
      Month := MonthOF(Dt);
      Case Month of
        2:
          begin
            UpdateXday(Wblank + wborder + W1Calendar);
            YHeader2 := Days[i].Rect.Bottom;
            YDay := HeaderHeight;
          end;
        3:
          begin
            UpdateXday(Wblank + 2 * (wborder + W1Calendar));
            if Days[i].Rect.Bottom > YHeader2 then
              YHeader2 := Days[i].Rect.Bottom;
            YDay := HeaderHeight;
          end;
        4:
          begin
            UpdateXday(Wblank);
            if Days[i].Rect.Bottom > YHeader2 then
              YHeader2 := Days[i].Rect.Bottom;
            YHeader2 := YHeader2 + HMonthPadding;
            YDay := YHeader2 + HeaderHeight;
          end;
        5:
          begin
            UpdateXday(Wblank + wborder + W1Calendar);
            YHeader3 := Days[i].Rect.Bottom;
            YDay := YHeader2 + HeaderHeight;
          end;
        6:
          begin
            UpdateXday(Wblank + 2 * (wborder + W1Calendar));
            if Days[i].Rect.Bottom > YHeader3 then
              YHeader3 := Days[i].Rect.Bottom;
            YDay := YHeader2 + HeaderHeight;
          end;
        7:
          begin
            UpdateXday(Wblank);
            If Days[i].Rect.Bottom > YHeader3 then
              YHeader3 := Days[i].Rect.Bottom;
            YHeader3 := YHeader3 + HMonthPadding;
            YDay := YHeader3 + HeaderHeight;
          end;
        8:
          begin
            UpdateXday(Wblank + wborder + W1Calendar);
            YHeader4 := Days[i].Rect.Bottom;
            YDay := YHeader3 + HeaderHeight;
          end;
        9:
          begin
            UpdateXday(Wblank + 2 * (wborder + W1Calendar));
            if Days[i].Rect.Bottom > YHeader4 then
              YHeader4 := Days[i].Rect.Bottom;
            YDay := YHeader3 + HeaderHeight;
          end;
        10:
          begin
            UpdateXday(Wblank);
            If Days[i].Rect.Bottom > YHeader4 then
              YHeader4 := Days[i].Rect.Bottom;
            YHeader4 := YHeader4 + HMonthPadding;
            YDay := YHeader4 + HeaderHeight;
          end;
        11:
          begin
            UpdateXday(Wblank + wborder + W1Calendar);
            YDay := YHeader4 + HeaderHeight;
            Bottom := Days[i].Rect.Bottom;
          end;
        12:
          begin
            UpdateXday(Wblank + 2 * (wborder + W1Calendar));
            YDay := YHeader4 + HeaderHeight;
            if Days[i].Rect.Bottom > Bottom then
              Bottom := Days[i].Rect.Bottom;
          end;
      end;
    end;

  end;

  if Days[high(Days)].Rect.Bottom > Bottom then
    Bottom := Days[high(Days)].Rect.Bottom;
end;

{ ************************************* }
Function ALDrawCalendar(Canvas: Tcanvas;
  // ----
  MonthHeaderStyle, DayHeaderStyle, DayDisabledStyle: TALDrawCalendarStyle;
  // ----
  BackGroundColor,
  // ----
  MonthsHeaderBorderColorBottom, MonthsHeaderBorderColorLo,
  MonthsHeaderBorderColorHi, DaysHeaderBorderColorBottom: TColor;
  // ----
  Year: Integer; FormatSettings: TALFormatSettings;
  // LongMonthNames and LongDayNames must be in UTF8
  // ----
  Var CalendarWidth: Integer; Var CalendarHeight: Integer;
  // ----
  Var Days: TALDrawCalendarDays;
  // ----
  Const DayHeaderOn2Char: Boolean = False;
  // ----
  Const MonthBoxPaddingLeftAndRight: Integer = 7;
  Const MonthBoxPaddingTop: Integer = 5): Boolean;

Var
  hbox, wbox: Integer;
  Day, GrayDay: Tdatetime;
  Wblank: Integer;
  YHeader2, YHeader3, YHeader4: Integer;
  HHeader1, Hheader2: Integer;
  W1Calendar: Integer;
  CalcHCalendar: Integer;
  WideLongMonthNames: array [1 .. 12] of WideString;
  WideLongDayNames: array [1 .. 7] of WideString;
  R: Trect;
  i: Integer;

begin

  // init WideLongMonthNames and WideLongDayNames
  Result := True;
  with FormatSettings do
  begin
    For i := 1 to 12 do
    begin
{$IFDEF UNICODE}
      WideLongMonthNames[i] :=
        WideLowerCase(Utf8ToWideString(LongMonthNames[i]));
{$ELSE}
      WideLongMonthNames[i] := WideLowerCase(Utf8Decode(LongMonthNames[i]));
{$ENDIF}
      if WideLongMonthNames[i] <> '' then
        WideLongMonthNames[i][1] := WideUpperCase(WideLongMonthNames[i][1])[1];
    end;
    For i := 1 to 7 do
    begin
{$IFDEF UNICODE}
      WideLongDayNames[i] := Utf8ToWideString(LongDayNames[i]);
{$ELSE}
      WideLongDayNames[i] := Utf8Decode(LongDayNames[i]);
{$ENDIF}
      if DayHeaderOn2Char then
      begin
        if (Length(WideLongDayNames[i]) >= 2) then
          WideLongDayNames[i] := WideUpperCase(WideLongDayNames[i][1]) +
            WideLowerCase(WideLongDayNames[i][2])
        else if (WideLongDayNames[i] <> '') then
          WideLongDayNames[i] := WideUpperCase(WideLongDayNames[i][1]);
      end
      else
      begin
        if (WideLongDayNames[i] <> '') then
          WideLongDayNames[i] := WideUpperCase(WideLongDayNames[i][1]);
      end;
    end;
  end;

  // Calc the width and the height
  Canvas.Font.Color := DayHeaderStyle.FontColor;
  Canvas.Font.Name := DayHeaderStyle.FontName;
  Canvas.Font.Height := DayHeaderStyle.FontHeight;
  Canvas.Font.Style := DayHeaderStyle.FontStyle;
  hbox := ALDrawCalendarGetTextSizeW(Canvas, '^_').cY +
    DayHeaderStyle.PaddingBottom + DayHeaderStyle.PaddingTop;
  wbox := ALDrawCalendarGetTextSizeW(Canvas, '__').cX +
    DayHeaderStyle.PaddingLeft + DayHeaderStyle.PaddingRight;
  Canvas.Font.Color := MonthHeaderStyle.FontColor;
  Canvas.Font.Name := MonthHeaderStyle.FontName;
  Canvas.Font.Height := MonthHeaderStyle.FontHeight;
  Canvas.Font.Style := MonthHeaderStyle.FontStyle;
  HHeader1 := ALDrawCalendarGetTextSizeW(Canvas, '^_').cY +
    MonthHeaderStyle.PaddingBottom + MonthHeaderStyle.PaddingTop;
  Hheader2 := hbox;
  W1Calendar := (CalendarWidth - 4) div 3;
  // the number 4 is the width of the header1 separator (their is 2 separators of 2 pixels each)
  Wblank := ((W1Calendar - (7 * wbox)) div 2);
  if Wblank <> MonthBoxPaddingLeftAndRight then
  begin
    Result := False;
    CalendarWidth := 3 * 7 * wbox + MonthBoxPaddingLeftAndRight * 6 + 4;
  end;
  ALCalculateDayBoxRect(Year, Wblank, HHeader1 + Hheader2 + 1, wbox, hbox, 2,
    W1Calendar, MonthBoxPaddingTop, Days, YHeader2, YHeader3, YHeader4,
    CalcHCalendar);
  if CalcHCalendar <> CalendarHeight then
  begin
    Result := False;
    CalendarHeight := CalcHCalendar;
  end;
  if not Result then
    exit;

  // draw the background
  Canvas.Brush.Color := BackGroundColor;
  Canvas.FillRect(Rect(0, 0, CalendarWidth, CalendarHeight));

  // Draw Month titles
  Canvas.Font.Color := MonthHeaderStyle.FontColor;
  Canvas.Font.Name := MonthHeaderStyle.FontName;
  Canvas.Font.Height := MonthHeaderStyle.FontHeight;
  Canvas.Font.Style := MonthHeaderStyle.FontStyle;
  R := Rect(0, 0, CalendarWidth, HHeader1);
  ALDrawCalendarHeader(WideLongMonthNames[1], WideLongMonthNames[2],
    WideLongMonthNames[3], IntToStr(Year), R, Canvas, W1Calendar, HHeader1,
    MonthHeaderStyle.BackGroundColor, MonthsHeaderBorderColorBottom,
    MonthsHeaderBorderColorLo, MonthsHeaderBorderColorHi);
  R := Rect(0, YHeader2, CalendarWidth, YHeader2 + HHeader1);
  ALDrawCalendarHeader(WideLongMonthNames[4], WideLongMonthNames[5],
    WideLongMonthNames[6], IntToStr(Year), R, Canvas, W1Calendar, HHeader1,
    MonthHeaderStyle.BackGroundColor, MonthsHeaderBorderColorBottom,
    MonthsHeaderBorderColorLo, MonthsHeaderBorderColorHi);
  R := Rect(0, YHeader3, CalendarWidth, YHeader3 + HHeader1);
  ALDrawCalendarHeader(WideLongMonthNames[7], WideLongMonthNames[8],
    WideLongMonthNames[9], IntToStr(Year), R, Canvas, W1Calendar, HHeader1,
    MonthHeaderStyle.BackGroundColor, MonthsHeaderBorderColorBottom,
    MonthsHeaderBorderColorLo, MonthsHeaderBorderColorHi);
  R := Rect(0, YHeader4, CalendarWidth, YHeader4 + HHeader1);
  ALDrawCalendarHeader(WideLongMonthNames[10], WideLongMonthNames[11],
    WideLongMonthNames[12], IntToStr(Year), R, Canvas, W1Calendar, HHeader1,
    MonthHeaderStyle.BackGroundColor, MonthsHeaderBorderColorBottom,
    MonthsHeaderBorderColorLo, MonthsHeaderBorderColorHi);

  // Draw Day titles
  Canvas.Font.Color := DayHeaderStyle.FontColor;
  Canvas.Font.Name := DayHeaderStyle.FontName;
  Canvas.Font.Height := DayHeaderStyle.FontHeight;
  Canvas.Font.Style := DayHeaderStyle.FontStyle;
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank, HHeader1, wbox, hbox,
    Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color,
    DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank + W1Calendar + 2, HHeader1,
    wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color,
    DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank + 2 * (W1Calendar + 2),
    HHeader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor,
    Canvas.Font.Color, DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank, YHeader2 + HHeader1, wbox,
    hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color,
    DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank + W1Calendar + 2,
    YHeader2 + HHeader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor,
    Canvas.Font.Color, DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank + 2 * (W1Calendar + 2),
    YHeader2 + HHeader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor,
    Canvas.Font.Color, DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank, YHeader3 + HHeader1, wbox,
    hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color,
    DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank + W1Calendar + 2,
    YHeader3 + HHeader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor,
    Canvas.Font.Color, DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank + 2 * (W1Calendar + 2),
    YHeader3 + HHeader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor,
    Canvas.Font.Color, DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank, YHeader4 + HHeader1, wbox,
    hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color,
    DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank + W1Calendar + 2,
    YHeader4 + HHeader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor,
    Canvas.Font.Color, DaysHeaderBorderColorBottom);
  ALDrawCalendarHeaderDay(WideLongDayNames[7], WideLongDayNames[1],
    WideLongDayNames[2], WideLongDayNames[3], WideLongDayNames[4],
    WideLongDayNames[5], WideLongDayNames[6], Wblank + 2 * (W1Calendar + 2),
    YHeader4 + HHeader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor,
    Canvas.Font.Color, DaysHeaderBorderColorBottom);

  // Draw Days
  Day := encodeDate(Year, 01, 01);
  For i := low(Days) to high(Days) do
  begin
    if DayOfTheMonth(Day) = 1 then
    begin
      GrayDay := Day - 1;
      R := Days[i].Rect;
      R.left := R.left - wbox;
      R.Right := R.Right - wbox;
      Canvas.Font.Color := DayDisabledStyle.FontColor;
      Canvas.Font.Name := DayDisabledStyle.FontName;
      Canvas.Font.Height := DayDisabledStyle.FontHeight;
      Canvas.Font.Style := DayDisabledStyle.FontStyle;
      while DayOfTheWeek(GrayDay) <> DayFriday do
      begin
        ALDrawCalendarBox(IntToStr(DayOfTheMonth(GrayDay)),
          DayDisabledStyle.BackGroundColor, DayDisabledStyle.FontColor,
          R, Canvas);
        R.left := R.left - wbox;
        R.Right := R.Right - wbox;
        GrayDay := GrayDay - 1;
      end;
    end;
    Canvas.Font.Color := Days[i].Style.FontColor;
    Canvas.Font.Name := Days[i].Style.FontName;
    Canvas.Font.Height := Days[i].Style.FontHeight;
    Canvas.Font.Style := Days[i].Style.FontStyle;
    ALDrawCalendarBox(IntToStr(DayOfTheMonth(Day)),
      Days[i].Style.BackGroundColor, Days[i].Style.FontColor,
      Days[i].Rect, Canvas);
    Day := Day + 1;
    if DayOfTheMonth(Day) = 1 then
    begin
      GrayDay := Day;
      R := Days[i].Rect;
      R.left := R.left + wbox;
      R.Right := R.Right + wbox;
      Canvas.Font.Color := DayDisabledStyle.FontColor;
      Canvas.Font.Name := DayDisabledStyle.FontName;
      Canvas.Font.Height := DayDisabledStyle.FontHeight;
      Canvas.Font.Style := DayDisabledStyle.FontStyle;
      while DayOfTheWeek(GrayDay) <> DaySaturday do
      begin
        ALDrawCalendarBox(IntToStr(DayOfTheMonth(GrayDay)),
          DayDisabledStyle.BackGroundColor, DayDisabledStyle.FontColor,
          R, Canvas);
        R.left := R.left + wbox;
        R.Right := R.Right + wbox;
        GrayDay := GrayDay + 1;
      end;
    end;
  end;

end;

end.
