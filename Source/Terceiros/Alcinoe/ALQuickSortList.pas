{ *************************************************************
  www:          http://sourceforge.net/projects/alcinoe/
  svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
  Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
  Sponsor(s):   Arkadia SA (http://www.arkadia.com)

  product:      ALQuickSortList
  Version:      4.00

  Description:  TALIntegerList or TALDoubleList that work exactly
  like TstringList but with integer or Double.

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

  History :     16/06/2012: Add xe2 Support

  Link :

  * Please send all your feedback to alcinoe@arkadia.com
  * If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
  * Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
  ************************************************************** }
unit ALQuickSortList;

{ Exemple of a QuickSort Algorithm :

  procedure _SampleQuickSort(aArray: TArrayOfxxx; L, R: Integer);

  Function _Compare(I1,I2: xxx): Integer;
  Begin
  if I1 < I2 then result := -1
  else if I1 > I2 then result := 1
  else result := 0;
  end;

  var aItem: xxx;
  I, J, P: Integer;

  begin
  repeat
  I := L;
  J := R;
  P := (L + R) shr 1;
  repeat
  while _Compare(aArray[I], aArray[P]) < 0 do Inc(I);
  while _Compare(aArray[J], aArray[P]) > 0 do Dec(J);
  if I <= J then
  begin
  if I <> J then begin
  aItem := aArray[i];
  aArray[i] := aArray[j];
  aArray[j] := aItem;
  end;
  if P = I then
  P := J
  else if P = J then
  P := I;
  Inc(I);
  Dec(J);
  end;
  until I > J;
  if L < J then _SampleQuickSort(L, J);
  L := I;
  until I >= R;
  end;

}

interface

{$IF CompilerVersion >= 25} { Delphi XE4 }
{$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
  System.Classes,
{$ELSE}
  Classes,
{$IFEND}
  ALAVLBinaryTree;

{$IF CompilerVersion<=18.5}

// http://stackoverflow.com/questions/7630781/delphi-2007-and-xe2-using-nativeint
type
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$IFEND}

Type

  { ---------------------------------------------------------------------------------- }
  TALQuickSortListCompare = function(List: TObject;
    Index1, Index2: Integer): Integer;

  { ----------------------------------------- }
  TALQuickSortPointerList = array of Pointer;

  { ----------------------------------- }
  TALBaseQuickSortList = class(TObject)
  private
    FList: TALQuickSortPointerList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    procedure SetSorted(Value: Boolean);
    procedure QuickSort(L, R: Integer; SCompare: TALQuickSortListCompare);
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function CompareItems(const Index1, Index2: Integer): Integer; virtual;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure InsertItem(Index: Integer; Item: Pointer);
    procedure Insert(Index: Integer; Item: Pointer);
    property List: TALQuickSortPointerList read FList;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: NativeInt);
      overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TALBaseQuickSortList;
    procedure CustomSort(Compare: TALQuickSortListCompare); virtual;
    procedure Sort; virtual;
    property Sorted: Boolean read FSorted write SetSorted;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  { --------------------------------------- }
  PALIntegerListItem = ^TALIntegerListItem;

  TALIntegerListItem = record
    FInteger: Integer;
    FObject: TObject;
  end;

  { ------------------------------------------ }
  TALIntegerList = class(TALBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; const Item: Integer);
    function GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const Item: Integer; AObject: TObject);
    function CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function IndexOf(Item: Integer): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    function Add(const Item: Integer): Integer;
    Function AddObject(const Item: Integer; AObject: TObject): Integer;
    function Find(Item: Integer; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const Item: Integer);
    procedure InsertObject(Index: Integer; const Item: Integer;
      AObject: TObject);
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

  { ----------------------------------------- }
  PALCardinalListItem = ^TALCardinalListItem;

  TALCardinalListItem = record
    FCardinal: Cardinal;
    FObject: TObject;
  end;

  { ------------------------------------------- }
  TALCardinalList = class(TALBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function GetItem(Index: Integer): Cardinal;
    procedure SetItem(Index: Integer; const Item: Cardinal);
    function GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const Item: Cardinal;
      AObject: TObject);
    function CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function IndexOf(Item: Cardinal): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    function Add(const Item: Cardinal): Integer;
    Function AddObject(const Item: Cardinal; AObject: TObject): Integer;
    function Find(Item: Cardinal; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const Item: Cardinal);
    procedure InsertObject(Index: Integer; const Item: Cardinal;
      AObject: TObject);
    property Items[Index: Integer]: Cardinal read GetItem
      write SetItem; default;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

  { ----------------------------------- }
  PALInt64ListItem = ^TALInt64ListItem;

  TALInt64ListItem = record
    FInt64: Int64;
    FObject: TObject;
  end;

  { ---------------------------------------- }
  TALInt64List = class(TALBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function GetItem(Index: Integer): Int64;
    procedure SetItem(Index: Integer; const Item: Int64);
    function GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const Item: Int64; AObject: TObject);
    function CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function IndexOf(Item: Int64): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    function Add(const Item: Int64): Integer;
    Function AddObject(const Item: Int64; AObject: TObject): Integer;
    function Find(Item: Int64; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const Item: Int64);
    procedure InsertObject(Index: Integer; const Item: Int64; AObject: TObject);
    property Items[Index: Integer]: Int64 read GetItem write SetItem; default;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

  { ------------------------------------- }
  PALDoubleListItem = ^TALDoubleListItem;

  TALDoubleListItem = record
    FDouble: Double;
    FObject: TObject;
  end;

  { ------------------------------------------ }
  TALDoubleList = class(TALBaseQuickSortList)
  private
    FOwnsObject: Boolean;
    function GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Item: Double);
    function GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure InsertItem(Index: Integer; const Item: Double; AObject: TObject);
    function CompareItems(const Index1, Index2: Integer): Integer; override;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    function IndexOf(Item: Double): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    function Add(const Item: Double): Integer;
    Function AddObject(const Item: Double; AObject: TObject): Integer;
    function Find(Item: Double; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const Item: Double);
    procedure InsertObject(Index: Integer; const Item: Double;
      AObject: TObject);
    property Items[Index: Integer]: Double read GetItem write SetItem; default;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

  { ----------------------------- }
  TALBaseAVLList = class(TObject)
  private
    FList: TALQuickSortPointerList;
    FCount: Integer;
    FCapacity: Integer;
    FDuplicates: TDuplicates;
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow;
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure InsertItem(Index: Integer; Item: Pointer);
    property List: TALQuickSortPointerList read FList;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: NativeInt);
      overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
    function Expand: TALBaseAVLList;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  { ---------------------- }
  TALInt64AVLList = class;

  { --------------------------------------------------------- }
  TALInt64AVLListBinaryTree = class(TALInt64KeyAVLBinaryTree)
  Private
    fOwner: TALInt64AVLList;
  Protected
    procedure FreeNodeObj(aNode: TALBaseAVLBinaryTreeNode); override;
  Public
    Constructor Create(aOwner: TALInt64AVLList); reintroduce;
    property Owner: TALInt64AVLList read fOwner write fOwner;
  end;

  { ----------------------------------------------------------------- }
  TALInt64AVLListBinaryTreeNode = class(TALInt64KeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Obj: TObject; // Object
    Idx: Integer; // Index in the NodeList
    Constructor Create; Override;
  end;

  { ------------------------------------- }
  TALInt64AVLList = class(TALBaseAVLList)
  private
    FOwnsObject: Boolean;
    FAVLBinTree: TALInt64AVLListBinaryTree;
    function GetItem(Index: Integer): Int64;
    function GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
  protected
    procedure InsertItem(Index: Integer; const Item: Int64; AObject: TObject);
  public
    Constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function IndexOf(Item: Int64): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    function Add(const Item: Int64): Integer;
    Function AddObject(const Item: Int64; AObject: TObject): Integer;
    function Find(Item: Int64; var Index: Integer): Boolean;
    property Items[Index: Integer]: Int64 read GetItem; default;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

resourcestring
  SALDuplicateItem = 'List does not allow duplicates';
  SALListCapacityError = 'List capacity out of bounds (%d)';
  SALListCountError = 'List count out of bounds (%d)';
  SALListIndexError = 'List index out of bounds (%d)';
  SALSortedListError = 'Operation not allowed on sorted list';

implementation

uses ALString;

{ *********************************************************************************** }
function AlBaseQuickSortListCompare(List: TObject;
  Index1, Index2: Integer): Integer;
Begin
  result := TALBaseQuickSortList(List).CompareItems(Index1, Index2);
end;

{ ************************************** }
constructor TALBaseQuickSortList.Create;
begin
  SetLength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FDuplicates := dupIgnore;
end;

{ ************************************** }
destructor TALBaseQuickSortList.Destroy;
begin
  Clear;
end;

{ *********************************** }
procedure TALBaseQuickSortList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{ *********************************************************************** }
procedure TALBaseQuickSortList.InsertItem(Index: Integer; Item: Pointer);
begin
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    ALMove(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(Pointer));
  FList[Index] := Item;
  Inc(FCount);
  if (Item <> nil) then
    Notify(Item, lnAdded);
end;

{ ******************************************************************** }
procedure TALBaseQuickSortList.ExchangeItems(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

{ **************************************************** }
procedure TALBaseQuickSortList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  Temp := FList[Index];
  Dec(FCount);
  if Index < FCount then
    ALMove(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(Pointer));
  if (Temp <> nil) then
    Notify(Temp, lnDeleted);
end;

{ ***************************************************************************** }
class procedure TALBaseQuickSortList.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

{ ****************************************************************************** }
class procedure TALBaseQuickSortList.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]);
end;

{ *************************************************************** }
procedure TALBaseQuickSortList.Exchange(Index1, Index2: Integer);
begin
  { Warning:	Do not call Exchange on a sorted list except to swap two identical
    items with different associated objects. Exchange does not check whether
    the list is sorted, and can destroy the sort order of a sorted list. }
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SALListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SALListIndexError, Index2);
  ExchangeItems(Index1, Index2);
end;

{ ******************************************************************* }
procedure TALBaseQuickSortList.Insert(Index: Integer; Item: Pointer);
begin
  if Sorted then
    Error(@SALSortedListError, 0);
  if (Index < 0) or (Index > FCount) then
    Error(@SALListIndexError, Index);
  InsertItem(Index, Item);
end;

{ **************************************************************** }
procedure TALBaseQuickSortList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if Sorted then
    Error(@SALSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  if Item <> FList[Index] then
  begin
    Temp := FList[Index];
    FList[Index] := Item;
    if Temp <> nil then
      Notify(Temp, lnDeleted);
    if Item <> nil then
      Notify(Item, lnAdded);
  end;
end;

{ ********************************************************* }
function TALBaseQuickSortList.Get(Index: Integer): Pointer;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SALListIndexError, Index);
  result := FList[Index];
end;

{ ********************************************************* }
function TALBaseQuickSortList.Expand: TALBaseQuickSortList;
begin
  if FCount = FCapacity then
    Grow;
  result := Self;
end;

{ ********************************** }
procedure TALBaseQuickSortList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{ *************************************************************** }
procedure TALBaseQuickSortList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SALListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{ ********************************************************* }
procedure TALBaseQuickSortList.SetCount(NewCount: Integer);
var
  I: Integer;
  Temp: Pointer;
begin
  if NewCount < 0 then
    Error(@SALListCountError, NewCount);
  if NewCount <> FCount then
  begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if NewCount > FCount then
      FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
    else
      for I := FCount - 1 downto NewCount do
      begin
        Dec(FCount);
        Temp := List[I];
        if Temp <> nil then
          Notify(Temp, lnDeleted);
      end;
    FCount := NewCount;
  end;
end;

{ ***************************************************************************** }
procedure TALBaseQuickSortList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

{ ******************************************************* }
procedure TALBaseQuickSortList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort;
    FSorted := Value;
  end;
end;

{ ***************************************************************************************** }
procedure TALBaseQuickSortList.QuickSort(L, R: Integer;
  SCompare: TALQuickSortListCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do
        Inc(I);
      while SCompare(Self, J, P) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
          ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

{ ************************************************************************** }
procedure TALBaseQuickSortList.CustomSort(Compare: TALQuickSortListCompare);
begin
  if not Sorted and (FCount > 1) then
    QuickSort(0, FCount - 1, Compare);
end;

{ ********************************** }
procedure TALBaseQuickSortList.Sort;
begin
  CustomSort(AlBaseQuickSortListCompare);
end;

{ ********************************************************************************* }
function TALBaseQuickSortList.CompareItems(const Index1,
  Index2: Integer): Integer;
begin
  result := 0;
end;

{ ******************************************************** }
function TALIntegerList.Add(const Item: Integer): Integer;
begin
  result := AddObject(Item, nil);
end;

{ ******************************************************************************** }
function TALIntegerList.AddObject(const Item: Integer;
  AObject: TObject): Integer;
begin
  if not Sorted then
    result := FCount
  else if Find(Item, result) then
    case Duplicates of
      dupIgnore:
        Exit;
      dupError:
        Error(@SALDuplicateItem, 0);
    end;
  InsertItem(result, Item, AObject);
end;

{ ***************************************************************************************** }
procedure TALIntegerList.InsertItem(Index: Integer; const Item: Integer;
  AObject: TObject);
Var
  aPALIntegerListItem: PALIntegerListItem;
begin
  New(aPALIntegerListItem);
  aPALIntegerListItem^.FInteger := Item;
  aPALIntegerListItem^.FObject := AObject;
  try
    inherited InsertItem(index, aPALIntegerListItem);
  except
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{ *************************************************************************** }
function TALIntegerList.CompareItems(const Index1, Index2: Integer): Integer;
begin
  result := PALIntegerListItem(Get(Index1))^.FInteger -
    PALIntegerListItem(Get(Index2))^.FInteger;
end;

{ *********************************************************************** }
function TALIntegerList.Find(Item: Integer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := GetItem(I) - Item;
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

{ ******************************************************* }
function TALIntegerList.GetItem(Index: Integer): Integer;
begin
  result := PALIntegerListItem(Get(index))^.FInteger
end;

{ ******************************** }
constructor TALIntegerList.Create;
begin
  inherited Create;
  FOwnsObject := False;
end;

{ ****************************************************** }
constructor TALIntegerList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{ ****************************************************** }
function TALIntegerList.IndexOf(Item: Integer): Integer;
begin
  if not Sorted then
  Begin
    result := 0;
    while (result < FCount) and (GetItem(result) <> Item) do
      Inc(result);
    if result = FCount then
      result := -1;
  end
  else if not Find(Item, result) then
    result := -1;
end;

{ ******************************************************************* }
procedure TALIntegerList.Insert(Index: Integer; const Item: Integer);
begin
  InsertObject(Index, index, nil);
end;

{ ******************************************************************************************* }
procedure TALIntegerList.InsertObject(Index: Integer; const Item: Integer;
  AObject: TObject);
Var
  aPALIntegerListItem: PALIntegerListItem;
begin
  New(aPALIntegerListItem);
  aPALIntegerListItem^.FInteger := Item;
  aPALIntegerListItem^.FObject := AObject;
  try
    inherited Insert(index, aPALIntegerListItem);
  except
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{ *********************************************************************** }
procedure TALIntegerList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    if OwnsObjects then
      PALIntegerListItem(Ptr).FObject.Free;
    Dispose(Ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{ ******************************************************************** }
procedure TALIntegerList.SetItem(Index: Integer; const Item: Integer);
Var
  aPALIntegerListItem: PALIntegerListItem;
begin
  New(aPALIntegerListItem);
  aPALIntegerListItem^.FInteger := Item;
  aPALIntegerListItem^.FObject := nil;
  Try
    Put(Index, aPALIntegerListItem);
  except
    Dispose(aPALIntegerListItem);
    raise;
  end;
end;

{ ********************************************************* }
function TALIntegerList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  result := PALIntegerListItem(Get(index))^.FObject;
end;

{ *************************************************************** }
function TALIntegerList.IndexOfObject(AObject: TObject): Integer;
begin
  for result := 0 to Count - 1 do
    if GetObject(result) = AObject then
      Exit;
  result := -1;
end;

{ ******************************************************************* }
procedure TALIntegerList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  PALIntegerListItem(Get(index))^.FObject := AObject;
end;

{ ********************************************************** }
function TALCardinalList.Add(const Item: Cardinal): Integer;
begin
  result := AddObject(Item, nil);
end;

{ ********************************************************************************** }
function TALCardinalList.AddObject(const Item: Cardinal;
  AObject: TObject): Integer;
begin
  if not Sorted then
    result := FCount
  else if Find(Item, result) then
    case Duplicates of
      dupIgnore:
        Exit;
      dupError:
        Error(@SALDuplicateItem, 0);
    end;
  InsertItem(result, Item, AObject);
end;

{ ******************************************************************************************* }
procedure TALCardinalList.InsertItem(Index: Integer; const Item: Cardinal;
  AObject: TObject);
Var
  aPALCardinalListItem: PALCardinalListItem;
begin
  New(aPALCardinalListItem);
  aPALCardinalListItem^.FCardinal := Item;
  aPALCardinalListItem^.FObject := AObject;
  try
    inherited InsertItem(index, aPALCardinalListItem);
  except
    Dispose(aPALCardinalListItem);
    raise;
  end;
end;

{ **************************************************************************** }
function TALCardinalList.CompareItems(const Index1, Index2: Integer): Integer;
var
  aCard1: Cardinal;
  aCard2: Cardinal;
begin
  aCard1 := PALCardinalListItem(Get(Index1))^.FCardinal;
  aCard2 := PALCardinalListItem(Get(Index2))^.FCardinal;
  if aCard1 < aCard2 then
    result := -1
  else if aCard1 > aCard2 then
    result := 1
  else
    result := 0;
end;

{ ************************************************************************* }
function TALCardinalList.Find(Item: Cardinal; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;

  { -------------------------------------------------- }
  Function _CompareCardinal(D1, D2: Cardinal): Integer;
  Begin
    if D1 < D2 then
      result := -1
    else if D1 > D2 then
      result := 1
    else
      result := 0;
  end;

begin
  result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := _CompareCardinal(GetItem(I), Item);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

{ ********************************************************* }
function TALCardinalList.GetItem(Index: Integer): Cardinal;
begin
  result := PALCardinalListItem(Get(index))^.FCardinal
end;

{ ********************************* }
constructor TALCardinalList.Create;
begin
  inherited Create;
  FOwnsObject := False;
end;

{ ******************************************************* }
constructor TALCardinalList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{ ******************************************************** }
function TALCardinalList.IndexOf(Item: Cardinal): Integer;
begin
  if not Sorted then
  Begin
    result := 0;
    while (result < FCount) and (GetItem(result) <> Item) do
      Inc(result);
    if result = FCount then
      result := -1;
  end
  else if not Find(Item, result) then
    result := -1;
end;

{ ********************************************************************* }
procedure TALCardinalList.Insert(Index: Integer; const Item: Cardinal);
begin
  InsertObject(Index, index, nil);
end;

{ ********************************************************************************************* }
procedure TALCardinalList.InsertObject(Index: Integer; const Item: Cardinal;
  AObject: TObject);
Var
  aPALCardinalListItem: PALCardinalListItem;
begin
  New(aPALCardinalListItem);
  aPALCardinalListItem^.FCardinal := Item;
  aPALCardinalListItem^.FObject := AObject;
  try
    inherited Insert(index, aPALCardinalListItem);
  except
    Dispose(aPALCardinalListItem);
    raise;
  end;
end;

{ ************************************************************************ }
procedure TALCardinalList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    if OwnsObjects then
      PALCardinalListItem(Ptr).FObject.Free;
    Dispose(Ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{ ********************************************************************** }
procedure TALCardinalList.SetItem(Index: Integer; const Item: Cardinal);
Var
  aPALCardinalListItem: PALCardinalListItem;
begin
  New(aPALCardinalListItem);
  aPALCardinalListItem^.FCardinal := Item;
  aPALCardinalListItem^.FObject := nil;
  Try
    Put(Index, aPALCardinalListItem);
  except
    Dispose(aPALCardinalListItem);
    raise;
  end;
end;

{ ********************************************************** }
function TALCardinalList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  result := PALCardinalListItem(Get(index))^.FObject;
end;

{ **************************************************************** }
function TALCardinalList.IndexOfObject(AObject: TObject): Integer;
begin
  for result := 0 to Count - 1 do
    if GetObject(result) = AObject then
      Exit;
  result := -1;
end;

{ ******************************************************************** }
procedure TALCardinalList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  PALCardinalListItem(Get(index))^.FObject := AObject;
end;

{ **************************************************** }
function TALInt64List.Add(const Item: Int64): Integer;
begin
  result := AddObject(Item, nil);
end;

{ **************************************************************************** }
function TALInt64List.AddObject(const Item: Int64; AObject: TObject): Integer;
begin
  if not Sorted then
    result := FCount
  else if Find(Item, result) then
    case Duplicates of
      dupIgnore:
        Exit;
      dupError:
        Error(@SALDuplicateItem, 0);
    end;
  InsertItem(result, Item, AObject);
end;

{ ************************************************************************************* }
procedure TALInt64List.InsertItem(Index: Integer; const Item: Int64;
  AObject: TObject);
Var
  aPALInt64ListItem: PALInt64ListItem;
begin
  New(aPALInt64ListItem);
  aPALInt64ListItem^.FInt64 := Item;
  aPALInt64ListItem^.FObject := AObject;
  try
    inherited InsertItem(index, aPALInt64ListItem);
  except
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{ ************************************************************************* }
function TALInt64List.CompareItems(const Index1, Index2: Integer): Integer;
var
  aInt64: Int64;
begin
  aInt64 := PALInt64ListItem(Get(Index1))^.FInt64 -
    PALInt64ListItem(Get(Index2))^.FInt64;
  if aInt64 < 0 then
    result := -1
  else if aInt64 > 0 then
    result := 1
  else
    result := 0;
end;

{ ******************************************************************* }
function TALInt64List.Find(Item: Int64; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;

  { -------------------------------------------- }
  Function _CompareInt64(D1, D2: Int64): Integer;
  Begin
    if D1 < D2 then
      result := -1
    else if D1 > D2 then
      result := 1
    else
      result := 0;
  end;

begin
  result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := _CompareInt64(GetItem(I), Item);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

{ *************************************************** }
function TALInt64List.GetItem(Index: Integer): Int64;
begin
  result := PALInt64ListItem(Get(index))^.FInt64
end;

{ ****************************** }
constructor TALInt64List.Create;
begin
  inherited Create;
  FOwnsObject := False;
end;

{ **************************************************** }
constructor TALInt64List.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{ ************************************************** }
function TALInt64List.IndexOf(Item: Int64): Integer;
begin
  if not Sorted then
  Begin
    result := 0;
    while (result < FCount) and (GetItem(result) <> Item) do
      Inc(result);
    if result = FCount then
      result := -1;
  end
  else if not Find(Item, result) then
    result := -1;
end;

{ *************************************************************** }
procedure TALInt64List.Insert(Index: Integer; const Item: Int64);
begin
  InsertObject(Index, index, nil);
end;

{ *************************************************************************************** }
procedure TALInt64List.InsertObject(Index: Integer; const Item: Int64;
  AObject: TObject);
Var
  aPALInt64ListItem: PALInt64ListItem;
begin
  New(aPALInt64ListItem);
  aPALInt64ListItem^.FInt64 := Item;
  aPALInt64ListItem^.FObject := AObject;
  try
    inherited Insert(index, aPALInt64ListItem);
  except
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{ ********************************************************************* }
procedure TALInt64List.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    if OwnsObjects then
      PALInt64ListItem(Ptr).FObject.Free;
    Dispose(Ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{ **************************************************************** }
procedure TALInt64List.SetItem(Index: Integer; const Item: Int64);
Var
  aPALInt64ListItem: PALInt64ListItem;
begin
  New(aPALInt64ListItem);
  aPALInt64ListItem^.FInt64 := Item;
  aPALInt64ListItem^.FObject := nil;
  Try
    Put(Index, aPALInt64ListItem);
  except
    Dispose(aPALInt64ListItem);
    raise;
  end;
end;

{ ******************************************************* }
function TALInt64List.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  result := PALInt64ListItem(Get(index))^.FObject;
end;

{ ************************************************************* }
function TALInt64List.IndexOfObject(AObject: TObject): Integer;
begin
  for result := 0 to Count - 1 do
    if GetObject(result) = AObject then
      Exit;
  result := -1;
end;

{ ***************************************************************** }
procedure TALInt64List.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  PALInt64ListItem(Get(index))^.FObject := AObject;
end;

{ ****************************************************** }
function TALDoubleList.Add(const Item: Double): Integer;
begin
  result := AddObject(Item, nil);
end;

{ ****************************************************************************** }
function TALDoubleList.AddObject(const Item: Double; AObject: TObject): Integer;
begin
  if not Sorted then
    result := FCount
  else if Find(Item, result) then
    case Duplicates of
      dupIgnore:
        Exit;
      dupError:
        Error(@SALDuplicateItem, 0);
    end;
  InsertItem(result, Item, AObject);
end;

{ *************************************************************************************** }
procedure TALDoubleList.InsertItem(Index: Integer; const Item: Double;
  AObject: TObject);
Var
  aPALDoubleListItem: PALDoubleListItem;
begin
  New(aPALDoubleListItem);
  aPALDoubleListItem^.FDouble := Item;
  aPALDoubleListItem^.FObject := AObject;
  try
    inherited InsertItem(index, aPALDoubleListItem);
  except
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{ ************************************************************************** }
function TALDoubleList.CompareItems(const Index1, Index2: Integer): Integer;
var
  aDouble: Double;
begin
  aDouble := PALDoubleListItem(Get(Index1))^.FDouble -
    PALDoubleListItem(Get(Index2))^.FDouble;
  if aDouble < 0 then
    result := -1
  else if aDouble > 0 then
    result := 1
  else
    result := 0;
end;

{ ********************************************************************* }
function TALDoubleList.Find(Item: Double; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;

  { ---------------------------------------------- }
  Function _CompareDouble(D1, D2: Double): Integer;
  Begin
    if D1 < D2 then
      result := -1
    else if D1 > D2 then
      result := 1
    else
      result := 0;
  end;

begin
  result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := _CompareDouble(GetItem(I), Item);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

{ ***************************************************** }
function TALDoubleList.GetItem(Index: Integer): Double;
begin
  result := PALDoubleListItem(Get(index))^.FDouble
end;

{ ******************************* }
constructor TALDoubleList.Create;
begin
  inherited Create;
  FOwnsObject := False;
end;

{ ***************************************************** }
constructor TALDoubleList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

{ **************************************************** }
function TALDoubleList.IndexOf(Item: Double): Integer;
begin
  if not Sorted then
  Begin
    result := 0;
    while (result < FCount) and (GetItem(result) <> Item) do
      Inc(result);
    if result = FCount then
      result := -1;
  end
  else if not Find(Item, result) then
    result := -1;
end;

{ ***************************************************************** }
procedure TALDoubleList.Insert(Index: Integer; const Item: Double);
begin
  InsertObject(Index, index, nil);
end;

{ ***************************************************************************************** }
procedure TALDoubleList.InsertObject(Index: Integer; const Item: Double;
  AObject: TObject);
Var
  aPALDoubleListItem: PALDoubleListItem;
begin
  New(aPALDoubleListItem);
  aPALDoubleListItem^.FDouble := Item;
  aPALDoubleListItem^.FObject := AObject;
  try
    inherited Insert(index, aPALDoubleListItem);
  except
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{ ********************************************************************** }
procedure TALDoubleList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    if OwnsObjects then
      PALDoubleListItem(Ptr).FObject.Free;
    Dispose(Ptr);
  end;
  inherited Notify(Ptr, Action);
end;

{ ****************************************************************** }
procedure TALDoubleList.SetItem(Index: Integer; const Item: Double);
Var
  aPALDoubleListItem: PALDoubleListItem;
begin
  New(aPALDoubleListItem);
  aPALDoubleListItem^.FDouble := Item;
  aPALDoubleListItem^.FObject := nil;
  Try
    Put(Index, aPALDoubleListItem);
  except
    Dispose(aPALDoubleListItem);
    raise;
  end;
end;

{ ******************************************************** }
function TALDoubleList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  result := PALDoubleListItem(Get(index))^.FObject;
end;

{ ************************************************************** }
function TALDoubleList.IndexOfObject(AObject: TObject): Integer;
begin
  for result := 0 to Count - 1 do
    if GetObject(result) = AObject then
      Exit;
  result := -1;
end;

{ ****************************************************************** }
procedure TALDoubleList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  PALDoubleListItem(Get(index))^.FObject := AObject;
end;

{ ******************************** }
constructor TALBaseAVLList.Create;
begin
  SetLength(FList, 0);
  FCount := 0;
  FCapacity := 0;
  FDuplicates := dupIgnore;
end;

{ ******************************** }
destructor TALBaseAVLList.Destroy;
begin
  Clear;
end;

{ ***************************** }
procedure TALBaseAVLList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{ ***************************************************************** }
procedure TALBaseAVLList.InsertItem(Index: Integer; Item: Pointer);
begin
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    ALMove(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(Pointer));
  FList[Index] := Item;
  Inc(FCount);
  if (Item <> nil) then
    Notify(Item, lnAdded);
end;

{ ********************************************** }
procedure TALBaseAVLList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  Temp := FList[Index];
  Dec(FCount);
  if Index < FCount then
    ALMove(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(Pointer));
  if (Temp <> nil) then
    Notify(Temp, lnDeleted);
end;

{ *********************************************************************** }
class procedure TALBaseAVLList.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

{ ************************************************************************ }
class procedure TALBaseAVLList.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]);
end;

{ *************************************************** }
function TALBaseAVLList.Get(Index: Integer): Pointer;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SALListIndexError, Index);
  result := FList[Index];
end;

{ ********************************************* }
function TALBaseAVLList.Expand: TALBaseAVLList;
begin
  if FCount = FCapacity then
    Grow;
  result := Self;
end;

{ **************************** }
procedure TALBaseAVLList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{ *********************************************************************** }
procedure TALBaseAVLList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

{ ********************************************************* }
procedure TALBaseAVLList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SALListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{ *************************************************** }
procedure TALBaseAVLList.SetCount(NewCount: Integer);
var
  I: Integer;
  Temp: Pointer;
begin
  if NewCount < 0 then
    Error(@SALListCountError, NewCount);
  if NewCount <> FCount then
  begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if NewCount > FCount then
      FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
    else
      for I := FCount - 1 downto NewCount do
      begin
        Dec(FCount);
        Temp := List[I];
        if Temp <> nil then
          Notify(Temp, lnDeleted);
      end;
    FCount := NewCount;
  end;
end;

{ ******************************************************************** }
constructor TALInt64AVLListBinaryTree.Create(aOwner: TALInt64AVLList);
begin
  inherited Create;
  fOwner := aOwner;
end;

{ ******************************************************************************* }
procedure TALInt64AVLListBinaryTree.FreeNodeObj
  (aNode: TALBaseAVLBinaryTreeNode);
begin
  if Owner.OwnsObjects then
    TALInt64AVLListBinaryTreeNode(aNode).Obj.Free;
  inherited;
end;

{ *********************************************** }
constructor TALInt64AVLListBinaryTreeNode.Create;
begin
  inherited;
  Obj := nil;
  Idx := -1;
end;

{ ********************************* }
constructor TALInt64AVLList.Create;
begin
  inherited Create;
  FAVLBinTree := TALInt64AVLListBinaryTree.Create(Self);
  FOwnsObject := False;
end;

{ ******************************************************* }
constructor TALInt64AVLList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FAVLBinTree := TALInt64AVLListBinaryTree.Create(Self);
  FOwnsObject := OwnsObjects;
end;

{ ********************************* }
destructor TALInt64AVLList.Destroy;
begin
  inherited;
  FAVLBinTree.Free;
end;

{ ****************************** }
procedure TALInt64AVLList.Clear;
begin
  FAVLBinTree.Clear;
  inherited;
end;

{ ******************************************************* }
function TALInt64AVLList.Add(const Item: Int64): Integer;
begin
  result := AddObject(Item, nil);
end;

{ ******************************************************************************* }
function TALInt64AVLList.AddObject(const Item: Int64; AObject: TObject)
  : Integer;
begin
  result := FCount;
  InsertItem(result, Item, AObject);
end;

{ **************************************************************************************** }
procedure TALInt64AVLList.InsertItem(Index: Integer; const Item: Int64;
  AObject: TObject);
Var
  aNode: TALInt64AVLListBinaryTreeNode;
  I: Integer;
begin
  aNode := TALInt64AVLListBinaryTreeNode.Create;
  aNode.ID := Item;
  aNode.Obj := AObject;
  aNode.Idx := Index;
  if not FAVLBinTree.AddNode(aNode) then
  begin
    aNode.Free;
    case Duplicates of
      dupIgnore:
        Exit;
    else
      Error(@SALDuplicateItem, 0);
    end;
  end;

  try
    inherited InsertItem(index, aNode);
  except
    FAVLBinTree.DeleteNode(Item);
    raise;
  end;

  for I := Index + 1 to Count - 1 do
    Inc(TALInt64AVLListBinaryTreeNode(Objects[I]).Idx);
end;

{ ********************************************************************** }
function TALInt64AVLList.Find(Item: Int64; var Index: Integer): Boolean;
var
  aNode: TALInt64KeyAVLBinaryTreeNode;
begin
  aNode := FAVLBinTree.FindNode(Item);
  result := assigned(aNode);
  if result then
    Index := TALInt64AVLListBinaryTreeNode(aNode).Idx;
end;

{ ****************************************************** }
function TALInt64AVLList.GetItem(Index: Integer): Int64;
begin
  result := TALInt64AVLListBinaryTreeNode(Get(index)).ID
end;

{ ***************************************************** }
function TALInt64AVLList.IndexOf(Item: Int64): Integer;
begin
  if not Find(Item, result) then
    result := -1;
end;

{ *********************************************** }
procedure TALInt64AVLList.Delete(Index: Integer);
var
  I: Integer;
begin
  FAVLBinTree.DeleteNode(GetItem(Index));

  for I := Index + 1 to Count - 1 do
    Dec(TALInt64AVLListBinaryTreeNode(Get(I)).Idx);

  inherited Delete(Index);
end;

{ ********************************************************** }
function TALInt64AVLList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  result := TALInt64AVLListBinaryTreeNode(Get(index)).Obj;
end;

{ **************************************************************** }
function TALInt64AVLList.IndexOfObject(AObject: TObject): Integer;
begin
  for result := 0 to Count - 1 do
    if GetObject(result) = AObject then
      Exit;
  result := -1;
end;

{ ******************************************************************** }
procedure TALInt64AVLList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SALListIndexError, Index);
  TALInt64AVLListBinaryTreeNode(Get(index)).Obj := AObject;
end;

end.
