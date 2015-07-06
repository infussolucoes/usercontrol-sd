(* *************************************************************
  www:          http://sourceforge.net/projects/alcinoe/
  svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
  Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
  Sponsor(s):   Arkadia SA (http://www.arkadia.com)

  product:      ALJsonDocument
  Version:      4.01

  Description:  TALJsonDocument is a Delphi parser/writer for JSON / BSON data
  format. it's support DOM and SAX parser, support BSON format,
  and use a similar syntax than TALXMLDocument / TXMLDocument.
  TALJsonDocument can also export Json / Bson data in TALStringList.

  When it deals with parsing some (textual) content, two directions
  are usually envisaged. In the JSON world, you have usually to
  make a choice between:
  - A DOM parser, which creates an in-memory tree structure of
  objects mapping the JSON content;
  - A SAX parser, which reads the JSON content, then call pre-defined
  events for each JSON content element.

  In fact, DOM parsers use internally a SAX parser to read the JSON
  content. Therefore, with the overhead of object creation and
  their property initialization, DOM parsers are typically three
  to five times slower than SAX (and use much much more memory to
  store all the nodes). But, DOM parsers are much more powerful for
  handling the data: as soon as it's mapped in native objects,
  code can access with no time to any given node, whereas a
  SAX-based access will have to read again the whole JSON content.

  Most JSON parser available in Delphi use a DOM-like approach.
  For instance, the DBXJSON unit included since Delphi 2010
  or the SuperObject library create a class instance mapping
  each JSON node. In order to achieve best speed, TALJsonDocument
  implement DOM parser and also a SAX parser.

  TALJsonDocument syntax is very similar
  to TALXMLdocument / TXMLDocument

  exemple :

  {
  _id: 1,
  name: { first: "John", last: "Backus" },
  birth: new Date('1999-10-21T21:04:54.234Z'),
  contribs: [ "Fortran", "ALGOL", "Backus-Naur Form", "FP" ],
  awards: [
  { award: "National Medal of Science",
  year: 1975,
  by: "National Science Foundation" },
  { award: "Turing Award",
  year: 1977,
  by: "ACM" }
  ],
  spouse: "",
  address: {},
  phones: []
  }

  ------------------------------
  To access the document nodes :

  MyJsonDoc.loadFromJson(AJsonStr, False);
  MyJsonDoc.childnodes['_id'].int32;
  MyJsonDoc.childnodes['name'].childnodes['first'].text;
  MyJsonDoc.childnodes['name'].childnodes['last'].text;
  MyJsonDoc.childnodes['birth'].datetime;
  for i := 0 to MyJsonDoc.childnodes['contribs'].ChildNodes.count - 1 do
  MyJsonDoc.childnodes['contribs'].childnodes[i].text;
  for i := 0 to MyJsonDoc.childnodes['awards'].ChildNodes.count - 1 do begin
  MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['award'].text;
  MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['year'].text;
  MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['by'].text;
  end;

  ------------------------------
  To create the document nodes :

  MyJsonDoc.addchild('_id').int32 := 1;
  with MyJsonDoc.addchild('name', ntObject) do begin
  addchild('first').text := 'John';
  addchild('last').text := 'Backus';
  end;
  MyJsonDoc.addchild('birth').dateTime := Now;
  with MyJsonDoc.addchild('contribs', ntArray) do begin
  addchild.text := 'Fortran';
  addchild.text := 'ALGOL';
  addchild.text := 'Backus-Naur Form';
  addchild.text := 'FP';
  end;
  with MyJsonDoc.addchild('awards', ntArray) do begin
  with addchild(ntObject) do begin
  addchild('award').text := 'National Medal of Science';
  addchild('year').int32 := 1975;
  addchild('by').text := 'National Science Foundation';
  end;
  with addchild(ntObject) do begin
  addchild('award').text := 'Turing Award';
  addchild('year').int32 := 1977;
  addchild('by').text := 'ACM';
  end;
  end;
  MyJsonDoc.addchild('spouse');
  MyJsonDoc.addchild('address', ntObject);
  MyJsonDoc.addchild('phones', ntArray);

  ----------------------------
  To load and save from BSON :

  MyJsonDoc.LoadFromFile(aBSONFileName, False{saxMode}, True{BSON});
  MyJsonDoc.SaveToFile(aBSONFileName, False{saxMode}, True{BSON});

  ---------------------------------------
  To parse an JSON document in Sax Mode :

  MyJsonDoc.onParseText := procedure (Sender: TObject;
  const Path: AnsiString;
  const name: AnsiString;
  const str: AnsiString;
  const NodeSubType: TALJSONNodeSubType)
  begin
  Writeln(Path + '=' + str);
  end;
  MyJsonDoc.LoadFromJSON(AJsonStr, true{saxMode});


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

  History :

  Link :

  * Please send all your feedback to alcinoe@arkadia.com
  * If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
  * Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
  ************************************************************* *)
unit ALJSONDoc;

interface

{$IF CompilerVersion >= 25} { Delphi XE4 }
{$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
  system.Classes,
  system.sysutils,
{$ELSE}
  Classes,
  sysutils,
{$IFEND}
  ALString,
  AlStringList,
  ALXmlDoc;

resourcestring
  cALJSONNotActive = 'No active document';
  cAlJSONNodeNotFound = 'Node "%s" not found';
  cALJSONInvalidNodeType = 'Invalid node type';
  cALJSONInvalidBSONNodeSubType = 'Invalid node sub type';
  cALJSONListCapacityError = 'Node list capacity out of bounds (%d)';
  cALJSONListCountError = 'Node list count out of bounds (%d)';
  cALJSONListIndexError = 'Node list index out of bounds (%d)';
  cALJSONOperationError =
    'This operation can not be performed with a node of type %s';
  cALJSONParseError = 'JSON Parse error';
  cALBSONParseError = 'BSON Parse error';

const
  cALJSONNodeMaxListSize = Maxint div 16;

type

  { class definition }
  TALJSONNode = Class;
  TALJSONNodeList = Class;
  TALJSONDocument = Class;

  TALJSONNodeType = (ntObject,
    // The node represents an object: { ... } or "name": { ... }
    ntArray, // The node represents an array: [ ... ] or "name": [ ... ]
    ntText); // The node represents a text content (javascript, string, number, true, false, null): "..." or "name": "..."

  // from http://bsonspec.org/#/specification
  TALJSONNodeSubType = (nstFloat,
    // \x01 | Floating point             | ex { a: 123.4 }
    nstText, // \x02 | UTF-8 string               | ex { a: "xxx" }
    nstObject, // \x03 | Embedded document          | ex { a: {} }
    nstArray, // \x04 | Array                      | ex { a: [] }
    nstBinary, // \x05 | Binary data
    // \x06 | Undefined — Deprecated
    nstObjectID,
    // \x07 | ObjectId                   | ex { a: ObjectId("507f1f77bcf86cd799439011") }
    nstBoolean, // \x08 | Boolean "false"            | ex { a: False }
    // \x08 | Boolean "true"             | ex { a: true }
    nstDateTime,
    // \x09 | UTC datetime               | ex { a: ISODate("yyyy-mm-ddThh:nn:ss.zzzZ") }
    nstNull, // \x0A | Null value                 | ex { a: null }
    nstRegEx, // \x0B | Regular expression
    // \x0C | DBPointer — Deprecated
    nstJavascript, // \x0D | JavaScript code            | ex { a: function() }
    // \x0E | Symbol — Deprecated
    // \x0F | JavaScript code w/ scope
    nstInt32, // \x10 | 32-bit Integer             | ex { a: NumberInt(123) }
    nstTimestamp,
    // \x11 | Timestamp                  | ex { a: Timestamp(0, 0) }
    nstInt64); // \x12 | 64-bit integer             | ex { a: NumberLong(123) }
  // \xFF | Min key
  // \x7F | Max key

  TALJSONObjectID = Array [1 .. 12] of AnsiChar;
  // ObjectId is a 12-byte BSON type, constructed using:
  // a 4-byte value representing the seconds since the Unix epoch,
  // a 3-byte machine identifier,
  // a 2-byte process id, and
  // a 3-byte counter, starting with a random value.

  TALBSONTimestamp = packed record
  // Special internal type used by MongoDB replication and sharding.
    case X: integer of
    // First 4 bytes are an increment, second 4 are a timestamp. Setting the
      0:
        (I64: Int64); // timestamp to 0 has special semantics.
      1:
        (W1: LongWord;
          W2: LongWord);
  end;

  TALJSONRegEx = record
    Expression: AnsiString;
    Options: AnsiString;
  end;

  TALJSONBinary = record
    Subtype: byte;
    Data: AnsiString;
  end;

{$IF CompilerVersion >= 23}
  { Delphi XE2 }
  TAlJSONParseDocument = reference to procedure(Sender: TObject);
  TAlJSONParseTextEvent = reference to procedure(Sender: TObject;
    const Path: AnsiString; const name: AnsiString; const Str: AnsiString;
    const NodeSubType: TALJSONNodeSubType);
  TAlJSONParseObjectEvent = reference to procedure(Sender: TObject;
    const Path: AnsiString; const Name: AnsiString);
  TAlJSONParseArrayEvent = reference to procedure(Sender: TObject;
    const Path: AnsiString; const Name: AnsiString);
{$ELSE}
  TAlJSONParseDocument = procedure(Sender: TObject) of object;
  TAlJSONParseTextEvent = procedure(Sender: TObject; const Path: AnsiString;
    const name: AnsiString; const Str: AnsiString;
    const NodeSubType: TALJSONNodeSubType) of object;
  TAlJSONParseObjectEvent = procedure(Sender: TObject; const Path: AnsiString;
    const name: AnsiString) of object;
  TAlJSONParseArrayEvent = procedure(Sender: TObject; const Path: AnsiString;
    const name: AnsiString) of object;
{$IFEND}
{$IF CompilerVersion >= 23} { Delphi XE2 }
  TALJSONNodeListSortCompare = reference to function(List: TALJSONNodeList;
    Index1, Index2: integer): integer;
{$ELSE}
  TALJSONNodeListSortCompare = function(List: TALJSONNodeList;
    Index1, Index2: integer): integer;
{$IFEND}
  TALJSONDocOption = (doNodeAutoCreate, // create only ntText Node !
    doNodeAutoIndent); // affect only the SaveToStream
  TALJSONDocOptions = set of TALJSONDocOption;

  TALJSONParseOption = (poIgnoreControlCharacters,
    // don't decode escaped characters (like \") and not encode them also (when save / load)
    poAddNodeSubTypeHelperFunct);
  // By default json (ie: javascript) treats all numbers as floating-point values.
  // To let other system (ie: mongoDB) understand the type of the number
  // we provide the helper functions NumberLong() to handle 64-bit integers
  // and NumberInt() to handle 64-bit integers. theses helper functions are
  // used when saving the json document.
  TALJSONParseOptions = set of TALJSONParseOption;

  PALPointerJSONNodeList = ^TALPointerJSONNodeList;
  TALPointerJSONNodeList = array [0 .. cALJSONNodeMaxListSize - 1]
    of TALJSONNode;

  { Exception }
  EALJSONDocError = class(Exception)
  end;

  { TALJSONNodeList }
  { TALJSONNodeList is used to represent a set of related nodes (TALJSONNode object) in an JSON document. For example, TALJSONNodeList is used to
    represent all of the children of a node, or all of the attributes of a node. TALJSONNodeList can be used to add or delete nodes from the
    List, or to access specific nodes. }
  TALJSONNodeList = class(TObject)
  Private
    FCapacity: integer;
    FCount: integer;
    FList: PALPointerJSONNodeList;
    FOwner: TALJSONNode;
    procedure QuickSort(L, R: integer; XCompare: TALJSONNodeListSortCompare);
  protected
    procedure Grow;
    procedure SetCapacity(NewCapacity: integer);
    procedure SetCount(NewCount: integer);
    property Owner: TALJSONNode read FOwner;
    function GetCount: integer;
    function Get(Index: integer): TALJSONNode;
{$IF CompilerVersion < 18.5}
    function GetNode(const IndexOrName: OleVariant): TALJSONNode;
{$IFEND}
    function GetNodeByIndex(Const Index: integer): TALJSONNode;
    function GetNodeByName(Const name: AnsiString): TALJSONNode;
  public
    constructor Create(Owner: TALJSONNode);
    destructor Destroy; override;
    procedure CustomSort(Compare: TALJSONNodeListSortCompare);
    function Add(const Node: TALJSONNode): integer;
    function Delete(const Index: integer): integer; overload;
    function Delete(const name: AnsiString): integer; overload;
    function Extract(const Index: integer): TALJSONNode; overload;
    function Extract(const Node: TALJSONNode): TALJSONNode; overload;
    procedure Exchange(Index1, Index2: integer);
    function FindNode(NodeName: AnsiString): TALJSONNode; overload;
    function FindSibling(const Node: TALJSONNode; Delta: integer): TALJSONNode;
    function First: TALJSONNode;
    function IndexOf(const name: AnsiString): integer; overload;
    function IndexOf(const Node: TALJSONNode): integer; overload;
    function Last: TALJSONNode;
    function Remove(const Node: TALJSONNode): integer;
    function ReplaceNode(const OldNode, NewNode: TALJSONNode): TALJSONNode;
    procedure Clear;
    procedure Insert(Index: integer; const Node: TALJSONNode);
    property Count: integer read GetCount;
{$IF CompilerVersion < 18.5}
    property Nodes[const IndexOrName: OleVariant]: TALJSONNode
      read GetNode; default;
{$ELSE}
    property Nodes[const Name: AnsiString]: TALJSONNode
      read GetNodeByName; default;
    property Nodes[const Index: integer]: TALJSONNode
      read GetNodeByIndex; default;
{$IFEND}
  end;

  { TALJSONNode }
  { TALJSONNode represents a node in an JSON document. }
  TALJSONNode = class(TObject)
  private
    FDocument: TALJSONDocument;
    FParentNode: TALJSONNode;
    fNodeName: AnsiString;
  protected
    function CreateChildList: TALJSONNodeList;
    function InternalGetChildNodes: TALJSONNodeList; virtual;
    function GetChildNodes: TALJSONNodeList; virtual;
    procedure SetChildNodes(const Value: TALJSONNodeList); virtual;
    function GetHasChildNodes: Boolean;
    function GetNodeName: AnsiString;
    procedure SetNodeName(const Value: AnsiString);
    function GetNodeType: TALJSONNodeType; virtual; abstract;
    function GetNodeSubType: TALJSONNodeSubType; virtual; abstract;
    procedure SetNodeSubType(const Value: TALJSONNodeSubType); virtual;
      abstract;
    function GetNodeTypeStr: AnsiString;
    function GetNodeValue: AnsiString; virtual;
    procedure SetNodeValue(const Value: AnsiString;
      const NodeSubType: TALJSONNodeSubType); virtual;
    function GetText: AnsiString;
    procedure SetText(const Value: AnsiString);
    function GetFloat: Double;
    procedure SetFloat(const Value: Double);
    function GetDateTime: TDateTime;
    procedure SetDateTime(const Value: TDateTime);
    function GetTimestamp: TALBSONTimestamp;
    procedure SetTimestamp(const Value: TALBSONTimestamp);
    function GetObjectID: TALJSONObjectID;
    procedure SetObjectID(const Value: TALJSONObjectID);
    function GetInt32: integer;
    procedure SetInt32(const Value: integer);
    function GetInt64: Int64;
    procedure SetInt64(const Value: Int64);
    function GetBool: Boolean;
    procedure SetBool(const Value: Boolean);
    function GetNull: Boolean;
    procedure SetNull(const Value: Boolean);
    function GetJavascript: AnsiString;
    procedure SetJavascript(const Value: AnsiString);
    function GetRegEx: TALJSONRegEx;
    procedure SetRegEx(const Value: TALJSONRegEx);
    function GetBinary: TALJSONBinary;
    procedure SetBinary(const Value: TALJSONBinary);
    function GetOwnerDocument: TALJSONDocument;
    procedure SetOwnerDocument(const Value: TALJSONDocument);
    function GetParentNode: TALJSONNode;
    procedure SetParentNode(const Value: TALJSONNode);
    function GetJSON: AnsiString;
    procedure SetJSON(const Value: AnsiString);
    function GetBSON: AnsiString;
    procedure SetBSON(const Value: AnsiString);
    function NestingLevel: integer;
  public
    constructor Create(const NodeName: AnsiString); virtual;
    function AddChild(const NodeName: AnsiString;
      const NodeType: TALJSONNodeType = ntText; const Index: integer = -1)
      : TALJSONNode; overload;
    function AddChild(const NodeType: TALJSONNodeType = ntText;
      const Index: integer = -1): TALJSONNode; overload;
    function NextSibling: TALJSONNode;
    function PreviousSibling: TALJSONNode;
    procedure SaveToFile(const AFileName: AnsiString;
      const BSONFile: Boolean = False);
    procedure SaveToStream(const Stream: TStream;
      const BSONStream: Boolean = False);
    procedure SaveToJSON(var JSON: AnsiString);
    procedure SaveToBSON(var BSON: AnsiString);
    procedure LoadFromFile(const AFileName: AnsiString;
      const BSONFile: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromStream(const Stream: TStream;
      const BSONStream: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSON(const JSON: AnsiString;
      Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSON(const BSON: AnsiString;
      Const ClearChildNodes: Boolean = True);
    property ChildNodes: TALJSONNodeList read GetChildNodes write SetChildNodes;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property NodeName: AnsiString read GetNodeName write SetNodeName;
    property NodeType: TALJSONNodeType read GetNodeType;
    property NodeValue: AnsiString read GetNodeValue;
    // same as text property but without formating
    property NodeSubType: TALJSONNodeSubType read GetNodeSubType
      write SetNodeSubType;
    property OwnerDocument: TALJSONDocument read GetOwnerDocument
      Write SetOwnerDocument;
    property ParentNode: TALJSONNode read GetParentNode;
    property Text: AnsiString read GetText write SetText;
    property int32: integer read GetInt32 write SetInt32;
    property Int64: Int64 read GetInt64 write SetInt64;
    property Float: Double read GetFloat write SetFloat;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property Timestamp: TALBSONTimestamp read GetTimestamp write SetTimestamp;
    // Use only by MongoDB, do not use it, use DateTime instead !
    property ObjectID: TALJSONObjectID read GetObjectID write SetObjectID;
    property Bool: Boolean read GetBool write SetBool;
    property Null: Boolean read GetNull write SetNull;
    property Javascript: AnsiString read GetJavascript write SetJavascript;
    property RegEx: TALJSONRegEx read GetRegEx write SetRegEx;
    property Binary: TALJSONBinary read GetBinary write SetBinary;
    property JSON: AnsiString read GetJSON write SetJSON;
    property BSON: AnsiString read GetBSON write SetBSON;
  end;

  // JSON object represents {} or { members }
  TALJSONObjectNode = Class(TALJSONNode)
  private
    FChildNodes: TALJSONNodeList;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    procedure SetNodeSubType(const Value: TALJSONNodeSubType); override;
    function InternalGetChildNodes: TALJSONNodeList; override;
    function GetChildNodes: TALJSONNodeList; override;
    procedure SetChildNodes(const Value: TALJSONNodeList); override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    Destructor Destroy; override;
  end;

  { implements JSON array [] | [ elements ] }
  TALJSONArrayNode = Class(TALJSONNode)
  private
    FChildNodes: TALJSONNodeList;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    procedure SetNodeSubType(const Value: TALJSONNodeSubType); override;
    function InternalGetChildNodes: TALJSONNodeList; override;
    function GetChildNodes: TALJSONNodeList; override;
    procedure SetChildNodes(const Value: TALJSONNodeList); override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    Destructor Destroy; override;
  end;

  { Groups javascript, string, number, true, false, null }
  TALJSONTextNode = Class(TALJSONNode)
  private
    fNodeSubType: TALJSONNodeSubType;
    fNodeValue: AnsiString;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    procedure SetNodeSubType(const Value: TALJSONNodeSubType); override;
    function GetNodeValue: AnsiString; override;
    procedure SetNodeValue(const Value: AnsiString;
      const NodeSubType: TALJSONNodeSubType); override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
  end;

  { TALJSONDocument }
  TALJSONDocument = class(TObject)
  private
    FTag: NativeInt;
    FDocumentNode: TALJSONNode;
    FNodeIndentStr: AnsiString;
    FOptions: TALJSONDocOptions;
    FParseOptions: TALJSONParseOptions;
    fPathSeparator: AnsiString;
    FOnParseStartDocument: TAlJSONParseDocument;
    FOnParseEndDocument: TAlJSONParseDocument;
    FonParseText: TAlJSONParseTextEvent;
    FonParseStartObject: TAlJSONParseObjectEvent;
    FonParseEndObject: TAlJSONParseObjectEvent;
    FonParseStartArray: TAlJSONParseArrayEvent;
    FonParseEndArray: TAlJSONParseArrayEvent;
    fFormatSettings: TALFormatSettings;
  protected
    procedure CheckActive;
    procedure DoParseStartDocument;
    procedure DoParseEndDocument;
    procedure DoParseText(const Path: AnsiString; const name: AnsiString;
      const Str: AnsiString; const NodeSubType: TALJSONNodeSubType);
    procedure DoParseStartObject(const Path: AnsiString;
      const name: AnsiString);
    procedure DoParseEndObject(const Path: AnsiString; const name: AnsiString);
    procedure DoParseStartArray(const Path: AnsiString; const name: AnsiString);
    procedure DoParseEndArray(const Path: AnsiString; const name: AnsiString);
    Procedure ParseJSONStream(Const RawJSONStream: TStream;
      Const ContainerNode: TALJSONNode);
    Procedure ParseBSONStream(Const RawBSONStream: TStream;
      Const ContainerNode: TALJSONNode);
    procedure ReleaseDoc;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetChildNodes: TALJSONNodeList;
    function GetDocumentNode: TALJSONNode;
    function GetNodeIndentStr: AnsiString;
    function GetOptions: TALJSONDocOptions;
    function GetParseOptions: TALJSONParseOptions;
    function GetPathSeparator: AnsiString;
    procedure SetPathSeparator(const Value: AnsiString);
    function GetJSON: AnsiString;
    function GetBSON: AnsiString;
    procedure SetOptions(const Value: TALJSONDocOptions);
    procedure SetParseOptions(const Value: TALJSONParseOptions);
    procedure SetJSON(const Value: AnsiString);
    procedure SetBSON(const Value: AnsiString);
    procedure SetNodeIndentStr(const Value: AnsiString);
  public
    constructor Create(const aActive: Boolean = True); overload; virtual;
    constructor Create(const aFormatSettings: TALFormatSettings;
      const aActive: Boolean = True); overload; virtual;
    destructor Destroy; override;
    function AddChild(const NodeName: AnsiString;
      const NodeType: TALJSONNodeType = ntText; const Index: integer = -1)
      : TALJSONNode;
    function CreateNode(const NodeName: AnsiString; NodeType: TALJSONNodeType)
      : TALJSONNode;
    function IsEmptyDoc: Boolean;
    procedure LoadFromFile(const AFileName: AnsiString;
      const saxMode: Boolean = False; const BSONFile: Boolean = False;
      Const ClearChildNodes: Boolean = True);
    procedure LoadFromStream(const Stream: TStream;
      const saxMode: Boolean = False; const BSONStream: Boolean = False;
      Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSON(const JSON: AnsiString;
      const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSON(const BSON: AnsiString;
      const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure SaveToFile(const AFileName: AnsiString;
      const BSONFile: Boolean = False);
    procedure SaveToStream(const Stream: TStream;
      const BSONStream: Boolean = False);
    procedure SaveToJSON(var JSON: AnsiString);
    procedure SaveToBSON(var BSON: AnsiString);
    property ChildNodes: TALJSONNodeList read GetChildNodes;
    property Node: TALJSONNode read GetDocumentNode;
    property Active: Boolean read GetActive write SetActive;
    property NodeIndentStr: AnsiString read GetNodeIndentStr
      write SetNodeIndentStr;
    property Options: TALJSONDocOptions read GetOptions write SetOptions;
    property ParseOptions: TALJSONParseOptions read GetParseOptions
      write SetParseOptions;
    property PathSeparator: AnsiString read GetPathSeparator
      write SetPathSeparator;
    property JSON: AnsiString read GetJSON write SetJSON;
    property BSON: AnsiString read GetBSON write SetBSON;
    property OnParseStartDocument: TAlJSONParseDocument
      read FOnParseStartDocument write FOnParseStartDocument;
    property OnParseEndDocument: TAlJSONParseDocument read FOnParseEndDocument
      write FOnParseEndDocument;
    property onParseText: TAlJSONParseTextEvent read FonParseText
      write FonParseText;
    property onParseStartObject: TAlJSONParseObjectEvent
      read FonParseStartObject write FonParseStartObject;
    property onParseEndObject: TAlJSONParseObjectEvent read FonParseEndObject
      write FonParseEndObject;
    property onParseStartArray: TAlJSONParseArrayEvent read FonParseStartArray
      write FonParseStartArray;
    property onParseEndArray: TAlJSONParseArrayEvent read FonParseEndArray
      write FonParseEndArray;
    property FormatSettings: TALFormatSettings read fFormatSettings
      write fFormatSettings;
    // this is use only on GetText/OnParseText to retrieve float and DateTime formatted according to FormatSettings
    property Tag: NativeInt read FTag write FTag;
  end;

  { misc constants }
Const
  CALDefaultNodeIndent = '  '; { 2 spaces }

  { misc function }
{$IF CompilerVersion >= 23} { Delphi XE2 }
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
  const aFormatSettings: TALFormatSettings; const aPath: AnsiString;
  aLst: TALStrings; Const aNullStr: AnsiString = 'null';
  Const aTrueStr: AnsiString = 'true';
  Const aFalseStr: AnsiString = 'false'); overload;
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
  const aFormatSettings: TALFormatSettings; aLst: TALStrings;
  Const aNullStr: AnsiString = 'null'; Const aTrueStr: AnsiString = 'true';
  Const aFalseStr: AnsiString = 'false'); overload;
{$IFEND}
Procedure ALJSONToTStrings(const aJsonNode: TALJSONNode;
  Const aPath: AnsiString; aLst: TALStrings;
  Const aNullStr: AnsiString = 'null'; Const aTrueStr: AnsiString = 'true';
  Const aFalseStr: AnsiString = 'false'); overload;
Procedure ALJSONToTStrings(const aJsonNode: TALJSONNode; aLst: TALStrings;
  Const aNullStr: AnsiString = 'null'; Const aTrueStr: AnsiString = 'true';
  Const aFalseStr: AnsiString = 'false'); overload;
procedure ALTStringsToJson(const aLst: TALStrings; aJsonNode: TALJSONNode;
  Const aPath: AnsiString = ''; Const aNameToLowerCase: Boolean = False;
  Const aNullStr: AnsiString = 'null');

Procedure ALJSONToXML(aJsonNode: TALJSONNode; aXMLNode: TALXmlNode;
  aXMLElementNameForJSONArrayEntries: TALStrings;
  // JSONArrayNodeName=XMLElementName | ex: transactions=transaction
  // |     features=feature
  const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec');
  overload;
Procedure ALJSONToXML(aJsonNode: TALJSONNode; aXMLNode: TALXmlNode;
  const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec');
  overload;

function ALJsonEncodeWithNodeSubTypeHelperFunction(aValue: AnsiString;
  aNodeSubType: TALJSONNodeSubType; aFormatSettings: TALFormatSettings)
  : AnsiString;

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
  system.Math,
  system.Contnrs,
  system.DateUtils,
{$ELSE}
  Math,
  Contnrs,
  DateUtils,
{$IFEND}
  AlHTML,
  ALMime,
  ALMisc;

{ ************************************************************************************* }
function ALJSONDocTryStrToRegEx(const S: AnsiString;
  out Value: TALJSONRegEx): Boolean;
var
  aRegEx: TALPerlRegEx;
  P1: integer;
  i: integer;
begin

  // regular expression in JSON must look like: /pattern/options
  // list of valid options is:
  // 'i' for case insensitive matching,
  // 'm' for multiline matching,
  // 'x' for verbose mode,
  // 'l' to make \w, \W, etc. locale dependent,
  // 's' for dotall mode ('.' matches everything),
  // 'u' to make \w, \W, etc. match unicode.
  result := False;

  // check that first character is /
  if (S <> '') and (S[1] = '/') then
  begin

    P1 := ALLastDelimiter('/', S);
    if P1 <> 1 then
    begin

      // init Value
      Value.Options := ALCopyStr(S, P1 + 1, Maxint);
      Value.Expression := ALCopyStr(S, 2, P1 - 2);

      // loop on all the options characters
      // to check if they are allowed.
      for i := 1 to Length(Value.Options) do
        if not(Value.Options[i] in ['i', 'm', 'x', 'l', 's', 'u']) then
          Exit;

      // check if it's compiling
      aRegEx := TALPerlRegEx.Create;
      try
        aRegEx.RegEx := Value.Expression;
        result := aRegEx.Compile(False { RaiseException } );
      finally
        aRegEx.Free;
      end;

    end;

  end;

end;

{ *************************************************************************************** }
function ALJSONDocTryStrTobinary(const S: AnsiString;
  out Value: TALJSONBinary): Boolean;
var
  P1, P2: integer;
  aInt: integer;
begin

  // s must look like
  // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // BinData ( 0 , "JliB6gIMRuSphAD2KmhzgQ==" )
  result := False;
  P1 := 1;
  while (P1 <= Length(S)) and (S[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 + 6 > Length(S)) or (S[P1] <> 'B') or (S[P1 + 1] <> 'i') or
    (S[P1 + 2] <> 'n') or (S[P1 + 3] <> 'D') or (S[P1 + 4] <> 'a') or
    (S[P1 + 5] <> 't') or (S[P1 + 6] <> 'a') then
    Exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // ^
  P1 := P1 + 7 { Length('BinData') }; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // ^
  while (P1 <= Length(S)) and (S[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(S)) or (S[P1] <> '(') then
    Exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // ^P1
  inc(P1); // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // ^P1
  if (P1 > Length(S)) then
    Exit;
  P2 := ALPosEx(',', S, P1);
  if P2 <= P1 then
    Exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // ^P2

  // init Value.subtype
  if not ALTryStrToInt(ALTrim(ALCopyStr(S, P1, P2 - P1)), aInt) then
    Exit;
  Value.Subtype := aInt;

  // init Value.Data
  P1 := P2 + 1; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // ^P1
  while (P1 <= Length(S)) and (S[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(S)) or (not(S[P1] in ['"', ''''])) then
    Exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // ^P1
  P2 := Length(S);
  while (P2 > P1) and (S[P2] in [#9, ' ']) do
    dec(P2);
  if (P2 <= P1) or (S[P2] <> ')') then
    Exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // ^P2
  dec(P2);
  if (P2 <= P1) then
    Exit;
  while (P2 > P1) and (S[P2] in [#9, ' ']) do
    dec(P2);
  if (P2 <= P1) or (S[P2] <> S[P1]) then
    Exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // ^P2
  inc(P1);
  Value.Data := ALMimeBase64DecodeString(ALTrim(ALCopyStr(S, P1, P2 - P1)));

  // set the result
  result := True;

end;

{ ************************************************************************************* }
function ALJSONDocTryStrToDateTime(const S: AnsiString;
  out Value: TDateTime): Boolean;
var
  aFormatSettings: TALFormatSettings;
  aDateStr: AnsiString;
  aQuoteChar: AnsiChar;
  P1, P2: integer;
begin

  // s must look like
  // new Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // new ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  // ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  result := False;
  aDateStr := ALTrim(S); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
  if alpos('new', aDateStr) = 1 then
    P1 := 4 { length('new') + 1 } // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
    // ^P1
  else
    P1 := 1; // Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
  // ^P1
  while (P1 <= Length(aDateStr)) and (aDateStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 <= Length(aDateStr) - 3) and (aDateStr[P1] = 'D') and
    (aDateStr[P1 + 1] = 'a') and (aDateStr[P1 + 2] = 't') and
    (aDateStr[P1 + 3] = 'e') then
    inc(P1, 4) // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
    // ^P1
  else if (P1 <= Length(aDateStr) - 6) and (aDateStr[P1] = 'I') and
    (aDateStr[P1 + 1] = 'S') and (aDateStr[P1 + 2] = 'O') and
    (aDateStr[P1 + 3] = 'D') and (aDateStr[P1 + 4] = 'a') and
    (aDateStr[P1 + 5] = 't') and (aDateStr[P1 + 6] = 'e') then
    inc(P1, 7) // ISODate ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
    // ^P1
  else
    Exit;
  while (P1 <= Length(aDateStr)) and (aDateStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(aDateStr)) or (aDateStr[P1] <> '(') then
    Exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
  // ^P1
  inc(P1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
  // ^P1
  while (P1 <= Length(aDateStr)) and (aDateStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(aDateStr)) or (not(aDateStr[P1] in ['''', '"'])) then
    Exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
  // ^P1
  aQuoteChar := aDateStr[P1]; // "
  inc(P1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
  // ^P1
  P2 := P1;
  while (P1 <= Length(aDateStr)) and (aDateStr[P1] <> aQuoteChar) do
    inc(P1);
  if (P1 > Length(aDateStr)) then
    Exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
  // ^P1
  aFormatSettings := ALDefaultFormatSettings;
  aFormatSettings.DateSeparator := '-';
  aFormatSettings.TimeSeparator := ':';
  aFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  aFormatSettings.ShortTimeFormat := 'hh:nn:ss.zzz';
  result := ALTryStrToDateTime
    (alStringReplace(alStringReplace(ALCopyStr(aDateStr, P2, P1 - P2), 'T', ' ',
    []), 'Z', '', []), Value, aFormatSettings);
  if not result then
    Exit;

  inc(P1); // // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
  // ^P1
  while (P1 <= Length(aDateStr)) and (aDateStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 <> Length(aDateStr)) or (aDateStr[P1] <> ')') then
  begin
    result := False;
    Exit;
  end;

end;

{ ******************************************************************************************* }
function ALJSONDocTryStrToObjectID(const S: AnsiString;
  out Value: TALJSONObjectID): Boolean;
var
  aObjectIDStr: AnsiString;
  aObjectIDhex: AnsiString;
  aQuoteChar: AnsiChar;
  P1: integer;
begin

  // s must look like
  // ObjectId ( "507f1f77bcf86cd799439011" )
  result := False;
  aObjectIDStr := ALTrim(S); // ObjectId ( "507f1f77bcf86cd799439011" )
  if alpos('ObjectId', aObjectIDStr) <> 1 then
    Exit;
  P1 := 9 { length('ObjectId') + 1 }; // ObjectId ( "507f1f77bcf86cd799439011" )
  // ^P1
  while (P1 <= Length(aObjectIDStr)) and (aObjectIDStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(aObjectIDStr)) or (aObjectIDStr[P1] <> '(') then
    Exit; // ObjectId ( "507f1f77bcf86cd799439011" )
  // ^P1
  inc(P1); // ObjectId ( "507f1f77bcf86cd799439011" )
  // ^P1
  while (P1 <= Length(aObjectIDStr)) and (aObjectIDStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(aObjectIDStr)) or (not(aObjectIDStr[P1] in ['''', '"'])) then
    Exit; // ObjectId ( "507f1f77bcf86cd799439011" )
  // ^P1
  aQuoteChar := aObjectIDStr[P1]; // "
  inc(P1); // ObjectId ( "507f1f77bcf86cd799439011" )
  // ^P1
  if (P1 + 23 { (length(aObjectIDhex)) - 1 } > Length(aObjectIDStr)) then
    Exit;
  setlength(aObjectIDhex, 24);
  aObjectIDhex := allowerCase(ALCopyStr(aObjectIDStr, P1,
    24 { length(aObjectIDhex) } )); // 507f1f77bcf86cd799439011
  inc(P1, 24 { length(aObjectIDhex) } );
  // ObjectId ( "507f1f77bcf86cd799439011" )
  // ^P1
  if (P1 > Length(aObjectIDStr)) or (aObjectIDStr[P1] <> aQuoteChar) then
    Exit; // ObjectId ( "507f1f77bcf86cd799439011" )
  // ^P1
  inc(P1); // ObjectId ( "507f1f77bcf86cd799439011" )
  // ^P1
  while (P1 <= Length(aObjectIDStr)) and (aObjectIDStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 <> Length(aObjectIDStr)) or (aObjectIDStr[P1] <> ')') then
    Exit; // ObjectId ( "507f1f77bcf86cd799439011" )
  // ^P1
  // convert 507f1f77bcf86cd799439011 to binary
  result := HexToBin(PansiChar(aObjectIDhex), @Value[1], Length(Value))
    = Length(Value);

end;

{ ********************************************************************************************* }
function ALJSONDocTryStrToTimestamp(const S: AnsiString;
  out Value: TALBSONTimestamp): Boolean;
var
  aTimestampStr: AnsiString;
  P1, P2: integer;
  aArgs: AnsiString;
  aArg1: integer;
  aArg2: integer;
begin

  // s must look like
  // Timestamp(0, 0)
  result := False;
  aTimestampStr := ALTrim(S);
  if alpos('Timestamp', aTimestampStr) <> 1 then
    Exit;
  P1 := 10 { Length('Timestamp') + 1 }; // Timestamp(0, 0)
  // ^
  while (P1 <= Length(aTimestampStr)) and (aTimestampStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(aTimestampStr)) or (aTimestampStr[P1] <> '(') then
    Exit; // Timestamp(0, 0)
  // ^P1
  P2 := ALPosEx(')', aTimestampStr, P1);
  if P2 <> Length(aTimestampStr) then
    Exit; // Timestamp(0, 0)
  // ^P2
  aArgs := ALTrim(ALCopyStr(aTimestampStr, P1, P2 - P1)); // 0, 0

  // take arguments of function Timestamp
  P1 := alpos(',', aArgs);
  if not ALTryStrToInt(ALTrim(ALCopyStr(aArgs, 1, P1 - 1)), aArg1) then
    Exit;
  if not ALTryStrToInt(ALTrim(ALCopyStr(aArgs, P1 + 1, Maxint)), aArg2) then
    Exit;

  // build result
  result := True;
  Value.W1 := aArg1; // higher 4 bytes - increment
  Value.W2 := aArg2; // lower  4 bytes - timestamp
end;

{ ********************************************************************************** }
function ALJSONDocTryStrToInteger(const S: AnsiString;
  out Value: integer): Boolean;
var
  aNumberStr: AnsiString;
  aTmpStr: AnsiString;
  aQuoteChar: AnsiChar;
  P1, P2: integer;
begin

  // s must look like
  // NumberInt ( "12391293" )
  // NumberInt ( 12391293 )
  // 12391293
  result := ALTryStrToInt(S, Value);
  if result then
    Exit;
  aNumberStr := ALTrim(S); // NumberInt ( "12391293" )
  if alpos('NumberInt', aNumberStr) <> 1 then
    Exit;
  P1 := 10 { length('NumberInt') + 1 }; // NumberInt ( "12391293" )
  // ^P1
  while (P1 <= Length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(aNumberStr)) or (aNumberStr[P1] <> '(') then
    Exit; // NumberInt ( "12391293" )
  // ^P1
  inc(P1); // NumberInt ( "12391293" )
  // ^P1
  while (P1 <= Length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(aNumberStr)) then
    Exit
  else if (not(aNumberStr[P1] in ['''', '"'])) then
  begin // NumberInt ( 12391293 )
    // ^P1
    P2 := P1 + 1;
    while (P2 <= Length(aNumberStr)) and (aNumberStr[P2] in ['0' .. '9']) do
      inc(P2); // NumberInt ( 12391293 )
    // ^P2
    if P2 > Length(aNumberStr) then
      Exit;
    aTmpStr := ALCopyStr(aNumberStr, P1, P2 - P1); // 12391293
    P1 := P2; // NumberInt ( 12391293 )
    // ^P2

    while (P1 <= Length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do
      inc(P1);
    if (P1 <> Length(aNumberStr)) or (aNumberStr[P1] <> ')') then
      Exit; // NumberInt ( "12391293" )
    // ^P1
  end
  else
  begin // NumberInt ( "12391293" )
    // ^P1

    aQuoteChar := aNumberStr[P1]; // "
    inc(P1); // NumberInt ( "12391293" )
    // ^P1
    P2 := ALPosEx(aQuoteChar, aNumberStr, P1);
    if P2 <= P1 then
      Exit;
    aTmpStr := ALCopyStr(aNumberStr, P1, P2 - P1); // 12391293
    P1 := P2 + 1; // NumberInt ( "12391293" )
    // ^P1
    while (P1 <= Length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do
      inc(P1);
    if (P1 <> Length(aNumberStr)) or (aNumberStr[P1] <> ')') then
      Exit; // NumberInt ( "12391293" )
    // ^P1
  end;

  // convert 12391293 to integer
  result := ALTryStrToInt(aTmpStr, Value);

end;

{ ****************************************************************************** }
function ALJSONDocTryStrToInt64(const S: AnsiString; out Value: Int64): Boolean;
var
  aNumberStr: AnsiString;
  aTmpStr: AnsiString;
  aQuoteChar: AnsiChar;
  P1, P2: integer;
begin

  // s must look like
  // NumberLong ( "12391293" )
  // NumberLong ( 12391293 )
  // 12391293
  result := ALTryStrToInt64(S, Value);
  if result then
    Exit;
  aNumberStr := ALTrim(S); // NumberLong ( "12391293" )
  if alpos('NumberLong', aNumberStr) <> 1 then
    Exit;
  P1 := 11 { length('NumberLong') + 1 }; // NumberLong ( "12391293" )
  // ^P1
  while (P1 <= Length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(aNumberStr)) or (aNumberStr[P1] <> '(') then
    Exit; // NumberLong ( "12391293" )
  // ^P1
  inc(P1); // NumberLong ( "12391293" )
  // ^P1
  while (P1 <= Length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do
    inc(P1);
  if (P1 > Length(aNumberStr)) then
    Exit
  else if (not(aNumberStr[P1] in ['''', '"'])) then
  begin // NumberLong ( 12391293 )
    // ^P1
    P2 := P1 + 1;
    while (P2 <= Length(aNumberStr)) and (aNumberStr[P2] in ['0' .. '9']) do
      inc(P2); // NumberLong ( 12391293 )
    // ^P2
    if P2 > Length(aNumberStr) then
      Exit;
    aTmpStr := ALCopyStr(aNumberStr, P1, P2 - P1); // 12391293
    P1 := P2; // NumberLong ( 12391293 )
    // ^P2

    while (P1 <= Length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do
      inc(P1);
    if (P1 <> Length(aNumberStr)) or (aNumberStr[P1] <> ')') then
      Exit; // NumberLong ( "12391293" )
    // ^P1
  end
  else
  begin // NumberLong ( "12391293" )
    // ^P1

    aQuoteChar := aNumberStr[P1]; // "
    inc(P1); // NumberLong ( "12391293" )
    // ^P1
    P2 := ALPosEx(aQuoteChar, aNumberStr, P1);
    if P2 <= P1 then
      Exit;
    aTmpStr := ALCopyStr(aNumberStr, P1, P2 - P1); // 12391293
    P1 := P2 + 1; // NumberLong ( "12391293" )
    // ^P1
    while (P1 <= Length(aNumberStr)) and (aNumberStr[P1] in [#9, ' ']) do
      inc(P1);
    if (P1 <> Length(aNumberStr)) or (aNumberStr[P1] <> ')') then
      Exit; // NumberLong ( "12391293" )
    // ^P1
  end;

  // convert 12391293 to integer
  result := ALTryStrToInt64(aTmpStr, Value);

end;

{ **************************************************** }
procedure ALJSONDocError(const Msg: String); overload;
begin
  raise EALJSONDocError.Create(Msg);
end;

{ ******************************************************************************** }
procedure ALJSONDocError(const Msg: String;
  const Args: array of const); overload;
begin
  raise EALJSONDocError.CreateFmt(Msg, Args);
end;

{ *********************************************************************************** }
function ALNodeMatches(const Node: TALJSONNode;
  const NodeName: AnsiString): Boolean;
begin
  result := (Node.NodeName = NodeName);
end;

{ ******************************************************************************************** }
{ Call CreateNode to create a new generic JSON node. The resulting node does not have a parent,
  but can be added to the ChildNodes list of any node in the document. }
function ALCreateJSONNode(const NodeName: AnsiString; NodeType: TALJSONNodeType)
  : TALJSONNode;
begin
  case NodeType of
    ntObject:
      result := TALJSONObjectNode.Create(NodeName);
    ntArray:
      result := TALJSONArrayNode.Create(NodeName);
    ntText:
      result := TALJSONTextNode.Create(NodeName);
  else
    begin
      result := nil; // for hide warning
      ALJSONDocError(cALJSONInvalidNodeType);
    end;
  end;
end;

{ **************************************************************** }
constructor TALJSONDocument.Create(const aActive: Boolean = True);
begin
  inherited Create;
  FDocumentNode := nil;
  FParseOptions := [];
  fPathSeparator := '.';
  FOnParseStartDocument := nil;
  FOnParseEndDocument := nil;
  FonParseText := nil;
  FonParseStartObject := nil;
  FonParseEndObject := nil;
  FonParseStartArray := nil;
  FonParseEndArray := nil;
  FOptions := [];
  NodeIndentStr := CALDefaultNodeIndent;
  fFormatSettings := ALDefaultFormatSettings;
  FTag := 0;
  SetActive(aActive);
end;

{ ********************************************************************************************************** }
constructor TALJSONDocument.Create(const aFormatSettings: TALFormatSettings;
  const aActive: Boolean = True);
begin
  Create(aActive);
  fFormatSettings := aFormatSettings;
end;

{ ********************************* }
destructor TALJSONDocument.Destroy;
begin
  ReleaseDoc;
  inherited;
end;

{ **************************************** }
{ Returns the value of the Active property.
  GetActive is the read implementation of the Active property. }
function TALJSONDocument.GetActive: Boolean;
begin
  result := Assigned(FDocumentNode);
end;

{ ************************************* }
{ Sets the value of the Active property.
  SetActive is the write implementation of the Active property.
  *Value is the new value to set. }
procedure TALJSONDocument.SetActive(const Value: Boolean);
begin
  if Value <> GetActive then
  begin
    if Value then
    begin
      FDocumentNode := TALJSONObjectNode.Create;
      FDocumentNode.OwnerDocument := Self;
    end
    else
      ReleaseDoc;
  end;
end;

{ *****=******** }
{ The JSON format
  There are just a few rules that you need to remember:
  *Objects are encapsulated within opening and closing brackets { } {
  *An empty object can be represented by { } {
  *Arrays are encapsulated within opening and closing square brackets [ ]
  *An empty array can be represented by [ ]
  *A member is represented by a key-value pair
  *The key of a member should be contained in double quotes. (JavaScript does not require this. JavaScript and some parsers will tolerate single-quotes)
  *Each member should have a unique key within an object structure
  *The value of a member must be contained in double quotes if it's a string (JavaScript and some parsers will tolerates single-quotes)
  *Boolean values are represented using the true or false literals in lower case
  *Number values are represented using double-precision floating-point format. Scientific notation is supported
  *Numbers should not have leading zeroes
  *"Offensive"" characters in a string need to be escaped using the backslash character
  *Null values are represented by the null literal in lower case
  *Other object types, such as dates, are not properly supported and should be converted to strings. It becomes the responsability of the parser/client to manage this.
  *Each member of an object or each array value must be followed by a comma if it's not the last one
  *The common extension for json files is '.json'
  *The mime type for json files is 'application/json' }
Procedure TALJSONDocument.ParseJSONStream(Const RawJSONStream: TStream;
  Const ContainerNode: TALJSONNode);

Const
  BufferSize: integer = 8192;
Var
  RawJSONString: AnsiString;
  RawJSONStringLength: integer;
  RawJSONStringPos: integer;
  NotSaxMode: Boolean;
  WorkingNode: TALJSONNode;
  DecodeJSONReferences: Boolean;
  UseContainerNodeInsteadOfAddingChildNode: Boolean;
  Paths: TALStringList;
  ArrayIdx: integer;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  function ExpandRawJSONString: Boolean; overload;
  Var
    ByteReaded, Byte2Read: integer;
  Begin
    If (RawJSONStringLength > 0) and (RawJSONStringPos > 1) then
    begin
      if (RawJSONStringPos > RawJSONStringLength) then
        RawJSONStream.Position := RawJSONStream.Position - RawJSONStringLength +
          RawJSONStringPos - 1;
      Byte2Read := min(RawJSONStringPos - 1, RawJSONStringLength);
      if RawJSONStringPos <= Length(RawJSONString) then
        ALMove(RawJSONString[RawJSONStringPos], RawJSONString[1],
          RawJSONStringLength - RawJSONStringPos + 1);
      RawJSONStringPos := 1;
    end
    else
    begin
      Byte2Read := BufferSize;
      RawJSONStringLength := RawJSONStringLength + BufferSize;
      setlength(RawJSONString, RawJSONStringLength);
    end;

    // range check error is we not do so
    if RawJSONStream.Position < RawJSONStream.Size then
      ByteReaded := RawJSONStream.
        Read(RawJSONString[RawJSONStringLength - Byte2Read + 1], Byte2Read)
    else
      ByteReaded := 0;

    If ByteReaded <> Byte2Read then
    begin
      RawJSONStringLength := RawJSONStringLength - Byte2Read + ByteReaded;
      setlength(RawJSONString, RawJSONStringLength);
      result := ByteReaded > 0;
    end
    else
      result := True;
  end;

{ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  function ExpandRawJSONString(var PosToKeepSync: integer): Boolean; overload;
  var
    P1: integer;
  begin
    P1 := RawJSONStringPos;
    result := ExpandRawJSONString;
    PosToKeepSync := PosToKeepSync - (P1 - RawJSONStringPos);
  end;

{ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  function GetPathStr(Const ExtraItems: AnsiString = ''): AnsiString;
  var
    i, L, P, Size: integer;
    S, LB: AnsiString;
  begin
    LB := PathSeparator;
    Size := Length(ExtraItems);
    if Size <> 0 then
      inc(Size, Length(LB));
    for i := 1 to Paths.Count - 1 do
      inc(Size, Length(Paths[i]) + Length(LB));
    setlength(result, Size);
    P := 1;
    for i := 1 to Paths.Count - 1 do
    begin
      S := Paths[i];
      L := Length(S);
      if L <> 0 then
      begin
        ALMove(S[1], result[P], L);
        inc(P, L);
      end;
      L := Length(LB);
      if (L <> 0) and ((i <> Paths.Count - 1) or (ExtraItems <> '')) and
        (((NotSaxMode) and (TALJSONNode(Paths.Objects[i]).NodeType <> ntArray))
        or ((not NotSaxMode) and (integer(Paths.Objects[i]) <> 2 { ntarray } )))
      then
      begin
        ALMove(LB[1], result[P], L);
        inc(P, L);
      end;
    end;
    if ExtraItems <> '' then
    begin
      L := Length(ExtraItems);
      ALMove(ExtraItems[1], result[P], L);
      inc(P, L);
    end;
    setlength(result, P - 1);
  end;

{ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  Function GetNodeSubTypeFromStrValue(const aStrValue: AnsiString;
    const AQuotedValue: Boolean): TALJSONNodeSubType;
  var
    aDT: TDateTime;
    aObjectID: TALJSONObjectID;
    aTimestamp: TALBSONTimestamp;
    aRegEx: TALJSONRegEx;
    aBinary: TALJSONBinary;
    aInt32: integer;
    aInt64: Int64;
  begin
    if AQuotedValue then
      result := nstText
    else if ALIsFloat(aStrValue, ALDefaultFormatSettings) then
      result := nstFloat
    else if ALJSONDocTryStrToInteger(aStrValue, aInt32) then
      result := nstInt32
    else if ALJSONDocTryStrToInt64(aStrValue, aInt64) then
      result := nstInt64
    else if alSameText(aStrValue, 'null') then
      result := nstNull
    else if alSameText(aStrValue, 'true') or alSameText(aStrValue, 'false') then
      result := nstBoolean
    else if ALJSONDocTryStrToDateTime(aStrValue, aDT) then
      result := nstDateTime
    else if ALJSONDocTryStrToTimestamp(aStrValue, aTimestamp) then
      result := nstTimestamp
    else if ALJSONDocTryStrToObjectID(aStrValue, aObjectID) then
      result := nstObjectID
    else if ALJSONDocTryStrTobinary(aStrValue, aBinary) then
      result := nstBinary
    else if ALJSONDocTryStrToRegEx(aStrValue, aRegEx) then
      result := nstRegEx
    else
      result := nstJavascript;
  end;

{ ~~~~~~~~~~~~~~~~~~~~ }
  procedure AnalyzeNode;
  Var
    aName, aValue: AnsiString;
    aNode: TALJSONNode;
    aQuoteChar: AnsiChar;
    aNameValueSeparator: AnsiChar;
    aNodeTypeInt: integer;
    aNodeSubType: TALJSONNodeSubType;
    aInSingleQuote: Boolean;
    aInDoubleQuote: Boolean;
    aInSquareBracket: integer;
    aInRoundBracket: integer;
    aInCurlyBracket: integer;
    P1: integer;
  Begin

{$IFDEF undef}{$REGION 'end Object/Array'}{$ENDIF}
    // ... } ....
    // ... ] ....
    if RawJSONString[RawJSONStringPos] in ['}', ']'] then
    begin // ... } ...
      // ^RawJsonStringPos

      // Reset the ArrayIdx
      ArrayIdx := -1;

      // error if Paths.Count = 0 (mean one end object/array without any starting)
      if (Paths.Count = 0) then
        ALJSONDocError(cALJSONParseError);

      // if we are not in sax mode
      if NotSaxMode then
      begin

        // init anode to one level up
        aNode := TALJSONNode(Paths.Objects[Paths.Count - 1]);

        // if anode <> workingNode aie aie aie
        if (aNode <> WorkingNode) then
          ALJSONDocError(cALJSONParseError);

        // check that the end object/array correspond to the aNodeType
        if ((RawJSONString[RawJSONStringPos] = '}') and
          (aNode.NodeType <> ntObject)) or
          ((RawJSONString[RawJSONStringPos] = ']') and
          (aNode.NodeType <> ntArray)) then
          ALJSONDocError(cALJSONParseError);

        // if working node <> containernode then we can go to one level up
        If WorkingNode <> ContainerNode then
        begin

          // init WorkingNode to the parentNode
          WorkingNode := WorkingNode.ParentNode;

          // update ArrayIdx if WorkingNode.NodeType = ntArray
          if WorkingNode.NodeType = ntArray then
            ArrayIdx := ALStrToInt(ALCopyStr(Paths[Paths.Count - 1], 2,
              Length(Paths[Paths.Count - 1]) - 2)) + 1;

        end

        // if working node = containernode then we can no go to the parent node so set WorkingNode to nil
        Else
          WorkingNode := nil;

      end

      // if we are in sax mode
      else
      begin

        // init aNodeTypeInt (we use this as flag in the Paths)
        if RawJSONString[RawJSONStringPos] = '}' then
          aNodeTypeInt := 1
        else
          aNodeTypeInt := 2;

        // check that the end object/array correspond to the aNodeType
        if integer(Paths.Objects[Paths.Count - 1]) <> aNodeTypeInt then
          ALJSONDocError(cALJSONParseError);

        // update ArrayIdx if WorkingNode.NodeType = ntArray
        if (Paths.Count >= 2) and
          (integer(Paths.Objects[Paths.Count - 2]) = 2 { ntarray } ) then
          ArrayIdx := ALStrToInt(ALCopyStr(Paths[Paths.Count - 1], 2,
            Length(Paths[Paths.Count - 1]) - 2)) + 1;

      end;

      // call the DoParseEndObject/array event
      if RawJSONString[RawJSONStringPos] = '}' then
        DoParseEndObject(GetPathStr, Paths[Paths.Count - 1])
      else
        DoParseEndArray(GetPathStr, Paths[Paths.Count - 1]);

      // delete the last entry from the path
      Paths.Delete(Paths.Count - 1);

      // update rawJSONStringPos
      RawJSONStringPos := RawJSONStringPos + 1; // ... } ...
      // ^RawJsonStringPos

      // finallly exit from this procedure, everything was done
      Exit;

    end;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Begin Object/Array Without NAME'}{$ENDIF}
    // ... { ....
    // ... [ ....
    if RawJSONString[RawJSONStringPos] in ['{', '['] then
    begin // ... { ...
      // ^RawJsonStringPos

      // if we are not in sax mode
      if NotSaxMode then
      begin

        // if workingnode = nil then it's mean we are outside the containerNode
        if not Assigned(WorkingNode) then
          ALJSONDocError(cALJSONParseError);

        // we are at the very beginning of the json document
        If UseContainerNodeInsteadOfAddingChildNode then
        begin

          // check that we are in object
          if RawJSONString[RawJSONStringPos] <> '{' then
            ALJSONDocError(cALJSONParseError);

          // inform to next loop that we will be not anymore at the very beginning of the json document
          UseContainerNodeInsteadOfAddingChildNode := False;

        end

        // we are inside the json document
        else
        begin

          // Node without name can be ONLY present inside an array node
          if (ArrayIdx < 0) or (WorkingNode.NodeType <> ntArray) then
            ALJSONDocError(cALJSONParseError);

          // create the node according the the braket char and add it to the workingnode
          if RawJSONString[RawJSONStringPos] = '{' then
            aNode := CreateNode('', ntObject)
          else
            aNode := CreateNode('', ntArray);
          try
            WorkingNode.ChildNodes.Add(aNode);
          except
            aNode.Free;
            raise;
          end;

          // set that the current working node will be now the new node newly created
          WorkingNode := aNode;

        end;

        // update the path
        Paths.AddObject('[' + alinttostr(ArrayIdx) + ']', WorkingNode);

      end

      // if we are in sax mode
      else
      begin

        // we are at the very beginning of the json document
        If UseContainerNodeInsteadOfAddingChildNode then
        begin

          // check that we are in object
          if RawJSONString[RawJSONStringPos] <> '{' then
            ALJSONDocError(cALJSONParseError);

          // inform to next loop that we will be not anymore at the very beginning of the json document
          UseContainerNodeInsteadOfAddingChildNode := False;

        end

        // we are inside the json document
        else
        begin

          // Node without name can be ONLY present inside an array node
          if (ArrayIdx < 0) or
            ((Paths.Count = 0) or (integer(Paths.Objects[Paths.Count - 1]) <>
            2 { array } )) then
            ALJSONDocError(cALJSONParseError);

        end;

        // update the path
        if RawJSONString[RawJSONStringPos] = '{' then
          aNodeTypeInt := 1
        else
          aNodeTypeInt := 2;
        Paths.AddObject('[' + alinttostr(ArrayIdx) + ']',
          pointer(aNodeTypeInt));

      end;

      // call the DoParseStartObject/array event
      if RawJSONString[RawJSONStringPos] = '{' then
      begin
        DoParseStartObject(GetPathStr, '');
        ArrayIdx := -1;
      end
      else
      begin
        DoParseStartArray(GetPathStr, '');
        ArrayIdx := 0;
      end;

      // update rawJSONStringPos
      RawJSONStringPos := RawJSONStringPos + 1; // ... { ...
      // ^RawJsonStringPos

      // finallly exit from this procedure, everything was done
      Exit;

    end;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'error if UseContainerNodeInsteadOfAddingChildNode'}{$ENDIF}
    If UseContainerNodeInsteadOfAddingChildNode then
      ALJSONDocError(cALJSONParseError);
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'extract the quoted name part'}{$ENDIF}
    // "" : ""
    // "name" : "value"
    // "name" : 1.23
    // "name" : true
    // "name" : false
    // "name" : null
    // "name" : ISODATE('1/1/2001')
    // "name" : function(){return(new Date).getTime()}, ...}
    // "name" : new Date(''Dec 03, 1924'')
    // "name" : { ... }
    // "name" : [ ... ]
    // 'name' : '...'
    // "value"
    // 'value'
    aQuoteChar := #0;
    if RawJSONString[RawJSONStringPos] in ['"', ''''] then
    begin // ... " ...
      // ^RawJsonStringPos
      aQuoteChar := RawJSONString[RawJSONStringPos]; // "
      P1 := RawJSONStringPos + 1; // ... "...\"..."
      // ^P1
      If P1 + 1 > RawJSONStringLength then
        ExpandRawJSONString(P1);
      While P1 <= RawJSONStringLength do
      begin

        If (P1 < RawJSONStringLength) and (RawJSONString[P1] = '\') and
          (RawJSONString[P1 + 1] = aQuoteChar) then
          inc(P1, 2) // ... "...\"..."
          // ^^^P1
        else if RawJSONString[P1] = aQuoteChar then
        begin
          if DecodeJSONReferences then
            aName := ALUTF8JavascriptDecode(ALCopyStr(RawJSONString,
              RawJSONStringPos + 1, P1 - RawJSONStringPos - 1)) // ..."...
          else
            aName := ALCopyStr(RawJSONString, RawJSONStringPos + 1,
              P1 - RawJSONStringPos - 1); // ...\"...
          break;
        end
        else
          inc(P1); // ... "...\"..."
        // ^^^^^^^^^P1

        if P1 + 1 > RawJSONStringLength then
          ExpandRawJSONString(P1);

      end;
      if P1 > RawJSONStringLength then
        ALJSONDocError(cALJSONParseError);
      RawJSONStringPos := P1 + 1; // ... "...\"..."
      // ^^^^^^^^^^RawJsonStringPos
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'extract the unquoted name part'}{$ENDIF}
    // name : "value"
    // name : 1.23
    // name : true
    // name : false
    // name : null
    // name : ISODATE('1/1/2001')
    // name : function(){return(new Date).getTime()}, ...}
    // name : new Date('Dec 03, 1924')
    // name : { ... }
    // name : [ ... ]
    // 1.23
    // true
    // false
    // null
    // ISODATE('1/1/2001')
    // function(){return(new Date).getTime()}, ...}
    // new Date('Dec 03, 1924')
    else
    begin

      aInSingleQuote := False;
      aInDoubleQuote := False;
      aInSquareBracket := 0;
      aInRoundBracket := 0;
      aInCurlyBracket := 0;

      P1 := RawJSONStringPos; // ... new Date('Dec 03, 1924'), ....
      // ^P1
      If P1 > RawJSONStringLength then
        ExpandRawJSONString(P1);
      While P1 <= RawJSONStringLength do
      begin

        if (not aInSingleQuote) and (not aInDoubleQuote) and
          (aInSquareBracket = 0) and (aInRoundBracket = 0) and
          (aInCurlyBracket = 0) and (RawJSONString[P1] in [',', '}', ']', ':'])
        then
        begin
          aName := ALTrim(ALCopyStr(RawJSONString, RawJSONStringPos,
            P1 - RawJSONStringPos)); // new Date('Dec 03, 1924')
          break;
        end
        else if (RawJSONString[P1] = '"') then
        begin
          if (P1 <= 1) or (RawJSONString[P1 - 1] <> '\') then
            aInDoubleQuote := (not aInDoubleQuote) and (not aInSingleQuote);
        end
        else if (RawJSONString[P1] = '''') then
        begin
          if (P1 <= 1) or (RawJSONString[P1 - 1] <> '\') then
            aInSingleQuote := (not aInSingleQuote) and (not aInDoubleQuote)
        end
        else if (not aInSingleQuote) and (not aInDoubleQuote) then
        begin
          if (RawJSONString[P1] = '[') then
            inc(aInSquareBracket)
          else if (RawJSONString[P1] = ']') then
            dec(aInSquareBracket)
          else if (RawJSONString[P1] = '(') then
            inc(aInRoundBracket)
          else if (RawJSONString[P1] = ')') then
            dec(aInRoundBracket)
          else if (RawJSONString[P1] = '}') then
            inc(aInCurlyBracket)
          else if (RawJSONString[P1] = '{') then
            dec(aInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
        // ^^^^^^^^^^^^^^^^^^^^^^^^^P1

        if P1 > RawJSONStringLength then
          ExpandRawJSONString(P1);

      end;
      if P1 > RawJSONStringLength then
        ALJSONDocError(cALJSONParseError);
      RawJSONStringPos := P1; // ... new Date('Dec 03, 1924'), ....
      // ^RawJsonStringPos

    end;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'extract the name value separator part'}{$ENDIF}
    aNameValueSeparator := #0;
    if RawJSONStringPos > RawJSONStringLength then
      ExpandRawJSONString;
    While RawJSONStringPos <= RawJSONStringLength do
    begin
      If RawJSONString[RawJSONStringPos] in [' ', #13, #10, #9] then
        inc(RawJSONStringPos)
      else
      begin
        aNameValueSeparator := RawJSONString[RawJSONStringPos];
        break;
      end;
      if RawJSONStringPos > RawJSONStringLength then
        ExpandRawJSONString;
    end;
    if RawJSONStringPos > RawJSONStringLength then
      ALJSONDocError(cALJSONParseError); // .... : ....
    // ^RawJSONStringPos
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'if aNameValueSeparator is absent then it is just a value'}{$ENDIF}
    if aNameValueSeparator <> ':' then
    begin

      // init aNodeSubType
      aNodeSubType := GetNodeSubTypeFromStrValue(aName,
        aQuoteChar in ['"', '''']);

      // if we are not in sax mode
      if NotSaxMode then
      begin

        // if workingnode = nil then it's mean we are outside the containerNode
        if not Assigned(WorkingNode) then
          ALJSONDocError(cALJSONParseError);

        // Node without name can be ONLY present inside an array node
        if (ArrayIdx < 0) or (WorkingNode.NodeType <> ntArray) then
          ALJSONDocError(cALJSONParseError);

        // create the node and add it to the workingnode
        aNode := CreateNode('', ntText);
        try
          aNode.SetNodeValue(aName, aNodeSubType);
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;

      end

      // we are inside the json document
      else
      begin

        // Node without name can be ONLY present inside an array node
        if (ArrayIdx < 0) or
          ((Paths.Count = 0) or (integer(Paths.Objects[Paths.Count - 1]) <>
          2 { array } )) then
          ALJSONDocError(cALJSONParseError);

      end;

      // call the DoParseText event
      DoParseText(GetPathStr('[' + alinttostr(ArrayIdx)) + ']', '', aName,
        aNodeSubType);

      // increase the ArrayIdx
      inc(ArrayIdx);

      // finallly exit from this procedure, everything was done
      Exit;

    end;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'remove the blank space between the name valueeparator and the value'}{$ENDIF}
    inc(RawJSONStringPos); // ... : ....
    // ^RawJSONStringPos
    if RawJSONStringPos > RawJSONStringLength then
      ExpandRawJSONString;
    While RawJSONStringPos <= RawJSONStringLength do
    begin
      If RawJSONString[RawJSONStringPos] in [' ', #13, #10, #9] then
        inc(RawJSONStringPos)
      else
        break;
      if RawJSONStringPos > RawJSONStringLength then
        ExpandRawJSONString;
    end;
    if RawJSONStringPos > RawJSONStringLength then
      ALJSONDocError(cALJSONParseError); // .... " ....
    // ^RawJSONStringPos
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'if the value is an object/array'}{$ENDIF}
    // name : { ... }
    // name : [ ... ]
    if RawJSONString[RawJSONStringPos] in ['{', '['] then
    begin // ... { ...
      // ^RawJsonStringPos

      // if we are not in sax mode
      if NotSaxMode then
      begin

        // if workingnode = nil then it's mean we are outside the containerNode
        if not Assigned(WorkingNode) then
          ALJSONDocError(cALJSONParseError);

        // Node withe name MUST be ONLY present inside an object node
        if (ArrayIdx >= 0) or (WorkingNode.NodeType <> ntObject) then
          ALJSONDocError(cALJSONParseError);

        // create the node according the the braket char and add it to the workingnode
        if RawJSONString[RawJSONStringPos] = '{' then
          aNode := CreateNode(aName, ntObject)
        else
          aNode := CreateNode(aName, ntArray);
        try
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;

        // set that the current working node will be now the new node newly created
        WorkingNode := aNode;

        // update the path
        Paths.AddObject(aName, WorkingNode);

      end

      // if we are in sax mode
      else
      begin

        // Node withe name MUST be ONLY present inside an object node
        if (ArrayIdx >= 0) or
          ((Paths.Count > 0) and (integer(Paths.Objects[Paths.Count - 1]) <>
          1 { object } )) then
          ALJSONDocError(cALJSONParseError);

        // update the path
        if RawJSONString[RawJSONStringPos] = '{' then
          aNodeTypeInt := 1
        else
          aNodeTypeInt := 2;
        Paths.AddObject(aName, pointer(aNodeTypeInt));

      end;

      // call the DoParseStartObject/array event and update the ArrayIdx if it's an array
      if RawJSONString[RawJSONStringPos] = '{' then
        DoParseStartObject(GetPathStr, aName)
      else
      begin
        DoParseStartArray(GetPathStr, aName);
        ArrayIdx := 0;
      end;

      // update rawJSONStringPos
      RawJSONStringPos := RawJSONStringPos + 1; // ... { ...
      // ^RawJsonStringPos

      // finallly exit from this procedure, everything was done
      Exit;

    end;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'if the value is a quoted string'}{$ENDIF}
    // name : "value"
    // name : 'value'
    aQuoteChar := #0;
    if RawJSONString[RawJSONStringPos] in ['"', ''''] then
    begin // ... " ...
      // ^RawJsonStringPos
      aQuoteChar := RawJSONString[RawJSONStringPos]; // "
      P1 := RawJSONStringPos + 1; // ... "...\"..."
      // ^P1
      If P1 + 1 > RawJSONStringLength then
        ExpandRawJSONString(P1);
      While P1 <= RawJSONStringLength do
      begin

        If (P1 < RawJSONStringLength) and (RawJSONString[P1] = '\') and
          (RawJSONString[P1 + 1] = aQuoteChar) then
          inc(P1, 2) // ... "...\"..."
          // ^^^P1
        else if RawJSONString[P1] = aQuoteChar then
        begin
          if DecodeJSONReferences then
            aValue := ALUTF8JavascriptDecode(ALCopyStr(RawJSONString,
              RawJSONStringPos + 1, P1 - RawJSONStringPos - 1)) // ..."...
          else
            aValue := ALCopyStr(RawJSONString, RawJSONStringPos + 1,
              P1 - RawJSONStringPos - 1); // ...\"...
          break;
        end
        else
          inc(P1); // ... "...\"..."
        // ^^^^^^^^^P1

        if P1 + 1 > RawJSONStringLength then
          ExpandRawJSONString(P1);

      end;
      if P1 > RawJSONStringLength then
        ALJSONDocError(cALJSONParseError);
      RawJSONStringPos := P1 + 1; // ... "...\"..."
      // ^^^^^^^^^^RawJsonStringPos

    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'if the value is a UNquoted string'}{$ENDIF}
    // name : 1.23
    // name : true
    // name : false
    // name : null
    // name : ISODATE('1/1/2001')
    // name : function(){return(new Date).getTime()}, ...}
    // name : new Date(''Dec 03, 1924'')
    else
    begin

      aInSingleQuote := False;
      aInDoubleQuote := False;
      aInSquareBracket := 0;
      aInRoundBracket := 0;
      aInCurlyBracket := 0;

      P1 := RawJSONStringPos; // ... new Date('Dec 03, 1924'), ....
      // ^P1
      If P1 > RawJSONStringLength then
        ExpandRawJSONString(P1);
      While P1 <= RawJSONStringLength do
      begin

        if (not aInSingleQuote) and (not aInDoubleQuote) and
          (aInSquareBracket = 0) and (aInRoundBracket = 0) and
          (aInCurlyBracket = 0) and (RawJSONString[P1] in [',', '}', ']']) then
        begin
          aValue := ALTrim(ALCopyStr(RawJSONString, RawJSONStringPos,
            P1 - RawJSONStringPos)); // new Date('Dec 03, 1924')
          break;
        end
        else if (RawJSONString[P1] = '"') then
        begin
          if (P1 <= 1) or (RawJSONString[P1 - 1] <> '\') then
            aInDoubleQuote := (not aInDoubleQuote) and (not aInSingleQuote);
        end
        else if (RawJSONString[P1] = '''') then
        begin
          if (P1 <= 1) or (RawJSONString[P1 - 1] <> '\') then
            aInSingleQuote := (not aInSingleQuote) and (not aInDoubleQuote)
        end
        else if (not aInSingleQuote) and (not aInDoubleQuote) then
        begin
          if (RawJSONString[P1] = '[') then
            inc(aInSquareBracket)
          else if (RawJSONString[P1] = ']') then
            dec(aInSquareBracket)
          else if (RawJSONString[P1] = '(') then
            inc(aInRoundBracket)
          else if (RawJSONString[P1] = ')') then
            dec(aInRoundBracket)
          else if (RawJSONString[P1] = '}') then
            inc(aInCurlyBracket)
          else if (RawJSONString[P1] = '{') then
            dec(aInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
        // ^^^^^^^^^^^^^^^^^^^^^^^^^P1

        if P1 > RawJSONStringLength then
          ExpandRawJSONString(P1);

      end;
      if P1 > RawJSONStringLength then
        ALJSONDocError(cALJSONParseError);
      RawJSONStringPos := P1; // ... new Date('Dec 03, 1924'), ....
      // ^RawJsonStringPos

    end;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'create the named text node'}{$ENDIF}
    // init aNodeSubType
    aNodeSubType := GetNodeSubTypeFromStrValue(aValue,
      aQuoteChar in ['"', '''']);

    // if we are not in sax mode
    if NotSaxMode then
    begin

      // if workingnode = nil then it's mean we are outside the containerNode
      if not Assigned(WorkingNode) then
        ALJSONDocError(cALJSONParseError);

      // Node withe name MUST be ONLY present inside an object node
      if (ArrayIdx >= 0) or (WorkingNode.NodeType <> ntObject) then
        ALJSONDocError(cALJSONParseError);

      // create the node and add it to the workingnode
      aNode := CreateNode(aName, ntText);
      try
        aNode.SetNodeValue(aValue, aNodeSubType);
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;

    end

    // if we are in sax mode
    else
    begin

      // Node with name MUST be ONLY present inside an object node
      if (ArrayIdx >= 0) or
        ((Paths.Count > 0) and (integer(Paths.Objects[Paths.Count - 1]) <>
        1 { object } )) then
        ALJSONDocError(cALJSONParseError);

    end;

    // call the DoParseStartObject/array event
    DoParseText(GetPathStr(aName), aName, aValue, aNodeSubType);
{$IFDEF undef}{$ENDREGION}{$ENDIF}
  end;

Begin

  //
  // NOTE: the childNodes of the ContainerNode
  // must have been cleared by the calling function!
  //
  // NOTE: ContainerNode must have fDocument assigned
  //
  // NOTE: ContainerNode must be ntobject or nil (sax mode)
  //

  Paths := TALStringList.Create;
  Try

    DoParseStartDocument;

    ArrayIdx := -1;
    WorkingNode := ContainerNode;
    NotSaxMode := Assigned(ContainerNode);
    UseContainerNodeInsteadOfAddingChildNode := True;
    DecodeJSONReferences := not(poIgnoreControlCharacters in ParseOptions);
    RawJSONString := '';
    RawJSONStringLength := 0;
    RawJSONStringPos := 1;
    ExpandRawJSONString;
    if AlUTF8DetectBOM(PansiChar(RawJSONString), Length(RawJSONString)) then
      RawJSONStringPos := 4;

    While RawJSONStringPos <= RawJSONStringLength do
    begin

      If RawJSONString[RawJSONStringPos] in [' ', ',', #13, #10, #9] then
        inc(RawJSONStringPos)
      else
        AnalyzeNode;

      if RawJSONStringPos > RawJSONStringLength then
        ExpandRawJSONString;

    end;

    // some tags are not closed
    if Paths.Count > 0 then
      ALJSONDocError(cALJSONParseError);

    // mean the node was not update (empty stream?)
    if UseContainerNodeInsteadOfAddingChildNode then
      ALJSONDocError(cALJSONParseError);

    DoParseEndDocument;

  finally
    Paths.Free;
  end;

end;

{ ************************************************************* }
{ Last version of the spec: http://bsonspec.org/#/specification }
procedure TALJSONDocument.ParseBSONStream(const RawBSONStream: TStream;
  const ContainerNode: TALJSONNode);

Const
  BufferSize: integer = 8192;
Var
  RawBSONString: AnsiString;
  RawBSONStringLength: integer;
  RawBSONStringPos: integer;
  NotSaxMode: Boolean;
  WorkingNode: TALJSONNode;
  Paths: TALStringList;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  function ExpandRawBSONString: Boolean; overload;
  Var
    ByteReaded, Byte2Read: integer;
  Begin
    If (RawBSONStringLength > 0) and (RawBSONStringPos > 1) then
    begin
      if (RawBSONStringPos > RawBSONStringLength) then
        RawBSONStream.Position := RawBSONStream.Position - RawBSONStringLength +
          RawBSONStringPos - 1;
      Byte2Read := min(RawBSONStringPos - 1, RawBSONStringLength);
      if RawBSONStringPos <= Length(RawBSONString) then
        ALMove(RawBSONString[RawBSONStringPos], RawBSONString[1],
          RawBSONStringLength - RawBSONStringPos + 1);
      RawBSONStringPos := 1;
    end
    else
    begin
      Byte2Read := BufferSize;
      RawBSONStringLength := RawBSONStringLength + BufferSize;
      setlength(RawBSONString, RawBSONStringLength);
    end;

    // range check error is we not do so
    if RawBSONStream.Position < RawBSONStream.Size then
      ByteReaded := RawBSONStream.
        Read(RawBSONString[RawBSONStringLength - Byte2Read + 1], Byte2Read)
    else
      ByteReaded := 0;

    If ByteReaded <> Byte2Read then
    begin
      RawBSONStringLength := RawBSONStringLength - Byte2Read + ByteReaded;
      setlength(RawBSONString, RawBSONStringLength);
      result := ByteReaded > 0;
    end
    else
      result := True;
  end;

{ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  function ExpandRawBSONString(var PosToKeepSync: integer): Boolean; overload;
  var
    P1: integer;
  begin
    P1 := RawBSONStringPos;
    result := ExpandRawBSONString;
    PosToKeepSync := PosToKeepSync - (P1 - RawBSONStringPos);
  end;

{ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  function GetPathStr(Const ExtraItems: AnsiString = ''): AnsiString;
  var
    i, L, P, Size: integer;
    S, LB: AnsiString;
  begin
    LB := PathSeparator;
    Size := Length(ExtraItems);
    if Size <> 0 then
      inc(Size, Length(LB));
    for i := 1 to Paths.Count - 1 do
      inc(Size, Length(Paths[i]) + Length(LB));
    setlength(result, Size);
    P := 1;
    for i := 1 to Paths.Count - 1 do
    begin
      S := Paths[i];
      L := Length(S);
      if L <> 0 then
      begin
        ALMove(S[1], result[P], L);
        inc(P, L);
      end;
      L := Length(LB);
      if (L <> 0) and ((i <> Paths.Count - 1) or (ExtraItems <> '')) and
        (((NotSaxMode) and (TALJSONNode(Paths.Objects[i]).NodeType <> ntArray))
        or ((not NotSaxMode) and (integer(Paths.Objects[i]) <> 2 { ntarray } )))
      then
      begin
        ALMove(LB[1], result[P], L);
        inc(P, L);
      end;
    end;
    if ExtraItems <> '' then
    begin
      L := Length(ExtraItems);
      ALMove(ExtraItems[1], result[P], L);
      inc(P, L);
    end;
    setlength(result, P - 1);
  end;

{ ~~~~~~~~~~~~~~~~~~~~ }
  procedure AnalyzeNode;
  Var
    aNode: TALJSONNode;
    aNodeTypeInt: integer;
    aNodeSubType: TALJSONNodeSubType;
    aName: AnsiString;
    aDouble: Double;
    aInt32: integer;
    aInt64: Int64;
    aDateTime: TDateTime;
    aObjectID: TALJSONObjectID;
    aTimestamp: TALBSONTimestamp;
    aBool: Boolean;
    aTextValue: AnsiString;
    aRegEx: TALJSONRegEx;
    aBinary: TALJSONBinary;
    P1: integer;
  Begin

{$IFDEF undef}{$REGION 'End Object/Array'}{$ENDIF}
    // ... } ....
    // ... ] ....
    if RawBSONString[RawBSONStringPos] = #$00 then
    begin

      // error if Paths.Count = 0 (mean one end object/array without any starting)
      if (Paths.Count = 0) then
        ALJSONDocError(cALBSONParseError);

      // fucking warning
      aNodeTypeInt := 0;

      // if we are not in sax mode
      if NotSaxMode then
      begin

        // init anode to one level up
        aNode := TALJSONNode(Paths.Objects[Paths.Count - 1]);

        // if anode <> workingNode aie aie aie
        if (aNode <> WorkingNode) then
          ALJSONDocError(cALBSONParseError);

        // calculate anodeTypeInt
        if aNode.NodeType = ntObject then
          aNodeTypeInt := 1
        else if aNode.NodeType = ntArray then
          aNodeTypeInt := 2
        else
          ALJSONDocError(cALBSONParseError);

        // if working node <> containernode then we can go to one level up
        If WorkingNode <> ContainerNode then
        begin

          // init WorkingNode to the parentNode
          WorkingNode := WorkingNode.ParentNode;

        end

        // if working node = containernode then we can no go to the parent node so set WorkingNode to nil
        Else
          WorkingNode := nil;

      end

      // if we are in sax mode
      else
      begin

        // calculate anodeTypeInt
        aNodeTypeInt := integer(Paths.Objects[Paths.Count - 1]);
        if not(aNodeTypeInt in [1, 2]) then
          ALJSONDocError(cALBSONParseError);

      end;

      // call the DoParseEndObject/array event
      if aNodeTypeInt = 1 then
        DoParseEndObject(GetPathStr, Paths[Paths.Count - 1])
      else
        DoParseEndArray(GetPathStr, Paths[Paths.Count - 1]);

      // delete the last entry from the path
      Paths.Delete(Paths.Count - 1);

      // update RawBSONStringPos
      RawBSONStringPos := RawBSONStringPos + 1;

      // finallly exit from this procedure, everything was done
      Exit;

    end;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Get the node sub type'}{$ENDIF}
    aNodeSubType := nstText; // to hide fucking warning
    case RawBSONString[RawBSONStringPos] of
      #$01:
        aNodeSubType := nstFloat;
      #$02:
        aNodeSubType := nstText;
      #$03:
        aNodeSubType := nstObject;
      #$04:
        aNodeSubType := nstArray;
      #$05:
        aNodeSubType := nstBinary;
      #$07:
        aNodeSubType := nstObjectID;
      #$08:
        aNodeSubType := nstBoolean;
      #$09:
        aNodeSubType := nstDateTime;
      #$0A:
        aNodeSubType := nstNull;
      #$0B:
        aNodeSubType := nstRegEx;
      #$0D:
        aNodeSubType := nstJavascript;
      #$10:
        aNodeSubType := nstInt32;
      #$11:
        aNodeSubType := nstTimestamp;
      #$12:
        aNodeSubType := nstInt64;
    else
      ALJSONDocError(cALBSONParseError);
    end;
    RawBSONStringPos := RawBSONStringPos + 1;
    If RawBSONStringPos > RawBSONStringLength then
      ExpandRawBSONString;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Get the node name'}{$ENDIF}
    P1 := RawBSONStringPos;
    While P1 <= RawBSONStringLength do
    begin
      If RawBSONString[P1] <> #$00 then
        inc(P1)
      else
      begin
        aName := ALCopyStr(RawBSONString, RawBSONStringPos,
          P1 - RawBSONStringPos);
        break;
      end;
      if P1 > RawBSONStringLength then
        ExpandRawBSONString(P1);
    end;
    if P1 > RawBSONStringLength then
      ALJSONDocError(cALBSONParseError);
    RawBSONStringPos := P1 + 1;
    if RawBSONStringPos > RawBSONStringLength then
      ExpandRawBSONString;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Begin Object/Array'}{$ENDIF}
    // ... { ....
    // ... [ ....
    if aNodeSubType in [nstObject, nstArray] then
    begin

      // if we are not in sax mode
      if NotSaxMode then
      begin

        // if workingnode = nil then it's mean we are outside the containerNode
        if not Assigned(WorkingNode) then
          ALJSONDocError(cALBSONParseError);

        // create the node according the the braket char and add it to the workingnode
        if aNodeSubType = nstObject then
          aNode := CreateNode(ALIfThen(WorkingNode.NodeType = ntArray, '',
            aName), ntObject)
        else
          aNode := CreateNode(ALIfThen(WorkingNode.NodeType = ntArray, '',
            aName), ntArray);
        try
          WorkingNode.ChildNodes.Add(aNode);
        except
          aNode.Free;
          raise;
        end;

        // set that the current working node will be now the new node newly created
        WorkingNode := aNode;

        // update the path
        Paths.AddObject(ALIfThen(WorkingNode.NodeType = ntArray,
          '[' + aName + ']', aName), WorkingNode);

      end

      // if we are in sax mode
      else
      begin

        // update the path
        if aNodeSubType = nstObject then
          aNodeTypeInt := 1
        else
          aNodeTypeInt := 2;
        Paths.AddObject(ALIfThen((Paths.Count > 0) and
          (integer(Paths.Objects[Paths.Count - 1]) = 2 { array } ),
          '[' + aName + ']', aName), pointer(aNodeTypeInt));

      end;

      // call the DoParseStartObject/array event
      if aNodeSubType = nstObject then
        DoParseStartObject(GetPathStr, '')
      else
        DoParseStartArray(GetPathStr, '');

      // update RawBSONStringPos
      RawBSONStringPos := RawBSONStringPos + 4;
      // we don't need the size of the object/array (4 bytes)

      // finallly exit from this procedure, everything was done
      Exit;

    end;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Remove Delphi warning'}{$ENDIF}
    aBool := False;
    aDateTime := 0;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: Floating point'}{$ENDIF}
    // \x01 + name + \x00 + double
    if aNodeSubType = nstFloat then
    begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(Double) + 1 then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(Double) + 1 then
          ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aDouble, sizeof(Double));
      aTextValue := ALFloatToStr(aDouble, ALDefaultFormatSettings);
      RawBSONStringPos := RawBSONStringPos + sizeof(Double);
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: UTF-8 string'}{$ENDIF}
    // \x02 + name + \x00 + length (int32) + string + \x00
    else if aNodeSubType = nstText then
    begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then
          ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt32, sizeof(aInt32));
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt32);
      while (RawBSONStringPos + aInt32 - 1 > RawBSONStringLength) do
        if not ExpandRawBSONString then
          ALJSONDocError(cALBSONParseError);
      aTextValue := ALCopyStr(RawBSONString, RawBSONStringPos,
        aInt32 - 1 { for the trailing #0 } );
      RawBSONStringPos := RawBSONStringPos + aInt32;
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: Binary data'}{$ENDIF}
    // \x05 + name + \x00 + int32 + subtype + (byte*)
    else if aNodeSubType = nstBinary then
    begin

{$REGION 'Get size'}
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then
          ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt32, sizeof(aInt32));
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt32);
{$ENDREGION}
{$REGION 'Get the subtype'}
      if RawBSONStringPos > RawBSONStringLength then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength then
          ALJSONDocError(cALBSONParseError);
      end;
      aBinary.Subtype := byte(RawBSONString[RawBSONStringPos]);
      RawBSONStringPos := RawBSONStringPos + 1;
{$ENDREGION}
{$REGION 'Get the data'}
      while (RawBSONStringPos + aInt32 - 1 > RawBSONStringLength) do
        if not ExpandRawBSONString then
          ALJSONDocError(cALBSONParseError);
      aBinary.Data := ALCopyStr(RawBSONString, RawBSONStringPos, aInt32);
      RawBSONStringPos := RawBSONStringPos + aInt32;
{$ENDREGION}
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: ObjectId'}{$ENDIF}
    // \x07 + name + \x00 + (byte*12)
    else if aNodeSubType = nstObjectID then
    begin
      if RawBSONStringPos > RawBSONStringLength - Length(aObjectID) + 1 then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - Length(aObjectID) + 1 then
          ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aObjectID[1], Length(aObjectID));
      setlength(aTextValue, Length(aObjectID) * 2);
      BintoHex(@aObjectID[1], PansiChar(aTextValue), Length(aObjectID));
      aTextValue := 'ObjectId("' + allowerCase(aTextValue) + '")';
      RawBSONStringPos := RawBSONStringPos + Length(aObjectID);
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: Boolean'}{$ENDIF}
    // \x08 + name + \x00 + \x00 => Boolean "false"
    // \x08 + name + \x00 + \x01	=> Boolean "true"
    else if aNodeSubType = nstBoolean then
    begin
      if RawBSONStringPos > RawBSONStringLength then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength then
          ALJSONDocError(cALBSONParseError);
      end;
      if RawBSONString[RawBSONStringPos] = #$00 then
      begin
        aBool := False;
        aTextValue := 'false';
      end
      else if RawBSONString[RawBSONStringPos] = #$01 then
      begin
        aBool := True;
        aTextValue := 'true';
      end
      else
        ALJSONDocError(cALBSONParseError);
      RawBSONStringPos := RawBSONStringPos + 1;
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: UTC datetime'}{$ENDIF}
    // \x09 + name + \x00 + int64
    else if aNodeSubType = nstDateTime then
    begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then
          ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt64, sizeof(aInt64));
      aDateTime := ALUnixMsToDateTime(aInt64);
      aTextValue := ALFormatDateTime
        ('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''',
        aDateTime, ALDefaultFormatSettings);
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt64);
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: Timestamp'}{$ENDIF}
    // \x11 + name + \x00 + int64
    else if aNodeSubType = nstTimestamp then
    begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then
          ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt64, sizeof(aInt64));
      aTimestamp.I64 := aInt64;
      aTextValue := 'Timestamp(' + ALUIntToStr(aTimestamp.W1) + ', ' +
        ALUIntToStr(aTimestamp.W2) + ')';
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt64);
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: Null value'}{$ENDIF}
    // \x0A + name + \x00
    else if aNodeSubType = nstNull then
    begin
      aTextValue := 'null';
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: Regular expression'}{$ENDIF}
    // \x0B + name + \x00 + (byte*) + \x00 + (byte*) + \x00
    else if aNodeSubType = nstRegEx then
    begin

{$REGION 'Get pattern'}
      P1 := RawBSONStringPos;
      While P1 <= RawBSONStringLength do
      begin
        If RawBSONString[P1] <> #$00 then
          inc(P1)
        else
        begin
          aRegEx.Expression := ALCopyStr(RawBSONString, RawBSONStringPos,
            P1 - RawBSONStringPos);
          break;
        end;
        if P1 > RawBSONStringLength then
          ExpandRawBSONString(P1);
      end;
      if P1 > RawBSONStringLength then
        ALJSONDocError(cALBSONParseError);
      RawBSONStringPos := P1 + 1;
      if RawBSONStringPos > RawBSONStringLength then
        ExpandRawBSONString;
{$ENDREGION}
{$REGION 'Get options'}
      P1 := RawBSONStringPos;
      While P1 <= RawBSONStringLength do
      begin
        If RawBSONString[P1] <> #$00 then
          inc(P1)
        else
        begin
          aRegEx.Options := ALCopyStr(RawBSONString, RawBSONStringPos,
            P1 - RawBSONStringPos);
          break;
        end;
        if P1 > RawBSONStringLength then
          ExpandRawBSONString(P1);
      end;
      if P1 > RawBSONStringLength then
        ALJSONDocError(cALBSONParseError);
      RawBSONStringPos := P1 + 1;
      if RawBSONStringPos > RawBSONStringLength then
        ExpandRawBSONString;
{$ENDREGION}
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: JavaScript code'}{$ENDIF}
    // \x0D + name + \x00 + length (int32) + string + \x00
    else if aNodeSubType = nstJavascript then
    begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then
          ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt32, sizeof(aInt32));
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt32);
      while (RawBSONStringPos + aInt32 - 1 > RawBSONStringLength) do
        if not ExpandRawBSONString then
          ALJSONDocError(cALBSONParseError);
      aTextValue := ALCopyStr(RawBSONString, RawBSONStringPos,
        aInt32 - 1 { for the trailing #0 } );
      RawBSONStringPos := RawBSONStringPos + aInt32;
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: 32-bit Integer'}{$ENDIF}
    // \x10 + name + \x00 + int32
    else if aNodeSubType = nstInt32 then
    begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt32) + 1 then
          ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt32, sizeof(aInt32));
      aTextValue := 'NumberInt(' + alinttostr(aInt32) + ')';
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt32);
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: 64-bit integer'}{$ENDIF}
    // \x12 + name + \x00 + int64
    else if aNodeSubType = nstInt64 then
    begin
      if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then
      begin
        ExpandRawBSONString;
        if RawBSONStringPos > RawBSONStringLength - sizeof(aInt64) + 1 then
          ALJSONDocError(cALBSONParseError);
      end;
      ALMove(RawBSONString[RawBSONStringPos], aInt64, sizeof(aInt64));
      aTextValue := 'NumberLong(' + alinttostr(aInt64) + ')';
      RawBSONStringPos := RawBSONStringPos + sizeof(aInt64);
    end
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'Extract value: Undefined'}{$ENDIF}
    else
      ALJSONDocError(cALBSONParseError);
{$IFDEF undef}{$ENDREGION}{$ENDIF}
{$IFDEF undef}{$REGION 'create the named text node'}{$ENDIF}
    if NotSaxMode then
    begin

      // if workingnode = nil then it's mean we are outside the containerNode
      if not Assigned(WorkingNode) then
        ALJSONDocError(cALBSONParseError);

      // create the node and add it to the workingnode
      aNode := CreateNode(ALIfThen(WorkingNode.NodeType = ntArray, '',
        aName), ntText);
      try
        case aNodeSubType of
          nstFloat:
            aNode.Float := aDouble;
          nstText:
            aNode.Text := aTextValue;
          nstObjectID:
            aNode.ObjectID := aObjectID;
          nstBoolean:
            aNode.Bool := aBool;
          nstDateTime:
            aNode.DateTime := aDateTime;
          nstTimestamp:
            aNode.Timestamp := aTimestamp;
          nstNull:
            aNode.Null := True;
          nstRegEx:
            aNode.RegEx := aRegEx;
          nstBinary:
            aNode.Binary := aBinary;
          nstJavascript:
            aNode.Javascript := aTextValue;
          nstInt32:
            aNode.int32 := aInt32;
          nstInt64:
            aNode.Int64 := aInt64;
        else
          ALJSONDocError(cALBSONParseError);
        end;
        WorkingNode.ChildNodes.Add(aNode);
      except
        aNode.Free;
        raise;
      end;

      // call the DoParseStartObject/array event
      if WorkingNode.NodeType = ntArray then
        DoParseText(GetPathStr('[' + aName + ']'), '', aTextValue, aNodeSubType)
      else
        DoParseText(GetPathStr(aName), aName, aTextValue, aNodeSubType);

    end

    // if we are in sax mode
    else
    begin

      // call the DoParseStartObject/array event
      if (Paths.Count > 0) and (integer(Paths.Objects[Paths.Count - 1])
        = 2 { array } ) then
        DoParseText(GetPathStr('[' + aName + ']'), '', aTextValue, aNodeSubType)
      else
        DoParseText(GetPathStr(aName), aName, aTextValue, aNodeSubType);

    end;
{$IFDEF undef}{$ENDREGION}{$ENDIF}
  end;

Begin

  //
  // NOTE: the childNodes of the ContainerNode
  // must have been cleared by the calling function!
  //
  // NOTE: ContainerNode must have fDocument assigned
  //
  // NOTE: ContainerNode must be ntobject or nil (sax mode)
  //

  Paths := TALStringList.Create;
  Try

    DoParseStartDocument;

    WorkingNode := ContainerNode;
    NotSaxMode := Assigned(ContainerNode);
    RawBSONString := '';
    RawBSONStringLength := 0;
    RawBSONStringPos := 5;
    // the first 4 bytes are the length of the document and we don't need it
    ExpandRawBSONString;
    if NotSaxMode then
      Paths.AddObject('[-1]', WorkingNode)
    else
      Paths.AddObject('[-1]', pointer(1));

    While RawBSONStringPos <= RawBSONStringLength do
    begin
      AnalyzeNode;
      if RawBSONStringPos > RawBSONStringLength then
        ExpandRawBSONString;
    end;

    // some tags are not closed
    if Paths.Count > 0 then
      ALJSONDocError(cALBSONParseError);

    // mean the node was not update (empty stream?) or not weel closed
    if WorkingNode <> nil then
      ALJSONDocError(cALBSONParseError);

    DoParseEndDocument;

  finally
    Paths.Free;
  end;

end;

{ *********************************** }
procedure TALJSONDocument.ReleaseDoc;
begin
  if Assigned(FDocumentNode) then
    FreeAndNil(FDocumentNode);
end;

{ ************************************** }
{ Loads an JSON document and activates it.
  Call LoadFromFile to load the JSON document specified by AFileName and set the Active property to true so
  that you can examine or modify the document.
  *AFileName is the name of the JSON document to load from disk. If AFileName is an empty string, TALJSONDocument uses the value of the
  FileName property. If AFileName is not an empty string, TALJSONDocument changes the FileName property to AFileName.
  Once you have loaded an JSON document, any changes you make to the document are not saved back to disk until you call the SaveToFile method. }
procedure TALJSONDocument.LoadFromFile(const AFileName: AnsiString;
  const saxMode: Boolean = False; const BSONFile: Boolean = False;
  Const ClearChildNodes: Boolean = True);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(string(AFileName), fmOpenRead or
    fmShareDenyWrite);
  try
    LoadFromStream(FileStream, saxMode, BSONFile, ClearChildNodes);
  finally
    FileStream.Free;
  end;
end;

{ **************************************************** }
{ Loads an JSON document from a stream and activates it.
  Call LoadFromStream to load the JSON document from a stream.
  *Stream is a stream object that can be used to read the string of JSON that makes up the document.
  After loading the document from Stream, LoadFromStream sets the Active property to true. }
procedure TALJSONDocument.LoadFromStream(const Stream: TStream;
  const saxMode: Boolean = False; const BSONStream: Boolean = False;
  Const ClearChildNodes: Boolean = True);
begin
  if saxMode then
    SetActive(False)
  else
  begin
    if ClearChildNodes then
      ReleaseDoc;
    SetActive(True);
  end;
  if BSONStream then
    ParseBSONStream(Stream, FDocumentNode)
  else
    ParseJSONStream(Stream, FDocumentNode);
end;

{ ***************************************************************** }
{ Loads a string representation of an JSON document and activates it.
  Call LoadFromJSON to assign a string as the value of the JSON document. Unlike the JSON property, which lets you assign JSON on a line-by-line
  basis, LoadFromJSON treats the text of the JSON document as a whole.
  The JSON parameter is a string containing the text of an JSON document. It should represent the JSON text encoded using 8 bits char (utf-8, iso-8859-1, etc)
  After assigning the JSON property as the contents of the document, LoadFromJSON sets the Active property to true. }
procedure TALJSONDocument.LoadFromJSON(const JSON: AnsiString;
  const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
var
  StringStream: TALStringStream;
begin
  StringStream := TALStringStream.Create(JSON);
  try
    LoadFromStream(StringStream, saxMode, False { BSONStream } ,
      ClearChildNodes);
  finally
    StringStream.Free;
  end;
end;

{ ************************************************************************************************************************************ }
procedure TALJSONDocument.LoadFromBSON(const BSON: AnsiString;
  const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
var
  StringStream: TALStringStream;
begin
  StringStream := TALStringStream.Create(BSON);
  try
    LoadFromStream(StringStream, saxMode, True { BSONStream } ,
      ClearChildNodes);
  finally
    StringStream.Free;
  end;
end;

{ ****************************** }
{ Saves the JSON document to disk.
  Call SaveToFile to save any modifications you have made to the parsed JSON document.
  AFileName is the name of the file to save. }
procedure TALJSONDocument.SaveToFile(const AFileName: AnsiString;
  const BSONFile: Boolean = False);
Var
  afileStream: TFileStream;
begin
  afileStream := TFileStream.Create(String(AFileName), fmCreate);
  Try
    SaveToStream(afileStream, BSONFile);
  finally
    afileStream.Free;
  end;
end;

{ ************************************************ }
{ Saves the JSON document to a string-type variable.
  Call SaveToJSON to save the contents of the JSON document to the string-type variable specified by JSON. SaveToJSON writes the contents of JSON document
  using 8 bits char (utf-8, iso-8859-1, etc) as an encoding system, depending on the type of the JSON parameter.
  Unlike the JSON property, which lets you write individual lines from the JSON document, SaveToJSON writes the entire text of the JSON document. }
procedure TALJSONDocument.SaveToJSON(var JSON: AnsiString);
Var
  StringStream: TALStringStream;
begin
  StringStream := TALStringStream.Create('');
  Try
    SaveToStream(StringStream, False { BSONStream } );
    JSON := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

{ ********************************************************** }
{ Saves the JSON document to binary representation of JSON -
  BSON. Specification of this format can be found here:
  http://bsonspec.org/#/specification.
  Format BSON is mostly using for the purposes of
  MongoDB interconnection. }
procedure TALJSONDocument.SaveToBSON(var BSON: AnsiString);
Var
  StringStream: TALStringStream;
begin
  StringStream := TALStringStream.Create('');
  Try
    SaveToStream(StringStream, True { BSONStream } );
    BSON := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

{ ********************************** }
{ Saves the JSON document to a stream.
  Call SaveToStream to save the contents of the JSON document to the stream specified by Stream. }
procedure TALJSONDocument.SaveToStream(const Stream: TStream;
  const BSONStream: Boolean = False);
begin
  CheckActive;
  Node.SaveToStream(Stream, BSONStream);
end;

{ ************************************* }
{ Returns the value of the JSON property.
  GetJSON is the read implementation of the JSON property. }
function TALJSONDocument.GetJSON: AnsiString;
begin
  SaveToJSON(result);
end;

{ ************************************* }
{ Returns the value of the BSON property.
  GetBSON is the read implementation of the BSON property. }
function TALJSONDocument.GetBSON: AnsiString;
begin
  SaveToBSON(result);
end;

{ ********************************** }
{ Sets the value of the JSON property.
  SetJSON is the write implementation of the JSON property.
  *Value contains the raw (unparsed) JSON to assign. }
procedure TALJSONDocument.SetJSON(const Value: AnsiString);
begin
  LoadFromJSON(Value, False { saxMode } , True { ClearChildNodes } );
end;

{ ********************************** }
{ Sets the value of the BSON property.
  SetBSON is the write implementation of the BSON property.
  *Value contains the raw (unparsed) BSON to assign. }
procedure TALJSONDocument.SetBSON(const Value: AnsiString);
begin
  LoadFromBSON(Value, False { saxMode } , True { ClearChildNodes } );
end;

{ *********************************** }
procedure TALJSONDocument.CheckActive;
begin
  if not Assigned(FDocumentNode) then
    ALJSONDocError(cALJSONNotActive);
end;

{ ********************************************************************************************************************************************** }
function TALJSONDocument.AddChild(const NodeName: AnsiString;
  const NodeType: TALJSONNodeType = ntText; const Index: integer = -1)
  : TALJSONNode;
begin
  result := Node.AddChild(NodeName, NodeType, Index);
end;

{ ****************************************************************************************************** }
function TALJSONDocument.CreateNode(const NodeName: AnsiString;
  NodeType: TALJSONNodeType): TALJSONNode;
begin
  result := ALCreateJSONNode(NodeName, NodeType);
end;

{ ******************************************** }
{ Returns the value of the ChildNodes property.
  GetChildNodes is the read implementation of the ChildNodes property. }
function TALJSONDocument.GetChildNodes: TALJSONNodeList;
begin
  result := Node.ChildNodes;
end;

{ ************************************************************************ }
{ Indicates whether the TJSONDocument instance represents an empty document.
  Call IsEmptyDoc to determine whether the TALJSONDocument instance represents an empty document.
  IsEmptyDoc returns true if the Document property is not set or if this object represents a
  document with no child nodes. }
function TALJSONDocument.IsEmptyDoc: Boolean;
begin
  result := not(Assigned(FDocumentNode) and FDocumentNode.HasChildNodes);
end;

{ ************************************** }
{ Returns the value of the Node property.
  GetDocumentNode is the read implementation of the Node property. }
function TALJSONDocument.GetDocumentNode: TALJSONNode;
begin
  CheckActive;
  result := FDocumentNode;
end;

{ *********************************************** }
{ Returns the value of the NodeIndentStr property.
  GetNodeIndentStr is the read implementation of the NodeIndentStr property. }
function TALJSONDocument.GetNodeIndentStr: AnsiString;
begin
  result := FNodeIndentStr;
end;

{ ******************************************** }
{ Sets the value of the NodeIndentStr property.
  SetNodeIndentStr is the write implementation of the NodeIndentStr property.
  *Value is the string that is inserted before nested nodes to indicate a level of nesting. }
procedure TALJSONDocument.SetNodeIndentStr(const Value: AnsiString);
begin
  FNodeIndentStr := Value;
end;

{ ***************************************** }
{ Returns the value of the Options property.
  GetOptions is the read implementation of the Options property. }
function TALJSONDocument.GetOptions: TALJSONDocOptions;
begin
  result := FOptions;
end;

{ ************************************** }
{ Sets the value of the Options property.
  GetOptions is the write implementation of the Options property.
  *Value is the set of options to assign. }
procedure TALJSONDocument.SetOptions(const Value: TALJSONDocOptions);
begin
  FOptions := Value;
end;

{ ********************************************** }
{ Returns the value of the ParseOptions property.
  GetParseOptions is the read implementation of the ParseOptions property. }
function TALJSONDocument.GetParseOptions: TALJSONParseOptions;
begin
  result := FParseOptions;
end;

{ ******************************************* }
{ Sets the value of the ParseOptions property.
  GetParseOptions is the write implementation of the ParseOptions property.
  *Value is the set of parser options to assign. }
procedure TALJSONDocument.SetParseOptions(const Value: TALJSONParseOptions);
begin
  FParseOptions := Value;
end;

{ ****************************************************************** }
procedure TALJSONDocument.SetPathSeparator(const Value: AnsiString);
begin
  fPathSeparator := Value;
end;

{ **************************************************** }
function TALJSONDocument.GetPathSeparator: AnsiString;
begin
  result := fPathSeparator;
end;

{ ********************************************* }
procedure TALJSONDocument.DoParseStartDocument;
begin
  if Assigned(FOnParseStartDocument) then
    FOnParseStartDocument(Self);
end;

{ ******************************************* }
procedure TALJSONDocument.DoParseEndDocument;
begin
  if Assigned(FOnParseEndDocument) then
    FOnParseEndDocument(Self);
end;

{ ************************************************************************************************************************************************** }
procedure TALJSONDocument.DoParseText(const Path: AnsiString;
  const name: AnsiString; const Str: AnsiString;
  const NodeSubType: TALJSONNodeSubType);
begin
  if Assigned(FonParseText) then
  begin
    case NodeSubType of
      nstFloat:
        FonParseText(Self, Path, name,
          ALFloatToStr(ALStrToFloat(Str, ALDefaultFormatSettings),
          FormatSettings), NodeSubType);
      nstDateTime:
        FonParseText(Self, Path, name,
          ALDateTimeToStr(ALStrToDateTime(Str, ALDefaultFormatSettings),
          FormatSettings), NodeSubType);
    else
      FonParseText(Self, Path, name, Str, NodeSubType);
    end;
  end;
end;

{ ******************************************************************************************* }
procedure TALJSONDocument.DoParseStartObject(const Path: AnsiString;
  const name: AnsiString);
begin
  if Assigned(FonParseStartObject) then
    FonParseStartObject(Self, Path, name);
end;

{ ***************************************************************************************** }
procedure TALJSONDocument.DoParseEndObject(const Path: AnsiString;
  const name: AnsiString);
begin
  if Assigned(FonParseEndObject) then
    FonParseEndObject(Self, Path, name);
end;

{ ****************************************************************************************** }
procedure TALJSONDocument.DoParseStartArray(const Path: AnsiString;
  const name: AnsiString);
begin
  if Assigned(FonParseStartArray) then
    FonParseStartArray(Self, Path, name);
end;

{ **************************************************************************************** }
procedure TALJSONDocument.DoParseEndArray(const Path: AnsiString;
  const name: AnsiString);
begin
  if Assigned(FonParseEndArray) then
    FonParseEndArray(Self, Path, name);
end;

{ ********************************************************** }
{ Creates the object that implements the ChildNodes property }
function TALJSONNode.CreateChildList: TALJSONNodeList;
begin
  result := TALJSONNodeList.Create(Self);
end;

{ ******************************************** }
{ Get Childnode without create it if not exist }
function TALJSONNode.InternalGetChildNodes: TALJSONNodeList;
begin
  result := nil; // virtual;
end;

{ ************************************************** }
function TALJSONNode.GetChildNodes: TALJSONNodeList;
begin
  result := nil; // hide warning
  ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr])
end;

{ **************************************************************** }
procedure TALJSONNode.SetChildNodes(const Value: TALJSONNodeList);
begin
  ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr])
end;

{ *********************************************** }
{ Indicates whether this node has any child nodes }
function TALJSONNode.GetHasChildNodes: Boolean;
Var
  aNodeList: TALJSONNodeList;
begin
  aNodeList := InternalGetChildNodes;
  result := Assigned(aNodeList) and (aNodeList.Count > 0);
end;

{ ******************************************* }
function TALJSONNode.GetNodeName: AnsiString;
begin
  result := fNodeName;
end;

{ ******************************************************** }
procedure TALJSONNode.SetNodeName(const Value: AnsiString);
begin
  fNodeName := Value;
end;

{ ********************************************** }
function TALJSONNode.GetNodeTypeStr: AnsiString;
begin
  case NodeType of
    ntObject:
      result := 'ntObject';
    ntArray:
      result := 'ntArray';
    ntText:
      result := 'ntText';
  else
    begin
      result := ''; // for hide warning
      ALJSONDocError(cALJSONInvalidNodeType);
    end;
  end;
end;

{ ******************************************** }
function TALJSONNode.GetNodeValue: AnsiString;
begin
  result := ''; // hide warning
  ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr])
end;

{ ************************************************************************************************* }
procedure TALJSONNode.SetNodeValue(const Value: AnsiString;
  const NodeSubType: TALJSONNodeSubType);
begin
  ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr])
end;

{ *********************************** }
{ Returns the text value of the node. }
function TALJSONNode.GetText: AnsiString;

{ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  function _GetFormatSettings: TALFormatSettings;
  begin
    if Assigned(FDocument) then
      result := FDocument.FormatSettings
    else
      result := ALDefaultFormatSettings;
  end;

begin

  case NodeSubType of
    nstFloat:
      result := ALFloatToStr(GetFloat, _GetFormatSettings);
    nstText:
      result := GetNodeValue;
    nstObject:
      result := GetNodeValue;
    nstArray:
      result := GetNodeValue;
    nstObjectID:
      result := GetObjectID;
    nstBoolean:
      result := GetNodeValue;
    nstDateTime:
      result := ALDateTimeToStr(GetDateTime, _GetFormatSettings);
    nstNull:
      result := GetNodeValue;
    nstRegEx:
      result := GetNodeValue;
    nstBinary:
      result := GetBinary.Data;
    nstJavascript:
      result := GetNodeValue;
    nstInt32:
      result := GetNodeValue;
    nstTimestamp:
      result := GetNodeValue;
    nstInt64:
      result := GetNodeValue;
  else
    ALJSONDocError(cALJSONInvalidBSONNodeSubType);
  end;

end;

{ ******************************** }
{ Sets the text value of the node. }
procedure TALJSONNode.SetText(const Value: AnsiString);
begin
  SetNodeValue(Value, nstText);
end;

{ ************************************ }
function TALJSONNode.GetFloat: Double;
begin
  if NodeSubType = nstText then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  If not ALTryStrToFloat(GetNodeValue, result, ALDefaultFormatSettings) then
    ALJSONDocError(String(GetNodeValue) + ' is not a valid float');
end;

{ ************************************************** }
procedure TALJSONNode.SetFloat(const Value: Double);
begin
  SetNodeValue(ALFloatToStr(Value, ALDefaultFormatSettings), nstFloat);
end;

{ ****************************************** }
function TALJSONNode.GetDateTime: TDateTime;
begin
  if NodeSubType = nstText then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  if not ALJSONDocTryStrToDateTime(GetNodeValue, result) then
    ALJSONDocError(String(GetNodeValue) + ' is not a valid date and time');
end;

{ ******************************************************** }
procedure TALJSONNode.SetDateTime(const Value: TDateTime);
begin
  SetNodeValue(ALFormatDateTime
    ('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''',
    Value, ALDefaultFormatSettings), nstDateTime);
end;

{ ************************************************** }
function TALJSONNode.GetTimestamp: TALBSONTimestamp;
begin
  if NodeSubType = nstText then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  if not ALJSONDocTryStrToTimestamp(GetNodeValue, result) then
    ALJSONDocError(String(GetNodeValue) + ' is not a valid BSON-Timestamp');
end;

{ *************************************************************** }
procedure TALJSONNode.SetTimestamp(const Value: TALBSONTimestamp);
begin
  SetNodeValue('Timestamp(' + ALUIntToStr(Value.W1) + ', ' +
    ALUIntToStr(Value.W2) + ')', nstTimestamp);
end;

{ ************************************************ }
function TALJSONNode.GetObjectID: TALJSONObjectID;
begin
  if NodeSubType = nstText then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  if not ALJSONDocTryStrToObjectID(GetNodeValue, result) then
    ALJSONDocError(String(GetNodeValue) + ' is not a valid ObjectID');
end;

{ ************************************************************* }
procedure TALJSONNode.SetObjectID(const Value: TALJSONObjectID);
var
  aStr: AnsiString;
begin
  setlength(aStr, Length(Value) * 2);
  BintoHex(@Value[1], PansiChar(aStr), Length(Value));
  SetNodeValue('ObjectId("' + allowerCase(aStr) + '")', nstObjectID);
end;

{ ************************************* }
function TALJSONNode.GetInt32: integer;
begin
  if NodeSubType = nstText then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  if not ALJSONDocTryStrToInteger(GetNodeValue, result) then
    ALJSONDocError(String(GetNodeValue) + ' is not a valid Int32');
end;

{ *************************************************** }
procedure TALJSONNode.SetInt32(const Value: integer);
begin
  SetNodeValue(alinttostr(Value), nstInt32);
end;

{ *********************************** }
function TALJSONNode.GetInt64: Int64;
begin
  if NodeSubType = nstText then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  if not ALJSONDocTryStrToInt64(GetNodeValue, result) then
    ALJSONDocError(String(GetNodeValue) + ' is not a valid Int64');
end;

{ ************************************************* }
procedure TALJSONNode.SetInt64(const Value: Int64);
begin
  SetNodeValue(alinttostr(Value), nstInt64);
end;

{ ************************************ }
function TALJSONNode.GetBool: Boolean;
begin
  if NodeSubType = nstText then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  If not ALTryStrToBool(GetNodeValue, result) then
    ALJSONDocError(String(GetNodeValue) + ' is not a valid Boolean');
end;

{ ************************************************** }
procedure TALJSONNode.SetBool(const Value: Boolean);
begin
  if Value then
    SetNodeValue('true', nstBoolean)
  else
    SetNodeValue('false', nstBoolean);
end;

{ ************************************ }
function TALJSONNode.GetNull: Boolean;
begin
  result := (NodeSubType <> nstText) and (alSameText(GetNodeValue, 'null'));
end;

{ ************************************************** }
procedure TALJSONNode.SetNull(const Value: Boolean);
begin
  if Value then
    SetNodeValue('null', nstNull)
  else
    ALJSONDocError('Only "true" is allowed for setNull property');
end;

{ ********************************************* }
function TALJSONNode.GetJavascript: AnsiString;
begin
  if NodeSubType = nstText then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  result := GetNodeValue;
end;

{ *********************************************************** }
procedure TALJSONNode.SetJavascript(const Value: AnsiString);
begin
  SetNodeValue(Value, nstJavascript);
end;

{ ****************************************** }
function TALJSONNode.GetRegEx: TALJSONRegEx;
begin
  if NodeSubType = nstText then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  if not ALJSONDocTryStrToRegEx(GetNodeValue, result) then
    ALJSONDocError(String(GetNodeValue) + ' is not a valid regular expression');
end;

{ ******************************************************** }
procedure TALJSONNode.SetRegEx(const Value: TALJSONRegEx);
begin
  SetNodeValue('/' + Value.Expression + '/' + Value.Options, nstRegEx);
end;

{ ******************************************** }
function TALJSONNode.GetBinary: TALJSONBinary;
begin
  if NodeSubType = nstText then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  if not ALJSONDocTryStrTobinary(GetNodeValue, result) then
    ALJSONDocError(String(GetNodeValue) + ' is not a valid binary');
end;

{ ********************************************************** }
procedure TALJSONNode.SetBinary(const Value: TALJSONBinary);
begin
  SetNodeValue('BinData(' + alinttostr(Value.Subtype) + ', "' +
    ALMimeBase64EncodeStringNoCRLF(Value.Data) + '")', nstBinary);
end;

{ ******************************************************* }
{ Returns the document object in which this node appears. }
function TALJSONNode.GetOwnerDocument: TALJSONDocument;
begin
  result := FDocument;
end;

{ ******************************************************************* }
procedure TALJSONNode.SetOwnerDocument(const Value: TALJSONDocument);
var
  i: integer;
  aNodeList: TALJSONNodeList;
begin
  FDocument := Value;
  aNodeList := InternalGetChildNodes;
  if Assigned(aNodeList) then
    for i := 0 to aNodeList.Count - 1 do
      aNodeList[i].SetOwnerDocument(Value);
end;

{ ************************ }
{ returns the parent node. }
function TALJSONNode.GetParentNode: TALJSONNode;
begin
  result := FParentNode;
end;

{ ****************************************** }
{ Sets the value of the ParentNode property. }
procedure TALJSONNode.SetParentNode(const Value: TALJSONNode);
begin
  If Assigned(Value) then
    SetOwnerDocument(Value.OwnerDocument)
  else
    SetOwnerDocument(nil);
  FParentNode := Value
end;

{ ******************************************************************* }
{ Returns the JSON that corresponds to the subtree rooted at this node.
  GetJSON returns the JSON that corresponds to this node and any child nodes it contains. }
function TALJSONNode.GetJSON: AnsiString;
begin
  SaveToJSON(result);
end;

{ ************************************************ }
{ SetJSON reload the node with the new given value }
procedure TALJSONNode.SetJSON(const Value: AnsiString);
Begin
  LoadFromJSON(Value, True { ClearChildNodes } );
end;

{ ******************************************************************* }
{ Returns the BSON that corresponds to the subtree rooted at this node.
  GetBSON returns the BSON that corresponds to this node and any child nodes it contains. }
function TALJSONNode.GetBSON: AnsiString;
begin
  SaveToBSON(result);
end;

{ ************************************************ }
{ SetBSON reload the node with the new given value }
procedure TALJSONNode.SetBSON(const Value: AnsiString);
Begin
  LoadFromBSON(Value, True { ClearChildNodes } );
end;

{ ***************************************************************** }
{ Returns the number of parents for this node in the node hierarchy.
  NestingLevel returns the number of ancestors for this node in the node hierarchy. }
function TALJSONNode.NestingLevel: integer;
var
  PNode: TALJSONNode;
begin
  result := 0;
  PNode := ParentNode;
  while PNode <> nil do
  begin
    inc(result);
    PNode := PNode.ParentNode;
  end;
end;

{ ********************************************************* }
constructor TALJSONNode.Create(const NodeName: AnsiString);
Begin
  FDocument := nil;
  FParentNode := nil;
  fNodeName := NodeName;
end;

{ ****************************************************************************************************************************************** }
function TALJSONNode.AddChild(const NodeName: AnsiString;
  const NodeType: TALJSONNodeType = ntText; const Index: integer = -1)
  : TALJSONNode;
begin
  result := ALCreateJSONNode(NodeName, NodeType);
  Try
    ChildNodes.Insert(Index, result);
  except
    FreeAndNil(result);
    raise;
  end;
end;

{ ************************************************************************************************************** }
function TALJSONNode.AddChild(const NodeType: TALJSONNodeType = ntText;
  const Index: integer = -1): TALJSONNode;
begin
  result := AddChild('', NodeType, Index);
end;

{ ****************************************** }
{ Returns the next child of this node’s parent.
  NextSibling returns the node that follows this one in the parent node’s ChildNodes property list.
  If this node is the last node in its parent’s child list, NextSibling raises an exception. }
function TALJSONNode.NextSibling: TALJSONNode;
begin
  if Assigned(ParentNode) then
    result := ParentNode.ChildNodes.FindSibling(Self, 1)
  else
    result := nil;
end;

{ ************************************************ }
{ Returns the previous child of this node’s parent.
  PreviousSibling returns the node that precedes this one in the parent node’s ChildNodes property list.
  If this node is the first node in its parent’s child list, PreviousSibling raises an exception. }
function TALJSONNode.PreviousSibling: TALJSONNode;
begin
  if Assigned(ParentNode) then
    result := ParentNode.ChildNodes.FindSibling(Self, -1)
  else
    result := nil;
end;

{ ****************************************************************************** }
{ Returns the JSON that corresponds to this node and any child nodes it contains. }
procedure TALJSONNode.SaveToStream(const Stream: TStream;
  const BSONStream: Boolean = False);

{ ~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  procedure _SaveToJSONStream;

  Const
    BufferSize: integer = 32768;
  Var
    NodeStack: Tstack;
    CurrentNode: TALJSONNode;
    CurrentParentNode: TALJSONNode;
    CurrentIndentStr: AnsiString;
    IndentStr: AnsiString;
    EncodeControlCharacters: Boolean;
    AddNodeSubTypeHelperFunct: Boolean;
    AutoIndentNode: Boolean;
    BufferString: AnsiString;
    BufferStringPos: integer;
    LastWrittenChar: AnsiChar;

    { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteBuffer2Stream(const buffer: AnsiString;
      BufferLength: integer);
    Begin
      If BufferLength > 0 then
        Stream.Write(buffer[1], BufferLength);
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteStr2Buffer(const Str: AnsiString);
    var
      L: integer;
    Begin
      L := Length(Str);
      if L = 0 then
        Exit;
      LastWrittenChar := Str[L];
      if L >= BufferSize then
      begin
        WriteBuffer2Stream(BufferString, BufferStringPos);
        BufferStringPos := 0;
        WriteBuffer2Stream(Str, L)
      end
      else
      begin
        if L + BufferStringPos > Length(BufferString) then
          setlength(BufferString, L + BufferStringPos);
        ALMove(Str[1], BufferString[BufferStringPos + 1], L);
        BufferStringPos := BufferStringPos + L;
        if BufferStringPos >= BufferSize then
        begin
          WriteBuffer2Stream(BufferString, BufferStringPos);
          BufferStringPos := 0;
        end;
      end;
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteTextNode2Stream(aTextNode: TALJSONNode);
    var
      aInt32: integer;
      aInt64: system.Int64;
    Begin
      with aTextNode do
      begin

        if not(LastWrittenChar in ['{', '[']) then
          WriteStr2Buffer(',');

        if AutoIndentNode then
          WriteStr2Buffer(#13#10 + CurrentIndentStr);

        if (Assigned(ParentNode)) and (ParentNode.NodeType <> ntArray) then
        begin
          if EncodeControlCharacters then
            WriteStr2Buffer('"' + ALJavascriptEncode(NodeName) + '":')
          else
            WriteStr2Buffer('"' + NodeName + '":');
        end;

        if NodeSubType = nstText then
        begin
          if EncodeControlCharacters then
            WriteStr2Buffer('"' + ALJavascriptEncode(Text) + '"')
          else
            WriteStr2Buffer('"' + Text + '"');
        end
        else if (NodeSubType = nstInt32) and (AddNodeSubTypeHelperFunct) and
          (ALTryStrToInt(NodeValue, aInt32)) then
          WriteStr2Buffer('NumberInt(' + alinttostr(aInt32) + ')')
        else if (NodeSubType = nstInt64) and (AddNodeSubTypeHelperFunct) and
          (ALTryStrToInt64(NodeValue, aInt64)) then
          WriteStr2Buffer('NumberLong(' + alinttostr(aInt64) + ')')
        else
          WriteStr2Buffer(NodeValue);

      end;
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteStartObjectNode2Stream(aObjectNode: TALJSONNode);
    var
      aNodeList: TALJSONNodeList;
      aEmptyNode: Boolean;
      i: integer;
    Begin
      with aObjectNode do
      begin

        if not(LastWrittenChar in ['{', '[']) then
          WriteStr2Buffer(',');

        if AutoIndentNode and (CurrentIndentStr <> '') then
          WriteStr2Buffer(#13#10 + CurrentIndentStr);

        if aObjectNode = Self then
          WriteStr2Buffer('{')
        else if (Assigned(ParentNode)) and (ParentNode.NodeType <> ntArray) then
        begin
          if EncodeControlCharacters then
            WriteStr2Buffer('"' + ALJavascriptEncode(NodeName) + '":{')
          else
            WriteStr2Buffer('"' + NodeName + '":{');
        end
        else
          WriteStr2Buffer('{');

        aEmptyNode := True;
        aNodeList := InternalGetChildNodes;
        If Assigned(aNodeList) then
        begin
          with aNodeList do
            If Count > 0 then
            begin
              aEmptyNode := False;
              NodeStack.Push(aObjectNode);
              For i := Count - 1 downto 0 do
                NodeStack.Push(Nodes[i]);
            end
        end;

        If aEmptyNode then
          WriteStr2Buffer('}')
        else
          CurrentIndentStr := CurrentIndentStr + IndentStr;

      end;
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteEndObjectNode2Stream(aObjectNode: TALJSONNode);
    Begin
      if AutoIndentNode then
      begin
        Delete(CurrentIndentStr, Length(CurrentIndentStr) - Length(IndentStr) +
          1, Maxint);
        WriteStr2Buffer(#13#10 + CurrentIndentStr);
      end;
      WriteStr2Buffer('}');
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteStartArrayNode2Stream(aArrayNode: TALJSONNode);
    var
      aNodeList: TALJSONNodeList;
      aEmptyNode: Boolean;
      i: integer;
    Begin
      with aArrayNode do
      begin

        if not(LastWrittenChar in ['{', '[']) then
          WriteStr2Buffer(',');

        if AutoIndentNode and (CurrentIndentStr <> '') then
          WriteStr2Buffer(#13#10 + CurrentIndentStr);

        if aArrayNode = Self then
          WriteStr2Buffer('[')
        else if (Assigned(ParentNode)) and (ParentNode.NodeType <> ntArray) then
        begin
          if EncodeControlCharacters then
            WriteStr2Buffer('"' + ALJavascriptEncode(NodeName) + '":[')
          else
            WriteStr2Buffer('"' + NodeName + '":[');
        end
        else
          WriteStr2Buffer('[');

        aEmptyNode := True;
        aNodeList := InternalGetChildNodes;
        If Assigned(aNodeList) then
        begin
          with aNodeList do
            If Count > 0 then
            begin
              aEmptyNode := False;
              NodeStack.Push(aArrayNode);
              For i := Count - 1 downto 0 do
                NodeStack.Push(Nodes[i]);
            end
        end;

        If aEmptyNode then
          WriteStr2Buffer(']')
        else
          CurrentIndentStr := CurrentIndentStr + IndentStr;

      end;
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteEndArrayNode2Stream(aArrayNode: TALJSONNode);
    Begin
      if AutoIndentNode then
      begin
        Delete(CurrentIndentStr, Length(CurrentIndentStr) - Length(IndentStr) +
          1, Maxint);
        WriteStr2Buffer(#13#10 + CurrentIndentStr);
      end;
      WriteStr2Buffer(']');
    end;

  begin
    If not(NodeType in [ntObject, ntArray]) then
      Exit; // normally only Object node can gave a valid json stream
    // but their is some situation where the array (containing json node)
    // is also usefull
    CurrentParentNode := nil;
    NodeStack := Tstack.Create;
    Try

      { init buffer string }
      setlength(BufferString, BufferSize * 2);
      BufferStringPos := 0;
      LastWrittenChar := '{';
      EncodeControlCharacters :=
        not(poIgnoreControlCharacters in FDocument.ParseOptions);
      AddNodeSubTypeHelperFunct :=
        (poAddNodeSubTypeHelperFunct in FDocument.ParseOptions);
      AutoIndentNode := (doNodeAutoIndent in FDocument.Options);
      IndentStr := FDocument.NodeIndentStr;
      CurrentIndentStr := '';

      { SaveOnlyChildNode }
      NodeStack.Push(Self);

      { loop on all nodes }
      While NodeStack.Count > 0 Do
      begin
        CurrentNode := TALJSONNode(NodeStack.Pop);

        with CurrentNode do
          case NodeType of
            ntObject:
              begin
                if CurrentNode = CurrentParentNode then
                  WriteEndObjectNode2Stream(CurrentNode)
                else
                  WriteStartObjectNode2Stream(CurrentNode);
              end;
            ntArray:
              begin
                if CurrentNode = CurrentParentNode then
                  WriteEndArrayNode2Stream(CurrentNode)
                else
                  WriteStartArrayNode2Stream(CurrentNode);
              end;
            ntText:
              WriteTextNode2Stream(CurrentNode);
          else
            ALJSONDocError(cALJSONInvalidNodeType);
          end;

        CurrentParentNode := CurrentNode.ParentNode;
      end;

      { Write the buffer }
      WriteBuffer2Stream(BufferString, BufferStringPos);

    finally
      NodeStack.Free;
    end;
  end;

{ ~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  procedure _SaveToBSONStream;

  Const
    BufferSize: integer = 32768;
  Var
    NodeStack: Tstack;
    NodeIndexStack: Tstack;
    NodeStartPosStack: Tstack;
    CurrentNode: TALJSONNode;
    CurrentParentNode: TALJSONNode;
    CurrentNodeIndex: integer;
    CurrentNodeStartPos: system.Int64;
    BufferString: AnsiString;
    BufferStringPos: integer;

    { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteBuffer2Stream(const buffer: AnsiString;
      BufferLength: integer);
    Begin
      If BufferLength > 0 then
        Stream.Write(buffer[1], BufferLength);
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteStr2Buffer(const Str: AnsiString);
    var
      L: integer;
    Begin
      L := Length(Str);
      if L = 0 then
        Exit;
      if L >= BufferSize then
      begin
        WriteBuffer2Stream(BufferString, BufferStringPos);
        BufferStringPos := 0;
        WriteBuffer2Stream(Str, L);
      end
      else
      begin
        if L + BufferStringPos > Length(BufferString) then
          setlength(BufferString, L + BufferStringPos);
        ALMove(Str[1], BufferString[BufferStringPos + 1], L);
        BufferStringPos := BufferStringPos + L;
        if BufferStringPos >= BufferSize then
        begin
          WriteBuffer2Stream(BufferString, BufferStringPos);
          BufferStringPos := 0;
        end;
      end;
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
  // take care because fucking TStringStream (for exemple) do not permit
  // to write previous to the current position (it's set the size of the
  // new stream to the current position ... unbelievable!)
    Procedure WriteInt2Pos(const aInt: integer; const aPos: system.Int64);
    var
      aTmpPos: system.Int64;
    Begin
      if aPos < Stream.Position then
      begin
        aTmpPos := Stream.Position;
        Stream.Position := aPos;
        Stream.Write(aInt, sizeof(aInt));
        Stream.Position := aTmpPos;
      end
      else
        ALMove(aInt, BufferString[aPos - Stream.Position + 1], sizeof(aInt));
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteTextNode2Stream(aTextNode: TALJSONNode; aNodeIndex: integer);
    var
      aBinStr: AnsiString;
      aNodeName: AnsiString;
      aDouble: Double;
      aInt32: integer;
      aInt64: system.Int64;
      aObjectID: TALJSONObjectID;
      aTimestamp: TALBSONTimestamp;
      aRegEx: TALJSONRegEx;
      aBinary: TALJSONBinary;
    Begin
      with aTextNode do
      begin

        // calculate the aNodeName
        if (Assigned(ParentNode)) and (ParentNode.NodeType = ntArray) then
          aNodeName := alinttostr(aNodeIndex)
        else
          aNodeName := NodeName;

        // add the nodevalue to the buffer
        case NodeSubType of

          // \x01 + name + \x00 + double
          nstFloat:
            begin
              aDouble := Float;
              setlength(aBinStr, sizeof(aDouble));
              ALMove(aDouble, aBinStr[1], sizeof(aDouble));
              WriteStr2Buffer(#$01 + aNodeName + #$00 + aBinStr);
            end;

          // \x02 + name + \x00 + length (int32) + string + \x00
          nstText:
            begin
              aInt32 := Length(Text) + 1 { for the trailing #0 };
              setlength(aBinStr, sizeof(aInt32));
              ALMove(aInt32, aBinStr[1], sizeof(aInt32));
              WriteStr2Buffer(#$02 + aNodeName + #$00 + aBinStr + Text + #$00);
            end;

          // \x05 + name + \x00 + int32 + subtype + (byte*)
          nstBinary:
            begin
              aBinary := Binary;
              aInt32 := Length(aBinary.Data);
              setlength(aBinStr, sizeof(aInt32));
              ALMove(aInt32, aBinStr[1], sizeof(aInt32));
              WriteStr2Buffer(#$05 + aNodeName + #$00 + aBinStr +
                AnsiChar(aBinary.Subtype) + aBinary.Data);
            end;

          // \x07 + name + \x00 + (byte*12)
          nstObjectID:
            begin
              aObjectID := ObjectID;
              setlength(aBinStr, sizeof(aObjectID));
              ALMove(aObjectID[1], aBinStr[1], sizeof(aObjectID));
              WriteStr2Buffer(#$07 + aNodeName + #$00 + aBinStr);
            end;

          // \x08 + name + \x00 + \x00 => Boolean "false"
          // \x08 + name + \x00 + \x01	=> Boolean "true"
          nstBoolean:
            begin
              if not Bool then
                WriteStr2Buffer(#$08 + aNodeName + #$00 + #$00)
              else
                WriteStr2Buffer(#$08 + aNodeName + #$00 + #$01);
            end;

          // \x09 + name + \x00 + int64
          nstDateTime:
            begin
              aInt64 := ALDateTimeToUnixMs(DateTime);
              setlength(aBinStr, sizeof(aInt64));
              ALMove(aInt64, aBinStr[1], sizeof(aInt64));
              WriteStr2Buffer(#$09 + aNodeName + #$00 + aBinStr);
            end;

          // \x11 + name + \x00 + int64
          nstTimestamp:
            begin
              aTimestamp := Timestamp;
              setlength(aBinStr, sizeof(aInt64));
              ALMove(aTimestamp.I64, aBinStr[1], sizeof(aInt64));
              WriteStr2Buffer(#$11 + aNodeName + #$00 + aBinStr);
            end;

          // \x0A + name + \x00
          nstNull:
            begin
              WriteStr2Buffer(#$0A + aNodeName + #$00);
            end;

          // \xOB + name + \x00 + (byte*) + \x00 + (byte*) + \x00
          nstRegEx:
            begin
              aRegEx := RegEx;
              WriteStr2Buffer(#$0B + aNodeName + #$00 + aRegEx.Expression + #$00
                + aRegEx.Options + #$00);
            end;

          // \x0D + name + \x00 + length (int32) + string + \x00
          nstJavascript:
            begin
              aInt32 := Length(Text) + 1 { for the trailing #0 };
              setlength(aBinStr, sizeof(aInt32));
              ALMove(aInt32, aBinStr[1], sizeof(aInt32));
              WriteStr2Buffer(#$0D + aNodeName + #$00 + aBinStr + Text + #$00);
            end;

          // \x10 + name + \x00 + int32
          nstInt32:
            begin
              aInt32 := int32;
              setlength(aBinStr, sizeof(aInt32));
              ALMove(aInt32, aBinStr[1], sizeof(aInt32));
              WriteStr2Buffer(#$10 + aNodeName + #$00 + aBinStr);
            end;

          // \x12 + name + \x00 + int64
          nstInt64:
            begin
              aInt64 := Int64;
              setlength(aBinStr, sizeof(aInt64));
              ALMove(aInt64, aBinStr[1], sizeof(aInt64));
              WriteStr2Buffer(#$12 + aNodeName + #$00 + aBinStr);
            end

        else
          ALJSONDocError(cALJSONInvalidBSONNodeSubType);

        end;

      end;
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteStartObjectNode2Stream(aObjectNode: TALJSONNode;
      aNodeIndex: integer);
    var
      aNodeList: TALJSONNodeList;
      aEmptyNode: Boolean;
      aPos: system.Int64;
      i: integer;
    Begin
      with aObjectNode do
      begin

        if aObjectNode = Self then
          WriteStr2Buffer(#$00 + #$00 + #$00 + #$00)
        else if (Assigned(ParentNode)) and (ParentNode.NodeType = ntArray) then
          WriteStr2Buffer(#$03 + alinttostr(aNodeIndex) + #$00 + #$00 + #$00 +
            #$00 + #$00)
        else
          WriteStr2Buffer(#$03 + NodeName + #$00 + #$00 + #$00 + #$00 + #$00);

        aPos := Stream.Position + BufferStringPos -
          4 { length of the #$00+#$00+#$00+#$00 };

        aEmptyNode := True;
        aNodeList := InternalGetChildNodes;
        If Assigned(aNodeList) then
        begin
          with aNodeList do
            If Count > 0 then
            begin
              aEmptyNode := False;
              NodeStack.Push(aObjectNode);
              NodeIndexStack.Push(pointer(aNodeIndex));
              NodeStartPosStack.Push(pointer(aPos));
              For i := Count - 1 downto 0 do
              begin
                NodeStack.Push(Nodes[i]);
                NodeIndexStack.Push(pointer(i));
                NodeStartPosStack.Push(pointer(-1));
              end;
            end
        end;

        If aEmptyNode then
        begin
          WriteStr2Buffer(#$00);
          WriteInt2Pos(5 { length of the object } , aPos);
        end;

      end;
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteEndObjectNode2Stream(aObjectNode: TALJSONNode;
      aNodeStartPos: system.Int64);
    Begin
      WriteStr2Buffer(#$00);
      WriteInt2Pos(Stream.Position + BufferStringPos - aNodeStartPos,
        aNodeStartPos);
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteStartArrayNode2Stream(aArrayNode: TALJSONNode;
      aNodeIndex: integer);
    var
      aNodeList: TALJSONNodeList;
      aEmptyNode: Boolean;
      aPos: system.Int64;
      i: integer;
    Begin
      with aArrayNode do
      begin

        if (Assigned(ParentNode)) and (ParentNode.NodeType = ntArray) then
          WriteStr2Buffer(#$04 + alinttostr(aNodeIndex) + #$00 + #$00 + #$00 +
            #$00 + #$00)
        else
          WriteStr2Buffer(#$04 + NodeName + #$00 + #$00 + #$00 + #$00 + #$00);

        aPos := Stream.Position + BufferStringPos -
          4 { length of the #$00+#$00+#$00+#$00 };

        aEmptyNode := True;
        aNodeList := InternalGetChildNodes;
        If Assigned(aNodeList) then
        begin
          with aNodeList do
            If Count > 0 then
            begin
              aEmptyNode := False;
              NodeStack.Push(aArrayNode);
              NodeIndexStack.Push(pointer(aNodeIndex));
              NodeStartPosStack.Push(pointer(aPos));
              For i := Count - 1 downto 0 do
              begin
                NodeStack.Push(Nodes[i]);
                NodeIndexStack.Push(pointer(i));
                NodeStartPosStack.Push(pointer(-1));
              end;
            end
        end;

        If aEmptyNode then
        begin
          WriteStr2Buffer(#$00);
          WriteInt2Pos(5 { length of the object } , aPos);
        end;

      end;
    end;

  { ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ }
    Procedure WriteEndArrayNode2Stream(aArrayNode: TALJSONNode;
      aNodeStartPos: system.Int64);
    Begin
      WriteStr2Buffer(#$00);
      WriteInt2Pos(Stream.Position + BufferStringPos - aNodeStartPos,
        aNodeStartPos);
    end;

  begin
    If NodeType <> ntObject then
      Exit;

    CurrentParentNode := nil;
    NodeStack := Tstack.Create;
    NodeIndexStack := Tstack.Create;
    NodeStartPosStack := Tstack.Create;
    Try

      { init buffer string }
      setlength(BufferString, BufferSize * 2);
      BufferStringPos := 0;

      { SaveOnlyChildNode }
      NodeStack.Push(Self);
      NodeIndexStack.Push(pointer(0));
      NodeStartPosStack.Push(pointer(Stream.Position));

      { loop on all nodes }
      While NodeStack.Count > 0 Do
      begin
        CurrentNode := TALJSONNode(NodeStack.Pop);
        CurrentNodeIndex := integer(NodeIndexStack.Pop);
        CurrentNodeStartPos := system.Int64(NodeStartPosStack.Pop);

        with CurrentNode do
          case NodeType of
            ntObject:
              begin
                if CurrentNode = CurrentParentNode then
                  WriteEndObjectNode2Stream(CurrentNode, CurrentNodeStartPos)
                else
                  WriteStartObjectNode2Stream(CurrentNode, CurrentNodeIndex);
              end;
            ntArray:
              begin
                if CurrentNode = CurrentParentNode then
                  WriteEndArrayNode2Stream(CurrentNode, CurrentNodeStartPos)
                else
                  WriteStartArrayNode2Stream(CurrentNode, CurrentNodeIndex);
              end;
            ntText:
              WriteTextNode2Stream(CurrentNode, CurrentNodeIndex);
          else
            ALJSONDocError(cALJSONInvalidNodeType);
          end;

        CurrentParentNode := CurrentNode.ParentNode;
      end;

      { Write the buffer }
      WriteBuffer2Stream(BufferString, BufferStringPos);

    finally
      NodeStack.Free;
      NodeIndexStack.Free;
      NodeStartPosStack.Free;
    end;
  end;

begin
  if BSONStream then
    _SaveToBSONStream
  else
    _SaveToJSONStream;
end;

{ ************************************************************************************************************************************ }
procedure TALJSONNode.LoadFromStream(const Stream: TStream;
  const BSONStream: Boolean = False; Const ClearChildNodes: Boolean = True);
Begin
  If NodeType <> ntObject then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  if ClearChildNodes then
    ChildNodes.Clear;
  Try
    if BSONStream then
      FDocument.ParseBSONStream(Stream, Self)
    else
      FDocument.ParseJSONStream(Stream, Self);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{ ********************************************************************************************* }
procedure TALJSONNode.SaveToFile(const AFileName: AnsiString;
  const BSONFile: Boolean = False);
Var
  afileStream: TFileStream;
begin
  afileStream := TFileStream.Create(String(AFileName), fmCreate);
  Try
    SaveToStream(afileStream, BSONFile);
  finally
    afileStream.Free;
  end;
end;

{ ************************************************************************************************************************************** }
procedure TALJSONNode.LoadFromFile(const AFileName: AnsiString;
  const BSONFile: Boolean = False; Const ClearChildNodes: Boolean = True);
Var
  afileStream: TFileStream;
Begin
  afileStream := TFileStream.Create(string(AFileName), fmOpenRead or
    fmShareDenyWrite);
  Try
    LoadFromStream(afileStream, BSONFile, ClearChildNodes);
  finally
    afileStream.Free;
  end;
end;

{ ***************************************************** }
procedure TALJSONNode.SaveToJSON(var JSON: AnsiString);
Var
  aStringStream: TALStringStream;
begin
  aStringStream := TALStringStream.Create('');
  Try
    SaveToStream(aStringStream, False { BSONStream } );
    JSON := aStringStream.DataString;
  finally
    aStringStream.Free;
  end;
end;

{ ***************************************************** }
procedure TALJSONNode.SaveToBSON(var BSON: AnsiString);
Var
  aStringStream: TALStringStream;
begin
  aStringStream := TALStringStream.Create('');
  Try
    SaveToStream(aStringStream, True { BSONStream } );
    BSON := aStringStream.DataString;
  finally
    aStringStream.Free;
  end;
end;

{ ************************************************************************************************ }
procedure TALJSONNode.LoadFromJSON(const JSON: AnsiString;
  Const ClearChildNodes: Boolean = True);
Var
  aStringStream: TALStringStream;
Begin
  aStringStream := TALStringStream.Create(JSON);
  Try
    LoadFromStream(aStringStream, False { BSONStream } , ClearChildNodes);
  finally
    aStringStream.Free;
  end;
end;

{ ************************************************************************************************ }
procedure TALJSONNode.LoadFromBSON(const BSON: AnsiString;
  Const ClearChildNodes: Boolean = True);
Var
  aStringStream: TALStringStream;
Begin
  aStringStream := TALStringStream.Create(BSON);
  Try
    LoadFromStream(aStringStream, True { BSONStream } , ClearChildNodes);
  finally
    aStringStream.Free;
  end;
end;

{ ******************************************************************** }
constructor TALJSONObjectNode.Create(const NodeName: AnsiString = '');
begin
  inherited Create(NodeName);
  FChildNodes := nil;
end;

{ *********************************** }
destructor TALJSONObjectNode.Destroy;
begin
  If Assigned(FChildNodes) then
    FreeAndNil(FChildNodes);
  inherited;
end;

{ ******************************************************* }
function TALJSONObjectNode.GetChildNodes: TALJSONNodeList;
begin
  if not Assigned(FChildNodes) then
    SetChildNodes(CreateChildList);
  result := FChildNodes;
end;

{ ********************************************************************* }
procedure TALJSONObjectNode.SetChildNodes(const Value: TALJSONNodeList);
begin
  If Assigned(FChildNodes) then
    FreeAndNil(FChildNodes);
  FChildNodes := Value;
end;

{ ****************************************************** }
function TALJSONObjectNode.GetNodeType: TALJSONNodeType;
begin
  result := ntObject;
end;

{ ************************************************************ }
function TALJSONObjectNode.GetNodeSubType: TALJSONNodeSubType;
begin
  result := nstObject;
end;

{ ************************************************************************** }
procedure TALJSONObjectNode.SetNodeSubType(const Value: TALJSONNodeSubType);
begin
  ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr])
end;

{ ******************************************** }
{ Get Childnode without create it if not exist }
function TALJSONObjectNode.InternalGetChildNodes: TALJSONNodeList;
begin
  result := FChildNodes;
end;

{ ******************************************************************* }
constructor TALJSONArrayNode.Create(const NodeName: AnsiString = '');
begin
  inherited Create(NodeName);
  FChildNodes := nil;
end;

{ *********************************** }
destructor TALJSONArrayNode.Destroy;
begin
  If Assigned(FChildNodes) then
    FreeAndNil(FChildNodes);
  inherited;
end;

{ ******************************************************* }
function TALJSONArrayNode.GetChildNodes: TALJSONNodeList;
begin
  if not Assigned(FChildNodes) then
    SetChildNodes(CreateChildList);
  result := FChildNodes;
end;

{ ********************************************************************* }
procedure TALJSONArrayNode.SetChildNodes(const Value: TALJSONNodeList);
begin
  If Assigned(FChildNodes) then
    FreeAndNil(FChildNodes);
  FChildNodes := Value;
end;

{ *************************************************** }
function TALJSONArrayNode.GetNodeType: TALJSONNodeType;
begin
  result := ntArray;
end;

{ *********************************************************** }
function TALJSONArrayNode.GetNodeSubType: TALJSONNodeSubType;
begin
  result := nstArray;
end;

{ ************************************************************************* }
procedure TALJSONArrayNode.SetNodeSubType(const Value: TALJSONNodeSubType);
begin
  ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr])
end;

{ ******************************************** }
{ Get Childnode without create it if not exist }
function TALJSONArrayNode.InternalGetChildNodes: TALJSONNodeList;
begin
  result := FChildNodes;
end;

{ ****************************************************************** }
constructor TALJSONTextNode.Create(const NodeName: AnsiString = '');
begin
  inherited Create(NodeName);
  fNodeSubType := nstText;
  fNodeValue := '';
end;

{ ************************************************** }
function TALJSONTextNode.GetNodeType: TALJSONNodeType;
begin
  result := ntText;
end;

{ ********************************************************** }
function TALJSONTextNode.GetNodeSubType: TALJSONNodeSubType;
begin
  result := fNodeSubType;
end;

{ ************************************************************************ }
procedure TALJSONTextNode.SetNodeSubType(const Value: TALJSONNodeSubType);
begin
  if Value in [nstObject, nstArray] then
    ALJSONDocError(cALJSONOperationError, [GetNodeTypeStr]);
  fNodeSubType := Value;
end;

{ ************************************************ }
function TALJSONTextNode.GetNodeValue: AnsiString;
begin
  result := fNodeValue;
end;

{ ***************************************************************************************************** }
procedure TALJSONTextNode.SetNodeValue(const Value: AnsiString;
  const NodeSubType: TALJSONNodeSubType);
begin
  fNodeSubType := NodeSubType;
  fNodeValue := Value;
end;

{ ***************************************************** }
constructor TALJSONNodeList.Create(Owner: TALJSONNode);
begin
  FList := nil;
  FCount := 0;
  FCapacity := 0;
  FOwner := Owner;
end;

{ ********************************* }
destructor TALJSONNodeList.Destroy;
begin
  Clear;
end;

{ *************************************** }
{ Returns the number of nodes in the list.
  GetCount is the read implementation of the Count property. }
function TALJSONNodeList.GetCount: integer;
begin
  result := FCount;
end;

{ ************************************* }
{ Returns the index of a specified node.
  Call IndexOf to locate a node in the list.
  *Node is the object node to locate.
  IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
  index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1. }
function TALJSONNodeList.IndexOf(const Node: TALJSONNode): integer;
begin
  result := 0;
  while (result < FCount) and (FList^[result] <> Node) do
    inc(result);
  if result = FCount then
    result := -1;
end;

{ ************************************* }
{ Returns the index of a specified node.
  Call IndexOf to locate a node in the list.
  *Name is the NodeName property of the node to locate.
  IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
  index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1. }
function TALJSONNodeList.IndexOf(const name: AnsiString): integer;
begin
  for result := 0 to Count - 1 do
    if ALNodeMatches(Get(result), Name) then
      Exit;
  result := -1;
end;

{ ************************************** }
{ Returns a specified node from the list.
  Call FindNode to access a particular node in the list.
  *NodeName is the node to access. It specifies the NodeName property of the desired node.
  FindNode returns the object of the node if it is in the list. If NodeName does not specify a node in the list,
  FindNode returns nil (Delphi) or NULL (C++). }
function TALJSONNodeList.FindNode(NodeName: AnsiString): TALJSONNode;
var
  Index: integer;
begin
  Index := IndexOf(NodeName);
  if Index >= 0 then
    result := Get(Index)
  else
    result := nil;
end;

{ ********************************** }
{ Returns the first node in the list.
  Call First to access the first node in the list. If the list is empty, First raises an exception }
function TALJSONNodeList.First: TALJSONNode;
begin
  if Count > 0 then
    result := Get(0)
  else
    result := nil;
end;

{ ********************************* }
{ Returns the last node in the list.
  Call Last to access the last node in the list. If the list is empty, Last raises an exception. }
function TALJSONNodeList.Last: TALJSONNode;
begin
  if Count > 0 then
    result := Get(FCount - 1)
  else
    result := nil;
end;

{ *************************************************************************** }
{ Returns a node that appears a specified amount before or after another node.
  Call FindSibling to access the node whose position has a specified relationship to another node.
  *Node is a node in the list to use as a reference point.
  *Delta indicates where the desired node appears, relative to Node. If Delta is positive, FindSibling returns
  the node that appears Delta positions after Node. If Delta is negative, FindSibling returns a node that appears before Node.
  FindSibling returns the node that appears at the position offset by Delta, relative to the position of Node. If Delta
  specifies a position before the first node or after the last node in the list, FindSibling returns nil (Delphi) or NULL (C++). }
function TALJSONNodeList.FindSibling(const Node: TALJSONNode; Delta: integer)
  : TALJSONNode;
var
  Index: integer;
begin
  Index := IndexOf(Node) + Delta;
  if (Index >= 0) and (Index < FCount) then
    result := Get(Index)
  else
    result := nil;
end;

{ ************************************ }
{ Returns a specified node in the list.
  Call Get to retrieve a node from the list, given its index.
  *Index specifies the node to fetch, where 0 identifies the first node, 1 identifies the second node, and so on.
  Index should be less than the value of the Count property. }
function TALJSONNodeList.Get(Index: integer): TALJSONNode;
begin
  if (Index < 0) or (Index >= FCount) then
    ALJSONDocError(cALJSONListIndexError, [Index]);
  result := FList^[Index];
end;

{ ************************** }
{$IF CompilerVersion < 18.5}

{ Returns a specified node from the list.
  GetNode is the read implementation of the Nodes property.
  *IndexOrName identifies the desired node. It can be The index of the node, where 0 is the index of the first node,
  1 is the index of the second node, and so on. The NodeName property of a node in the list.
  If IndexOrName does not identify a node in the list, GetNode tries to create a new node with the name specified by
  IndexOrName. If it can’t create the new node, GetNode raises an exception. }
function TALJSONNodeList.GetNode(const IndexOrName: OleVariant): TALJSONNode;
begin
  if VarIsOrdinal(IndexOrName) then
    result := GetNodeByIndex(IndexOrName)
  else
    result := GetNodeByName(AnsiString(IndexOrName));
end;
{$IFEND}

{ ************************************** }
{ Returns a specified node from the list.
  GetNode is the read implementation of the Nodes property.
  *Index identify the desired node. 0 is the index of the first node,
  1 is the index of the second node, and so on }
function TALJSONNodeList.GetNodeByIndex(const Index: integer): TALJSONNode;
begin
  result := Get(Index);
end;

{ ************************************** }
{ Returns a specified node from the list.
  GetNode is the read implementation of the Nodes property.
  *Name identify the desired node. it is the NodeName property of a node in the list.
  If Name does not identify a node in the list, GetNode tries to create a new node with the name specified by
  Name. If it can’t create the new node, GetNode raises an exception. }
function TALJSONNodeList.GetNodeByName(const name: AnsiString): TALJSONNode;
begin
  result := FindNode(Name);
  if (not Assigned(result)) and (Assigned(FOwner.OwnerDocument)) and
    (doNodeAutoCreate in FOwner.OwnerDocument.Options) then
    result := FOwner.AddChild(Name);
  // only text node will be added via doNodeAutoCreate
  if not Assigned(result) then
    ALJSONDocError(cAlJSONNodeNotFound, [Name]);
end;

{ *************************************************************************************** }
procedure TALJSONNodeList.QuickSort(L, R: integer;
  XCompare: TALJSONNodeListSortCompare);
var
  i, J, P: integer;
begin
  repeat
    i := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while XCompare(Self, i, P) < 0 do
        inc(i);
      while XCompare(Self, J, P) > 0 do
        dec(J);
      if i <= J then
      begin
        Exchange(i, J);
        if P = i then
          P := J
        else if P = J then
          P := i;
        inc(i);
        dec(J);
      end;
    until i > J;
    if L < J then
      QuickSort(L, J, XCompare);
    L := i;
  until i >= R;
end;

{ ********************************************************************** }
procedure TALJSONNodeList.CustomSort(Compare: TALJSONNodeListSortCompare);
begin
  if (FList <> nil) and (Count > 1) then
    QuickSort(0, Count - 1, Compare);
end;

{ ************************************** }
{ Adds a new node to the end of the list.
  Call Add to add a node to the end of the list. Add returns the index of the node once it is added, where 0 is the index
  of the first node in the list, 1 is the index of the second node, and so on.
  *Node is the node to add to the list. }
function TALJSONNodeList.Add(const Node: TALJSONNode): integer;
begin
  Insert(-1, Node);
  result := FCount - 1;
end;

{ ******************************************************** }
{ Inserts a new node into a specified position in the list.
  Call Insert to add a node at the position specified by Index.
  *Index specifies where to insert the node, where 0 is the first position, 1 is second position, and so on. If Index does not
  specify a valid index, Insert raises an exception.
  *Node is the node to add to the list. }
procedure TALJSONNodeList.Insert(Index: integer; const Node: TALJSONNode);
begin
  if Index = -1 then
  begin
    index := FCount;
    if index = FCapacity then
      Grow;
  end
  else
  begin
    if (Index < 0) or (Index > FCount) then
      ALJSONDocError(cALJSONListIndexError, [Index]);
    if FCount = FCapacity then
      Grow;
    if Index < FCount then
      ALMove(FList^[Index], FList^[Index + 1],
        (FCount - Index) * sizeof(pointer));
  end;
  FList^[index] := Node;
  inc(FCount);
  Node.SetParentNode(FOwner);
end;

{ ************************************** }
{ Removes a specified node from the list.
  Delete removes the node specified by the Index or Name parameter.
  *Index identifies the node to remove by index rather than name. Index ranges from 0 to one less than the value of the Count property.
  Delete returns the index of the node that was removed. If there was no node that matched the value of Index Delete returns –1. }
function TALJSONNodeList.Delete(const Index: integer): integer;
var
  Node: TALJSONNode;
begin
  Node := Get(Index);
  dec(FCount);
  if Index < FCount then
    ALMove(FList^[Index + 1], FList^[Index], (FCount - Index) *
      sizeof(pointer));
  if Assigned(Node) then
    FreeAndNil(Node);
  result := Index;
end;

{ ************************************** }
{ Removes a specified node from the list.
  Delete removes the node specified by the Index or Name parameter.
  *Name identifies the node to remove from the list. This is the local name of the node to remove.
  Delete returns the index of the node that was removed. If there was no node that matched the value of Name, Delete returns –1. }
function TALJSONNodeList.Delete(const name: AnsiString): integer;
begin
  result := IndexOf(Name);
  if result >= 0 then
    Delete(result);
end;

{ ************************************** }
{ Removes a specified node from the list.
  Remove removes the specified node from the list.
  *Node is the node to remove from the list.
  Remove returns the index of Node before it was removed. If node is not a node in the list, Remove returns -1. }
function TALJSONNodeList.Remove(const Node: TALJSONNode): integer;
begin
  result := IndexOf(Node);
  if result >= 0 then
    Delete(result);
end;

{ ************************************************************ }
{ Removes a specified object from the list without freeing it.
  Call Extract to remove an object from the list without freeing the object itself.
  After an object is removed, all the objects that follow it are moved up in index position and Count is decremented. }
function TALJSONNodeList.Extract(const Node: TALJSONNode): TALJSONNode;
var
  i: integer;
begin
  result := nil;
  i := IndexOf(Node);
  if i >= 0 then
    result := Extract(i);
end;

{ ********************************************************* }
procedure TALJSONNodeList.Exchange(Index1, Index2: integer);
var
  Item: pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    ALJSONDocError(cALJSONListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= FCount) then
    ALJSONDocError(cALJSONListIndexError, [Index2]);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

{ *********************************************************** }
{ Removes a specified object from the list without freeing it.
  Call Extract to remove an object from the list without freeing the object itself.
  After an object is removed, all the objects that follow it are moved up in index position and Count is decremented. }
function TALJSONNodeList.Extract(const Index: integer): TALJSONNode;
begin
  result := Get(index);
  result.SetParentNode(nil);
  FList^[index] := nil;
  Delete(index);
end;

{ ********************************************* }
{ Replaces a node in the list with another node.
  Call ReplaceNode to replace the node specified by OldNode with the node specified by NewNode.
  *OldNode is the node to replace. If OldNode does not appear in the list, then ReplaceNode adds the new node to the end of the list.
  *NewNode is the node to add to the list in place of OldNode.
  ReplaceNode returns OldNode (even if OldNode did not appear in the list). }
function TALJSONNodeList.ReplaceNode(const OldNode, NewNode: TALJSONNode)
  : TALJSONNode;
var
  Index: integer;
begin
  Index := IndexOf(OldNode);
  result := Extract(Index);
  Insert(Index, NewNode);
end;

{ ******************************* }
{ Removes all nodes from the list.
  Call Clear to empty the list.
  Note:	Clear does not call the BeginUpdate and EndUpdate methods, even though it may result in the
  deletion of more than one node. }
procedure TALJSONNodeList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{ ***************************** }
procedure TALJSONNodeList.Grow;
var
  Delta: integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{ ********************************************************** }
procedure TALJSONNodeList.SetCapacity(NewCapacity: integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > cALJSONNodeMaxListSize) then
    ALJSONDocError(cALJSONListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * sizeof(pointer));
    FCapacity := NewCapacity;
  end;
end;

{ **************************************************** }
procedure TALJSONNodeList.SetCount(NewCount: integer);
var
  i: integer;
begin
  if (NewCount < 0) or (NewCount > cALJSONNodeMaxListSize) then
    ALJSONDocError(cALJSONListCountError, [NewCount]);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * sizeof(pointer), 0)
  else
    for i := FCount - 1 downto NewCount do
      Delete(i);
  FCount := NewCount;
end;

{ ************************************** }
{$IF CompilerVersion >= 23}
{ Delphi XE2 }
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
  const aFormatSettings: TALFormatSettings; const aPath: AnsiString;
  aLst: TALStrings; Const aNullStr: AnsiString = 'null';
  Const aTrueStr: AnsiString = 'true'; Const aFalseStr: AnsiString = 'false');

var
  aALJsonDocument: TALJSONDocument;
  aContainChilds: Boolean;
begin
  aALJsonDocument := TALJSONDocument.Create(aFormatSettings);
  try

    aALJsonDocument.onParseText :=
        procedure(Sender: TObject; const Path: AnsiString;
        const name: AnsiString; const Str: AnsiString;
        const NodeSubType: TALJSONNodeSubType)
      begin
        if (NodeSubType = nstBoolean) and (alSameText(Str, 'true')) then
          aLst.Add(aPath + Path + aLst.NameValueSeparator + aTrueStr)
        else if (NodeSubType = nstBoolean) and (alSameText(Str, 'false')) then
          aLst.Add(aPath + Path + aLst.NameValueSeparator + aFalseStr)
        else if (NodeSubType = nstNull) then
          aLst.Add(aPath + Path + aLst.NameValueSeparator + aNullStr)
        else
          aLst.Add(aPath + Path + aLst.NameValueSeparator + Str);
        aContainChilds := True;
      end;

    aALJsonDocument.onParseStartObject :=
        procedure(Sender: TObject; const Path: AnsiString;
        const name: AnsiString)
      begin
        aContainChilds := False;
      end;

    aALJsonDocument.onParseEndObject :=
        procedure(Sender: TObject; const Path: AnsiString;
        const name: AnsiString)
      begin
        if (not aContainChilds) and
          (aPath + Path <> '' { Path = '' mean it's the root object } ) then
          aLst.Add(aPath + Path + aLst.NameValueSeparator + '{}');
        aContainChilds := True;
      end;

    aALJsonDocument.onParseStartArray :=
        procedure(Sender: TObject; const Path: AnsiString;
        const name: AnsiString)
      begin
        aContainChilds := False;
      end;

    aALJsonDocument.onParseEndArray :=
        procedure(Sender: TObject; const Path: AnsiString;
        const name: AnsiString)
      begin
        if not aContainChilds then
          aLst.Add(aPath + Path + aLst.NameValueSeparator + '[]');
        aContainChilds := True;
      end;

    aALJsonDocument.LoadFromJSON(AJsonStr, True { saxMode } );
  finally
    aALJsonDocument.Free;
  end;
end;
{$IFEND}
{ ************************************** }
{$IF CompilerVersion >= 23}
{ Delphi XE2 }
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
  const aFormatSettings: TALFormatSettings; aLst: TALStrings;
  Const aNullStr: AnsiString = 'null'; Const aTrueStr: AnsiString = 'true';
  Const aFalseStr: AnsiString = 'false');
begin
  ALJSONToTStrings(AJsonStr, aFormatSettings, '', aLst, aNullStr, aTrueStr,
    aFalseStr);
end;
{$IFEND}

{ ****************************************************** }
Procedure ALJSONToTStrings(const aJsonNode: TALJSONNode;
  Const aPath: AnsiString; aLst: TALStrings;
  Const aNullStr: AnsiString = 'null'; Const aTrueStr: AnsiString = 'true';
  Const aFalseStr: AnsiString = 'false');
var
  aTmpPath: AnsiString;
  i: integer;
begin
  if aJsonNode.ChildNodes.Count > 0 then
  begin
    for i := 0 to aJsonNode.ChildNodes.Count - 1 do
    begin

      if aJsonNode.NodeType = ntArray then
        aTmpPath := aPath + '[' + alinttostr(i) + ']'
      else
      begin
        if aJsonNode.ChildNodes[i].NodeName = '' then
          raise Exception.Create('Nodename can not be empty');
        aTmpPath := aPath + ALIfThen(aPath <> '', '.', '') +
          aJsonNode.ChildNodes[i].NodeName;
      end;

      case aJsonNode.ChildNodes[i].NodeType of

        ntObject:
          ALJSONToTStrings(aJsonNode.ChildNodes[i], aTmpPath, aLst, aNullStr,
            aTrueStr, aFalseStr);

        ntArray:
          ALJSONToTStrings(aJsonNode.ChildNodes[i], aTmpPath, aLst, aNullStr,
            aTrueStr, aFalseStr);

        ntText:
          begin
            if (aJsonNode.ChildNodes[i].NodeSubType = nstBoolean) then
            begin
              if aJsonNode.ChildNodes[i].Bool then
                aLst.Add(aTmpPath + aLst.NameValueSeparator + aTrueStr)
              else
                aLst.Add(aTmpPath + aLst.NameValueSeparator + aFalseStr);
            end
            else if (aJsonNode.ChildNodes[i].NodeSubType = nstNull) then
              aLst.Add(aTmpPath + aLst.NameValueSeparator + aNullStr)
            else
              aLst.Add(aTmpPath + aLst.NameValueSeparator +
                aJsonNode.ChildNodes[i].Text);
          end;

      else
        raise Exception.Create('Unknown NodeType');

      end;
    end;
  end
  else if (aPath <> '' { aPath = '' mean it's the root object } ) then
  begin
    if aJsonNode.NodeType = ntArray then
      aLst.Add(aPath + aLst.NameValueSeparator + '[]')
    else if aJsonNode.NodeType = ntObject then
      aLst.Add(aPath + aLst.NameValueSeparator + '{}');
  end;
end;

{ ****************************************************** }
Procedure ALJSONToTStrings(const aJsonNode: TALJSONNode; aLst: TALStrings;
  Const aNullStr: AnsiString = 'null'; Const aTrueStr: AnsiString = 'true';
  Const aFalseStr: AnsiString = 'false');
begin
  ALJSONToTStrings(aJsonNode, '', aLst, aNullStr, aTrueStr, aFalseStr)
end;

{ ************************************************ }
procedure ALTStringsToJson(const aLst: TALStrings; aJsonNode: TALJSONNode;
  Const aPath: AnsiString = ''; Const aNameToLowerCase: Boolean = False;
  Const aNullStr: AnsiString = 'null');

var
  aIndex: integer;
  aNames: TALStringList;
  aLowerName: AnsiString;
  aCurrJsonNode, aTmpJsonNode: TALJSONNode;
  i, J: integer;

begin

  // create list of the part of name,
  // from "aggregated_data.properties.types[3].translations.usa" =>
  // aggregated_data
  // properties
  // types
  // [3]
  // translations
  // usa
  aNames := TALStringList.Create;
  try

    // init aNames.linebreak
    aNames.LineBreak := '.';

    // scroll the aLst
    for i := 0 to aLst.Count - 1 do
    begin

      // if it's contain path
      if (aPath = '') or (alposExIgnoreCase(aPath + '.', aLst.Names[i]) = 1)
      then
      begin

        // path.aggregated_data.properties.types[3].translations.usa =>
        // aggregated_data
        // properties
        // types
        // [3]
        // translations
        // usa
        if (aPath <> '') then
          aNames.Text := alStringReplace(alStringReplace(aLst.Names[i],
            aPath + '.', '', [rfIgnoreCase]), '[', '.[', [rfReplaceAll])
        else
          aNames.Text := alStringReplace(aLst.Names[i], '[', '.[',
            [rfReplaceAll]);

        // loop on all the name
        aCurrJsonNode := aJsonNode;
        for J := 0 to aNames.Count - 1 do
        begin

          // if we are in array
          if aCurrJsonNode.NodeType = ntArray then
          begin
            if (Length(aNames[J]) <= 2) or (aNames[J][1] <> '[') or
              (aNames[J][Length(aNames[J])] <> ']') or
              (not ALTryStrToInt(ALCopyStr(aNames[J], 2, Length(aNames[J]) - 2),
              aIndex)) then
              raise EALException.Create('Wrong path: "' + aLst.Names[i] + '"');
            while aIndex > aCurrJsonNode.ChildNodes.Count - 1 do
            begin
              if J = aNames.Count - 1 then
                aCurrJsonNode.AddChild(ntText)
              else if (aNames[J + 1] <> '') and (aNames[J + 1][1] = '[') then
                aCurrJsonNode := aCurrJsonNode.AddChild(ntArray)
              else
                aCurrJsonNode := aCurrJsonNode.AddChild(ntObject);
            end;
            aCurrJsonNode := aCurrJsonNode.ChildNodes[aIndex];
          end

          // if we are not in array
          else
          begin
            aLowerName := ALIfThen(aNameToLowerCase, allowerCase(aNames[J]),
              aNames[J]);
            aTmpJsonNode := aCurrJsonNode.ChildNodes.FindNode(aLowerName);
            if not Assigned(aTmpJsonNode) then
            begin
              if J = aNames.Count - 1 then
                aCurrJsonNode := aCurrJsonNode.AddChild(aLowerName, ntText)
              else if (aNames[J + 1] <> '') and (aNames[J + 1][1] = '[') then
                aCurrJsonNode := aCurrJsonNode.AddChild(aLowerName, ntArray)
              else
                aCurrJsonNode := aCurrJsonNode.AddChild(aLowerName, ntObject);
            end
            else
              aCurrJsonNode := aTmpJsonNode;
          end;

          // set the value
          if J = aNames.Count - 1 then
          begin
            if aLst.ValueFromIndex[i] = aNullStr then
              aCurrJsonNode.Null := True
            else
              aCurrJsonNode.Text := aLst.ValueFromIndex[i];
          end;

        end;

      end;

    end;

  finally
    aNames.Free;
  end;

end;

{ ******************************************* }
Procedure ALJSONToXML(aJsonNode: TALJSONNode; aXMLNode: TALXmlNode;
  aXMLElementNameForJSONArrayEntries: TALStrings;
  // JSONArrayNodeName=XMLElementName | ex: transactions=transaction
  // |     features=feature
  const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec');
var
  aNodeName: AnsiString;
  i: integer;
begin
  for i := 0 to aJsonNode.ChildNodes.Count - 1 do
  begin

    if (aJsonNode.NodeType = ntArray) then
    begin
      if Assigned(aXMLElementNameForJSONArrayEntries) then
        aNodeName := aXMLElementNameForJSONArrayEntries.Values
          [aJsonNode.NodeName]
      else
        aNodeName := '';
      if aNodeName = '' then
        aNodeName := aDefaultXMLElementNameForJSONArrayEntries;
    end
    else
      aNodeName := aJsonNode.ChildNodes[i].NodeName;

    if aJsonNode.ChildNodes[i].NodeType = ntText then
      aXMLNode.AddChild(aNodeName).Text := aJsonNode.ChildNodes[i].Text
    else
      ALJSONToXML(aJsonNode.ChildNodes[i], aXMLNode.AddChild(aNodeName));

  end;
end;

{ ******************************************* }
Procedure ALJSONToXML(aJsonNode: TALJSONNode; aXMLNode: TALXmlNode;
  const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec');
begin
  ALJSONToXML(aJsonNode, aXMLNode, nil,
    aDefaultXMLElementNameForJSONArrayEntries);
end;

{ ******************************************************************** }
function ALJsonEncodeWithNodeSubTypeHelperFunction(aValue: AnsiString;
  aNodeSubType: TALJSONNodeSubType; aFormatSettings: TALFormatSettings)
  : AnsiString;
var
  aStr: AnsiString;
begin
  case aNodeSubType of
    nstFloat:
      result := ALFloatToStr(ALStrToFloat(aValue, aFormatSettings),
        ALDefaultFormatSettings);
    nstText:
      result := '"' + ALJavascriptEncode(aValue) + '"';
    nstBinary:
      result := 'BinData(0, "' + ALMimeBase64EncodeStringNoCRLF(aValue) + '")';
    nstObjectID:
      begin
        setlength(aStr, Length(aValue) * 2);
        BintoHex(@aValue[1], PansiChar(aStr), Length(aValue));
        result := 'ObjectId("' + allowerCase(aStr) + '")';
      end;
    nstBoolean:
      if ALStrToBool(aValue) then
        result := 'true'
      else
        result := 'false';
    nstDateTime:
      result := ALFormatDateTime
        ('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''',
        ALStrToDateTime(aValue, aFormatSettings), ALDefaultFormatSettings);
    nstJavascript:
      result := aValue;
    nstInt32:
      result := 'NumberInt(' + alinttostr(ALStrToInt(aValue)) + ')';
    nstInt64:
      result := 'NumberLong(' + alinttostr(ALstrToInt64(aValue)) + ')';
    nstNull:
      result := 'null';
    nstObject:
      raise Exception.Create('Unsupported Node SubType');
    nstArray:
      raise Exception.Create('Unsupported Node SubType');
    nstRegEx:
      raise Exception.Create('Unsupported Node SubType');
    nstTimestamp:
      raise Exception.Create('Unsupported Node SubType');
  else
    raise Exception.Create('Unknown Node SubType');
  end;
end;

end.
