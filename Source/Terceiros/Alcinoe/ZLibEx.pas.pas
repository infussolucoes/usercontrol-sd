unit ZLibEx;
 
interface
 
uses
  Sysutils, Classes;
 
const
  {** version ids ***********************************************************}
 
  ZLIB_VERSION   = '1.2.3';
  ZLIB_VERNUM    = $1230;
 
  {** compression methods ***************************************************}
 
  Z_DEFLATED = 8;
 
type
  TZAlloc = function (opaque: Pointer; items, size: Integer): Pointer;
  TZFree  = procedure (opaque, block: Pointer);
 
  TZCompressionLevel = (
    zcNone,
    zcFastest,
    zcDefault,
    zcMax,
    zcLevel1,
    zcLevel2,
    zcLevel3,
    zcLevel4,
    zcLevel5,
    zcLevel6,
    zcLevel7,
    zcLevel8,
    zcLevel9
  );
 
  TZStrategy = (
    zsDefault,
    zsFiltered,
    zsHuffman,
    zsRLE,
    zsFixed
  );
 
  {** TZStreamRec ***********************************************************}
 
  TZStreamRec = packed record
    next_in  : PChar;     // next input byte
    avail_in : Longint;   // number of bytes available at next_in
    total_in : Longint;   // total nb of input bytes read so far
 
    next_out : PChar;     // next output byte should be put here
    avail_out: Longint;   // remaining free space at next_out
    total_out: Longint;   // total nb of bytes output so far
 
    msg      : PChar;     // last error message, NULL if no error
    state    : Pointer;   // not visible by applications
 
    zalloc   : TZAlloc;   // used to allocate the internal state
    zfree    : TZFree;    // used to free the internal state
    opaque   : Pointer;   // private data object passed to zalloc and zfree
 
    data_type: Integer;   // best guess about the data type: ascii or binary
    adler    : Longint;   // adler32 value of the uncompressed data
    reserved : Longint;   // reserved for future use
  end;
 
  {** TCustomZStream ********************************************************}
 
  TCustomZStream = class(TStream)
  private
    FStream    : TStream;
    FStreamPos : Longint;
    FOnProgress: TNotifyEvent;
 
    FZStream   : TZStreamRec;
    FBuffer    : Array [Word] of Char;
  protected
    constructor Create(stream: TStream);
 
    procedure DoProgress; dynamic;
 
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;
 
  {** TZCompressionStream ***************************************************}
 
  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream;
      compressionLevel: TZCompressionLevel = zcDefault); overload;
 
    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel;
      windowBits, memLevel: Integer; strategy: TZStrategy); overload;
 
    destructor  Destroy; override;
 
    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;
 
    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;
 
  {** TZDecompressionStream *************************************************}
 
  TZDecompressionStream = class(TCustomZStream)
  public
    constructor Create(source: TStream); overload;
    constructor Create(source: TStream; windowBits: Integer); overload;
 
    destructor  Destroy; override;
 
    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;
 
    property OnProgress;
  end;
 
{** zlib public routines ****************************************************}
 
{*****************************************************************************
*  ZCompress                                                                 *
*                                                                            *
*  pre-conditions                                                            *
*    inBuffer  = pointer to uncompressed data                                *
*    inSize    = size of inBuffer (bytes)                                    *
*    outBuffer = pointer (unallocated)                                       *
*    level     = compression level                                           *
*                                                                            *
*  post-conditions                                                           *
*    outBuffer = pointer to compressed data (allocated)                      *
*    outSize   = size of outBuffer (bytes)                                   *
*****************************************************************************}
 
procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel = zcDefault);
 
{*****************************************************************************
*  ZCompress2                                                                *
*                                                                            *
*  pre-conditions                                                            *
*    inBuffer   = pointer to uncompressed data                               *
*    inSize     = size of inBuffer (bytes)                                   *
*    outBuffer  = pointer (unallocated)                                      *
*    level      = compression level                                          *
*    method     = compression method                                         *
*    windowBits = window bits                                                *
*    memLevel   = memory level                                               *
*    strategy   = compression strategy                                       *
*                                                                            *
*  post-conditions                                                           *
*    outBuffer = pointer to compressed data (allocated)                      *
*    outSize   = size of outBuffer (bytes)                                   *
*****************************************************************************}
 
procedure ZCompress2(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; level: TZCompressionLevel;
  windowBits, memLevel: Integer; strategy: TZStrategy);
 
{*****************************************************************************
*  ZDecompress                                                               *
*                                                                            *
*  pre-conditions                                                            *
*    inBuffer    = pointer to compressed data                                *
*    inSize      = size of inBuffer (bytes)                                  *
*    outBuffer   = pointer (unallocated)                                     *
*    outEstimate = estimated size of uncompressed data (bytes)               *
*                                                                            *
*  post-conditions                                                           *
*    outBuffer = pointer to decompressed data (allocated)                    *
*    outSize   = size of outBuffer (bytes)                                   *
*****************************************************************************}
 
procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
 out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer = 0);
 
{*****************************************************************************
*  ZDecompress2                                                              *
*                                                                            *
*  pre-conditions                                                            *
*    inBuffer    = pointer to compressed data                                *
*    inSize      = size of inBuffer (bytes)                                  *
*    outBuffer   = pointer (unallocated)                                     *
*    windowBits  = window bits                                               *
*    outEstimate = estimated size of uncompressed data (bytes)               *
*                                                                            *
*  post-conditions                                                           *
*    outBuffer = pointer to decompressed data (allocated)                    *
*    outSize   = size of outBuffer (bytes)                                   *
*****************************************************************************}
 
procedure ZDecompress2(const inBuffer: Pointer; inSize: Integer;
 out outBuffer: Pointer; out outSize: Integer; windowBits: Integer;
 outEstimate: Integer = 0);
 
{** string routines *********************************************************}
 
{*****************************************************************************
*  ZCompressStr                                                              *
*                                                                            *
*  pre-conditions                                                            *
*    s     = uncompressed data string                                        *
*    level = compression level                                               *
*                                                                            *
*  return                                                                    *
*    compressed data string                                                  *
*****************************************************************************}
 
function ZCompressStr(const s: String;
  level: TZCompressionLevel = zcDefault): String;
 
{*****************************************************************************
*  ZCompressStrEx                                                            *
*                                                                            *
*  pre-conditions                                                            *
*    s     = uncompressed data string                                        *
*    level = compression level                                               *
*                                                                            *
*  return                                                                    *
*    compressed data string with 4 byte (integer) header indicating          *
*    original uncompressed data length                                       *
*****************************************************************************}
 
function ZCompressStrEx(const s: String;
  level: TZCompressionLevel = zcDefault): String;
 
{*****************************************************************************
*  ZCompressStr2                                                             *
*                                                                            *
*  pre-conditions                                                            *
*    s          = uncompressed data string                                   *
*    level      = compression level                                          *
*    windowBits = window bits                                                *
*    memLevel   = memory level                                               *
*    strategy   = compression strategy                                       *
*                                                                            *
*  return                                                                    *
*    compressed data string                                                  *
*****************************************************************************}
 
function ZCompressStr2(const s: String; level: TZCompressionLevel;
  windowBits, memLevel: Integer; strategy: TZStrategy): String;
 
function ZCompressStrWeb(const s: String): String;
 
{*****************************************************************************
*  ZCompressStrG                                                             *
*                                                                            *
*  pre-conditions                                                            *
*    s          = uncompressed data string                                   *
*    fileName   = filename                                                   *
*    comment    = comment                                                    *
*    dateTime   = date/time                                                  *
*                                                                            *
*  return                                                                    *
*    compressed data string in gzip format                                   *
*****************************************************************************}
 
function ZCompressStrG(const s: String; const fileName, comment: String;
  dateTime: TDateTime): String;
 
{*****************************************************************************
*  ZDecompressStr                                                            *
*                                                                            *
*  pre-conditions                                                            *
*    s = compressed data string                                              *
*                                                                            *
*  return                                                                    *
*    uncompressed data string                                                *
*****************************************************************************}
 
function ZDecompressStr(const s: String): String;
 
{*****************************************************************************
*  ZDecompressStrEx                                                          *
*                                                                            *
*  pre-conditions                                                            *
*    s = compressed data string with 4 byte (integer) header indicating      *
*        original uncompressed data length                                   *
*                                                                            *
*  return                                                                    *
*    uncompressed data string                                                *
*****************************************************************************}
 
function ZDecompressStrEx(const s: String): String;
 
{*****************************************************************************
*  ZDecompressStr2                                                           *
*                                                                            *
*  pre-conditions                                                            *
*    s          = compressed data string                                     *
*    windowBits = window bits                                                *
*                                                                            *
*  return                                                                    *
*    uncompressed data string                                                *
*****************************************************************************}
 
function ZDecompressStr2(const s: String; windowBits: Integer): String;
 
{** stream routines *********************************************************}
 
procedure ZCompressStream(inStream, outStream: TStream;
  level: TZCompressionLevel = zcDefault);
 
procedure ZCompressStream2(inStream, outStream: TStream;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
 
procedure ZCompressStreamWeb(inStream, outStream: TStream);
 
procedure ZCompressStreamG(inStream, outStream: TStream; const fileName,
  comment: String; dateTime: TDateTime);
 
procedure ZDecompressStream(inStream, outStream: TStream);
 
procedure ZDecompressStream2(inStream, outStream: TStream;
  windowBits: Integer);
 
{** checksum routines *******************************************************}
 
function ZAdler32(adler: Longint; const buffer; size: Integer): Longint;
function ZCrc32(crc: Longint; const buffer; size: Integer): Longint;
 
{****************************************************************************}
 
type
  EZLibError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(code: Integer); overload;
 
    property ErrorCode: Integer read FErrorCode write FErrorCode;
  end;
 
  EZCompressionError = class(EZLibError);
  EZDecompressionError = class(EZLibError);
 
{****************************************************************************}
 
{** gzip ********************************************************************}
 
type
  PGZHeader = ^TGZHeader;
  TGZHeader = packed record
    Id1       : Byte;
    Id2       : Byte;
    Method    : Byte;
    Flags     : Byte;
    Time      : Cardinal;
    ExtraFlags: Byte;
    OS        : Byte;
  end;
 
  PGZTrailer = ^TGZTrailer;
  TGZTrailer = packed record
    Crc : Cardinal;
    Size: Cardinal;
  end;
 
const
  GZ_ASCII_TEXT  = $01;
  GZ_HEADER_CRC  = $02;
  GZ_EXTRA_FIELD = $04;
  GZ_FILENAME    = $08;
  GZ_COMMENT     = $10;
 
  GZ_EXTRA_DEFAULT = 0;
  GZ_EXTRA_MAX     = 2;
  GZ_EXTRA_FASTEST = 4;
 
const
  {** flush constants *******************************************************}
 
  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  Z_BLOCK         = 5;
 
  {** return codes **********************************************************}
 
  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);
 
  {** compression levels ****************************************************}
 
  Z_NO_COMPRESSION       =   0;
  Z_BEST_SPEED           =   1;
  Z_BEST_COMPRESSION     =   9;
  Z_DEFAULT_COMPRESSION  = (-1);
 
  {** compression strategies ************************************************}
 
  Z_FILTERED            = 1;
  Z_HUFFMAN_ONLY        = 2;
  Z_RLE                 = 3;
  Z_FIXED               = 4;
  Z_DEFAULT_STRATEGY    = 0;
 
  {** data types ************************************************************}
 
  Z_BINARY   = 0;
  Z_ASCII    = 1;
  Z_TEXT     = Z_ASCII;
  Z_UNKNOWN  = 2;
 
  {** return code messages **************************************************}
 
  _z_errmsg: array[0..9] of PChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    'ok',                   // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );
 
  ZLevels: Array [TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,       // zcNone
    Z_BEST_SPEED,           // zcFastest
    Z_DEFAULT_COMPRESSION,  // zcDefault
    Z_BEST_COMPRESSION,     // zcMax
    1,                      // zcLevel1
    2,                      // zcLevel2
    3,                      // zcLevel3
    4,                      // zcLevel4
    5,                      // zcLevel5
    6,                      // zcLevel6
    7,                      // zcLevel7
    8,                      // zcLevel8
    9                       // zcLevel9
  );
 
  ZStrategies: Array [TZStrategy] of Shortint = (
    Z_DEFAULT_STRATEGY,     // zsDefault
    Z_FILTERED,             // zsFiltered
    Z_HUFFMAN_ONLY,         // zsHuffman
    Z_RLE,                  // zsRLE
    Z_FIXED                 // zsFixed
  );
 
  SZInvalid = 'Invalid ZStream operation!';
{*****************************************************************************}
function inflateInit_(var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer; forward;
function inflateInit2_(var strm: TZStreamRec; windowBits: Integer;
  version: PChar; recsize: Integer): Integer;forward;
function inflate(var strm: TZStreamRec; flush: Integer): Integer; forward;
function inflateEnd(var strm: TZStreamRec): Integer; forward;
function inflateReset(var strm: TZStreamRec): Integer;forward;
{*****************************************************************************}
function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer; forward;
function deflateInit2_(var strm: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer; version: PChar; recsize: Integer): Integer;forward;
function deflate(var strm: TZStreamRec; flush: Integer): Integer; forward;
function deflateEnd(var strm: TZStreamRec): Integer; forward;
{*****************************************************************************}
function adler32(adler: Longint; const buf; len: Integer): Longint;forward;
function crc32(crc: Longint; const buf; len: Integer): Longint;forward;
//procedure adler32; external;
procedure compressBound; external;
//procedure crc32; external;
procedure _tr_init; external;
procedure _tr_tally; external;
procedure _tr_flush_block; external;
procedure _tr_align; external;
procedure _tr_stored_block; external;
{*****************************************************************************}
function DeflateInit(var stream: TZStreamRec; level: Integer): Integer;
function DeflateInit2(var stream: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer): Integer;
function InflateInit(var stream: TZStreamRec): Integer;
function InflateInit2(var stream: TZStreamRec; windowBits: Integer): Integer;
{*****************************************************************************}
function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
procedure zcfree(opaque, block: Pointer);
{*****************************************************************************}
procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
{*****************************************************************************}
function _malloc(Size: Integer): Pointer; cdecl;
procedure _free(Block: Pointer); cdecl;
{*****************************************************************************}
function ZCompressCheck(code: Integer): Integer;
function ZDecompressCheck(code: Integer): Integer;
function CCheck(code: Integer): Integer;
function DCheck(code: Integer): Integer;
{*****************************************************************************}
procedure MoveI32(const Source; var Dest; Count: Integer); register;
implementation
 
{** link zlib code **********************************************************}
 
{$L ZlibObj\deflate.obj}
{$L ZlibObj\inflate.obj}
{$L ZlibObj\inftrees.obj}
{$L ZlibObj\infback.obj}
{$L ZlibObj\inffast.obj}
{$L ZlibObj\trees.obj}
{$L ZlibObj\compress.obj}
{$L ZlibObj\adler32.obj}
{$L ZlibObj\crc32.obj}
 
{** deflate routines ********************************************************}
 
function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer;
  external;
 
function deflateInit2_(var strm: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer; version: PChar; recsize: Integer): Integer;
  external;
 
function deflate(var strm: TZStreamRec; flush: Integer): Integer;
  external;
 
function deflateEnd(var strm: TZStreamRec): Integer;
  external;
 
{** inflate routines ********************************************************}
 
function inflateInit_(var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer;
  external;
 
function inflateInit2_(var strm: TZStreamRec; windowBits: Integer;
  version: PChar; recsize: Integer): Integer;
  external;
 
function inflate(var strm: TZStreamRec; flush: Integer): Integer;
  external;
 
function inflateEnd(var strm: TZStreamRec): Integer;
  external;
 
function inflateReset(var strm: TZStreamRec): Integer;
  external;
 
{** checksum routines *******************************************************}
 
function adler32(adler: Longint; const buf; len: Integer): Longint;
  external;
 
function crc32(crc: Longint; const buf; len: Integer): Longint;
  external;
 
{** zlib function implementations *******************************************}
 
function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
begin
  GetMem(result,items * size);
end;
 
procedure zcfree(opaque, block: Pointer);
begin
  FreeMem(block);
end;
 
{** c function implementations **********************************************}
 
procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
begin
  FillChar(p^,count,b);
end;
 
procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^,dest^,count);
end;
 
{** bds function implementations **********************************************}
 
function _malloc(Size: Integer): Pointer; cdecl;
begin
  Result := AllocMem(Size);
end;
 
procedure _free(Block: Pointer); cdecl;
begin
  FreeMem(Block);
end;
 
{** custom zlib routines ****************************************************}
 
function DeflateInit(var stream: TZStreamRec; level: Integer): Integer;
begin
  result := deflateInit_(stream,level,ZLIB_VERSION,SizeOf(TZStreamRec));
end;
 
function DeflateInit2(var stream: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer): Integer;
begin
  result := deflateInit2_(stream,level,method,windowBits,memLevel,strategy,
    ZLIB_VERSION,SizeOf(TZStreamRec));
end;
 
function InflateInit(var stream: TZStreamRec): Integer;
begin
  result := inflateInit_(stream,ZLIB_VERSION,SizeOf(TZStreamRec));
end;
 
function InflateInit2(var stream: TZStreamRec; windowBits: Integer): Integer;
begin
  result := inflateInit2_(stream,windowBits,ZLIB_VERSION,SizeOf(TZStreamRec));
end;
 
{** DateTimeToUnix **********************************************************}
 
function DateTimeToUnix(const AValue: TDateTime): Cardinal;
begin
  Result := Round((AValue - UnixDateDelta) * SecsPerDay);
end;
 
{****************************************************************************}
 
function ZCompressCheck(code: Integer): Integer;
begin
  result := code;
 
  if code < 0 then
  begin
    raise EZCompressionError.Create(code);
  end;
end;
 
function ZDecompressCheck(code: Integer): Integer;
begin
  Result := code;
 
  if code < 0 then
  begin
    raise EZDecompressionError.Create(code);
  end;
end;
 
function CCheck(code: Integer): Integer;
begin
  Result := ZCompressCheck(code);
end;
 
function DCheck(code: Integer): Integer;
begin
  Result := ZDecompressCheck(code);
end;
 
{****************************************************************************}
procedure MoveI32(const Source; var Dest; Count: Integer); register;
asm
        cmp   ECX,0
        Je    @JustQuit
        push  ESI
        push  EDI
        mov   ESI, EAX
        mov   EDI, EDX
    @Loop:
        Mov   AL, [ESI]
        Inc   ESI
        mov   [EDI], AL
        Inc   EDI
        Dec   ECX
        Jnz   @Loop
        pop   EDI
        pop   ESI
    @JustQuit:
end;
{****************************************************************************}
procedure ZInternalCompress(var zstream: TZStreamRec; const inBuffer: Pointer;
  inSize: Integer; out outBuffer: Pointer; out outSize: Integer);
const
  delta = 256;
begin
  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  GetMem(outBuffer,outSize);
 
  try
    try
      zstream.next_in := inBuffer;
      zstream.avail_in := inSize;
      zstream.next_out := outBuffer;
      zstream.avail_out := outSize;
 
      while ZCompressCheck(deflate(zstream,Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(outSize,delta);
        ReallocMem(outBuffer,outSize);
 
        zstream.next_out := PChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream));
    end;
 
    ReallocMem(outBuffer,zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;
 
procedure ZInternalDecompress(zstream: TZStreamRec; const inBuffer: Pointer;
  inSize: Integer; out outBuffer: Pointer; out outSize: Integer;
  outEstimate: Integer);
var
  delta: Integer;
begin
  delta := (inSize + 255) and not 255;
 
  if outEstimate = 0 then outSize := delta
  else outSize := outEstimate;
 
  GetMem(outBuffer,outSize);
 
  try
    try
      zstream.next_in := inBuffer;
      zstream.avail_in := inSize;
      zstream.next_out := outBuffer;
      zstream.avail_out := outSize;
 
      while ZDecompressCheck(inflate(zstream,Z_NO_FLUSH)) <> Z_STREAM_END do
      begin
        Inc(outSize,delta);
        ReallocMem(outBuffer,outSize);
 
        zstream.next_out := PChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZDecompressCheck(inflateEnd(zstream));
    end;
 
    ReallocMem(outBuffer,zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;
 
procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);
 
  ZCompressCheck(DeflateInit(zstream,ZLevels[level]));
 
  ZInternalCompress(zstream,inBuffer,inSize,outBuffer,outSize);
end;
 
procedure ZCompress2(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; level: TZCompressionLevel;
  windowBits, memLevel: Integer; strategy: TZStrategy);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);
 
  ZCompressCheck(DeflateInit2(zstream,ZLevels[level],Z_DEFLATED,windowBits,
    memLevel,ZStrategies[strategy]));
 
  ZInternalCompress(zstream,inBuffer,inSize,outBuffer,outSize);
end;
 
procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);
 
  ZDecompressCheck(InflateInit(zstream));
 
  ZInternalDecompress(zstream,inBuffer,inSize,outBuffer,outSize,outEstimate);
end;
 
procedure ZDecompress2(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; windowBits: Integer;
  outEstimate: Integer);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);
 
  ZDecompressCheck(InflateInit2(zstream,windowBits));
 
  ZInternalDecompress(zstream,inBuffer,inSize,outBuffer,outSize,outEstimate);
end;
 
{** string routines *********************************************************}
 
function ZCompressStr(const s: String; level: TZCompressionLevel): String;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress(PChar(s),Length(s),buffer,size,level);
 
  SetLength(result,size);
  Move(buffer^,result[1],size);
 
  FreeMem(buffer);
end;
 
function ZCompressStrEx(const s: String; level: TZCompressionLevel): String;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress(PChar(s),Length(s),buffer,size,level);
 
  SetLength(result,size + SizeOf(Integer));
 
  Move(buffer^,result[5],size);
 
  size := Length(s);
  Move(size,result[1],SizeOf(Integer));
 
  FreeMem(buffer);
end;
 
function ZCompressStr2(const s: String; level: TZCompressionLevel;
  windowBits, memLevel: Integer; strategy: TZStrategy): String;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress2(PChar(s),Length(s),buffer,size,level,windowBits,memLevel,
    strategy);
 
  SetLength(result,size);
  Move(buffer^,result[1],size);
 
  FreeMem(buffer);
end;
 
function ZCompressStrWeb(const s: String): String;
begin
  result := ZCompressStr2(s,zcFastest,-15,9,zsDefault);
end;
 
function ZCompressStrG(const s: String; const fileName, comment: String;
  dateTime: TDateTime): String;
var
  header : PGZHeader;
  trailer: PGZTrailer;
  len    : Integer;
begin
  SetLength(result,SizeOf(TGZHeader));
 
  header := PGZHeader(@result[1]);
 
  FillChar(header^,SizeOf(TGZHeader),0);
 
  header^.Id1 := $1F;
  header^.Id2 := $8B;
  header^.Method := Z_DEFLATED;
 
  if dateTime <> 0 then header^.Time := DateTimeToUnix(dateTime);
 
  header^.ExtraFlags := GZ_EXTRA_DEFAULT;
  header^.OS := 0;
 
  header^.Flags := 0;
 
  if Length(fileName) > 0 then
  begin
    header^.Flags := header^.Flags or GZ_FILENAME;
 
    result := result + fileName + #$00;
  end;
 
  if Length(comment) > 0 then
  begin
    header^.Flags := header^.Flags or GZ_COMMENT;
 
    result := result + comment + #$00;
  end;
 
  result := result + ZCompressStr2(s,zcDefault,-15,9,zsDefault);
 
  len := Length(result);
 
  SetLength(result,len + SizeOf(TGZTrailer));
 
  trailer := PGZTrailer(@result[len + 1]);
 
  FillChar(trailer^,SizeOf(TGZTrailer),0);
 
  trailer^.Crc := crc32(0,s[1],Length(s));
  trailer^.Size := Length(s);
end;
 
function ZDecompressStr(const s: String): String;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress(PChar(s),Length(s),buffer,size);
 
  SetLength(result,size);
  Move(buffer^,result[1],size);
 
  FreeMem(buffer);
end;
 
function ZDecompressStrEx(const s: String): String;
var
  buffer  : Pointer;
  size    : Integer;
  data    : String;
  dataSize: Integer;
begin
  Move(s[1],size,SizeOf(Integer));
 
  dataSize := Length(s) - SizeOf(Integer);
 
  SetLength(data,dataSize);
  Move(s[5],data[1],dataSize);
 
  ZDecompress(PChar(data),dataSize,buffer,size,size);
 
  SetLength(result,size);
  Move(buffer^,result[1],size);
 
  FreeMem(buffer);
end;
 
function ZDecompressStr2(const s: String; windowBits: Integer): String;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress2(PChar(s),Length(s),buffer,size,windowBits);
 
  SetLength(result,size);
  Move(buffer^,result[1],size);
 
  FreeMem(buffer);
end;
 
{** stream routines *********************************************************}
 
procedure ZInternalCompressStream(zstream: TZStreamRec; inStream,
  outStream: TStream);
const
  bufferSize = 32768;
var
  zresult  : Integer;
  inBuffer : Array [0..bufferSize-1] of Char;
  outBuffer: Array [0..bufferSize-1] of Char;
  outSize  : Integer;
begin
  zresult := Z_STREAM_END;
 
  zstream.avail_in := inStream.Read(inBuffer,bufferSize);
 
  while zstream.avail_in > 0 do
  begin
    zstream.next_in := inBuffer;
 
    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;
 
      zresult := ZCompressCheck(deflate(zstream,Z_NO_FLUSH));
 
      outSize := bufferSize - zstream.avail_out;
 
      outStream.Write(outBuffer,outSize);
    until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);
 
    zstream.avail_in := inStream.Read(inBuffer,bufferSize);
  end;
 
  while zresult <> Z_STREAM_END do
  begin
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;
 
    zresult := ZCompressCheck(deflate(zstream,Z_FINISH));
 
    outSize := bufferSize - zstream.avail_out;
 
    outStream.Write(outBuffer,outSize);
  end;
 
  ZCompressCheck(deflateEnd(zstream));
end;
 
procedure ZInternalDecompressStream(zstream: TZStreamRec; inStream,
  outStream: TStream);
const
  bufferSize = 32768;
var
  zresult  : Integer;
  inBuffer : Array [0..bufferSize-1] of Char;
  outBuffer: Array [0..bufferSize-1] of Char;
  outSize  : Integer;
begin
  zresult := Z_STREAM_END;
 
  zstream.avail_in := inStream.Read(inBuffer,bufferSize);
 
  while zstream.avail_in > 0 do
  begin
    zstream.next_in := inBuffer;
 
    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;
 
      zresult := ZDecompressCheck(inflate(zstream,Z_NO_FLUSH));
 
      outSize := bufferSize - zstream.avail_out;
 
      outStream.Write(outBuffer,outSize);
    until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);
 
    if zresult <> Z_STREAM_END then
    begin
      zstream.avail_in := inStream.Read(inBuffer,bufferSize);
    end
    else if zstream.avail_in > 0 then
    begin
      inStream.Position := inStream.Position - zstream.avail_in;
      zstream.avail_in := 0;
    end;
  end;
 
  while zresult <> Z_STREAM_END do
  begin
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;
 
    zresult := ZDecompressCheck(inflate(zstream,Z_FINISH));
 
    outSize := bufferSize - zstream.avail_out;
 
    outStream.Write(outBuffer,outSize);
  end;
 
  ZDecompressCheck(inflateEnd(zstream));
end;
 
procedure ZCompressStream(inStream, outStream: TStream;
  level: TZCompressionLevel);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);
 
  ZCompressCheck(DeflateInit(zstream,ZLevels[level]));
 
  ZInternalCompressStream(zstream,inStream,outStream);
end;
 
procedure ZCompressStream2(inStream, outStream: TStream;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);
 
  ZCompressCheck(DeflateInit2(zstream,ZLevels[level],Z_DEFLATED,windowBits,
    memLevel,ZStrategies[strategy]));
 
  ZInternalCompressStream(zstream,inStream,outStream);
end;
 
procedure ZCompressStreamWeb(inStream, outStream: TStream);
begin
  ZCompressStream2(inStream,outStream,zcFastest,-15,9,zsDefault);
end;
 
procedure ZCompressStreamG(inStream, outStream: TStream; const fileName,
  comment: String; dateTime: TDateTime);
const
  bufferSize = 32768;
var
  header    : TGZHeader;
  trailer   : TGZTrailer;
  buffer    : Array [0..bufferSize-1] of Char;
  count     : Integer;
  position  : Longint ;
  nullString: String;
begin
  FillChar(header,SizeOf(TGZHeader),0);
 
  header.Id1 := $1F;
  header.Id2 := $8B;
  header.Method := Z_DEFLATED;
 
  if dateTime <> 0 then header.Time := DateTimeToUnix(dateTime);
 
  header.ExtraFlags := GZ_EXTRA_DEFAULT;
  header.OS := 0;
 
  header.Flags := 0;
 
  if Length(fileName) > 0 then header.Flags := header.Flags or GZ_FILENAME;
  if Length(comment) > 0 then header.Flags := header.Flags or GZ_COMMENT;
 
  FillChar(trailer,SizeOf(TGZTrailer),0);
 
  trailer.Crc := 0;
 
  position := inStream.Position;
 
  while inStream.Position < inStream.Size do
  begin
    count := inStream.Read(buffer[0],bufferSize);
 
    trailer.Crc := crc32(trailer.Crc,buffer[0],count);
  end;
 
  inStream.Position := position;
 
  trailer.Size := inStream.Size - inStream.Position;
 
  outStream.Write(header,SizeOf(TGZHeader));
 
  if Length(filename) > 0 then
  begin
    nullString := fileName + #$00;
 
    outStream.Write(nullString[1],Length(nullString));
  end;
 
  if Length(comment) > 0 then
  begin
    nullString := comment + #$00;
 
    outStream.Write(nullString[1],Length(nullString));
  end;
 
  ZCompressStream2(inStream,outStream,zcDefault,-15,9,zsDefault);
 
  outStream.Write(trailer,SizeOf(TGZTrailer));
end;
 
procedure ZDecompressStream(inStream, outStream: TStream);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);
 
  ZDecompressCheck(InflateInit(zstream));
 
  ZInternalDecompressStream(zstream,inStream,outStream);
end;
 
procedure ZDecompressStream2(inStream, outStream: TStream;
  windowBits: Integer);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);
 
  ZDecompressCheck(InflateInit2(zstream,windowBits));
 
  ZInternalDecompressStream(zstream,inStream,outStream);
end;
 
{** checksum routines *******************************************************}
 
function ZAdler32(adler: Longint; const buffer; size: Integer): Longint;
begin
  result := adler32(adler,buffer,size);
end;
 
function ZCrc32(crc: Longint; const buffer; size: Integer): Longint;
begin
  result := crc32(crc,buffer,size);
end;
 
{** TCustomZStream **********************************************************}
 
constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;
 
  FStream := stream;
  FStreamPos := stream.Position;
end;
 
procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then FOnProgress(Self);
end;
 
{** TZCompressionStream *****************************************************}
 
constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel);
begin
  inherited Create(dest);
 
  FZStream.next_out := FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);
 
  ZCompressCheck(DeflateInit(FZStream,ZLevels[compressionLevel]));
end;
 
constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
begin
  inherited Create(dest);
 
  FZStream.next_out := FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);
 
  ZCompressCheck(DeflateInit2(FZStream,ZLevels[compressionLevel],Z_DEFLATED,
    windowBits,memLevel,ZStrategies[strategy]));
end;
 
destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := Nil;
  FZStream.avail_in := 0;
 
  try
    if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;
 
    while ZCompressCheck(deflate(FZStream,Z_FINISH)) <> Z_STREAM_END do
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer) - FZStream.avail_out);
 
      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
    end;
 
    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    deflateEnd(FZStream);
  end;
 
  inherited Destroy;
end;
 
function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise EZCompressionError.Create(SZInvalid);
end;
 
function TZCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  FZStream.next_in := @buffer;
  FZStream.avail_in := count;
 
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;
 
  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(deflate(FZStream,Z_NO_FLUSH));
 
    if FZStream.avail_out = 0 then
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer));
 
      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
 
      FStreamPos := FStream.Position;
 
      DoProgress;
    end;
  end;
 
  result := Count;
end;
 
function TZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  if (offset = 0) and (origin = soFromCurrent) then
  begin
    result := FZStream.total_in;
  end
  else raise EZCompressionError.Create(SZInvalid);
end;
 
function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then result := 0
  else result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;
 
{** TZDecompressionStream ***************************************************}
 
constructor TZDecompressionStream.Create(source: TStream);
begin
  inherited Create(source);
 
  FZStream.next_in := FBuffer;
  FZStream.avail_in := 0;
 
  ZDecompressCheck(InflateInit(FZStream));
end;
 
constructor TZDecompressionStream.Create(source: TStream;
  windowBits: Integer);
begin
  inherited Create(source);
 
  FZStream.next_in := FBuffer;
  FZStream.avail_in := 0;
 
  ZDecompressCheck(InflateInit2(FZStream,windowBits));
end;
 
destructor TZDecompressionStream.Destroy;
begin
  inflateEnd(FZStream);
 
  inherited Destroy;
end;
 
function TZDecompressionStream.Read(var buffer; count: Longint): Longint;
var
  zresult: Integer;
begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := count;
 
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;
 
  zresult := Z_OK;
 
  while (FZStream.avail_out > 0) and (zresult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := FStream.Read(FBuffer,SizeOf(FBuffer));
 
      if FZStream.avail_in = 0 then
      begin
        result := count - FZStream.avail_out;
 
        Exit;
      end;
 
      FZStream.next_in := FBuffer;
      FStreamPos := FStream.Position;
 
      DoProgress;
    end;
 
    zresult := ZDecompressCheck(inflate(FZStream,Z_NO_FLUSH));
  end;
 
  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    FStream.Position := FStream.Position - FZStream.avail_in;
    FStreamPos := FStream.Position;
 
    FZStream.avail_in := 0;
  end;
 
  result := count - FZStream.avail_out;
end;
 
function TZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EZDecompressionError.Create(SZInvalid);
end;
 
function TZDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  buf: Array [0..8191] of Char;
  i  : Integer;
begin
  if (offset = 0) and (origin = soFromBeginning) then
  begin
    ZDecompressCheck(inflateReset(FZStream));
 
    FZStream.next_in := FBuffer;
    FZStream.avail_in := 0;
 
    FStream.Position := 0;
    FStreamPos := 0;
  end
  else if ((offset >= 0) and (origin = soFromCurrent)) or
          (((offset - FZStream.total_out) > 0) and (origin = soFromBeginning)) then
  begin
    if origin = soFromBeginning then Dec(offset,FZStream.total_out);
 
    if offset > 0 then
    begin
      for i := 1 to offset div SizeOf(buf) do ReadBuffer(buf,SizeOf(buf));
      ReadBuffer(buf,offset mod SizeOf(buf));
    end;
  end
  else if (offset = 0) and (origin = soFromEnd) then
  begin
    while Read(buf,SizeOf(buf)) > 0 do ;
  end
  else raise EZDecompressionError.Create(SZInvalid);
 
  result := FZStream.total_out;
end;
 
{** EZLibError **************************************************************}
 
constructor EZLibError.Create(code: Integer);
begin
  inherited Create(_z_errmsg[2 - code]);
 
  FErrorCode := code;
end;
 
end.