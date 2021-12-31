unit internetaccess_inflater_paszlib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, internetaccess, ZBase, ZStream, ZInflate, bbutils;

type
PTransferBlockWriteEvent = ^TTransferBlockWriteEvent;
TTransferContentInflaterZlib = class(TTransferContentInflater)
  stream: z_stream;
  buffer: pointer;
  headerRead: boolean;
  writeUncompressedBlock: TTransferBlockWriteEvent;
  pointerToBlockWrite: PTransferBlockWriteEvent;
  expectGZIP: boolean;

  headerBuffer: string;
  procedure writeCompressedBlock(const abuffer; Count: Longint);
  procedure endTransfer; override;
  constructor Create;
  destructor Destroy; override;
  class procedure injectDecoder(var transfer: TTransfer; const encoding: string); override;
end;

implementation
uses math;
const buffer_block_size=16384;
var DEBUG_DECOMPRESSION: boolean = false;

type THeader = (hExpectedHeader, hText, hInvalidHeader, hBufferTooSmall);
function getHeaderLength(expectGZip: boolean; p: PByte; length: cardinal; out headerLength: cardinal): THeader;
var
  flags: Byte;
  skipLength, xlen: cardinal;
begin
  if expectGZip then begin
    // RFC 1952
    skipLength := 10;
    if length < 10 then exit(hBufferTooSmall);
    if (p[0] <> 31) or (p[1] <> 139) or (p[2] <> 8) then exit(hInvalidHeader); //two check bytes, 8 means deflate algorithm
    result := hExpectedHeader;
    flags := p[3];
    if flags and 1 <> 0 then result := hText; //FTEXT, no compression, but has header
    if (flags and 4 <> 0) and (length >= skipLength + 2) then begin //FEXTRA, more header
      xlen := p[skipLength] + cardinal(256) * p[skipLength + 1];
      if length >= skipLength + 2 + xlen then skipLength += xlen + 2
      else exit(hBufferTooSmall);
    end;
    if flags and 8 <> 0 then begin //FNAME, zero-terminated
      while (skipLength <= length) and (p[skipLength] <> 0) do inc(skipLength);
      if skipLength <= length then inc(skipLength)
      else exit(hBufferTooSmall);
    end;
    if flags and 16 <> 0 then begin //Fcomment, zero-terminated
      while (skipLength <= length) and (p[skipLength] <> 0) do inc(skipLength);
      if skipLength <= length then inc(skipLength)
      else exit(hBufferTooSmall);
    end;
    if flags and 2 <> 0 then //FHCRC
      inc(skipLength, 2);
  end else begin
    //RFC 1950
    //however, the browser/Server implementation is wrong, so there usually are no header bytes (see https://stackoverflow.com/a/9856879/1501222 )
    skipLength := 0;
    headerLength := 0;
    result := hExpectedHeader;
    if length < 2 then exit(hBufferTooSmall);

    if p[0] and 15 <> 8 then exit; //8 means deflate algorithm
    if p[0] and not 15 > 7 then exit;
    if (p[0] * 256 + p[1] ) mod 31 <> 0 then exit; //FLG check bits
    skipLength := 2;
    if p[1] and 32 <> 0 then skipLength += 4; //dictionary
  end;
  //writeln(result, skipLength);
  if skipLength > length then exit(hBufferTooSmall);
  headerLength := skipLength;
end;

procedure TTransferContentInflaterZlib.writeCompressedBlock(const abuffer; Count: Longint);
var err:smallint;
  headerLength: cardinal;
  header: THeader;
  procedure raiseError;
  var
    s: String;
  begin
    s := zerror(err);
    if stream.msg <> '' then s := s + ': ' + stream.msg;
    s := s + LineEnding + 'Bytes in: ' + inttostr(stream.total_in) + ' Bytes out: ' + IntToStr(stream.total_out);
    s := s + 'Last received: ' + strFromPchar(@abuffer, count).EncodeHex;
    raise Edecompressionerror.create(s);
  end;

  procedure debug;
  begin
    writeln(strFromPchar(@abuffer, count).EncodeHex);
  end;

begin
  if DEBUG_DECOMPRESSION then debug;
  stream.next_in:=@abuffer;
  stream.avail_in:=count;
  if not headerRead then begin
    if headerBuffer <> '' then begin
      headerBuffer := headerBuffer + strFromPchar(@abuffer, count);
      stream.next_in:=@headerBuffer[1];
      stream.avail_in:=length(headerBuffer);
    end;
    header := getHeaderLength(expectGZIP, stream.next_in, stream.avail_in, headerLength);
    if header = hBufferTooSmall then begin
      if stream.next_in=@abuffer then headerBuffer := headerBuffer + strFromPchar(@abuffer, count);
      exit;
    end;
    headerRead := true;
  //  headerBuffer := ''; do not delete the buffer, it still needs to be read
    if header in [hExpectedHeader, hText] then begin
      inc(stream.next_in, headerLength);
      dec(stream.avail_in, headerLength);
    end;
    if header in [hInvalidHeader, hText] then begin
      writeUncompressedBlock(stream.next_in^, stream.avail_in);
      pointerToBlockWrite^ := writeUncompressedBlock;
      exit;
    end;
  end;




  while stream.avail_in > 0 do begin
    stream.next_out:=self.buffer;
    stream.avail_out:=  buffer_block_size;

    err:=inflate(stream, Z_NO_FLUSH);
    if (err<>Z_OK) and (err <> Z_STREAM_END) then
      raiseError;

    writeUncompressedBlock(self.buffer^, stream.next_out - self.buffer);

    if err=Z_STREAM_END then
      exit;
  end;

end;

procedure TTransferContentInflaterZlib.endTransfer;
var
  header: THeader;
  tempBuffer: pchar;
  headerLength: cardinal;
begin
  if (not headerRead) and (headerBuffer <> '') then begin
    header := getHeaderLength(expectGZIP, @headerBuffer[1], length(headerBuffer), headerLength);
    if header in [hInvalidHeader, hBufferTooSmall] then headerLength := 0;
    tempBuffer := @headerBuffer[1] + headerLength;
    if header in [hInvalidHeader, hText] then
      writeUncompressedBlock(tempBuffer^, length(headerBuffer) - headerLength)
     else
      writeCompressedBlock(tempBuffer^, length(headerBuffer) - headerLength);
    headerBuffer := '';
  end;
end;

constructor TTransferContentInflaterZlib.Create;
begin
  getmem(buffer,buffer_block_size);
end;

destructor TTransferContentInflaterZlib.Destroy;
begin
  Freemem(buffer);
  inherited Destroy;
end;

class procedure TTransferContentInflaterZlib.injectDecoder(var transfer: TTransfer; const encoding: string);
var
  zlibencoder: TTransferContentInflaterZlib;
  encodingIsGZIP: boolean;
  err: System.Integer;
begin
  case encoding of
  'gzip': encodingIsGZIP := true;
  'deflate': encodingIsGZIP := false;
  else exit;
  end;
  zlibencoder := TTransferContentInflaterZlib.Create;
  zlibencoder.writeUncompressedBlock := transfer.writeBlockCallback;
  zlibencoder.pointerToBlockWrite:=@transfer.writeBlockCallback;
  zlibencoder.expectGZIP:=encodingIsGZIP;

  {if Askipheader then
    err:=inflateInit2(Fstream,-MAX_WBITS)
  else
    err:=inflateInit(Fstream);
   }
   //inflateInit(zlibencoder.stream);
   err := inflateInit2(zlibencoder.stream, -MAX_WBITS);
   if err <> Z_OK then
     raise Edecompressionerror.Create('Failed to initialize decompression: ' + zerror(err));
   transfer.writeBlockCallback := @zlibencoder.writeCompressedBlock;
   transfer.inflater := zlibencoder;

   DEBUG_DECOMPRESSION := GetEnvironmentVariable('XIDEL_DEBUG_DECOMPRESSION') = 'true';
   if DEBUG_DECOMPRESSION then writeln(encodingIsGZIP, ' ', encoding);
end;

initialization
  defaultTransferInflater := TTransferContentInflaterZlib;

end.

