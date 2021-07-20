unit internetaccess_inflater_paszlib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, internetaccess, ZBase, ZStream, ZInflate;

type
PTransferBlockWriteEvent = ^TTransferBlockWriteEvent;
TTransferContentInflaterZlib = class(TTransferContentInflater)
  stream: z_stream;
  buffer: pointer;
  headerRead: boolean;
  writeUncompressedBlock: TTransferBlockWriteEvent;
  pointerToBlockWrite: PTransferBlockWriteEvent;
  expectGZIP: boolean;
  procedure writeCompressedBlock(const abuffer; Count: Longint);
  constructor Create;
  destructor Destroy; override;
  class procedure injectDecoder({%H-}sender: TInternetAccess; const encoding: string; var encoder: TTransferContentInflater; var blockWrite: TTransferBlockWriteEvent); override;
end;

implementation

const buffer_block_size=16384;

function skipHeader(expectGZip: boolean; var p: PByte; var length: cardinal): boolean;
var
  flags: Byte;
  skipLength, xlen: cardinal;
begin
  if expectGZip then begin
    // RFC 1952
    skipLength := 10;
    if length < 10 then exit(false);
    if (p[0] <> 31) or (p[1] <> 139) or (p[2] <> 8) then exit(false); //two check bytes, 8 means deflate algorithm
    result := true;
    flags := p[3];
    if flags and 1 <> 0 then result := false; //FTEXT, no compression, but has header
    if (flags and 4 <> 0) and (length >= skipLength + 2) then begin //FEXTRA, more header
      xlen := p[skipLength] + cardinal(256) * p[skipLength + 1];
      if length >= skipLength + 2 + xlen then skipLength += xlen + 2;
    end;
    if flags and 8 <> 0 then begin //FNAME, zero-terminated
      while (skipLength <= length) and (p[skipLength] <> 0) do inc(skipLength);
      if skipLength <= length then inc(skipLength);
    end;
    if flags and 16 <> 0 then begin //Fcomment, zero-terminated
      while (skipLength <= length) and (p[skipLength] <> 0) do inc(skipLength);
      if skipLength <= length then inc(skipLength);
    end;
    if flags and 2 <> 0 then //FHCRC
      inc(skipLength, 2);
  end else begin
    //RFC 1950
    //however, the browser/Server implementation is wrong, so there usually are no header bytes (see https://stackoverflow.com/a/9856879/1501222 )
    skipLength := 0;
    result := true;
    if length >= 2 then begin
      if p[0] and 15 <> 8 then exit; //8 means deflate algorithm
      if p[0] and not 15 > 7 then exit;
      if (p[0] * 256 + p[1] ) mod 31 <> 0 then exit; //FLG check bits
      skipLength := 2;
      if p[1] and 32 <> 0 then skipLength += 4; //dictionary
    end;
  end;
  //writeln(result, skipLength);
  if skipLength > length then skipLength := length;
  p += skipLength;
  length -= skipLength;
end;

procedure TTransferContentInflaterZlib.writeCompressedBlock(const abuffer; Count: Longint);
var err:smallint;
begin
  stream.next_in:=@abuffer;
  stream.avail_in:=count;

  if not headerRead then begin
    if not skipHeader(expectGZIP, stream.next_in, stream.avail_in) then begin
      writeUncompressedBlock(stream.next_in^, stream.avail_in);
      pointerToBlockWrite^ := writeUncompressedBlock;
      exit;
    end;
    headerRead := true;
  end;

  while stream.avail_in > 0 do begin
    stream.next_out:=self.buffer;
    stream.avail_out:=  buffer_block_size;

    err:=inflate(stream, Z_NO_FLUSH);
    if (err<>Z_OK) and (err <> Z_STREAM_END) then
      raise Edecompressionerror.create(zerror(err));

    writeUncompressedBlock(self.buffer^, stream.next_out - self.buffer);

    if err=Z_STREAM_END then
      exit;
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

class procedure TTransferContentInflaterZlib.injectDecoder(sender: TInternetAccess; const encoding: string;
  var encoder: TTransferContentInflater; var blockWrite: TTransferBlockWriteEvent);
var
  zlibencoder: TTransferContentInflaterZlib;
  encodingIsGZIP: boolean;
begin
  case encoding of
  'gzip': encodingIsGZIP := true;
  'deflate': encodingIsGZIP := false;
  else exit;
  end;
  zlibencoder := TTransferContentInflaterZlib.Create;
  zlibencoder.writeUncompressedBlock := blockWrite;
  zlibencoder.pointerToBlockWrite:=@blockWrite;
  zlibencoder.expectGZIP:=encodingIsGZIP;

  {if Askipheader then
    err:=inflateInit2(Fstream,-MAX_WBITS)
  else
    err:=inflateInit(Fstream);
   }
   //inflateInit(zlibencoder.stream);
   inflateInit2(zlibencoder.stream, -MAX_WBITS);
   blockWrite := @zlibencoder.writeCompressedBlock;
   encoder := zlibencoder;
end;

initialization
  defaultTransferInflater := TTransferContentInflaterZlib;

end.

