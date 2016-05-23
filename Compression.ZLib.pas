unit Compression.ZLib;

interface

uses SysUtils, Windows, Classes, Zlib;

procedure DecompressZlib(dwOffset: DWORD; inStream, outStream: TStream);
procedure ÑompressZlib(inStream, outStream: TStream);

implementation

procedure DecompressZlib(dwOffset: DWORD; inStream, outStream: TStream);
var
  decompressStream: TDecompressionStream;
  bytesread:integer;
  mainbuffer:array[0..1023] of char;
begin
  inStream.Seek(dwOffset, 0);
  decompressStream:=TDecompressionStream.Create(inStream);
    repeat
      bytesread:=decompressStream.Read(mainbuffer, 1024);
      outStream.Write(mainbuffer, bytesread);
    until bytesread<1024;
  decompressStream.Free;
end;

procedure ÑompressZlib(inStream, outStream: TStream);
var
  compressStream: TCompressionStream;
  bytesread: integer;
  mainbuffer: array[0..1023] of char;
begin
  compressStream:=TCompressionStream.Create(clMax, inStream);
  OutStream.Seek(0,0);
    repeat
      bytesread:=outStream.Read(mainbuffer, 1024);
      compressStream.Write(mainbuffer, bytesread);
    until bytesread<1024;
  compressStream.Free;
end;

end.
