unit Console.PS3.Graphics;

interface

uses Classes, Windows, SysUtils,

ImagingTypes, ImagingClasses, ImagingComponents,
Global.DirectDrawSurface;

procedure ExportImagePS3(InStream: TMemoryStream; dwTextureType, dwWidth, dwHeigth, dwOffset: DWORD; bSwizzled: boolean; ExportPath: String);
procedure ImportImagePS3(InStream: TMemoryStream; dwTextureType, dwWidth, dwHeigth, dwOffset: DWORD; bSwizzled: boolean; ImportPath: String);
function GetGPUTEXTUREFORMAT(dwTextureType: DWORD): string;
function GetMipOffsetPS3(dwTextureType: DWORD; var dwWidth, dwHeigth: DWORD; dwMipMaps, dwOffset: DWORD): DWORD;

implementation

// Graphics formats, see more at: http://dageron.com/?page_id=5238&lang=en

function GetGPUTEXTUREFORMAT(dwTextureType: DWORD): string;
begin
  case dwTextureType of
    133: result:= 'GPUTEXTUREFORMAT_8_8_8_8';
    134: result:= 'GPUTEXTUREFORMAT_DXT1';
    135: result:= 'GPUTEXTUREFORMAT_DXT2_3';
    136: result:= 'GPUTEXTUREFORMAT_DXT4_5';
    166: result:= 'GPUTEXTUREFORMAT_DXT1';
    167: result:= 'GPUTEXTUREFORMAT_DXT2_3';
    168: result:= 'GPUTEXTUREFORMAT_DXT4_5';
    148: result:= 'GPUTEXTUREFORMAT_DXT5A';
    129: result:= 'GPUTEXTUREFORMAT_8';
    161: result:= 'GPUTEXTUREFORMAT_8';
    else result:= '-unknown-';
  end;
end;

function GetMipOffsetPS3(dwTextureType: DWORD; var dwWidth, dwHeigth: DWORD; dwMipMaps, dwOffset: DWORD): DWORD;
var
  i: integer;
  dwSize: DWORD;
begin
 for i := 0 to dwMipMaps-2 do
 begin
    if  GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT1' then
    begin
      dwSize:=dwWidth*dwHeigth div 2;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT2_3' then
    begin
      dwSize:=dwWidth*dwHeigth;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT4_5' then
    begin
      dwSize:=dwWidth*dwHeigth;
    end;
  if  GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_8_8_8_8' then
    begin
      dwSize:=dwWidth*dwHeigth * 4;
    end;
  dwWidth:=dwWidth div 2;
  dwHeigth:=dwHeigth div 2;
  dwOffset:=dwOffset+dwSize;
 end;
 result:=dwOffset;
end;

procedure ExportImagePS3(InStream: TMemoryStream; dwTextureType, dwWidth, dwHeigth, dwOffset: DWORD; bSwizzled: boolean; ExportPath: String);
var
  DXT: Integer;
  DDSStream: TMemoryStream;
  dwSize, W: DWORD;
  i:integer;
begin
  DDSStream:=TMemoryStream.Create;
  if  GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT1' then
    begin
      dwSize:=dwWidth*dwHeigth div 2;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT2_3' then
    begin
      dwSize:=dwWidth*dwHeigth;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT4_5' then
    begin
      dwSize:=dwWidth*dwHeigth;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT5A' then
    begin
      dwSize:=dwWidth*dwHeigth;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_8' then
    begin
      dwSize:=dwWidth*dwHeigth;
    end;
  if  GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_8_8_8_8' then
    begin
      dwSize:=dwWidth*dwHeigth * 4;
    end;
  InStream.Seek(dwOffset, 0);
  DDSStream.CopyFrom(InStream, dwSize);
  MakeImage(GetGPUTEXTUREFORMAT(dwTextureType), dwWidth, dwHeigth, 1, 0, 1, DDSStream, 0, ExportPath);
  DDSStream.Free;
end;

procedure ImportImagePS3(InStream: TMemoryStream; dwTextureType, dwWidth, dwHeigth, dwOffset: DWORD; bSwizzled: boolean; ImportPath: String);
var
  DXT: Integer;
  DDSStream: TFileStream;
  dwSize: DWORD;
begin
  DDSStream:=TFileStream.Create(ImportPath, fmOpenRead);
  if  GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT1' then
    begin
      dwSize:=dwWidth*dwHeigth div 2;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT2_3' then
    begin
      dwSize:=dwWidth*dwHeigth;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT4_5' then
    begin
      dwSize:=dwWidth*dwHeigth;
    end;
  DDSStream.Seek(128, 0);
  InStream.Seek(dwOffset, 0);
  InStream.CopyFrom(DDSStream, dwSize);
  DDSStream.Free;
end;

end.
