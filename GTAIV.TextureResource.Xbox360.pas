unit GTAIV.TextureResource.Xbox360;

interface

uses SysUtils, Classes, Windows, Global.Endian, Compression.LZX, Console.Xbox360.Graphics;

{some structures}
type XTDHeader = record   //unpacked resource header, contains basic information about dictionary
  _vmt                : DWORD;
  dwOffsetMapOffset   : DWORD;
  dwHashTableOffset   : DWORD;
  _fC                 : DWORD;
  _f10                : DWORD;
  dwTextureCount      : DWORD;
  dwTextureCount2     : DWORD;
  dwTextureListOffset : DWORD;
  dwTextureCount3     : DWORD;
  dwTextureCount4     : DWORD;
end;

type grcTextureXenon = record    //item information block (Xbox360), each one contains information about relevant texture
  _vmt                : DWORD;
  _f4                 : DWORD;
  _f8                 : BYTE;
  _f9                 : BYTE;
  _fA                 : WORD;
  _fC                 : DWORD;
  _f10                : DWORD;
  pszNamePtr          : DWORD;
  dwD3DBaseTexture    : DWORD;
  dwWidth             : WORD;
  dwHeight            : WORD;
  _f20                : DWORD;
  //_f24                : D3DVECTOR;
  //_f30:               : D3DVECTOR;
end;

procedure LoadResource(InStream: TMemoryStream; var OutStream: TMemoryStream; var dwCPUSize, dwGPUSize : DWORD; var bIsCPUSizeUnknown: boolean);
procedure SaveResource(CompressedFile: TMemoryStream; InStream: TStream);
procedure LoadXTD(InStream: TMemoryStream; iWorkMode: DWORD);

implementation

uses MainUnit;

function GetOffset(dwOffset: DWORD): DWORD;
begin
if dwOffset = 0 then result:=0
  else if ((dwOffset shr 28 <> 5) and (dwOffset shr 28 <> 6)) then result:=0
    else result:=dwOffset and $0FFFFFFF;
end;

function DataOffset(i:longword):longword;
var
  j:  integer;
begin
  j:=i;
  j:=j shr 8;
  j:=j shl 16;
  j:=j shr 8;
  Result:=j;
end;


function GetValueRSC7(dwFlag, baseSize: integer): integer;
var
  newBaseSize, Size, i: integer;
begin
  newBaseSize := baseSize shl (dwFlag and $f);
  Size := ((((dwFlag shr 17) and $7f) + (((dwFlag shr 11) and $3f) shl 1) + (((dwFlag shr 7) and $f) shl 2) + (((dwFlag shr 5) and $3) shl 3) + (((dwFlag shr 4) and $1) shl 4)) * newBaseSize);
  for i:=0 to 3 do
  begin
    if (((dwFlag shr (24 + i)) and 1) = 1) then
      size:= size + newBaseSize shr (1 + i);
  end;
  result:=size;
end;

procedure LoadResource(InStream: TMemoryStream; var OutStream: TMemoryStream; var dwCPUSize, dwGPUSize : DWORD; var bIsCPUSizeUnknown: boolean);
var
  dwSignature, dwFlags, dwInSize, dwFlags1, dwFlags2: DWORD;
  isRSC7: boolean;
  isRSC7Compressed: boolean;
begin
  isRSC7:=true;
  isRSC7Compressed:=true;
  bIsCPUSizeUnknown:=false;
  InStream.Seek(0, 0);
  InStream.Read(dwSignature, 4);
  InStream.Seek(8,0);
  if (dwSignature=$52534305) or (dwSignature=$05435352) then
    begin
      isRSC7:=false;
      isRSC7Compressed:=false;
      InStream.Read(dwFlags, 4);
      if dwSignature=$52534305 then dwFlags:=EndianChangeDword(dwFlags);    //params in resource header can be big-endian or little-endian
      {memory blocks size calculation, at this stage "technical" definition of flags value is not important}
      dwCPUSize:=(dwFlags AND $7FF) shl (((dwFlags shr 11) AND $F) + 8);
      dwGPUSize:=((dwFlags shr 15) AND $7FF) shl (((dwFlags shr 26) AND $F)+8);
      InStream.Seek(16, 0);
    end;
  if (dwSignature=$52534385) or (dwSignature=$85435352) then    //params in resource header can be big-endian or little-endian
    begin
      isRSC7:=false;
      isRSC7Compressed:=false;
      inStream.Read(dwFlags1, 4);
      inStream.Read(dwFlags2, 4);
      if dwSignature=$52534385 then
        begin
          dwFlags1:=EndianChangeDword(dwFlags1);
          dwFlags2:=EndianChangeDword(dwFlags2);
          if (dwFlags2 and $80000000)=0 then dwCPUSize:=((dwFlags1 and $7FF) shl (((dwFlags1 shr 11) and 15)+8)) else dwCPUSize:=(dwFlags2 and $3FFF) shl 12;
          if (dwFlags2 and $80000000)=0 then dwGPUSize:=(((dwFlags1 shr 15) and $7FF) shl (((dwFlags1 shr 26) and 15)+8)) else dwGPUSize:=(dwFlags2 shr 2) and $3FFF000;
        end;
      InStream.Seek(20, 0);
    end;
  if (dwSignature=$37435352) then // GTA V
    begin
      isRSC7:=true;
      isRSC7Compressed:=false;
      inStream.Seek(8, 0);
      inStream.Read(dwFlags1, 4);
      inStream.Read(dwFlags2, 4);
      dwFlags1:=EndianChangeDword(dwFlags1);
      dwFlags2:=EndianChangeDword(dwFlags2);
      dwCPUSize:=GetValueRSC7(dwFlags1, $2000);
      dwGPUSize:=GetValueRSC7(dwFlags2, $2000);
    end;
  if not(isRSC7Compressed) then
  begin
    OutStream.Seek(0, 0);
    if not(isRSC7) then
      begin
        // usual game resources
        InStream.Read(dwInSize, 4);
        dwInSize:=EndianChangeDWORD(dwInSize);
        DecompressLZX(InStream.Position, dwInSize, dwCPUSize+dwGPUSize, InStream, OutStream, 0);   //Xbox360 resources are compressed with LZX algorythm
      end
      else
      begin
        // GTA V (RSC7: uncompressed)
        InStream.Seek(16, 0);
        OutStream.CopyFrom(InStream, InStream.Size-16);
      end;
    OutStream.Seek(0, 0);
  end
  else
  begin
    // GTA V (compressed, without RSC header)
    OutStream.Seek(0, 0);
    InStream.Seek(16, 0);
    DecompressLZX(InStream.Position, dwInSize, 0, InStream, OutStream, 1);
    bIsCPUSizeUnknown:=true;
  end;
 //OutStream.SaveToFile('C:\Users\Dageron\Desktop\tmp1');
end;

procedure SaveResource(CompressedFile: TMemoryStream; InStream: TStream);
var
  dwSignature, dwFlags, dwInSize, dwOutSize, dwFlags1, dwFlags2: DWORD;
  isRSC7: boolean;
  isRSC7Compressed: boolean;
  NewCompressedFile: TMemoryStream;
begin
  CompressedFile.Seek(0, 0);
  NewCompressedFile:=TMemoryStream.Create;
  NewCompressedFile.CopyFrom(CompressedFile, 32);
  isRSC7:=true;
  isRSC7Compressed:=true;
  CompressedFile.Seek(0, 0);
  CompressedFile.Read(dwSignature, 4);
  CompressedFile.Seek(8,0);
  if (dwSignature=$52534305) or (dwSignature=$05435352) then
    begin
      isRSC7:=false;
      isRSC7Compressed:=false;
      CompressedFile.Read(dwFlags, 4);
      if dwSignature=$52534305 then dwFlags:=EndianChangeDword(dwFlags);    //params in resource header can be big-endian or little-endian
      {memory blocks size calculation, at this stage "technical" definition of flags value is not important}
      dwCPUSize:=(dwFlags AND $7FF) shl (((dwFlags shr 11) AND $F) + 8);
      dwGPUSize:=((dwFlags shr 15) AND $7FF) shl (((dwFlags shr 26) AND $F)+8);
      CompressedFile.Seek(16, 0);
      NewCompressedFile.Seek(16, 0);
    end;
  if (dwSignature=$52534385) or (dwSignature=$85435352) then    //params in resource header can be big-endian or little-endian
    begin
      isRSC7:=false;
      isRSC7Compressed:=false;
      CompressedFile.Read(dwFlags1, 4);
      CompressedFile.Read(dwFlags2, 4);
      if dwSignature=$52534385 then
        begin
          dwFlags1:=EndianChangeDword(dwFlags1);
          dwFlags2:=EndianChangeDword(dwFlags2);
          if (dwFlags2 and $80000000)=0 then dwCPUSize:=((dwFlags1 and $7FF) shl (((dwFlags1 shr 11) and 15)+8)) else dwCPUSize:=(dwFlags2 and $3FFF) shl 12;
          if (dwFlags2 and $80000000)=0 then dwGPUSize:=(((dwFlags1 shr 15) and $7FF) shl (((dwFlags1 shr 26) and 15)+8)) else dwGPUSize:=(dwFlags2 shr 2) and $3FFF000;
        end;
      CompressedFile.Seek(20, 0);
      NewCompressedFile.Seek(20, 0);
    end;
    if (dwSignature=$37435352) then // GTA V
    begin
      isRSC7:=true;
      isRSC7Compressed:=false;
      CompressedFile.Seek(8, 0);
      CompressedFile.Read(dwFlags1, 4);
      CompressedFile.Read(dwFlags2, 4);
      dwFlags1:=EndianChangeDword(dwFlags1);
      dwFlags2:=EndianChangeDword(dwFlags2);
      dwCPUSize:=GetValueRSC7(dwFlags1, $2000);
      dwGPUSize:=GetValueRSC7(dwFlags2, $2000);
      CompressedFile.Seek(16, 0);
      InStream.Seek(0, 0);
      CompressedFile.CopyFrom(InStream, dwCPUSize+dwGPUSize);
    end;
    if (not(isRSC7) and not(isRSC7Compressed)) then
      begin
        // usual resources
        CompressedFile.Read(dwOutSize, 4); dwOutSize:=EndianChangeDWORD(dwOutSize);
        Compression.LZX.CompressLZX(CompressedFile.Position-8, dwCPUSize+dwGPUSize, dwOutSize, InStream, NewCompressedFile, 0);
        CompressedFile.Free;
        CompressedFile:=TMemoryStream.Create;
        CompressedFile.Seek(0,0);
        NewCompressedFile.Seek(0, 0);
        CompressedFile.CopyFrom(NewCompressedFile, NewCompressedFile.Size);
        NewCompressedFile.Free;
      end
      else
      begin
        if (isRSC7Compressed) then
          begin
            CompressedFile.Seek(16, 0);
            InStream.Seek(0, 0);
            Compression.LZX.CompressLZX(CompressedFile.Position, InStream.Size, 0, InStream, NewCompressedFile, 1);
            CompressedFile:=NewCompressedFile;
            NewCompressedFile.Free;
          end;
      end;
end;

procedure LoadXTD(InStream: TMemoryStream; iWorkMode: DWORD);
var
  i, j, k: integer;
  S: String;
  dwEndian, dwTextureType, dwGPUTextureOffset: DWORD;
  grcTextureXenonOffset: DWORD;
  TextureAdress, Number: DWORD;
  filename:array [1..64] of AnsiChar;
  m_XTDHeader: XTDHeader;
  m_grcTextureXenon: grcTextureXenon;
  m_D3DBaseTextureDWORDs: D3DBaseTextureDWORDs;
  m_D3DBaseTexture: D3DBaseTexture;
  dwOffset, dwPosition, dwPosition_1, dwPosition_2, dwPosition_3, dwObjectOffset: DWORD;
  wAmount, wCount: WORD;
  btType: byte;
begin
  MainForm.Names.Clear;
  tslGPUTextureDataOffsets.Clear;
  tslTextureListOffsets.Clear;
  tslWidths.Clear;
  tslHeights.Clear;
  tslTextureTypes.Clear;
  tslD3DBaseTextureOffsets.Clear;
  tslNameOffsets.Clear;
  tslEndians.Clear;
  tslMipMaps.Clear;
  tslMipMapOffsets.Clear;
  InStream.Seek(0,0);
  if iWorkMode=1 then   //work mode: *.xhm (in-game web)
    begin
      InStream.Seek(12, 0);
      InStream.Read(TextureAdress, 4);
      TextureAdress:=EndianChangeDWORD(TextureAdress);
      TextureAdress:=GetOffset(TextureAdress);
      InStream.Seek(TextureAdress, 0);
    end;
  with m_XTDHeader do   //texture dictionary header reading
    begin
      if iWorkMode=2 then   //work mode: *.xshp (some of Midnight Club: Los Angelos texture resources)
        begin
          InStream.Read(_vmt, 4);
          _vmt:=EndianChangeDWORD(_vmt);
          InStream.Read(dwOffsetMapOffset, 4);
          dwOffsetMapOffset:=EndianChangeDWORD(dwOffsetMapOffset);
          dwOffsetMapOffset:=GetOffset(dwOffsetMapOffset);
          InStream.Read(grcTextureXenonOffset,4);
          grcTextureXenonOffset:=EndianChangeDWORD(grcTextureXenonOffset);
          grcTextureXenonOffset:=GetOffset(grcTextureXenonOffset);
          tslTextureListOffsets.Add(IntToStr(grcTextureXenonOffset));
          m_XTDHeader.dwTextureCount:=1;
        end;
      if iWorkMode=4 then   //work mode: *.xtx (some of RDR texture resources)
        begin
          tslTextureListOffsets.Add(IntToStr(0));
          m_XTDHeader.dwTextureCount:=1;
        end;
      if iWorkMode=3 then   //work mode: *.xsf (some of Midnight Club: Los Angelos and RDR texture resources)
      begin
        InStream.Seek(0, 0);
        InStream.Read(_vmt, 4); _vmt:=EndianChangeDWORD(_vmt);
        if (_vmt=2955497728) or (_vmt=1145656576) or (_vmt=1144411392)  then   // work mode: MC:LA
        begin
        if (_vmt=2955497728) then
          begin
            InStream.Seek(8, 0);
            InStream.Read(dwOffsetMapOffset, 4);
            dwOffsetMapOffset:=EndianChangeDWORD(dwOffsetMapOffset);
            dwOffsetMapOffset:=GetOffset(dwOffsetMapOffset);
            InStream.Seek(dwOffsetMapOffset+24, 0);
            InStream.Read(dwOffsetMapOffset, 4);
            dwOffsetMapOffset:=EndianChangeDWORD(dwOffsetMapOffset);
            dwOffsetMapOffset:=GetOffset(dwOffsetMapOffset);
            InStream.Read(dwTextureCount, 2);
            dwTextureCount:=EndianChangeWORD(dwTextureCount);
            InStream.Seek(dwOffsetMapOffset+4, 0);
          end;
        if (_vmt=1145656576)  then
        begin
          InStream.Seek(24, 0);
          InStream.Read(dwOffsetMapOffset, 4);
          dwOffsetMapOffset:=EndianChangeDWORD(dwOffsetMapOffset);
          dwOffsetMapOffset:=GetOffset(dwOffsetMapOffset);
          InStream.Seek(50, 0);
          InStream.Read(dwTextureCount, 2);
          dwTextureCount:=EndianChangeWORD(dwTextureCount);
          InStream.Seek(dwOffsetMapOffset+4, 0);
        end;
       if (_vmt=1144411392) then
        begin
          InStream.Seek(24, 0);
          InStream.Read(dwOffsetMapOffset, 4);
          dwOffsetMapOffset:=EndianChangeDWORD(dwOffsetMapOffset);
          dwOffsetMapOffset:=GetOffset(dwOffsetMapOffset);
          InStream.Seek(28, 0);
          InStream.Read(dwTextureCount, 2);
          dwTextureCount:=EndianChangeWORD(dwTextureCount);
          InStream.Seek(dwOffsetMapOffset, 0);
        end;
          for I := 1 to dwTextureCount do
            begin
              InStream.Read(dwOffset, 4);
              dwOffset:=EndianChangeDWORD(dwOffset); dwOffset:=GetOffset(dwOffset);
              dwPosition:=InStream.Position;
              InStream.Seek(dwOffset, 0);
              InStream.Read(_vmt, 4);
              if (_vmt=5783828) then tslTextureListOffsets.Add(IntToStr(InStream.Position-4));
              if (_vmt=5599908) or (_vmt=5338308) then //fonts
                begin
                  InStream.Seek(InStream.Position+160, 0);
                  // poiter to pointer
                  for j := 1 to 8 do
                  begin
                  dwPosition_1:=InStream.Position;
                  InStream.Read(grcTextureXenonOffset, 4);
                  grcTextureXenonOffset:=EndianChangeDWORD(grcTextureXenonOffset); grcTextureXenonOffset:=GetOffset(grcTextureXenonOffset);
                  InStream.Seek(grcTextureXenonOffset, 0);
                  // pointer to pointer
                  InStream.Read(grcTextureXenonOffset, 4);
                  grcTextureXenonOffset:=EndianChangeDWORD(grcTextureXenonOffset); grcTextureXenonOffset:=GetOffset(grcTextureXenonOffset);
                  InStream.Seek(grcTextureXenonOffset, 0);
                  // pointer to grcTextureXenon
                  dwPosition_2:=InStream.Position;
                  InStream.Read(grcTextureXenonOffset, 4);
                  grcTextureXenonOffset:=EndianChangeDWORD(grcTextureXenonOffset); grcTextureXenonOffset:=GetOffset(grcTextureXenonOffset);
                  InStream.Seek(grcTextureXenonOffset, 0);
                  InStream.Read(_vmt, 4);
                  if (_vmt=5783828) or (_vmt=5548812) then
                  tslTextureListOffsets.Add(IntToStr(grcTextureXenonOffset));
                  InStream.Seek(dwPosition_2+4, 0);
                  InStream.Read(grcTextureXenonOffset, 4);
                  grcTextureXenonOffset:=EndianChangeDWORD(grcTextureXenonOffset); grcTextureXenonOffset:=GetOffset(grcTextureXenonOffset);
                  InStream.Seek(grcTextureXenonOffset, 0);
                  InStream.Read(_vmt, 4);
                  if (_vmt=5783828) or (_vmt=5548812) then
                  tslTextureListOffsets.Add(IntToStr(grcTextureXenonOffset));
                  InStream.Seek(dwPosition_1+4, 0);
                  end;
                end;
              if (_vmt=5586868) or (_vmt=5324220) then  // custom texture
                begin
                  InStream.Seek(InStream.Position+8, 0);
                  InStream.Read(grcTextureXenonOffset, 4);
                 // if grcTextureXenonOffset=0 then InStream.Read(grcTextureXenonOffset, 4);
                  grcTextureXenonOffset:=EndianChangeDWORD(grcTextureXenonOffset); grcTextureXenonOffset:=GetOffset(grcTextureXenonOffset);
                  InStream.Seek(grcTextureXenonOffset, 0);
                  InStream.Read(_vmt, 4);
                  if (_vmt=5783828) or (_vmt=5548812) then
                  tslTextureListOffsets.Add(IntToStr(grcTextureXenonOffset));
                end;
                InStream.Seek(dwPosition, 0);
            end;
          dwTextureCount:=tslTextureListOffsets.Count;
        end;
        if (_vmt=1756847104) then   // work mode: RDR
        begin
          InStream.Seek(44, 0);
          // read offset of swfObject struct
          InStream.Read(dwOffset, 4);
          dwOffset:=EndianChangeDWORD(dwOffset); dwOffset:=GetOffset(dwOffset);
          // get to the swfObject
          InStream.Seek(dwOffset, 0);
          // read offset to item offset array
          InStream.Seek(InStream.Position+24, 0);
          InStream.Read(dwPosition_1, 4);
          dwPosition_1:=EndianChangeDWORD(dwPosition_1); dwPosition_1:=GetOffset(dwPosition_1);
          // get to the item count
          InStream.Seek(dwOffset+50, 0);
          InStream.Read(wAmount, 2);   wAmount:=EndianChangeWORD(wAmount);
          // get to the item offset array
          InStream.Seek(dwPosition_1+4, 0);
          for I := 1 to wAmount do
            begin
                // read offset of item
                InStream.Read(dwPosition_2, 4);
                dwPosition_2:=EndianChangeDWORD(dwPosition_2); dwPosition_2:=GetOffset(dwPosition_2);
                // save current offset
                dwPosition:=InStream.Position;
                // get to the offset and check type
                InStream.Seek(dwPosition_2, 0);
                InStream.Seek(InStream.Position+8, 0);
                InStream.Read(btType, 1);
                if (btType=4) then     // type: bitmap
                begin
                  // get to the grcTextureXenon offset
                  InStream.Seek(InStream.Position+3, 0);
                  // read grcTextureXenon offset
                  InStream.Read(grcTextureXenonOffset, 4);
                  grcTextureXenonOffset:=EndianChangeDWORD(grcTextureXenonOffset); grcTextureXenonOffset:=GetOffset(grcTextureXenonOffset);
                  tslTextureListOffsets.Add(IntToStr(grcTextureXenonOffset));
                end;
                if (btType=5) then
                begin
                  // get to the font object offset
                  InStream.Seek(InStream.Position+155, 0);

                  for k := 1 to 3 do
                    begin
                      InStream.Read(dwObjectOffset, 4);
                      dwObjectOffset:=EndianChangeDWORD(dwObjectOffset); dwObjectOffset:=GetOffset(dwObjectOffset);
                      dwPosition_3 := InStream.Position;
                      if (dwObjectOffset<>0) then
                     begin

                      // get to the grdTextureXenon offset
                      InStream.Seek(dwObjectOffset, 0);
                      // read offset to the grcTextureXenon offset
                      InStream.Read(grcTextureXenonOffset, 4);
                      grcTextureXenonOffset:=EndianChangeDWORD(grcTextureXenonOffset); grcTextureXenonOffset:=GetOffset(grcTextureXenonOffset);
                      InStream.Seek(InStream.Position+16, 0);
                      InStream.Read(wCount, 2);
                      wCount:=EndianChangeWord(wCount);
                      InStream.Seek(grcTextureXenonOffset,0);
                      for j := 1 to wCount do
                        begin
                         // read grcTextureXenon offset
                         InStream.Read(grcTextureXenonOffset, 4);
                         grcTextureXenonOffset:=EndianChangeDWORD(grcTextureXenonOffset); grcTextureXenonOffset:=GetOffset(grcTextureXenonOffset);
                         tslTextureListOffsets.Add(IntToStr(grcTextureXenonOffset));
                         end;
                      end;
                      InStream.Seek(dwPosition_3, 0);
                    end;
                end;
                InStream.Seek(dwPosition, 0);
            end;
            dwTextureCount:=tslTextureListOffsets.Count;
        end;
      end;
      if (IWorkMode=0) or (iWorkMode=1) then    //usual *.xtd (basic texture resource) or *.xhm (in-game web) - resource header reading
        begin
          InStream.Read(_vmt, 4); _vmt:=EndianChangeDWORD(_vmt);
          InStream.Read(dwOffsetMapOffset, 4); dwOffsetMapOffset:=EndianChangeDWORD(dwOffsetMapOffset); dwOffsetMapOffset:=GetOffset(dwOffsetMapOffset);
          InStream.Read(_fC, 4); _fC:=EndianChangeDWORD(_fC);
          InStream.Read(_f10, 4); _f10:=EndianChangeDWORD(_f10);
          InStream.Read(dwHashTableOffset, 4); dwHashTableOffset:=EndianChangeDWORD(dwHashTableOffset); dwHashTableOffset:=GetOffset(dwHashTableOffset);
          InStream.Read(dwTextureCount, 2); dwTextureCount:=EndianChangeWORD(dwTextureCount);
          InStream.Read(dwTextureCount2, 2); dwTextureCount2:=EndianChangeWORD(dwTextureCount2);
          InStream.Read(dwTextureListOffset, 4); dwTextureListOffset:=EndianChangeDWORD(dwTextureListOffset); dwTextureListOffset:=GetOffset(dwTextureListOffset);
          InStream.Read(dwTextureCount3, 2); dwTextureCount3:=EndianChangeWORD(dwTextureCount3);
          InStream.Read(dwTextureCount4, 2); dwTextureCount4:=EndianChangeWORD(dwTextureCount4);
      end;
    end;
  if (iWorkMode=0) or (IWorkMode=1) then    //usual *.xtd (basic texture resource) or *.xhm (in-game web) - information offsets reading
    begin
      InStream.Seek(m_XTDHeader.dwTextureListOffset, 0);
        for I := 1 to m_XTDHeader.dwTextureCount do
          begin
            InStream.Read(grcTextureXenonOffset,4);
            grcTextureXenonOffset:=EndianChangeDWORD(grcTextureXenonOffset);
            grcTextureXenonOffset:=GetOffset(grcTextureXenonOffset);
            tslTextureListOffsets.Add(IntToStr(grcTextureXenonOffset));
          end;
    end;

    // RDR xvd
    if (iWorkMode = 5) then
    begin
      InStream.Seek(0, 0);
      InStream.Read(dwOffset, 4);
      if (dwOffset<>7477364)and(dwOffset<>7481572) then
      begin
        while (InStream.Position <= InStream.Size) and (dwOffset<>7477364) and (dwOffset<>7481572) do
        begin
          InStream.Read(dwOffset, 4);
          if (InStream.Position <= InStream.Size) then
          exit;
        end;
        InStream.Seek(InStream.Position + 40 - 4, 0);
      end
      else
      begin
        InStream.Seek(40, 0);
      end;
      InStream.Read(dwOffset, 4);
      if (dwOffset <> 0) then
      begin
        dwOffset := EndianChangeDword(dwOffset);
        dwOffset := GetOffset(dwOffset);
        InStream.Seek(dwOffset + 24, 0);
        InStream.Read(dwOffset, 4);
        InStream.Read(wAmount, 2);
        wAmount := EndianChangeWORD(wAmount);
        dwOffset := EndianChangeDword(dwOffset);
        dwOffset := GetOffset(dwOffset);
        InStream.Seek(dwOffset, 0);
        m_XTDHeader.dwTextureCount := wAmount;
        for i := 1 to wAmount do
        begin
          InStream.Read(grcTextureXenonOffset, 4);
          grcTextureXenonOffset := EndianChangeDword(grcTextureXenonOffset);
          grcTextureXenonOffset := GetOffset(grcTextureXenonOffset);
          tslTextureListOffsets.Add(IntToStr(grcTextureXenonOffset));
        end;

      end;
    end;

    // RDR xft
    if (iWorkMode = 6) then
    begin
      InStream.Seek(0, 0);
      InStream.Read(dwOffset, 4);
      if (dwOffset <> 7109988) then
      begin
        while (InStream.Position <= InStream.Size) and (dwOffset <> 7109988) do
        begin
          InStream.Read(dwOffset, 4);
          if (InStream.Position <= InStream.Size) then
          exit;
        end;
        InStream.Seek(InStream.Position + 20, 0);
      end
      else
      begin
        InStream.Seek(40, 0);
      end;

      InStream.Read(dwOffset, 4);
      dwOffset := EndianChangeDword(dwOffset);
      dwOffset := GetOffset(dwOffset);
      InStream.Read(wAmount, 2);
      wAmount := EndianChangeWORD(wAmount);
      InStream.Seek(dwOffset, 0);
      m_XTDHeader.dwTextureCount := wAmount;
      for i := 1 to wAmount do
      begin
        InStream.Read(grcTextureXenonOffset, 4);
        grcTextureXenonOffset := EndianChangeDword(grcTextureXenonOffset);
        grcTextureXenonOffset := GetOffset(grcTextureXenonOffset);
        tslTextureListOffsets.Add(IntToStr(grcTextureXenonOffset));
      end;
    end;

  for I := 1 to m_XTDHeader.dwTextureCount do   //item information blocks reading
    begin
      InStream.Seek(StrToInt(tslTextureListOffsets[I-1]), 0);
      InStream.Read(m_grcTextureXenon._vmt, 4);
      if (m_grcTextureXenon._vmt=5779548) or (m_grcTextureXenon._vmt=5783828) or (m_grcTextureXenon._vmt=5783828) or (m_grcTextureXenon._vmt=5548812) or (iWorkMode=2) then  InStream.Seek(InStream.Position+20, 0) else  InStream.Seek(InStream.Position+16, 0);  //check for MC:LA
      if (m_grcTextureXenon._vmt=6157940) then InStream.Seek(InStream.Position+4,0);
      if (m_grcTextureXenon._vmt shr 16 = 130) then InStream.Seek(InStream.Position+4, 0); // check for Max Payne 3
      //texture name offset reading
      if (m_grcTextureXenon._vmt div 100000 = 87) then InStream.Seek(StrToInt(tslTextureListOffsets[I-1])+32, 0);  //GTA V known: 8747684, 8739860, 8746740, 8739380, 8744004, 8758476, 8752532 ...
      if (m_grcTextureXenon._vmt=6163044) or (m_grcTextureXenon._vmt div 1000 = 6157)  or (m_grcTextureXenon._vmt = 7550668) or (m_grcTextureXenon._vmt div 100000 = 75)  or (m_grcTextureXenon._vmt div 100000 = 73) then InStream.Seek(StrToInt(tslTextureListOffsets[I-1])+24, 0); //RDR
      InStream.Read(m_grcTextureXenon.pszNamePtr, 4);
      if (m_grcTextureXenon.pszNamePtr=5779548) or (m_grcTextureXenon.pszNamePtr=6157940) then InStream.Read(m_grcTextureXenon.pszNamePtr, 4); //check for MC:LA
      m_grcTextureXenon.pszNamePtr:=EndianChangeDWORD(m_grcTextureXenon.pszNamePtr);
      m_grcTextureXenon.pszNamePtr:=GetOffset(m_grcTextureXenon.pszNamePtr);
      tslNameOffsets.Add(IntToStr(m_grcTextureXenon.pszNamePtr));
      //D3DBaseTexture offset reading
      if (m_grcTextureXenon._vmt div 100000 = 87) then InStream.Seek(StrToInt(tslTextureListOffsets[I-1])+52, 0); //GTA V known: 8747684, 8739860, 8746740, 8739380, 8744004, 8758476, 8752532 ...
      InStream.Read(m_grcTextureXenon.dwD3DBaseTexture, 4);
      m_grcTextureXenon.dwD3DBaseTexture:=EndianChangeDWORD(m_grcTextureXenon.dwD3DBaseTexture);
      m_grcTextureXenon.dwD3DBaseTexture:=GetOffset(m_grcTextureXenon.dwD3DBaseTexture);
      tslD3DBaseTextureOffsets.Add(IntToStr(m_grcTextureXenon.dwD3DBaseTexture));
      //texture width and height reading
      if (m_grcTextureXenon._vmt div 100000 = 87) then InStream.Seek(StrToInt(tslTextureListOffsets[I-1])+56, 0); //GTA V known: 8747684, 8739860, 8746740, 8739380, 8744004, 8758476, 8752532 ...
      m_grcTextureXenon.dwWidth:=0;
      InStream.Read(m_grcTextureXenon.dwWidth, 2);
      m_grcTextureXenon.dwWidth:=EndianChangeWORD(m_grcTextureXenon.dwWidth);
      m_grcTextureXenon.dwHeight:=0;
      InStream.Read(m_grcTextureXenon.dwHeight, 2);
      m_grcTextureXenon.dwHeight:=EndianChangeWORD(m_grcTextureXenon.dwHeight);
      tslHeights.Add(IntToStr(m_grcTextureXenon.dwHeight));
      tslWidths.Add(IntToStr(m_grcTextureXenon.dwWidth));
      InStream.Seek(InStream.Position+32, 0);
    end;
  for I := 0 to m_XTDHeader.dwTextureCount - 1 do
    begin
      InStream.Seek(StrToInt(tslD3DBaseTextureOffsets[I]),0);
        begin
          InStream.Read(m_D3DBaseTextureDWORDs, SizeOf(m_D3DBaseTextureDWORDs));
          m_D3DBaseTexture:=ReadD3DBaseTexture(m_D3DBaseTextureDWORDs);
          InStream.Seek(StrToInt(tslD3DBaseTextureOffsets[I]),0);
          dwGPUTextureOffset:=0;
          InStream.Seek(InStream.Position+32, 0);
          InStream.Read(dwGPUTextureOffset, 4);
          dwGPUTextureOffset:=DataOffset(dwGPUTextureOffset);
          dwGPUTextureOffset:=EndianChangeDWORD(dwGPUTextureOffset);
          tslGPUTextureDataOffsets.Add(IntToStr(dwGPUTextureOffset));
          InStream.Seek(InStream.Position-4, 0);
          InStream.Read(dwTextureType,4);
          dwTextureType:=EndianChangeDWORD(dwTextureType);
          dwTextureType:=dwTextureType shl 26;
          dwTextureType:=dwTextureType shr 26;
          InStream.Seek(InStream.Position-4, 0);
          InStream.Read(dwEndian,4);
          dwEndian:=dwEndian shl 24;
          dwEndian:=dwEndian shr 30;
          tslEndians.Add(IntToStr(dwEndian));
          tslTextureTypes.Add(IntToStr(dwTextureType));
         // tslMipMaps.Add('1');
          tslMipMaps.Add(IntToStr(m_D3DBaseTexture.MaxMipLevel+1));

          // offset for mipmaps
          dwGPUTextureOffset:=DataOffset((m_D3DBaseTextureDWORDs.dwDWORD_13 and $00F0FFFF));
          dwGPUTextureOffset:=EndianChangeDWORD(dwGPUTextureOffset);
          tslMipMapOffsets.Add(IntToStr(dwGPUTextureOffset));
        end;
    end;
  for I := 1 to m_XTDHeader.dwTextureCount do   //texture names reading
    begin
      InStream.Seek(StrToInt(tslNameOffsets[I-1]), 0);
      InStream.Read(FileName, 64);
      S:=FileName;
      MainForm.Names.Update;
      MainForm.Names.Items.Add(S);
      S:=MainForm.Names.Items[I-1];
      Number:=0;
      for J := 1 to length(S) do
        begin
          if S[J]='/' then Number:=J;
        end;
      Delete(S,1,number);
      MainForm.Names.Items[I-1]:=S;
      S:=MainForm.Names.Items[I-1];
      if pos('memory:$',s)<>0 then
      begin
        delete(s, 1, pos(':', s));
        delete(s, 1, pos(':', s));
      end;

      if (S[length(S)]<>'s') and (S[length(S)-1]<>'d') and (S[length(S)-2]<>'d') and (S[length(S)-3]<>'.') then S:=S+'.dds';
      if pos('.dds', s)=0 then S:=s+'.dds';
      if pos('..', s)<>0 then
        begin
          delete(s, pos('..', s), 1);
        end;
      MainForm.Names.Items[I-1]:=S;
    end;
end;

end.
