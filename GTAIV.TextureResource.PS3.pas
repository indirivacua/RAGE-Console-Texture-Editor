unit GTAIV.TextureResource.PS3;

interface

uses SysUtils, Classes, Windows,

Global.Endian,
Compression.Zlib;

{some structures}
type CTDHeader = record   //unpacked resource header, contains basic information about dictionary
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

type grcTextureCell = record    //item information block (PS3), each one contains information about relevant texture
  _vmt                : DWORD;
  _f4                 : DWORD;
  _f8                 : BYTE;
  _f9                 : BYTE;
  _fA                 : WORD;
  _fC                 : DWORD;
  _f10                : DWORD;
  pszNamePtr          : DWORD;
  dwTextureOffset     : DWORD;
  dwWidth             : WORD;
  dwHeight            : WORD;
  _f20                : DWORD;
  //_f24                : D3DVECTOR;
  //_f30:               : D3DVECTOR;
end;

procedure LoadResource(InStream: TStream; var OutStream: TMemoryStream; var dwCPUSize, dwGPUSize : DWORD; var bIsCPUSizeUnknown: boolean);
procedure SaveResource(CompressedFile: TMemoryStream; InStream: TStream);
procedure LoadCTD(InStream: TMemoryStream; iWorkMode: integer);

implementation

uses MainUnit;

function GetOffset(dwOffset: DWORD): DWORD;
begin
if dwOffset = 0 then result:=0
  else if ((dwOffset shr 28 <> 5) and (dwOffset shr 28 <> 6)) then result:=0
    else result:=dwOffset and $0FFFFFFF;
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

procedure LoadResource(InStream: TStream; var OutStream: TMemoryStream; var dwCPUSize, dwGPUSize : DWORD; var bIsCPUSizeUnknown: boolean);
var
  dwSignature, dwFlags, dwFlags1, dwFlags2, tmp: DWORD;
  isRSC7: boolean;
  isRSC7Compressed: boolean;
  DecompressionStream:TMemoryStream;
begin
  isRSC7:=true;
  isRSC7Compressed:=true;
  bIsCPUSizeUnknown:=false;
  InStream.Seek(0, 0);
  InStream.Read(dwSignature, 4);
  InStream.Seek(8,0);
  dwCPUSize:=0;
  dwGPUSize:=0;
  if (dwSignature=$52534305) or (dwSignature=$05435352) or (dwSignature=$52534306) or (dwSignature=$06435352) then
    begin
      isRSC7:=false;
      isRSC7Compressed:=false;
      InStream.Read(dwFlags, 4);
      if (dwSignature=$52534305) or (dwSignature=$52534306) then dwFlags:=EndianChangeDword(dwFlags);    //params in resource header can be big-endian or little-endian
      {memory blocks size calculation, at this stage "technical" definition of flags value is not important}
      dwCPUSize:=(dwFlags AND $7FF) shl (((dwFlags shr 11) AND $F) + 8);
      dwGPUSize:=((dwFlags shr 15) AND $7FF) shl (((dwFlags shr 26) AND $F)+8);
      InStream.Seek(12, 0);
    end;
  if (dwSignature=$52534385) or (dwSignature=$85435352) or (dwSignature=$52534386) or (dwSignature=$86435352) then    //params in resource header can be big-endian or little-endian
    begin
      isRSC7:=false;
      isRSC7Compressed:=false;
      inStream.Read(dwFlags1, 4);
      inStream.Read(dwFlags2, 4);
      if (dwSignature=$52534385) or (dwSignature=$52534386) then
        begin
          dwFlags1:=EndianChangeDword(dwFlags1);
          dwFlags2:=EndianChangeDword(dwFlags2);
          if (dwFlags2 and $80000000)=0 then dwCPUSize:=((dwFlags1 and $7FF) shl (((dwFlags1 shr 11) and 15)+8)) else dwCPUSize:=(dwFlags2 and $3FFF) shl 12;
          if (dwFlags2 and $80000000)=0 then dwGPUSize:=(((dwFlags1 shr 15) and $7FF) shl (((dwFlags1 shr 26) and 15)+8)) else dwGPUSize:=(dwFlags2 shl 2) and $3FFF000;
        end;
      InStream.Seek(16, 0);
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
      dwCPUSize:=GetValueRSC7(dwFlags1, $1000);
      dwGPUSize:=GetValueRSC7(dwFlags2, $1580);
    end;
  if not(isRSC7Compressed) then
  begin
    OutStream.Seek(0, 0);
    if not(isRSC7) then
      begin
        // usual game resources
        DecompressZLib(InStream.Position, InStream, OutStream);
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
    DecompressionStream:=TMemoryStream.Create;
    tmp:=$DA78;
    DecompressionStream.Write(tmp, 2);
    DecompressionStream.CopyFrom(InStream, InStream.Size-16);
    DecompressionStream.Seek(0, 0);
    DecompressZLib(0, DecompressionStream, OutStream);
    DecompressionStream.Free;
    bIsCPUSizeUnknown:=true;
  end;
 //OutStream.SaveToFile('C:\Users\Dageron\Desktop\zlib2'); // temporary
end;

procedure SaveResource(CompressedFile: TMemoryStream; InStream: TStream);
var
  dwSignature, dwFlags, dwInSize, dwFlags1, dwFlags2: DWORD;
  isRSC7: boolean;
  isRSC7Compressed: boolean;
  TempStream:TMemoryStream;
begin
  isRSC7:=true;
  isRSC7Compressed:=true;
  CompressedFile.Seek(0, 0);
  CompressedFile.Read(dwSignature, 4);
  CompressedFile.Seek(8,0);
 if (dwSignature=$52534305) or (dwSignature=$05435352) or (dwSignature=$52534306) or (dwSignature=$06435352) then
    begin
      CompressedFile.Seek(12, 0);
      Compression.Zlib.ÑompressZlib(CompressedFile, InStream);
      isRSC7:=false;
      isRSC7Compressed:=false;
    end;
  if (dwSignature=$52534385) or (dwSignature=$85435352) or (dwSignature=$52534386) or (dwSignature=$86435352) then
    begin
      CompressedFile.Seek(16, 0);
      Compression.Zlib.ÑompressZlib(CompressedFile, InStream);
      isRSC7:=false;
      isRSC7Compressed:=false;
    end;
  if (dwSignature=$37435352) then // GTA V (uncompressed, with header)
    begin
      isRSC7:=true;
      isRSC7Compressed:=false;
      CompressedFile.Seek(8, 0);
      CompressedFile.Read(dwFlags1, 4);
      CompressedFile.Read(dwFlags2, 4);
      dwFlags1:=EndianChangeDword(dwFlags1);
      dwFlags2:=EndianChangeDword(dwFlags2);
      dwCPUSize:=GetValueRSC7(dwFlags1, $1000);
      dwGPUSize:=GetValueRSC7(dwFlags2, $1580);
      CompressedFile.Seek(16, 0);
      InStream.Seek(0, 0);
      CompressedFile.CopyFrom(InStream, InStream.Size);
    end;
  if (isRSC7Compressed) then  // GTA V (compressed, without header)
    begin
      CompressedFile.Seek(16, 0);
      InStream.Seek(0, 0);
      TempStream:=TMemoryStream.Create;
      Compression.Zlib.ÑompressZlib(TempStream, InStream);
      TempStream.Seek(2, 0);
      CompressedFile.CopyFrom(TempStream, TempStream.Size-2);
    end;
end;

procedure LoadCTD(InStream: TMemoryStream; iWorkMode: integer);
var
  i, j, k, test: integer;
  S: String;
  dwEndian, dwTextureType: DWORD;
  m_CTDHeader: CTDHeader;
  m_grcTextureCell: grcTextureCell;
  grcTextureCellOffset: DWORD;
  TextureAdress, Number: DWORD;
  filename: array [1..50] of AnsiChar;
  btMipMaps: byte;
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
  InStream.Seek(0,0);
  if iWorkMode=1 then   //work mode: *.chm (in-game web)
    begin
      InStream.Seek(12, 0);
      InStream.Read(TextureAdress, 4);
      TextureAdress:=EndianChangeDWORD(TextureAdress);
      TextureAdress:=GetOffset(TextureAdress);
      InStream.Seek(TextureAdress, 0);
    end;
  with m_CTDHeader do   //texture dictionary header reading
    begin
      if iWorkMode=2 then   //work mode: *.cshp (some of Midnight Club: Los Angelos texture resources)
        begin
          InStream.Read(_vmt, 4);
          _vmt:=EndianChangeDWORD(_vmt);
          InStream.Read(dwOffsetMapOffset, 4);
          dwOffsetMapOffset:=EndianChangeDWORD(dwOffsetMapOffset);
          dwOffsetMapOffset:=GetOffset(dwOffsetMapOffset);
          InStream.Read(dwOffsetMapOffset,4);
          grcTextureCellOffset:=EndianChangeDWORD(grcTextureCellOffset);
          grcTextureCellOffset:=GetOffset(grcTextureCellOffset);
          tslTextureListOffsets.Add(IntToStr(grcTextureCellOffset));
          m_CTDHeader.dwTextureCount:=1;
        end;

      if iWorkMode=3 then   //work mode: *.xsf (some of Midnight Club: Los Angelos and RDR texture resources)
      begin
        InStream.Seek(0, 0);
        InStream.Read(_vmt, 4); _vmt:=EndianChangeDWORD(_vmt);
        //if (_vmt=2955497728) or (_vmt=1145656576) or (_vmt=1144411392)  then   // work mode: MC:LA

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
                  InStream.Read(grcTextureCellOffset, 4);
                  grcTextureCellOffset:=EndianChangeDWORD(grcTextureCellOffset); grcTextureCellOffset:=GetOffset(grcTextureCellOffset);
                  tslTextureListOffsets.Add(IntToStr(grcTextureCellOffset));
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
                        InStream.Read(grcTextureCellOffset, 4);
                        grcTextureCellOffset:=EndianChangeDWORD(grcTextureCellOffset); grcTextureCellOffset:=GetOffset(grcTextureCellOffset);
                        InStream.Seek(InStream.Position+16, 0);
                        InStream.Read(wCount, 2);
                        wCount:=EndianChangeWord(wCount);
                        InStream.Seek(grcTextureCellOffset,0);
                        for j := 1 to wCount do
                        begin
                         // read grcTextureCell offset
                         InStream.Read(grcTextureCellOffset, 4);
                         grcTextureCellOffset:=EndianChangeDWORD(grcTextureCellOffset); grcTextureCellOffset:=GetOffset(grcTextureCellOffset);
                         tslTextureListOffsets.Add(IntToStr(grcTextureCellOffset));
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


      if (iWorkMode=0) or (iWorkMode=1) then    //usual *.ctd (basic texture resource) or *.chm (in-game web) - resource header reading
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
  if (iWorkMode=0) or (IWorkMode=1) then    //usual *.ctd (basic texture resource) or *.chm (in-game web) - information offsets reading
    begin
      InStream.Seek(m_CTDHeader.dwTextureListOffset, 0);
        for I := 1 to m_CTDHeader.dwTextureCount do
          begin
            InStream.Read(grcTextureCellOffset,4);
            grcTextureCellOffset:=EndianChangeDWORD(grcTextureCellOffset);
            grcTextureCellOffset:=GetOffset(grcTextureCellOffset);
            tslTextureListOffsets.Add(IntToStr(grcTextureCellOffset));
          end;
    end;

  // RDR cvd
  if (iWorkMode = 5) then
  begin
    InStream.Seek(0, 0);
    InStream.Read(dwOffset, 4);
    if (dwOffset <> 7477364) and (dwOffset <> 7481572) then
    begin
      while (InStream.Position <= InStream.Size) and (dwOffset <> 7477364) and
        (dwOffset <> 7481572) do
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
    dwOffset := EndianChangeDword(dwOffset);
    dwOffset := GetOffset(dwOffset);
    InStream.Seek(dwOffset + 24, 0);
    InStream.Read(dwOffset, 4);
    InStream.Read(wAmount, 2);
    wAmount := EndianChangeWORD(wAmount);
    dwOffset := EndianChangeDword(dwOffset);
    dwOffset := GetOffset(dwOffset);
    InStream.Seek(dwOffset, 0);
    m_CTDHeader.dwTextureCount := wAmount;
    for i := 1 to wAmount do
    begin
      InStream.Read(grcTextureCellOffset, 4);
      grcTextureCellOffset := EndianChangeDword(grcTextureCellOffset);
      grcTextureCellOffset := GetOffset(grcTextureCellOffset);
      tslTextureListOffsets.Add(IntToStr(grcTextureCellOffset));
    end;
  end;

  // RDR cft
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
    m_CTDHeader.dwTextureCount := wAmount;
    for i := 1 to wAmount do
    begin
      InStream.Read(grcTextureCellOffset, 4);
      grcTextureCellOffset := EndianChangeDword(grcTextureCellOffset);
      grcTextureCellOffset := GetOffset(grcTextureCellOffset);
      tslTextureListOffsets.Add(IntToStr(grcTextureCellOffset));
    end;
  end;

  for i := 1 to m_CTDHeader.dwTextureCount do   //item information blocks reading
    begin
      inStream.Seek(StrToInt(tslTextureListOffsets[I-1]), 0);
      inStream.Read(m_grcTextureCell._vmt, 4);
      if (iWorkMode=2) or (m_grcTextureCell._vmt=5710932) then inStream.Seek(inStream.Position+20, 0) else  inStream.Seek(inStream.Position+16, 0);
      if (m_grcTextureCell._vmt=6157940) then inStream.Seek(inStream.Position+4,0);
      //texture type reading
      if (iWorkMode=2) or (m_grcTextureCell._vmt=5710932) then inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+32, 0) else inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+20, 0);
      if (m_grcTextureCell._vmt div 100000 = 90) or (m_grcTextureCell._vmt div 10000 = 906) then inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+8, 0);  // GTA V  known: 9058428, 9066748, 9058932, 9063068...
      if (m_grcTextureCell._vmt div 10000 = 615) or (m_grcTextureCell._vmt div 10000 = 616) or (m_grcTextureCell._vmt = 7529572) or (m_grcTextureCell._vmt div 100000 = 75) then  inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+32, 0); //RDR
      test := inStream.Position;
      inStream.Read(dwTextureType,1);
      tslTextureTypes.Add(IntToStr(dwTextureType));
      if (iWorkMode=2) or (m_grcTextureCell._vmt=5710932) then inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+24, 0) else inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+44, 0);
      //texture name offset reading
      if (m_grcTextureCell._vmt div 100000 = 90) or (m_grcTextureCell._vmt div 10000 = 906) then inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+32, 0);  // GTA V  known: 9058428, 9066748, 9058932, 9063068...
      if (m_grcTextureCell._vmt div 10000 = 615) or (m_grcTextureCell._vmt div 10000 = 616)  or (m_grcTextureCell._vmt = 7529572) or (m_grcTextureCell._vmt div 100000 = 75) then InStream.Seek(StrToInt(tslTextureListOffsets[I-1])+24, 0); //RDR

      inStream.Read(m_grcTextureCell.pszNamePtr, 4);
      m_grcTextureCell.pszNamePtr:=EndianChangeDWORD(m_grcTextureCell.pszNamePtr);
      m_grcTextureCell.pszNamePtr:=GetOffset(m_grcTextureCell.pszNamePtr);
      tslNameOffsets.Add(IntToStr(m_grcTextureCell.pszNamePtr));
      //Texture offset reading
       if (m_grcTextureCell._vmt div 100000 = 90) or (m_grcTextureCell._vmt div 10000 = 906) then inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+28, 0);  // GTA V  known: 9058428, 9066748, 9058932, 9063068...
      if (iWorkMode=2) or (m_grcTextureCell._vmt=5710932) then inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+52, 0);
      if (m_grcTextureCell._vmt div 10000 = 615) or (m_grcTextureCell._vmt div 10000 = 616) or (m_grcTextureCell._vmt = 7529572) or (m_grcTextureCell._vmt div 100000 = 75) then InStream.Seek(StrToInt(tslTextureListOffsets[I-1])+52, 0); //RDR
      inStream.Read(m_grcTextureCell.dwTextureOffset, 4);
      m_grcTextureCell.dwTextureOffset:=EndianChangeDWORD(m_grcTextureCell.dwTextureOffset);
      m_grcTextureCell.dwTextureOffset:=GetOffset(m_grcTextureCell.dwTextureOffset);
      tslGPUTextureDataOffsets.Add(IntToStr(m_grcTextureCell.dwTextureOffset));
      if (iWorkMode=2) or (m_grcTextureCell._vmt=5710932) then inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+40, 0) else inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+28, 0);
      //texture width and height reading
       if (m_grcTextureCell._vmt div 100000 = 90) or (m_grcTextureCell._vmt div 10000 = 906) then inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+16, 0);  // GTA V  known: 9058428, 9066748, 9058932, 9063068...
      if (m_grcTextureCell._vmt div 10000 = 615) or (m_grcTextureCell._vmt div 10000 = 616)or (m_grcTextureCell._vmt = 7529572) or (m_grcTextureCell._vmt div 100000 = 75) then  inStream.Seek(StrToInt(tslTextureListOffsets[I-1])+40, 0); //RDR

      m_grcTextureCell.dwWidth:=0;
      inStream.Read(m_grcTextureCell.dwWidth, 2);
      m_grcTextureCell.dwWidth:=EndianChangeWORD(m_grcTextureCell.dwWidth);
      tslWidths.Add(IntToStr(m_grcTextureCell.dwWidth));
      m_grcTextureCell.dwHeight:=0;
      inStream.Read(m_grcTextureCell.dwHeight, 2);
      m_grcTextureCell.dwHeight:=EndianChangeWORD(m_grcTextureCell.dwHeight);
      tslHeights.Add(IntToStr(m_grcTextureCell.dwHeight));
      inStream.Seek(inStream.Position-11, 0);
      inStream.Read(btMipMaps, 1);
      // MipMaps
      tslMipMaps.Add(IntToStr(btMipMaps));
      inStream.Seek(inStream.Position+10, 0);
      inStream.Seek(inStream.Position+32, 0);
    end;
  for i := 1 to m_CTDHeader.dwTextureCount do   //texture names reading
    begin
      InStream.Seek(StrToInt(tslNameOffsets[I-1]), 0);
      InStream.Read(FileName, 50);
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
      MainForm.Names.Items[I-1]:=S;
    end;
end;



end.
