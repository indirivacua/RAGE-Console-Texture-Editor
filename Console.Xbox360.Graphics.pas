unit Console.Xbox360.Graphics;

interface

uses SysUtils, Classes, Messages, Variants, Windows, Math,

Console.Xbox360.Swizzling,

Global.DirectDrawSurface,
Global.Endian;

type

  D3DBaseTextureDWORDs = record
    dwDWORD_1                 : DWORD;
    dwDWORD_2                 : DWORD;
    dwDWORD_3                 : DWORD;
    dwDWORD_4                 : DWORD;
    dwDWORD_5                 : DWORD;
    dwDWORD_6                 : DWORD;
    dwDWORD_7                 : DWORD;
    dwDWORD_8                 : DWORD;
    dwDWORD_9                 : DWORD;
    dwDWORD_10                : DWORD;
    dwDWORD_11                : DWORD;
    dwDWORD_12                : DWORD;
    dwDWORD_13                : DWORD;
  end;

  D3DBaseTexture = record
    Common_Flags              : DWORD;   // :28;
    Common_Type               : DWORD;   // : 4;                            read=D3DRESOURCETYPE_read
    ReferenceCount            : DWORD;
    Fence                     : DWORD;
    ReadFence                 : DWORD;
    Identifier                : DWORD;
    BaseFlush                 : DWORD;    // default = 0xFFFF0000
    // -- D3DResource ends here ---
    MipFlush                  : DWORD;    // default = 0xFFFF0000
    // texture only - GPUTEXTURE_FETCH_FORMAT
    // DWORD 0:
    Tiled                     : DWORD;    // : 1;    // BOOL
    Pitch                     : DWORD;    // : 9;    // DWORD
    unk1                      : DWORD;    // : 1;
    unk2                      : DWORD;    // : 2;
    ClampZ                    : DWORD;    // : 3;    // GPUCLAMP
    ClampY                    : DWORD;    // : 3;    // GPUCLAMP
    ClampX                    : DWORD;    // : 3;    // GPUCLAMP
    SignW                     : DWORD;    // : 2;    // GPUSIGN
    SignZ                     : DWORD;    // : 2;    // GPUSIGN
    SignY                     : DWORD;    // : 2;    // GPUSIGN
    SignX                     : DWORD;    // : 2;    // GPUSIGN
    dwType                    : DWORD;    // : 2;    // GPUCONSTANTTYPE     read=GPUCONSTANTTYPE_read
    // DWORD 1:
    BaseAddress               : DWORD;    // : 20;   // DWORD               read=DataAddress_read
    ClampPolicy               : DWORD;    // : 1;    // GPUCLAMPPOLICY
    Stacked                   : DWORD;    // : 1;    // BOOL
    RequestSize               : DWORD;    // : 2;    // GPUREQUESTSIZE
    Endian                    : DWORD;    // : 2;    // GPUENDIAN           procedure: GetGPUENDIAN
    DataFormat                : DWORD;    // : 6;    // GPUTEXTUREFORMAT    procedure: GetGPUTEXTUREFORMAT

    // DWORD 2:
    Size                      : DWORD;    // enum GPUTEXTURESIZE

    {union
        GPUTEXTURESIZE_1D OneD;
        GPUTEXTURESIZE_2D TwoD;
        GPUTEXTURESIZE_3D ThreeD;
        GPUTEXTURESIZE_STACK Stack;
     Size;   }
    // DWORD 3:
    BorderSize                : DWORD;    // : 1;    // DWORD
    unk3                      : DWORD;    // : 3;
    AnisoFilter               : DWORD;    // : 3;    // GPUANISOFILTER
    MipFilter                 : DWORD;    // : 2;    // GPUMIPFILTER
    MinFilter                 : DWORD;    // : 2;    // GPUMINMAGFILTER
    MagFilter                 : DWORD;    // : 2;    // GPUMINMAGFILTER
    ExpAdjust                 : integer;  // : 6;    // int
    SwizzleW                  : DWORD;    // : 3;    // GPUSWIZZLE
    SwizzleZ                  : DWORD;    // : 3;    // GPUSWIZZLE
    SwizzleY                  : DWORD;    // : 3;    // GPUSWIZZLE
    SwizzleX                  : DWORD;    // : 3;    // GPUSWIZZLE
    NumFormat                 : DWORD;    // : 1;    // GPUNUMFORMAT
    // DWORD 4:
    GradExpAdjustV            : integer;  // : 5;    // int
    GradExpAdjustH            : integer;  // : 5;    // int
    LODBias                   : integer;  // : 10;   // int
    MinAnisoWalk              : DWORD;    // : 1;    // BOOL
    MagAnisoWalk              : DWORD;    // : 1;    // BOOL
    MaxMipLevel               : DWORD;    // : 4;    // DWORD
    MinMipLevel               : DWORD;    // : 4;    // DWORD
    VolMinFilter              : DWORD;    // : 1;    // GPUMINMAGFILTER
    VolMagFilter              : DWORD;    // : 1;    // GPUMINMAGFILTER
    // DWORD 5:
    MipAddress                : DWORD;    // : 20    // DWORD
    PackedMips                : DWORD;    // : 1;    // BOOL
    Dimension                 : DWORD;    // : 2;    // GPUDIMENSION        read=GPUDIMENSION_read
    AnisoBias                 : integer;  // : 4;    // int
    TriClamp                  : DWORD;    // : 2;    // GPUTRICLAMP
    ForceBCWToMax             : DWORD;    // : 1;    // BOOL
    BorderColor               : DWORD;    // : 2;    // GPUBORDERCOLOR
  end;

procedure ExportImageXbox360(InStream: TMemoryStream; dwTextureType, dwWidth, dwHeight, dwEndian, dwOffset: DWORD; bSwizzled: boolean; ExportPath: String);
procedure ImportImageXbox360(InStream: TMemoryStream; dwTextureType, dwWidth, dwHeight, dwEndian, dwOffset: DWORD; bSwizzled: boolean; ImportPath: String);
function GetGPUTEXTUREFORMAT(dwTextureType: DWORD): string;
function GetGPUENDIAN(dwEndian: DWORD): string;
function ReadD3DBaseTexture(m_D3DBaseTextureDWORDs: D3DBaseTextureDWORDs): D3DBaseTexture;
function GetMipOffsetXbox360(dwTextureType: DWORD; var dwWidth: DWORD; var dwHeight: DWORD; dwMipMaps, dwOffset, dwMipsOffset: DWORD): DWORD;

implementation

function ReadD3DBaseTexture(m_D3DBaseTextureDWORDs: D3DBaseTextureDWORDs): D3DBaseTexture;
var
  m_D3DBaseTexture: D3DBaseTexture;
  value1, value2: DWORD;
begin
  m_D3DBaseTextureDWORDs.dwDWORD_9:=EndianChangeDWORD(m_D3DBaseTextureDWORDs.dwDWORD_9);
  m_D3DBaseTexture.DataFormat:=m_D3DBaseTextureDWORDs.dwDWORD_9 shl 26;
  m_D3DBaseTexture.DataFormat:=m_D3DBaseTexture.DataFormat shr 26;
  m_D3DBaseTexture.MaxMipLevel:=m_D3DBaseTextureDWORDS.dwDWORD_12;
  value1:=((m_D3DBaseTexture.MaxMipLevel and $C0000000) shr 6);
  value2:=((m_D3DBaseTexture.MaxMipLevel and $00030000) shl 10);
  m_D3DBaseTexture.MaxMipLevel:= EndianChangeDWORD(value1 or value2);
  result:=m_D3DBaseTexture;
end;

function GetGPUTEXTUREFORMAT(dwTextureType: DWORD): string;
begin
  case dwTextureType of
    0: result:= 'GPUTEXTUREFORMAT_1_REVERSE';
    1: result:= 'GPUTEXTUREFORMAT_1';
    2: result:= 'GPUTEXTUREFORMAT_8';
    3: result:= 'GPUTEXTUREFORMAT_1_5_5_5';
    4: result:= 'GPUTEXTUREFORMAT_5_6_5';
    5: result:= 'GPUTEXTUREFORMAT_6_5_5';
    6: result:= 'GPUTEXTUREFORMAT_8_8_8_8';
    7: result:= 'GPUTEXTUREFORMAT_2_10_10_10';
    8: result:= 'GPUTEXTUREFORMAT_8_A';
    9: result:= 'GPUTEXTUREFORMAT_8_B';
    10: result:= 'GPUTEXTUREFORMAT_8_8';
    11: result:= 'GPUTEXTUREFORMAT_Cr_Y1_Cb_Y0_REP';
    12: result:= 'GPUTEXTUREFORMAT_Y1_Cr_Y0_Cb_REP';
    13: result:= 'GPUTEXTUREFORMAT_16_16_EDRAM';
    14: result:= 'GPUTEXTUREFORMAT_8_8_8_8_A';
    15: result:= 'GPUTEXTUREFORMAT_4_4_4_4';
    16: result:= 'GPUTEXTUREFORMAT_10_11_11';
    17: result:= 'GPUTEXTUREFORMAT_11_11_10';
    18: result:= 'GPUTEXTUREFORMAT_DXT1';
    19: result:= 'GPUTEXTUREFORMAT_DXT2_3';
    20: result:= 'GPUTEXTUREFORMAT_DXT4_5';
    21: result:= 'GPUTEXTUREFORMAT_16_16_16_16_EDRAM';
    22: result:= 'GPUTEXTUREFORMAT_24_8';
    23: result:= 'GPUTEXTUREFORMAT_24_8_FLOAT';
    24: result:= 'GPUTEXTUREFORMAT_16';
    25: result:= 'GPUTEXTUREFORMAT_16_16';
    26: result:= 'GPUTEXTUREFORMAT_16_16_16_16';
    27: result:= 'GPUTEXTUREFORMAT_16_EXPAND';
    28: result:= 'GPUTEXTUREFORMAT_16_16_EXPAND';
    29: result:= 'GPUTEXTUREFORMAT_16_16_16_16_EXPAND';
    30: result:= 'GPUTEXTUREFORMAT_16_FLOAT';
    31: result:= 'GPUTEXTUREFORMAT_16_16_FLOAT';
    32: result:= 'GPUTEXTUREFORMAT_16_16_16_16_FLOAT';
    33: result:= 'GPUTEXTUREFORMAT_32';
    34: result:= 'GPUTEXTUREFORMAT_32_32';
    35: result:= 'GPUTEXTUREFORMAT_32_32_32_32';
    36: result:= 'GPUTEXTUREFORMAT_32_FLOAT';
    37: result:= 'GPUTEXTUREFORMAT_32_32_FLOAT';
    38: result:= 'GPUTEXTUREFORMAT_32_32_32_32_FLOAT';
    39: result:= 'GPUTEXTUREFORMAT_32_AS_8';
    40: result:= 'GPUTEXTUREFORMAT_32_AS_8_8';
    41: result:= 'GPUTEXTUREFORMAT_16_MPEG';
    42: result:= 'GPUTEXTUREFORMAT_16_16_MPEG';
    43: result:= 'GPUTEXTUREFORMAT_8_INTERLACED';
    44: result:= 'GPUTEXTUREFORMAT_32_AS_8_INTERLACED';
    45: result:= 'GPUTEXTUREFORMAT_32_AS_8_8_INTERLACED';
    46: result:= 'GPUTEXTUREFORMAT_16_INTERLACED';
    47: result:= 'GPUTEXTUREFORMAT_16_MPEG_INTERLACED';
    48: result:= 'GPUTEXTUREFORMAT_16_16_MPEG_INTERLACED';
    49: result:= 'GPUTEXTUREFORMAT_DXN';
    50: result:= 'GPUTEXTUREFORMAT_8_8_8_8_AS_16_16_16_16';
    51: result:= 'GPUTEXTUREFORMAT_DXT1_AS_16_16_16_16';
    52: result:= 'GPUTEXTUREFORMAT_DXT2_3_AS_16_16_16_16';
    53: result:= 'GPUTEXTUREFORMAT_DXT4_5_AS_16_16_16_16';
    54: result:= 'GPUTEXTUREFORMAT_2_10_10_10_AS_16_16_16_16';
    55: result:= 'GPUTEXTUREFORMAT_10_11_11_AS_16_16_16_16';
    56: result:= 'GPUTEXTUREFORMAT_11_11_10_AS_16_16_16_16';
    57: result:= 'GPUTEXTUREFORMAT_32_32_32_FLOAT';
    58: result:= 'GPUTEXTUREFORMAT_DXT3A';
    59: result:= 'GPUTEXTUREFORMAT_DXT5A';
    60: result:= 'GPUTEXTUREFORMAT_CTX1';
    61: result:= 'GPUTEXTUREFORMAT_DXT3A_AS_1_1_1_1';
    62: result:= 'GPUTEXTUREFORMAT_8_8_8_8_GAMMA_EDRAM';
    63: result:= 'GPUTEXTUREFORMAT_2_10_10_10_FLOAT_EDRAM';
    else result:= '-unknown-';
  end;
end;


function GetGPUENDIAN(dwEndian: DWORD): string;
begin
  case dwEndian of
    0: result:='GPUENDIAN_NONE';
    1: result:='GPUENDIAN_8IN16';
    2: result:='GPUENDIAN_8IN32';
    3: result:='GPUENDIAN_16IN32';
    else result:='-invalid-endian-';
  end;
end;

function GetMipOffsetXbox360(dwTextureType: DWORD; var dwWidth: DWORD; var dwHeight: DWORD; dwMipMaps, dwOffset, dwMipsOffset: DWORD): DWORD;
var
  i: integer;
  dwSize: DWORD;
  h, w: DWORD;
  Constant: integer;
begin
 if (dwMipMaps=1) then
 begin
    result:=dwOffset;
 end;

 if (dwMipMaps=2) then
 begin
   dwWidth:=dwWidth div 2;
   dwHeight:=dwHeight div 2;
   result:=dwMipsOffset;
 end;

 if (dwMipMaps>2) then
 begin
 for i := 0 to dwMipMaps-2 do
 begin
    if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT1' then
    begin
      W:=dwWidth;
      H:=dwHeight;
      Constant:=128;
      if (W mod Constant <> 0) then W := W + (Constant - W mod Constant);
      if (H mod Constant <> 0) then H := H + (Constant - H mod Constant);
      dwSize:=W*H div 2;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT2_3' then
    begin
      W:=dwWidth;
      H:=dwHeight;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT4_5' then
    begin
      W:=dwWidth;
      H:=dwHeight;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_8' then
   begin
      W:=dwWidth;
      H:=dwHeight;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H;
   end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT5A' then
   begin
    begin
      W:=dwWidth;
      H:=dwHeight;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H*4;
    end;
  end;
  if  GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_8_8_8_8' then
    begin
      W:=dwWidth;
      H:=dwHeight;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H*4;
    end;

  dwWidth:=dwWidth div 2;
  dwHeight:=dwHeight div 2;
  if (i<>0) then
  dwMipsOffset:=dwMipsOffset+dwSize;
 end;
  result:=dwMipsOffset;
 end;

end;

procedure ExportImageXbox360(InStream: TMemoryStream; dwTextureType, dwWidth, dwHeight, dwEndian, dwOffset: DWORD; bSwizzled: boolean; ExportPath: String);
var
  h, w, t, I, dwSize: DWORD;
  tempbyte: byte;
  test2: integer;
  x, y, off: integer;
  DDSStream: TMemoryStream;
  pBuffer, pGfx: Pointer;
  Constant: integer;
  tiledBuf: TBytes absolute pBuffer;
  graphBuf: TBytes absolute pGfx;
begin
  DDSStream:=TMemoryStream.Create;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT1' then    //if DXT = 1
    begin
      W:=dwWidth;
      H:=dwHeight;
      Constant:=128;
      if (W mod Constant <> 0) then W := W + (Constant - W mod Constant);
      if (H mod Constant <> 0) then H := H + (Constant - H mod Constant);
      dwSize:=W*H div 2;
      W:=dwWidth div 4;
      H:=dwHeight div 4;
      T:=8;
      dwEndian:=1;
      bSwizzled:=true;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT2_3' then    //if (DXT = 2) or (DXT = 3)
    begin
      W:=dwWidth;
      H:=dwHeight;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H;
      W:=dwWidth div 4;
      H:=dwHeight div 4;
      T:=16;
      dwEndian:=1;
      bSwizzled:=true;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT4_5' then    //if (DXT = 4) or (DXT = 5)
    begin
      W:=dwWidth;
      H:=dwHeight;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H;
      W:=dwWidth div 4;
      H:=dwHeight div 4;
      T:=16;
      dwEndian:=1;
      bSwizzled:=true;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT5A' then    //if DXT = 5A
    begin
      H:=dwHeight;
      W:=dwWidth;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H;
      W:=dwWidth div 4;
      H:=dwHeight div 4;
      T:=8;
      dwEndian:=1;
      bSwizzled:=true;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_8' then
  begin
    W:=dwWidth;
    H:=dwHeight;
    if (W mod 128 <> 0) then W := W + (128 - W mod 128);
    if (H mod 128 <> 0) then H := H + (128 - H mod 128);
    dwSize:=W*H;
    W:=dwWidth;
    H:=dwHeight;
    T:=1;
    dwEndian:=1;
    bSwizzled:=true;
  end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_8_8_8_8' then
  begin
    W:=dwWidth;
    H:=dwHeight;
    if (W mod 128 <> 0) then W := W + (128 - W mod 128);
    if (H mod 128 <> 0) then H := H + (128 - H mod 128);
    dwSize:=W*H*4;
    W:=dwWidth;
    H:=dwHeight;
    T:=4;
    dwEndian:=2;
    bSwizzled:=true;
  end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXN' then
  begin
    W:=dwWidth;
    H:=dwHeight;
    if (W mod 128 <> 0) then W := W + (128 - W mod 128);
    if (H mod 128 <> 0) then H := H + (128 - H mod 128);
    dwSize:=W*H*4;
    W:=dwWidth;
    H:=dwHeight;
    T:=4;
    dwEndian:=2;
    bSwizzled:=true;
  end;
  GetMem(pBuffer, dwSize);
  GetMem(pGfx, dwSize);
  InStream.Seek(dwOffset, 0);
  InStream.Read(pBuffer^, dwSize);
  if bSwizzled=true then
    begin
      for Y := 0 to H - 1 do
        for X := 0 to W - 1 do
          begin
            off:=0;
            off:=XGAddress2DTiledOffset(x, y, W, T);
            move(TiledBuf[off*T], graphBuf[(x+y*W)*T], T);
          end;
     end
     else
     begin
       graphBuf:=TiledBuf;
     end;
  if GetGPUENDIAN(dwEndian)='GPUENDIAN_8IN16' then    //change endian: swap two bytes in each WORD (2 bytes) value
    begin
      test2:=0;
      for I := 0 to dwSize do
        begin
          if test2=0 then
            begin
              tempbyte:=graphBuf[I];
              graphBuf[I]:=graphBuf[I+1];
              graphBuf[I+1]:=tempbyte;
            end;
          if test2=0 then  test2:=1 else test2:=0;
        end;
    end;
  if GetGPUENDIAN(dwEndian)='GPUENDIAN_8IN32' then    //change endian: swap four bytes in each DWORD (4 bytes) value
    begin
      test2:=0;
      for I := 0 to dwSize do
        begin
          if test2=0 then
            begin
              tempbyte:=graphBuf[I];
              graphBuf[I]:=graphBuf[I+3];
              graphBuf[I+3]:=tempbyte;
              tempbyte:=graphBuf[I+1];
              graphBuf[I+1]:=graphBuf[I+2];
              graphBuf[I+2]:=tempbyte;
            end;
          if test2=0 then  test2:=1 else test2:=0;
        end;
    end;
  DDSStream.Seek(0, 0);
  DDSStream.Write(pGfx^, dwSize);
  MakeImage(GetGPUTEXTUREFORMAT(dwTextureType), dwWidth, dwHeight, 1, 0, 1, DDSStream, 0, ExportPath);
  DDSStream.Free;
end;


procedure ImportImageXbox360(InStream: TMemoryStream; dwTextureType, dwWidth, dwHeight, dwEndian, dwOffset: DWORD; bSwizzled: boolean; ImportPath:String);
var
  h, w, t, I, dwSize: DWORD;
  tempbyte: byte;
  test2: integer;
  x, y, off: integer;
  DDSStream: TFileStream;
  pBuffer, pGfx: Pointer;
  tiledBuf: TBytes absolute pBuffer;
  graphBuf: TBytes absolute pGfx;
begin
  DDSStream:=TFileStream.Create(ImportPath, fmOpenRead);
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT1' then    //if DXT = 1
    begin
      W:=dwWidth;
      H:=dwHeight;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H div 2;
      W:=dwWidth div 4;
      H:=dwHeight div 4;
      T:=8;
      dwEndian:=1;
      bSwizzled:=true;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT2_3' then    //if (DXT = 2) or (DXT = 3)
    begin
      W:=dwWidth;
      H:=dwHeight;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H;
      W:=dwWidth div 4;
      H:=dwHeight div 4;
      T:=16;
      dwEndian:=1;
      bSwizzled:=true;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT4_5' then    //if (DXT = 4) or (DXT = 5)
    begin
      W:=dwWidth;
      H:=dwHeight;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H;
      W:=dwWidth div 4;
      H:=dwHeight div 4;
      T:=16;
      dwEndian:=1;
      bSwizzled:=true;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_DXT5A' then    //if DXT = 5A
    begin
      H:=dwHeight;
      W:=dwWidth;
      if (W mod 128 <> 0) then W := W + (128 - W mod 128);
      if (H mod 128 <> 0) then H := H + (128 - H mod 128);
      dwSize:=W*H;
      W:=dwWidth div 4;
      H:=dwHeight div 4;
      T:=8;
      dwEndian:=1;
      bSwizzled:=true;
    end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_8' then
  begin
    W:=dwWidth;
    H:=dwHeight;
    if (W mod 128 <> 0) then W := W + (128 - W mod 128);
    if (H mod 128 <> 0) then H := H + (128 - H mod 128);
    dwSize:=W*H;
    W:=dwWidth;
    H:=dwHeight;
    T:=1;
    dwEndian:=1;
    bSwizzled:=true;
  end;
  if GetGPUTEXTUREFORMAT(dwTextureType)='GPUTEXTUREFORMAT_8_8_8_8' then
  begin
    W:=dwWidth;
    H:=dwHeight;
    if (W mod 128 <> 0) then W := W + (128 - W mod 128);
    if (H mod 128 <> 0) then H := H + (128 - H mod 128);
    dwSize:=W*H*4;
    W:=dwWidth;
    H:=dwHeight;
    T:=4;
    dwEndian:=2;
    bSwizzled:=true;
  end;
  DDSStream.Seek(128, 0);
  GetMem(pBuffer, dwSize);
  GetMem(pGfx, dwSize);
  DDSStream.Read(pGfx^, dwSize);
  if GetGPUENDIAN(dwEndian)='GPUENDIAN_8IN16' then    //change endian: swap two bytes in each WORD (2 bytes) value
    begin
      test2:=0;
      for I := 0 to dwSize do
        begin
          if test2=0 then
            begin
              tempbyte:=graphBuf[I];
              graphBuf[I]:=graphBuf[I+1];
              graphBuf[I+1]:=tempbyte;
            end;
          if test2=0 then  test2:=1 else test2:=0;
        end;
    end;
  if GetGPUENDIAN(dwEndian)='GPUENDIAN_8IN32' then    //change endian: swap four bytes in each DWORD (4 bytes) value
    begin
      test2:=0;
      for I := 0 to dwSize do
        begin
          if test2=0 then
            begin
              tempbyte:=graphBuf[I];
              graphBuf[I]:=graphBuf[I+3];
              graphBuf[I+3]:=tempbyte;
              tempbyte:=TiledBuf[I+1];
              graphBuf[I+1]:=graphBuf[I+2];
              graphBuf[I+2]:=tempbyte;
            end;
          if test2=0 then  test2:=1 else test2:=0;
        end;
    end;
  DDSStream.Seek(128, 0);
  if bSwizzled=true then
    begin
      for Y := 0 to H - 1 do
        for X := 0 to W - 1 do
          begin
            off:=XGAddress2DTiledOffset(x, y, W, T);
            move(graphBuf[(x+y*W)*T], TiledBuf[off*T], T);
          end;
    end;
  InStream.Seek(dwOffset, 0);
  InStream.Write(pBuffer^, dwSize);
  DDSStream.Free;
end;

end.
