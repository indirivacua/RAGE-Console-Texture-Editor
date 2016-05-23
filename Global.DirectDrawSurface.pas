unit Global.DirectDrawSurface;

interface

uses SysUtils, Classes, Windows, ImagingTypes, ImagingClasses, ImagingComponents;

const
  { Four character codes.}
  DDSMagic    = LongWord(Byte('D') or (Byte('D') shl 8) or (Byte('S') shl 16) or (Byte(' ') shl 24));
  FOURCC_DXT1 = LongWord(Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('1') shl 24));
  FOURCC_DXT3 = LongWord(Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('3') shl 24));
  FOURCC_DXT5 = LongWord(Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('5') shl 24));
  FOURCC_ATI1 = LongWord(Byte('A') or (Byte('T') shl 8) or (Byte('I') shl 16) or (Byte('1') shl 24));
  FOURCC_ATI2 = LongWord(Byte('A') or (Byte('T') shl 8) or (Byte('I') shl 16) or (Byte('2') shl 24));

  { Some D3DFORMAT values used in DDS files as FourCC value.}
  D3DFMT_A16B16G16R16  = 36;
  D3DFMT_R32F          = 114;
  D3DFMT_A32B32G32R32F = 116;
  D3DFMT_R16F          = 111;
  D3DFMT_A16B16G16R16F = 113;

  { Constans used by TDDSurfaceDesc2.Flags.}
  DDSD_CAPS            = $00000001;
  DDSD_HEIGHT          = $00000002;
  DDSD_WIDTH           = $00000004;
  DDSD_PITCH           = $00000008;
  DDSD_PIXELFORMAT     = $00001000;
  DDSD_MIPMAPCOUNT     = $00020000;
  DDSD_LINEARSIZE      = $00080000;
  DDSD_DEPTH           = $00800000;

  { Constans used by TDDSPixelFormat.Flags.}
  DDPF_ALPHAPIXELS     = $00000001;    // used by formats which contain alpha
  DDPF_FOURCC          = $00000004;    // used by DXT and large ARGB formats
  DDPF_RGB             = $00000040;    // used by RGB formats
  DDPF_LUMINANCE       = $00020000;    // used by formats like D3DFMT_L16
  DDPF_BUMPLUMINANCE   = $00040000;    // used by mixed signed-unsigned formats
  DDPF_BUMPDUDV        = $00080000;    // used by signed formats

  { Constans used by TDDSCaps.Caps1.}
  DDSCAPS_COMPLEX      = $00000008;
  DDSCAPS_TEXTURE      = $00001000;
  DDSCAPS_MIPMAP       = $00400000;

  { Constans used by TDDSCaps.Caps2.}
  DDSCAPS2_CUBEMAP     = $00000200;
  DDSCAPS2_POSITIVEX   = $00000400;
  DDSCAPS2_NEGATIVEX   = $00000800;
  DDSCAPS2_POSITIVEY   = $00001000;
  DDSCAPS2_NEGATIVEY   = $00002000;
  DDSCAPS2_POSITIVEZ   = $00004000;
  DDSCAPS2_NEGATIVEZ   = $00008000;
  DDSCAPS2_VOLUME      = $00200000;

  { Flags for TDDSurfaceDesc2.Flags used when saving DDS file.}
  DDS_SAVE_FLAGS = DDSD_CAPS or DDSD_PIXELFORMAT or DDSD_WIDTH or DDSD_HEIGHT or DDSD_LINEARSIZE;

type
  { Stores the pixel format information.}
  TDDPixelFormat = {packed} record
    Size: LongWord;       // Size of the structure = 32 bytes
    Flags: LongWord;      // Flags to indicate valid fields
    FourCC: LongWord;     // Four-char code for compressed textures (DXT)
    BitCount: LongWord;   // Bits per pixel if uncomp. usually 16,24 or 32
    RedMask: LongWord;    // Bit mask for the Red component
    GreenMask: LongWord;  // Bit mask for the Green component
    BlueMask: LongWord;   // Bit mask for the Blue component
    AlphaMask: LongWord;  // Bit mask for the Alpha component
  end;

  { Specifies capabilities of surface.}
  TDDSCaps = {packed} record
    Caps1: LongWord;      // Should always include DDSCAPS_TEXTURE
    Caps2: LongWord;      // For cubic environment maps
    Reserved: array[0..1] of LongWord; // Reserved
  end;

  { Record describing DDS file contents.}
  TDDSurfaceDesc2 = {packed} record
    {+0x $+0x}Size: LongWord;       // Size of the structure = 124 Bytes
    {+0x $+0x}Flags: LongWord;      // Flags to indicate valid fields
    {+0x $+0x}Height: LongWord;     // Height of the main image in pixels
    {+0x $+0x}Width: LongWord;      // Width of the main image in pixels
    {+0x $+0x}PitchOrLinearSize: LongWord; // For uncomp formats number of bytes per
                            // scanline. For comp it is the size in
                            // bytes of the main image
    {+0x $+0x}Depth: LongWord;      // Only for volume text depth of the volume
    {+0x $+0x}MipMaps: LongInt;     // Total number of levels in the mipmap chain
    {+0x $+0x}Reserved1: array[0..10] of LongWord; // Reserved
    {+0x $+0x}PixelFormat: TDDPixelFormat; // Format of the pixel data
    {+0x $+0x}Caps: TDDSCaps;       // Capabilities
    {+0x $+0x}Reserved2: LongWord;  // Reserved
  end;

  { DDS file header.}
  TDDSFileHeader = {packed} record
    Magic: LongWord;       // File format magic
    Desc: TDDSurfaceDesc2; // Surface description
  end;

procedure MakeImage(sPixelFormat: string; wWidth, wHeight: WORD; TextureType, Levels: byte; Depth: byte; Stream: TStream; Pos: Integer; path: string);

implementation

uses Math;

procedure WriteDDSHeaderDXT5A(var Stream: TMemoryStream; dwWidth, dwHeight: integer);
var
  x1:integer;
  x2:char;
  i:integer;
begin
  x1:=$20534444; Stream.Write(x1, 4);   //*.dss file signature: 0x20534444 ("DDS")
  x1:=$0000007C; Stream.Write(x1, 4);   //unknown (or not-necessary): 0x0000007С
  x1:=$000A1007; Stream.Write(x1, 4);   //unknown (or not-necessary): 0x00001007
  Stream.Write(dwHeight, 4);   //Heigth
  Stream.Write(dwWidth, 4);    //Width
  x1:=$00000800; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
  x1:=$00000001; Stream.Write(x1, 4);
  x1:=0; for I := 1 to 44 do Stream.Write(x1, 1);
  x1:=$00000020; Stream.Write(x1, 4);
  x1:=$00000004; Stream.Write(x1, 4);
  x1:=$31495441; Stream.Write(x1, 4);
  x1:=0; for I := 1 to 20 do Stream.Write(x1, 1);
  x1:=$00401008; Stream.Write(x1, 4);
  x1:=0; for I := 1 to 16 do Stream.Write(x1, 1);
end;

procedure WriteDDSHeader8(var Stream: TMemoryStream; dwWidth, dwHeight: integer);
var
  x1:integer;
  x2:char;
  i:integer;
begin
  x1:=$20534444; Stream.Write(x1, 4);   //*.dss file signature: 0x20534444 ("DDS")
  x1:=$0000007C; Stream.Write(x1, 4);   //unknown (or not-necessary): 0x0000007С
  x1:=$00081007; Stream.Write(x1, 4);   //unknown (or not-necessary): 0x00001007
  Stream.Write(dwHeight, 4);   //Heigth
  Stream.Write(dwWidth, 4);    //Width
  x1:=$00010000; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
  x1:=0; for I := 1 to 44 do Stream.Write(x1, 1);
  x1:=$00000020; Stream.Write(x1, 4);
  x1:=$00020000; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
  x1:=$00000008; Stream.Write(x1, 4);
  x1:=$000000FF; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
  x1:=$00001000; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
  x1:=$00000000; Stream.Write(x1, 4);
end;

procedure MakeImage(sPixelFormat: string; wWidth, wHeight: WORD; TextureType, Levels: byte; Depth: byte; Stream: TStream; Pos: Integer; path: string);
type
  TPixelFormat = (DXT1 = $31545844, DXT3 = $33545844, DXT5 = $35545844, A8R8G8B8 = $15, L8 = $32, Index8 = $00);
  function GetTextureDataSize(width,height: Integer; PixelFormat: TPixelFormat; Levels: Integer = 1): Integer;
  var
    I: Integer;
    Size: Integer;
  begin
    case PixelFormat of
      DXT1:           Size := (width*height) div 2;
      DXT3, DXT5, L8: Size := width*height;
      A8R8G8B8:       Size := width*height*4;
    end;
    Result := 0;
    for I:=0 to Levels-1 do
    begin
      Inc(Result, Size shr I);
    end;
  end;
var
  tmpStream: TMemoryStream;
  DdsHeader: TDDSFileHeader;
  j: Integer;
  Image: TSingleImage;
  dwPixelFormat: TPixelFormat;
  dwSize: integer;
begin
  if sPixelFormat='GPUTEXTUREFORMAT_DXT1' then dwPixelFormat:=DXT1;
  if sPixelFormat='GPUTEXTUREFORMAT_DXT2_3' then dwPixelFormat:=DXT3;
  if sPixelFormat='GPUTEXTUREFORMAT_DXT4_5' then dwPixelFormat:=DXT5;
  if sPixelFormat='GPUTEXTUREFORMAT_DXT5A' then dwPixelFormat:=DXT5;
  if sPixelFormat='GPUTEXTUREFORMAT_8_8_8_8' then dwPixelFormat:=A8R8G8B8;
  if sPixelFormat='GPUTEXTUREFORMAT_8' then dwPixelFormat:=L8;
  if (sPixelFormat='GPUTEXTUREFORMAT_DXT5A') or (sPixelFormat='GPUTEXTUREFORMAT_8') then
    begin
      if (sPixelFormat='GPUTEXTUREFORMAT_DXT5A') then
        begin
          dwPixelFormat:=DXT5;
          if Stream.Position<>Pos then Stream.Seek(Pos, soFromBeginning);
          tmpStream:=TMemoryStream.Create;
          WriteDDSHeaderDXT5A(tmpStream, wWidth, wHeight);
          dwSize:=wWidth*wHeight div 2;
          tmpStream.CopyFrom(Stream, dwSize);
          tmpStream.SaveToFile(path);
          tmpStream.Free;
        end;
      if (sPixelFormat='GPUTEXTUREFORMAT_8') then
        begin
          dwPixelFormat:=L8;
          if Stream.Position<>Pos then Stream.Seek(Pos, soFromBeginning);
          tmpStream:=TMemoryStream.Create;
          WriteDDSHeader8(tmpStream, wWidth, wHeight);
          dwSize:=wWidth*wHeight;
          tmpStream.CopyFrom(Stream, dwSize);
          tmpStream.SaveToFile(path);
          tmpStream.Free;
        end;
    end
  else
  begin
  if Stream.Position<>Pos then
  begin
    Stream.Seek(Pos, soFromBeginning);
  end;
  tmpStream:=TMemoryStream.Create;
  try
    DdsHeader.Magic:=DDSMagic;
    with DDSHeader do
    begin
      Desc.Size:=SizeOf(DdsHeader.Desc);
      Desc.Flags:=DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or {DDSD_PITCH} DDSD_PIXELFORMAT or IfThen(Levels>1, DDSD_MIPMAPCOUNT) or IfThen((dwPixelFormat=A8R8G8B8) or (dwPixelFormat=L8), DDSD_LINEARSIZE) or IfThen(TextureType=3, DDSD_DEPTH);
      Desc.Height:=wHeight;
      Desc.Width:=wWidth;
      {TODO: нормальное чтение lights_occluders.wtd}
      case dwPixelFormat of
        DXT1, DXT3, DXT5: Desc.PitchOrLinearSize:=0;
        A8R8G8B8: Desc.PitchOrLinearSize:=wWidth*wHeight*4;
        L8:
          begin
            Desc.PitchOrLinearSize:=wWidth*wHeight*1;
          end;
      end;
      //Desc.PitchOrLinearSize:=IfThen((dwPixelFormat = A8R8G8B8) or (dwPixelFormat=L8),m_wStride);
      Desc.Depth:=Depth;
      Desc.MipMaps:=IfThen(Levels>1, Levels);
      for j:=0 to 10 do Desc.Reserved1[j]:=0;
      Desc.PixelFormat.Size:=SizeOf(Desc.PixelFormat);
      Desc.PixelFormat.Flags:=DDPF_ALPHAPIXELS or IfThen(not (dwPixelFormat in [A8R8G8B8,L8]), DDPF_FOURCC) or IfThen(dwPixelFormat=A8R8G8B8,DDPF_RGB);
      Desc.PixelFormat.FourCC:=IfThen(not ((dwPixelFormat=A8R8G8B8) or (dwPixelFormat=L8)), LongWord(dwPixelFormat));
      if dwPixelFormat=A8R8G8B8 then
        begin
          Desc.PixelFormat.BitCount  := 32;
          Desc.PixelFormat.RedMask   := $00FF0000;
          Desc.PixelFormat.GreenMask := $0000FF00;
          Desc.PixelFormat.BlueMask  := $000000FF;
          Desc.PixelFormat.AlphaMask := $FF000000;
        end
      else
        begin
          if dwPixelFormat=L8 then Desc.PixelFormat.BitCount := 8 else Desc.PixelFormat.BitCount := 0;
          Desc.PixelFormat.BitCount  := 0;
          Desc.PixelFormat.RedMask   := 0;
          Desc.PixelFormat.GreenMask := 0;
          Desc.PixelFormat.BlueMask  := 0;
          Desc.PixelFormat.AlphaMask := 0;
        end;
      Desc.Caps.Caps1:=DDSCAPS_TEXTURE or IfThen((Levels>1)or(TextureType in [1,3]), DDSCAPS_COMPLEX) or IfThen((Levels>1), DDSCAPS_MIPMAP);
      Desc.Caps.Caps2:=IfThen(TextureType=3,DDSCAPS2_VOLUME);
      Desc.Caps.Reserved[0]:=0;
      Desc.Caps.Reserved[1]:=0;
      Desc.Reserved2:=0;
      tmpStream.Write(DdsHeader,SizeOf(DdsHeader));
      tmpStream.CopyFrom(Stream,GetTextureDataSize(wWidth,wHeight,dwPixelFormat,Levels));
    end;
    tmpStream.Seek(0,soFromBeginning);
//    tmpStream.SaveToFile(path);
    Image:=TSingleImage.CreateFromStream(tmpStream);
    //Image.Format:=ifA8R8G8B8;
    Image.SaveToFile(path);
    Image.Free;
  finally
    tmpStream.Free;
  end;
  end;
end;

procedure WriteDDSHeader(var Stream: TFileStream; dwWidth, dwHeight, dwTextureType: Integer);
var
  x1:integer;
  x2:char;
  I:integer;
begin
  x1:=$20534444; Stream.Write(x1, 4);   //*.dss file signature: 0x20534444 ("DDS")
  x1:=$0000007C; Stream.Write(x1, 4);   //unknown (or not-necessary): 0x0000007С
  x1:=$00001007; Stream.Write(x1, 4);   //unknown (or not-necessary): 0x00001007
  Stream.Write(dwHeight, 4);   //Heigth
  Stream.Write(dwWidth, 4);    //Width
  x1:=0; for I := 1 to 56 do Stream.Write(x1, 1);   //56 bytes, zeros
  x1:=$00000020; Stream.Write(x1, 4);   //unknown (or not-necessary): 0x00000020
  x1:=$00000004; Stream.Write(x1, 4);   //unknown (or not-necessary): 0x00000004
  if dwTextureType<>0 then    //if texture has DXT-compression
    begin
      x2:='D'; Stream.Write(x2,1);    //0x44
      x2:='X'; Stream.Write(x2,1);    //0x58
      x2:='T'; Stream.Write(x2,1);    //0x54
      x2:=IntToStr(dwTextureType)[1]; Stream.Write(x2,1);   //DXT-value
      x1:=0; for I := 1 to 20 do Stream.Write(x1, 1);   //20 bytes, zeros
      x1:=$00001000; Stream.Write(x1, 4);   //unknown (or not-necessary):  0x00001000
      x1:=0; for I := 1 to 16 do Stream.Write(x1, 1);
    end
    else
    begin

    end;
end;

end.
