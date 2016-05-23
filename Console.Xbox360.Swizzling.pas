unit Console.Xbox360.Swizzling;

interface

uses SysUtils, Classes, Windows;

function XGAddress2DTiledOffset(x, y, w, texelPitch: DWORD): DWORD;

implementation

function XGAddress2DTiledOffset(x, y, w, texelPitch: DWORD): DWORD;
var
  alignedWidth, logBpp, Macro, Micro, Offset: DWORD;
begin
  alignedWidth := (w + 31) and not 31;
  logBpp := (TexelPitch shr 2) + ((TexelPitch shr 1) shr (TexelPitch shr 2));
  Macro := ((x shr 5) + (y shr 5) * (alignedWidth shr 5)) shl (logBpp + 7);
  Micro := (((x and 7) + ((y and 6) shl 2)) shl LogBpp);
  Offset := Macro + ((Micro and not 15) shl 1) + (Micro and 15) + ((y and 8) shl (3 + logBpp)) + ((y and 1) shl 4);
  Result:= (((Offset and not 511) shl 3) + ((Offset and 448) shl 2) + (Offset and 63) + ((y and 16) shl 7) + (((((y and 8) shr 2) + (x shr 3)) and 3) shl 6)) shr logBpp;
end;

end.
