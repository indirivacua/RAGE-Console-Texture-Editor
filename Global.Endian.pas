unit Global.Endian;

// Endian swap

interface

function EndianChangeDWORD(i:longword):longword;
function EndianChangeWORD(i:longword):longword;

implementation

function EndianChangeDWORD(i:longword):longword;
var
  j:integer;
  b1,b2,b3,b4:byte;
begin
  j:=i;
  asm
    mov eax,j
    mov b4,al
    mov b3,ah
    shr eax,10h
    mov b2,al
    mov b1,ah
    mov al,b3
    mov ah,b4
    shl eax,10h
    mov ah,b2
    mov al,b1
    mov j,eax
  end;
  Result:=j;
end;

function EndianChangeWORD(i:longword):longword;
var
  j:integer;
  b1,b2:byte;
begin
  j:=i;
  asm
    mov eax,j
    mov b2,al
    mov b1,ah
    mov al,b1
    mov ah,b2
    mov j,eax
  end;
  Result:=j;
end;

end.
