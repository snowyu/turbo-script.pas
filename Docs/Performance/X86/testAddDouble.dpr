program testAddDouble;

{$APPTYPE CONSOLE}

uses
  Windows,  SysUtils
  ;

var
  tBegin, tEnd: Int64;
  a, b: Double;

function add(a,b: integer): integer;
asm
  mov EAX, a
  add EAX, b
end;

var
  p: pointer;
begin
  tBegin := $EFEF111122223333;
  p := @tBegin;
  tBegin := 0;
  QueryPerformanceCounter(tBegin);
  a := 300.00001;
  b := 300.00001;
  asm 
    FLD a   //push a to st(0)
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FADD b
    FSTP a
  end;//}
  QueryPerformanceCounter(tEnd);
  writeln('Count:', a:8:6);
  writeln('time:', tEnd- tBegin);
end.
