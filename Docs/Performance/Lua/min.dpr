program min;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Classes,
  lua;

var
  tB, tE: Int64;

function lua_startCount(L: Plua_State): Integer; cdecl;
begin
  QueryPerformanceCounter(tB);
  Result := 0;
end;

function lua_stopCount(L: Plua_State): Integer; cdecl;
begin
  QueryPerformanceCounter(tE);
  Result := 0;
end;

function lua_ShowCount(L: Plua_State): Integer; cdecl;
begin
  Writeln('Count:', tE-tB);
  Result := 0;
end;

//function to print lua data via delphi
function lua_print(L: Plua_State): Integer; cdecl;
var
  i, n: Integer;
begin
  write('Lua: ');
  n := lua_gettop(L);
  for i := 1 to n do
  begin
    if i > 1 then
      Write(#9);
    if lua_isstring(L, i) then
      Write(lua_tostring(L, i))
    else
      Write(Format('%s:%p', [lua_type(L, i), lua_topointer(L, i)]));
  end;
  WriteLn;
  Result := 0;
end;

//this is the class we want to use from lua
type
  TNumArray = class
    size: integer;
    values: array of single;
  end;

//makes a new delphi array (class) structure from lua
function lua_newarray(L: Plua_State): Integer; cdecl;
var
  n: integer;
  nbytes: integer;
  a: TNumArray;
begin
  n:=round(lua_tonumber(L,1));
  writeln('DEBUG: '+inttostr(n));
  nbytes:=sizeof(TNumArray) + (n-1) * sizeof(single);
  a:=TNumArray(lua_newuserdata(L, nbytes));
  SetLength(a.values, n);
  a.size:=n;
  result:=1;
end;

//stores a value in the array
function lua_setarray(L: Plua_State): Integer; cdecl;
var
  a: TNumArray;
  index: integer;
  value: single;
begin
  a:=TNumArray(lua_touserdata(L, 1));
  index:= Round(lua_tonumber(L, 2));
  value:= lua_tonumber(L, 3);
  //do some error checking here....
  a.values[index-1]:=value;
  result:=0;
end;

//returns a value from the array
function lua_getarray(L: Plua_State): Integer; cdecl;
var
  a: TNumArray;
  index: integer;
begin
  a:=TNumArray(lua_touserdata(L, 1));
  index:= Round(lua_tonumber(L, 2));
  // do some error checking here....
  lua_pushnumber(L, a.values[index-1]);
  result:=1;
end;

//retuns the size of the array to lua
function lua_getsize(L: Plua_State): Integer; cdecl;
var
  a: TNumArray;
begin
  a:=TNumArray(lua_touserdata(L, 1));
  lua_pushnumber(L, a.size);
  result:=1;
end;

//structure for delphi array (class) to lua
const
  arraylib: array [0..4] of lual_reg = (
   (name:'new';func:lua_newarray),
   (name:'set';func:lua_setarray),
   (name:'get';func:lua_getarray),
   (name:'size';func:lua_getsize),
   (name:nil;func:nil)
   );

//bring delphi array (class) to lua
function luaopen_array(L: Plua_state): boolean;
begin
  luaL_openlib(L, 'array', @arraylib[0], 0);
  result:=true;
end;

var
  L: Plua_State = nil; //lua state
  script: tstringlist; //a stringlist to hold the lua script
  result: integer;     //0 if script executes ok

begin
  if ParamCount <= 0 then
  begin
    WriteLn('Usage: min.exe filename');
    Exit;
  end;

  //init lua dll
  LoadLua;
  LoadLuaLib;
  L := lua_open;
 try
 //register pseudo delphi array (class) in lua
  luaopen_array(L);

  //Register a delphi procedure/funtion for use in Lua
  lua_register(L, 'print', lua_print);
  lua_register(L, 'startCount', lua_startCount);
  lua_register(L, 'stopCount', lua_stopCount);
  lua_register(L, 'showCount', lua_ShowCount);

  //Load a lua script from a buffer
  script:=tstringList.Create;
  try
    try
     script.LoadFromFile(PChar(ParamStr(1)));
    except
      writeln('Can not load Script File:'+ParamStr(1));
      exit;
    end;
    lual_loadbuffer(L, script.gettext, length(script.gettext), 'myluascript');
  finally
    Script.Free; //clean up
  end;

  //Ask Lua to run our little script
  result := 0;
  result := lua_pcall(l, 0, LUA_MULTRET, 0);
  if result>0 then
  begin
    writeln('bad, bad script'); //should provide more usefull info
    lua_error(L);
  end
  else
    writeln('done with script');

 finally
  //close lua dll
  lua_close(L);
  UnLoadLua;
  UnLoadLuaLib;
 end;
end.
