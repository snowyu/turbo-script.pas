program mLua;

{$APPTYPE CONSOLE}

{.$DEFINE LUA_5_1}

uses
  Windows,  SysUtils,  Classes
  {$IFDEF LUA_5_1}
  , LuaPas
  {$ELSE}
  , lua
  {$ENDIF}
  ;

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
 {$IFNDEF LUA_5_1}
  LoadLua;
  LoadLuaLib;
 {$ENDIF}
  L := lua_open;
 try
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
 {$IFNDEF LUA_5_1}
  UnLoadLua;
  UnLoadLuaLib;
 {$ENDIF}
 end;
end.
