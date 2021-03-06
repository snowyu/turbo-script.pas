unit LuaUtils;

interface

uses
  SysUtils, Classes, ComCtrls, LuaPas;

type
  TOnLuaStdout = procedure (S: PChar; N: Integer);
  ELuaException = class(Exception)
    fTitle: string;
    fLine: Integer;
    fMsg: string;
    constructor Create(Title: string; Line: Integer; Msg: string);
  end;

  {$ifdef FPC}

  {$else}

  {$endif}
  
procedure CheckArg(L: Plua_State; N: Integer);

function lua_print(L: Plua_State): Integer; cdecl;
function lua_io_write(L: Plua_State): Integer; cdecl;

function LuaToPointer(L: Plua_State; Index: Integer): Pointer;
procedure LuaPushPointer(L: Plua_State; P:Pointer);
function LuaToBoolean(L: Plua_State; Index: Integer): Boolean;
procedure LuaPushBoolean(L: Plua_State; B: Boolean);
function LuaToInteger(L: Plua_State; Index: Integer): Integer;
procedure LuaPushInteger(L: Plua_State; N: Integer);
function LuaToString(L: Plua_State; Index: Integer): string;
procedure LuaPushString(L: Plua_State; const S: string);
function LuaIncIndex(L: Plua_State; Index: Integer): Integer;
function LuaAbsIndex(L: Plua_State; Index: Integer): Integer;
procedure LuaGetTable(L: Plua_State; TableIndex: Integer; const Key: string);
function LuaGetTableBoolean(L: Plua_State; TableIndex: Integer; const Key: string): Boolean;
function LuaGetTableNumber(L: Plua_State; TableIndex: Integer; const Key: string): Double;
function LuaGetTableString(L: Plua_State; TableIndex: Integer; const Key: string): string;
function LuaGetTableFunction(L: Plua_State; TableIndex: Integer; const Key: string): lua_CFunction;
function LuaGetTableLightUserData(L: Plua_State; TableIndex: Integer; const Key: string): Pointer;
procedure LuaRawGetTable(L: Plua_State; TableIndex: Integer; const Key: string);
function LuaRawGetTableBoolean(L: Plua_State; TableIndex: Integer; const Key: string): Boolean;
function LuaRawGetTableNumber(L: Plua_State; TableIndex: Integer; const Key: string): Double;
function LuaRawGetTableString(L: Plua_State; TableIndex: Integer; const Key: string): string;
function LuaRawGetTableFunction(L: Plua_State; TableIndex: Integer; const Key: string): lua_CFunction;
function LuaRawGetTableLightUserData(L: Plua_State; TableIndex: Integer; const Key: string): Pointer;
procedure LuaSetTableValue(L: Plua_State; TableIndex: Integer; const Key: string; ValueIndex: Integer);
procedure LuaSetTableNil(L: Plua_State; TableIndex: Integer; const Key: string);
procedure LuaSetTableBoolean(L: Plua_State; TableIndex: Integer; const Key: string; B: Boolean);
procedure LuaSetTableNumber(L: Plua_State; TableIndex: Integer; const Key: string; N: Double);
procedure LuaSetTableString(L: Plua_State; TableIndex: Integer; const Key: string; S: string);
procedure LuaSetTableFunction(L: Plua_State; TableIndex: Integer; const Key: string; F: lua_CFunction);
procedure LuaSetTableLightUserData(L: Plua_State; TableIndex: Integer; const Key: string; P: Pointer);
procedure LuaSetTableClear(L: Plua_State; TableIndex: Integer);
procedure LuaRawSetTableValue(L: Plua_State; TableIndex: Integer; const Key: string; ValueIndex: Integer);
procedure LuaRawSetTableNil(L: Plua_State; TableIndex: Integer; const Key: string);
procedure LuaRawSetTableBoolean(L: Plua_State; TableIndex: Integer; const Key: string; B: Boolean);
procedure LuaRawSetTableNumber(L: Plua_State; TableIndex: Integer; const Key: string; N: Double);
procedure LuaRawSetTableString(L: Plua_State; TableIndex: Integer; const Key: string; S: string);
procedure LuaRawSetTableFunction(L: Plua_State; TableIndex: Integer; const Key: string; F: lua_CFunction);
procedure LuaRawSetTableLightUserData(L: Plua_State; TableIndex: Integer; const Key: string; P: Pointer);
procedure LuaRawSetTableClear(L: Plua_State; TableIndex: Integer);
function LuaGetMetaFunction(L: Plua_State; Index: Integer; Key: string): lua_CFunction;
procedure LuaSetMetaFunction(L: Plua_State; Index: Integer; Key: string; F: lua_CFunction);

procedure LuaShowStack(L: Plua_State; Caption: string = '');
function LuaStackToStr(L: Plua_State; Index: Integer; MaxTable: Integer = -1): string;
procedure LuaRegisterCustom(L: Plua_State; TableIndex: Integer; const Name: PChar; F: lua_CFunction);
procedure LuaRegister(L: Plua_State; const Name: PChar; F: lua_CFunction);
procedure LuaRegisterMetatable(L: Plua_State; const Name: PChar; F: lua_CFunction);
procedure LuaRegisterProperty(L: Plua_State; const Name: PChar; ReadFunc, WriteFunc: lua_CFunction);
procedure LuaStackToStrings(L: Plua_State; Lines: TStrings; MaxTable: Integer = -1);
procedure LuaLocalToStrings(L: Plua_State; Lines: TStrings; MaxTable: Integer = -1);
procedure LuaTableToStrings(L: Plua_State; Index: Integer; Lines: TStrings; MaxTable: Integer = -1);
procedure LuaTableToTreeView(L: Plua_State; Index: Integer; TV: TTreeView; MaxTable: Integer = -1);
function LuaGetIdentValue(L: Plua_State; Ident: string; MaxTable: Integer = -1): string;
procedure LuaSetIdentValue(L: Plua_State; Ident, Value: string; MaxTable: Integer = -1);
procedure LuaLoadBuffer(L: Plua_State; const Code: string; const Name: string);
procedure LuaPCall(L: Plua_State; NArgs, NResults, ErrFunc: Integer);
procedure LuaError(L: Plua_State; const Msg: string);
procedure LuaErrorFmt(L: Plua_State; const Fmt: string; const Args: array of Const);
function LuaDataStrToStrings(const TableStr: string; Strings: TStrings): string;
function LuaDoFile(L: Plua_State): Integer; cdecl;

const
  LuaGlobalVariableStr = '{?O???[?o??????}';
var
  OnLuaStdout: TOnLuaStdout;
  DefaultMaxTable: Integer;

implementation

uses
  Dialogs;

const
  QuoteStr = '"';
  CR = #$0D;
  LF = #$0A;
  CRLF = CR + LF;

function Quote(const Str: string): string;
begin
  Result := AnsiQuotedStr(Str, QuoteStr);
end;

function Dequote(const QuotedStr: string): string;
  {$ifdef FPC}
  Var
  PQS: PChar;
  {$endif}
begin
  {$ifdef FPC}
  PQS := getmem(length(QuotedStr));
  strpcopy(PQS,QuotedStr);
  Result := AnsiExtractQuotedStr(PQS, QuoteStr);
  FreeMem(PQS);
  {$else}
  Result := AnsiDequotedStr(QuotedStr, QuoteStr);
  {$endif}
end;

function fwrite(S: PChar; Un, Len: Integer; Dummy: Integer): Integer;
// ?W???o??
var
  Size: Integer;
begin
  Size := Un * Len;
  if (Assigned(OnLuaStdout)) then
    OnLuaStdout(S, Size);
  Result := Size;
end;

function fputs(const S: string; Dummy: Integer): Integer;
// ?W???o??
{$ifdef FPC}
  Var
  PS: PChar;
  {$endif}
begin
  {$ifdef FPC}
  PS := getmem(length(S));
  strpcopy(PS,S);
  Result := fwrite(PS, SizeOf(Char), Length(S), Dummy);
  FreeMem(PS);
  {$else}
  Result := fwrite(PChar(S), SizeOf(Char), Length(S), Dummy);
  {$endif}
end;

function lua_print(L: Plua_State): Integer; cdecl;
// ?W???o??????
const
  TAB = #$08;
  NL = #$0A;
  stdout = 0;
var
  N, I: Integer;
  S: PChar;
begin
  N := lua_gettop(L);  (* number of arguments *)
  lua_getglobal(L, 'tostring');
  for I := 1 to N do
  begin
    lua_pushvalue(L, -1);  (* function to be called *)
    lua_pushvalue(L, i);   (* value to print *)
    lua_call(L, 1, 1);
    S := lua_tostring(L, -1);  (* get result *)
    if (S = nil) then
    begin
      Result := luaL_error(L, '`tostring'' must return a string to `print''');
      Exit;
    end;
    if (I > 1) then fputs(TAB, stdout);
    fputs(S, stdout);
    lua_pop(L, 1);  (* pop result *)
  end;
  fputs(NL, stdout);
  Result := 0;
end;

function lua_io_write(L: Plua_State): Integer; cdecl;
  function pushresult(L: Plua_State; I: Boolean; FileName: PChar): Integer;
  begin
    lua_pushboolean(L, true);
    Result := 1;
  end;
const
  F = 0;
var
  NArgs: Integer;
  Status: Boolean;
  Arg: Integer;
  Len: Psize_t;
  S: PChar;
begin
  Arg := 1;
  NArgs := lua_gettop(L);
  Status := True;
  while (NArgs > 0) do
  begin
    Dec(NArgs);
    if (lua_type(L, Arg) = LUA_TNUMBER) then
    begin
      (* optimization: could be done exactly as for strings *)
      Status := Status and
          (fputs(Format(LUA_NUMBER_FMT, [lua_tonumber(L, Arg)]), 0) > 0);
    end else
    begin
      S := luaL_checklstring(L, Arg, Len);
      // S := luaL_checklstring(L, Arg, Len);
      Status := Status and (fwrite(S, SizeOf(Char), Integer(Len), F) = Integer(Len));
    end;
    Inc(Arg);
  end;
  Result := pushresult(L, Status, nil);
end;

function LuaToPointer(L: Plua_State; Index: Integer): Pointer;
begin
  Result := lua_touserdata(L, Index);
end;

procedure LuaPushPointer(L: Plua_State; P: Pointer);
begin
  lua_pushlightuserdata(L,P);
end;

function LuaToBoolean(L: Plua_State; Index: Integer): Boolean;
begin
  Result := lua_toboolean(L, Index);
end;

procedure LuaPushBoolean(L: Plua_State; B: Boolean);
begin
  lua_pushboolean(L, B);
end;

function LuaToInteger(L: Plua_State; Index: Integer): Integer;
begin
  Result := Round(lua_tonumber(L, Index));
end;

procedure LuaPushInteger(L: Plua_State; N: Integer);
begin
  lua_pushnumber(L, N);
end;

function LuaToString(L: Plua_State; Index: Integer): string;
var
  Size: Integer;
begin
  Size := lua_strlen(L, Index);
  SetLength(Result, Size);
  if (Size > 0) then
    Move(lua_tostring(L, Index)^, Result[1], Size);
end;

procedure LuaPushString(L: Plua_State; const S: string);
{$ifdef FPC}
  Var
  PS: PChar;
  {$endif}
begin
{$ifdef FPC}
  PS := getmem(length(S));
  strpcopy(PS,S);
  lua_pushstring(L, PS);
  FreeMem(PS);
  {$else}
  lua_pushstring(L, PChar(S));
  {$endif}
end;

function LuaIncIndex(L: Plua_State; Index: Integer): Integer;
// ?????C???f?b?N?X -1 ?` -N ??????
begin
  if ((Index = LUA_GLOBALSINDEX) or (Index = LUA_REGISTRYINDEX)) then
  begin
    Result := Index;
    Exit;
  end;

  Result := LuaAbsIndex(L, Index) - lua_gettop(L) - 1;
end;

function LuaAbsIndex(L: Plua_State; Index: Integer): Integer;
// ?????C???f?b?N?X 1 ?` N ??????
begin
  if ((Index = LUA_GLOBALSINDEX) or (Index = LUA_REGISTRYINDEX)) then
  begin
    Result := Index;
    Exit;
  end;

  if (Index < 0) then
    Result := Index + lua_gettop(L) + 1
  else
    Result := Index;
end;

procedure LuaPushKeyString(L: Plua_State; var Index: Integer; const Key: string);
begin
  Index := LuaAbsIndex(L, Index);
  LuaPushString(L,Key);
end;

procedure LuaGetTable(L: Plua_State; TableIndex: Integer; const Key: string);
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_gettable(L, TableIndex);
end;

function LuaGetTableBoolean(L: Plua_State; TableIndex: Integer; const Key: string): Boolean;
begin
  LuaGetTable(L, TableIndex, Key);
  Result := lua_toboolean(L, -1);
  lua_pop(L, 1);
end;

function LuaGetTableNumber(L: Plua_State; TableIndex: Integer; const Key: string): Double;
begin
  LuaGetTable(L, TableIndex, Key);
  Result := lua_tonumber(L, -1);
  lua_pop(L, 1);
end;

function LuaGetTableString(L: Plua_State; TableIndex: Integer; const Key: string): string;
begin
  LuaGetTable(L, TableIndex, Key);
  Result := lua_tostring(L, -1);
  lua_pop(L, 1);
end;

function LuaGetTableFunction(L: Plua_State; TableIndex: Integer; const Key: string): lua_CFunction;
begin
  LuaGetTable(L, TableIndex, Key);
  Result := lua_tocfunction(L, -1);
  lua_pop(L, 1);
end;

function LuaGetTableLightUserData(L: Plua_State; TableIndex: Integer; const Key: string): Pointer;
begin
  LuaGetTable(L, TableIndex, Key);
  Result := lua_touserdata(L, -1);
  lua_pop(L, 1);
end;

procedure LuaRawGetTable(L: Plua_State; TableIndex: Integer; const Key: string);
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_rawget(L, TableIndex);
end;

function LuaRawGetTableBoolean(L: Plua_State; TableIndex: Integer; const Key: string): Boolean;
begin
  LuaRawGetTable(L, TableIndex, Key);
  Result := lua_toboolean(L, -1);
  lua_pop(L, 1);
end;

function LuaRawGetTableNumber(L: Plua_State; TableIndex: Integer; const Key: string): Double;
begin
  LuaRawGetTable(L, TableIndex, Key);
  Result := lua_tonumber(L, -1);
  lua_pop(L, 1);
end;

function LuaRawGetTableString(L: Plua_State; TableIndex: Integer; const Key: string): string;
begin
  LuaRawGetTable(L, TableIndex, Key);
  Result := lua_tostring(L, -1);
  lua_pop(L, 1);
end;

function LuaRawGetTableFunction(L: Plua_State; TableIndex: Integer; const Key: string): lua_CFunction;
begin
  LuaRawGetTable(L, TableIndex, Key);
  Result := lua_tocfunction(L, -1);
  lua_pop(L, 1);
end;

function LuaRawGetTableLightUserData(L: Plua_State; TableIndex: Integer; const Key: string): Pointer;
begin
  LuaRawGetTable(L, TableIndex, Key);
  Result := lua_touserdata(L, -1);
  lua_pop(L, 1);
end;

procedure LuaSetTableValue(L: Plua_State; TableIndex: Integer; const Key: string; ValueIndex: Integer);
begin
  TableIndex := LuaAbsIndex(L, TableIndex);
  ValueIndex := LuaAbsIndex(L, ValueIndex);
  LuaPushString(L, Key);
  lua_pushvalue(L, ValueIndex);
  lua_settable(L, TableIndex);
end;

procedure LuaSetTableNil(L: Plua_State; TableIndex: Integer; const Key: string);
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_pushnil(L);
  lua_settable(L, TableIndex);
end;

procedure LuaSetTableBoolean(L: Plua_State; TableIndex: Integer; const Key: string; B: Boolean);
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_pushboolean(L, B);
  lua_settable(L, TableIndex);
end;

procedure LuaSetTableNumber(L: Plua_State; TableIndex: Integer; const Key: string; N: Double); 
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_pushnumber(L, N);
  lua_settable(L, TableIndex);
end;

procedure LuaSetTableString(L: Plua_State; TableIndex: Integer; const Key: string; S: string);
begin
  LuaPushKeyString(L, TableIndex, Key);
  LuaPushString(L, S);
  lua_settable(L, TableIndex);
end;

procedure LuaSetTableFunction(L: Plua_State; TableIndex: Integer; const Key: string; F: lua_CFunction);
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_pushcfunction(L, F);
  lua_settable(L, TableIndex);
end;

procedure LuaSetTableLightUserData(L: Plua_State; TableIndex: Integer; const Key: string; P: Pointer);
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_pushlightuserdata(L, P);
  lua_settable(L, TableIndex);
end;

procedure LuaSetTableClear(L: Plua_State; TableIndex: Integer);
begin
  TableIndex := LuaAbsIndex(L, TableIndex);
  lua_pushnil(L);
  while (lua_next(L, TableIndex) <> 0) do
  begin
    lua_pushnil(L);
    lua_replace(L, -1 - 1);
    lua_settable(L, TableIndex);
    lua_pushnil(L);
  end;
end;

procedure LuaRawSetTableValue(L: Plua_State; TableIndex: Integer; const Key: string; ValueIndex: Integer);
begin
  TableIndex := LuaAbsIndex(L, TableIndex);
  ValueIndex := LuaAbsIndex(L, ValueIndex);
  LuaPushString(L, Key);
  lua_pushvalue(L, ValueIndex);
  lua_rawset(L, TableIndex);
end;

procedure LuaRawSetTableNil(L: Plua_State; TableIndex: Integer; const Key: string);
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_pushnil(L);
  lua_rawset(L, TableIndex);
end;

procedure LuaRawSetTableBoolean(L: Plua_State; TableIndex: Integer; const Key: string; B: Boolean); 
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_pushboolean(L, B);
  lua_rawset(L, TableIndex);
end;

procedure LuaRawSetTableNumber(L: Plua_State; TableIndex: Integer; const Key: string; N: Double); 
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_pushnumber(L, N);
  lua_rawset(L, TableIndex);
end;

procedure LuaRawSetTableString(L: Plua_State; TableIndex: Integer; const Key: string; S: string); 
begin
  LuaPushKeyString(L, TableIndex, Key);
  LuaPushString(L, S);
  lua_rawset(L, TableIndex);
end;

procedure LuaRawSetTableFunction(L: Plua_State; TableIndex: Integer; const Key: string; F: lua_CFunction);
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_pushcfunction(L, F);
  lua_rawset(L, TableIndex);
end;

procedure LuaRawSetTableLightUserData(L: Plua_State; TableIndex: Integer; const Key: string; P: Pointer);
begin
  LuaPushKeyString(L, TableIndex, Key);
  lua_pushlightuserdata(L, P);
  lua_rawset(L, TableIndex);
end;

procedure LuaRawSetTableClear(L: Plua_State; TableIndex: Integer);
begin
  TableIndex := LuaAbsIndex(L, TableIndex);

  lua_pushnil(L);
  while (lua_next(L, TableIndex) <> 0) do
  begin
    lua_pushnil(L);
    lua_replace(L, -1 - 1);
    lua_rawset(L, TableIndex);
    lua_pushnil(L);
  end;
end;

function LuaGetMetaFunction(L: Plua_State; Index: Integer; Key: string): lua_CFunction;
begin
  Result := nil;
  Index := LuaAbsIndex(L, Index);
  if not lua_getmetatable(L, Index) then
    Exit;

  LuaGetTable(L, -1, Key);
  if lua_iscfunction(L, -1) then
    Result := lua_tocfunction(L, -1);
  lua_pop(L, 2);
end;

procedure LuaSetMetaFunction(L: Plua_State; Index: Integer; Key: string; F: lua_CFunction);
// Key = __add, __sub, __mul, __div, __pow, __unm, __concat,
//       __eq, __lt, __le, __index, __newindex, __call
begin
  Index := LuaAbsIndex(L, Index);
  if not lua_getmetatable(L, Index) then
    lua_newtable(L);

  LuaRawSetTableFunction(L, -1, Key, F);
  lua_setmetatable(L, Index);
end;

function LuaStackToStr(L: Plua_State; Index: Integer; MaxTable: Integer): string;
//?X?^?b?N?????e??????????????
// nil    : nil
// Number : FloatToStr
// Boolean: True/False
// stirng : "..."
// Table  : { Key1=Value Key2=Value }
  function TableToStr(Index: Integer): string;
  var
    Key, Value: string;
    Count: Integer;
  begin
    Result := '{ ';
    Count := 0;
    lua_pushnil(L);
    while (lua_next(L, Index) <> 0) do
    begin
      Inc(Count);
      if (Count > MaxTable) then
      begin
        Result := Result + '... ';
        lua_pop(L, 2);
        Break;
      end;
      Key := Dequote(LuaStackToStr(L, -2, MaxTable));
      if (Key = '_G') then
        Value := LuaGlobalVariableStr
      else
        Value := LuaStackToStr(L, -1, MaxTable);
      if (lua_type(L, -1) = LUA_TFUNCTION) then
        Result := Result + Format('%s() ', [Key])
      else
        Result := Result + Format('%s=%s ', [Key, Value]);
      // Key ?????????????c??
      lua_pop(L, 1);
    end;
    Result := Result + '}';
  end;
var
  Size: Integer;
begin
  if (MaxTable < 0) then
    MaxTable := DefaultMaxTable;

  Index := LuaAbsIndex(L, Index);

  case (lua_type(L, Index)) of
  LUA_TNIL:
    Result := 'nil';
  LUA_TNUMBER:
    Result := Format('%g', [lua_tonumber(L, Index)]);
  LUA_TBOOLEAN:
  {$ifdef FPC}
    Result := BoolToStr(lua_toboolean(L, Index));
  {$else}
    Result := BoolToStr(lua_toboolean(L, Index), True);
  {$endif}
  LUA_TSTRING:
  begin
    Size := lua_strlen(L, Index);
    SetLength(Result, Size);
    if (Size > 0) then
      Move(lua_tostring(L, Index)^, Result[1], Size);
    Result := Quote(Result);
  end;
  LUA_TTABLE:
    Result := TableToStr(Index);
  LUA_TFUNCTION:
    if lua_iscfunction(L, Index) then
      Result := Format('CFUNC:%p', [Pointer(lua_tocfunction(L, Index))])
    else
      Result := Format('FUNC:%p', [lua_topointer(L, Index)]);
  LUA_TUSERDATA:
    Result := Format('USERDATA:%p', [lua_touserdata(L, Index)]);
  LUA_TTHREAD:
    Result := Format('THREAD:%p', [lua_tothread(L, Index)]);
  LUA_TLIGHTUSERDATA:
    Result := Format('LIGHTUSERDATA:%p', [lua_touserdata(L, Index)]);
  else
    Assert(False);
  end;
end;

procedure LuaShowStack(L: Plua_State; Caption: string);
var
  I, N: Integer;
  S: string;
begin
  N := lua_gettop(L);
  S := '[' + Caption + ']';
  for I := N downto 1 do
  begin
    S := S + CRLF + Format('%3d,%3d:%s', [LuaAbsIndex(L, I), LuaIncIndex(L, I),
      LuaStackToStr(L, I, -1)]);
  end;
  ShowMessage(S);
end;

procedure LuaProcessTableName(L: Plua_State; const Name: PChar;
  var LastName: string; var TableIndex, Count: Integer);
// Name ???e?[?u???v?f???X?^?b?N?????????A
// ?X?^?b?N???????????? Name ?????I?v?f?????O???????e?e?[?u?????C???f?b?N?X??????
// ?e?[?u??????????????????????
// LuaProcessTableName(L, 'print', S, TI, Count) ?? S = print, TI = LUA_GLOBALSINDEX, Count = 0
// LuaProcessTableName(L, 'io.write', S, TI, Count) ?? S = write, TI -> io, Count = 1
// LuaProcessTableName(L, 'a.b.c.func', S, TI, Count) ?? S = func, TI -> a.b.c, Count = 3
  function GetToken(var S: string): string;
  var
    Index: Integer;
  begin
    Index := Pos('.', S);
    if (Index = 0) then
    begin
      Result := S;
      S := '';
      Exit;
    end;
    Result := Copy(S, 1, Index - 1);
    S := Copy(S, Index + 1, Length(S));
  end;
var
  S: string;
begin
  S := Name;
  Count := 0;

  LastName := GetToken(S);
  while (S <> '') do
  begin
    Inc(Count);
    TableIndex := LuaAbsIndex(L, TableIndex);
    LuaGetTable(L, TableIndex, LastName);
    if (lua_type(L, -1) <> LUA_TTABLE) then
    begin
      lua_pop(L, 1);
      LuaPushstring(L, LastName);
      lua_newtable(L);
      lua_rawset(L, TableIndex);
      LuaGetTable(L, TableIndex, LastName);
    end;
    TableIndex := -1;
    LastName := GetToken(S);
  end;
end;

procedure LuaRegisterCustom(L: Plua_State; TableIndex: Integer; const Name: PChar; F: lua_CFunction);
var
  Count: Integer;
  S: string;
begin
  LuaProcessTableName(L, Name, S, TableIndex, Count);
  LuaRawSetTableFunction(L, TableIndex, S, F);
  lua_pop(L, Count);
end;

procedure LuaRegister(L: Plua_State; const Name: PChar; F: lua_CFunction);
// ???????o?^
// LuaRegister(L, 'print', lua_print);
// LuaRegister(L, 'io.write', lua_io_write);  // ?e?[?u?? io ????????????????
// LuaRegister(L, 'a.b.c.func', a_b_c_func);  // ?e?[?u?? a.b.c ????????????????
begin
  LuaRegisterCustom(L, LUA_GLOBALSINDEX, Name, F);
end;

procedure LuaRegisterMetatable(L: Plua_State; const Name: PChar; F: lua_CFunction);
begin
  LuaRegisterCustom(L, LUA_REGISTRYINDEX, Name, F);
end;

procedure LuaRegisterProperty(L: Plua_State; const Name: PChar; ReadFunc, WriteFunc: lua_CFunction);
var
  Count: Integer;
  TI: Integer;
  S: string;
begin
  TI := LUA_GLOBALSINDEX;
  LuaProcessTableName(L, Name, S, TI, Count);
  TI := LuaAbsIndex(L, TI);

  LuaGetTable(L, TI, S);
  if (lua_type(L, -1) <> LUA_TTABLE) then
  begin
    lua_pop(L, 1);
    LuaPushstring(L, S);
    lua_newtable(L);
    lua_settable(L, TI);
    LuaGetTable(L, TI, S);
  end;
  if (Assigned(ReadFunc)) then
    LuaSetMetaFunction(L, -1, '__index', ReadFunc);
  if (Assigned(WriteFunc)) then
    LuaSetMetaFunction(L, -1, '__newindex', WriteFunc);
  lua_pop(L, Count + 1);
end;

procedure LuaStackToStrings(L: Plua_State; Lines: TStrings; MaxTable: Integer);
var
  I: Integer;
begin
  Lines.Clear;
  for I := lua_gettop(L) downto 1 do
    Lines.Add(LuaStackToStr(L, I, MaxTable));
end;

procedure LuaLocalToStrings(L: Plua_State; Lines: TStrings; MaxTable: Integer);
var
  Name: PChar;
  Index: Integer;
  Debug: Plua_Debug;
  AR: Plua_Debug;
begin
  AR := Debug;
  Lines.Clear;
  Index := 1;
  if (lua_getstack(L, 0, AR) = 0) then
    Exit;

  Name := lua_getlocal(L, AR, Index);
  while (Name <> nil) do
  begin
    Lines.Values[Name] := LuaStackToStr(L, -1, MaxTable);
    lua_pop(L, 1);
    Inc(Index);
    Name := lua_getlocal(L, AR, Index);
  end;
end;

procedure LuaTableToStrings(L: Plua_State; Index: Integer; Lines: TStrings; MaxTable: Integer);
var
  Key, Value: string;
begin
  Index := LuaAbsIndex(L, Index);
  Lines.Clear;

  lua_pushnil(L);
  while (lua_next(L, Index) <> 0) do
  begin
    Key := Dequote(LuaStackToStr(L, -2, MaxTable));
    Value := LuaStackToStr(L, -1, MaxTable);
    Lines.Values[Key] := Value;
    lua_pop(L, 1);
  end;
end;

procedure LuaTableToTreeView(L: Plua_State; Index: Integer; TV: TTreeView; MaxTable: Integer);
// Index ?? Table ???? TreeView ????
  procedure ParseTreeNode(TreeNode: TTreeNode; Index: Integer);
  var
    Key: string;
  begin
    Index := LuaAbsIndex(L, Index);

    lua_pushnil(L);
    while (lua_next(L, Index) <> 0) do
    begin
      Key := Dequote(LuaStackToStr(L, -2, MaxTable));
      if (lua_type(L, -1) <> LUA_TTABLE) then
        TV.Items.AddChild(TreeNode, Key + '=' + LuaStackToStr(L, -1, MaxTable))
      else
      begin
        if (Key = '_G') then
          TV.Items.AddChild(TreeNode, Key + '={?O???[?o??????}')
        else
          ParseTreeNode(TV.Items.AddChild(TreeNode, Key), -1);
      end;
      lua_pop(L, 1);
    end;
  end;
begin
  Assert(lua_type(L, Index) = LUA_TTABLE);
  TV.Items.BeginUpdate;
  TV.Items.Clear;
  try
    ParseTreeNode(nil, Index);
  finally
    TV.Items.EndUpdate;
  end;
end;

function LuaGetIdentValue(L: Plua_State; Ident: string; MaxTable: Integer): string;
const
  DebugValue = '___DEBUG_VALUE___';
var
  Local: TStrings;
  Code: string;
  Hook: lua_Hook;
  Mask: Integer;
  Count: Integer;
  {$ifdef FPC}
  PS: PChar;
  {$endif}
begin
  if (Ident = '') then
  begin
    Result := '';
    Exit;
  end;

  Local := TStringList.Create;
  try
    LuaLocalToStrings(L, Local, MaxTable);
    Result := Local.Values[Ident];
    if (Result <> '') then
      Exit;
  finally
    Local.Free;
  end;

  Code := DebugValue + '=' + Ident;
  {$ifdef FPC}
  PS := getmem(length(Code));
  strpcopy(PS,Code);
  luaL_loadbuffer(L, PS, Length(Code), 'debug');
  FreeMem(PS);
  {$else}
  luaL_loadbuffer(L, PChar(Code), Length(Code), 'debug');
  {$endif}
  Hook := lua_gethook(L);
  Mask := lua_gethookmask(L);
  Count := lua_gethookcount(L);
  lua_sethook(L, Hook, 0, Count);
  if (lua_pcall(L, 0, 0, 0) = 0) then
    LuaRawGetTable(L, LUA_GLOBALSINDEX, DebugValue);
  Result := LuaStackToStr(L, -1, MaxTable);
  lua_remove(L, -1);
  luaL_dostring(L, DebugValue + '=nil');
  lua_sethook(L, Hook, Mask, Count);
end;

procedure LuaSetIdentValue(L: Plua_State; Ident, Value: string; MaxTable: Integer);
var
  Local: TStrings;
  Code: string;
  Index: Integer;
  Debug: Plua_Debug;
  AR: Plua_Debug;
  {$ifdef FPC}
  PS: PChar;
  {$endif}
begin
  Local := TStringList.Create;
  try
    AR := Debug;
    LuaLocalToStrings(L, Local, MaxTable);
    Index := Local.IndexOf(Ident);
    if (Index >= 0) then
    begin
      try
        lua_pushnumber(L, StrToFloat(Value));
      except
        LuaPushstring(L, Dequote(Value));
      end;
      lua_getstack(L, 0, AR);
      lua_getinfo(L, 'Snlu', AR);
      lua_setlocal(L, AR, Index + 1);
    end else
    begin
      Code := Ident + '=' + Value;
      {$ifdef FPC}
       PS := getmem(length(Code));
       strpcopy(PS,Code);
       luaL_loadbuffer(L, PS, Length(Code), 'debug');
       FreeMem(PS);
      {$else}
       luaL_loadbuffer(L, PChar(Code), Length(Code), 'debug');
      {$endif}
      if (lua_pcall(L, 0, 0, 0) <> 0) then
        lua_remove(L, -1);
    end;
  finally
    Local.Free;
  end;
end;

procedure LuaProcessErrorMessage(const ErrMsg: string; var Title: string; var Line: Integer; var Msg: string);
const
  Term = #$00;
  function S(Index: Integer): Char;
  begin
    if (Index <= Length(ErrMsg)) then
      Result := ErrMsg[Index]
    else
      Result := Term;
  end;
  function IsDigit(C: Char): Boolean;
  begin
    Result := ('0' <= C) and (C <= '9');
  end;
  function PP(var Index: Integer): Integer;
  begin
    Inc(Index);
    Result := Index;
  end;
var
  I, Start, Stop: Integer;
  LS: string;
  Find: Boolean;
begin
  // ErrMsg = Title:Line:Message
  Title := '';
  Line := 0;
  Msg := ErrMsg;
  Find := False;
  I := 1 - 1;
  Stop := 0;
  // :???l: ???T??
  repeat
    while (S(PP(I)) <> ':') do
      if (S(I) = Term) then
        Exit;
    Start := I;
    if (not IsDigit(S(PP(I)))) then
      Continue;
    while (IsDigit(S(PP(I)))) do
      if (S(I - 1) = Term) then
        Exit;
    Stop := I;
    if (S(I) = ':') then
      Find := True;
  until (Find);
  Title := Copy(ErrMsg, 1, Start - 1);
  LS := Copy(ErrMsg, Start + 1, Stop - Start - 1);
  Line := StrToIntDef(LS, 0);
  Msg := Copy(ErrMsg, Stop + 1, Length(ErrMsg));
end;

procedure LuaLoadBuffer(L: Plua_State; const Code: string; const Name: string);
var
  Title, Msg: string;
  Line: Integer;
  {$ifdef FPC}
  PS,PN: PChar;
  i: Integer;
  {$endif}
begin
  {$ifdef FPC}
   PS := getmem(length(Code));
   strpcopy(PS,Code);
   PN := getmem(length(Name));
   strpcopy(PN,Name);
   i := luaL_loadbuffer(L, PS, Length(Code), PN);
   FreeMem(PS);
   FreeMem(PN);
   if (i = 0) then
    Exit;
  {$else}
  if (luaL_loadbuffer(L, PChar(Code), Length(Code), PChar(Name)) = 0) then
    Exit;
  {$endif}

  LuaProcessErrorMessage(Dequote(LuaStackToStr(L, -1, -1)),
    Title, Line, Msg);
  raise ELuaException.Create(Title, Line, Msg);
end;

procedure LuaPCall(L: Plua_State; NArgs, NResults, ErrFunc: Integer);
var
  Title, Msg: string;
  Line: Integer;
begin
  if (lua_pcall(L, NArgs, NResults, ErrFunc) = 0) then
    Exit;

  LuaProcessErrorMessage(Dequote(LuaStackToStr(L, -1, -1)),
    Title, Line, Msg);
//  raise ELuaException.Create(Title, Line, Msg);
  LuaError(L,Msg);
end;

procedure LuaError(L: Plua_State; const Msg: string);
  {$ifdef FPC}
  Var
  PS: PChar;
  {$endif}
begin
  {$ifdef FPC}
   PS := getmem(length(Msg));
   strpcopy(PS,Msg);
   luaL_error(L, PS);
   FreeMem(PS);
  {$else}
  luaL_error(L, PChar(Msg));
  {$endif}
end;

procedure LuaErrorFmt(L: Plua_State; const Fmt: string; const Args: array of Const);
begin
  LuaError(L, Format(Fmt, Args));
end;

{ ELuaException }

constructor ELuaException.Create(Title: string; Line: Integer; Msg: string);
var
  LS: string;
begin
  if (Line > 0) then
    LS := Format('(%d)', [Line])
  else
    LS := '';
  inherited Create(Title + LS + Msg);
  Self.fTitle := Title;
  Self.fLine := Line;
  Self.fMsg := Msg;
end;

function LuaDataStrToStrings(const TableStr: string; Strings: TStrings): string;
(*
  LuaStackToStr ?`?????? Strings.Values[Name] ?\????????
  TableStr
  { Name = "Lua" Version = 5.0 }
  ??
  Strings
  Name="Lua"
  Version=5.0

  DataList  : Data DataList
            |

  Data      : Table
            | {?O???[?o??????}
            | Ident ( )
            | Ident = Value
            | Ident
            |

  Table     : { DataList }
            |

  Value     : "..."
            | Data

*)
const
  EOF = #$00;
var
  Index: Integer;
  Text: string;
  Token: Char;
  function S(Index: Integer): Char;
  begin
    if (Index <= Length(TableStr)) then
      Result := TableStr[Index]
    else
      Result := EOF;
  end;
  function GetString: string;
  var
    SI: Integer;
  begin
    Dec(Index);
    Result := '';
    repeat
      Assert(S(Index) = '"');
      SI := Index;
      Inc(Index);
      while (S(Index) <> '"') do
        Inc(Index);
      Result := Result + Copy(TableStr, SI, Index - SI + 1);
      Inc(Index);
    until (S(Index) <> '"');
  end;
  function GetValue: string;
    function IsIdent(C: Char): Boolean;
    const
      S = ' =(){}' + CR + LF;
    begin
      Result := (Pos(C, S) = 0);
    end;
  var
    SI: Integer;
  begin
    Dec(Index);
    SI := Index;
    while (IsIdent(S(Index))) do
      Inc(Index);
    Result := Copy(TableStr, SI, Index - SI);
  end;
  function GetToken: Char;
    function SkipSpace(var Index: Integer): Integer;
    const
      TAB = #$09;
      CR = #$0D;
      LF = #$0A;
    begin
      while (S(Index) in [' ', TAB, CR, LF]) do
        Inc(Index);
      Result := Index;
    end;
  begin
    SkipSpace(Index);
    Token := S(Index);
    Inc(Index);
    Text := Token;
    case (Token) of
    EOF: ;
    '"': Text := GetString;
    '{':
      if (Copy(TableStr, Index - 1, Length(LuaGlobalVariableStr)) = LuaGlobalVariableStr) then
      begin
        Token := 'G';
        Text := LuaGlobalVariableStr;
        Inc(Index, Length(LuaGlobalVariableStr) - 1);
      end;
    '}': ;
    '(': ;
    ')': ;
    '=': ;
    else Text := GetValue
    end;
    Result := Token;
  end;
  procedure Check(S: string);
  begin
    if (Pos(Token, S) = -1) then
      raise Exception.CreateFmt('Error %s is required :%s', [Copy(TableStr, Index - 1, Length(TableStr))]);
  end;
  function CheckGetToken(S: string): Char;
  begin
    Result := GetToken;
    Check(S);
  end;
  function ParseData: string; forward;
  function ParseTable: string; forward;
  function ParseValue: string; forward;
  function ParseDataList: string;
  begin
    with (TStringList.Create) do
    try
      while not (Token in [EOF, '}']) do
        Add(ParseData);
      Result := Text;
    finally
      Free;
    end;
  end;
  function ParseData: string;
  begin
    if (Token = EOF) then
    begin
      Result := '';
      Exit;
    end;

    case (Token) of
    '{': Result := ParseTable;
    'G':
      begin
        Result := Text;
        GetToken;
      end;
    else
      begin
        Result := Text;
        case (GetToken) of
        '(':
          begin
            CheckGetToken(')');
            Result := Format('%s=()', [Result]);
            GetToken;
          end;
        '=':
          begin
            GetToken;
            Result := Format('%s=%s', [Result, ParseValue]);
          end;
        end;
      end;
    end;
  end;
  function ParseTable: string;
  begin
    if (Token in [EOF]) then
    begin
      Result := '';
      Exit;
    end;
    Check('{');
    GetToken;
    with (TStringList.Create) do
    try
      Text := ParseDataList;
      Result := CommaText;
    finally
      Free;
    end;
    Check('}');
    GetToken;
  end;
  function ParseValue: string;
  begin
    if (Token = EOF) then
    begin
      Result := '';
      Exit;
    end;

    case (Token) of
    '"':
      begin
        Result := Text;
        GetToken;
      end;
    else
      Result := ParseData;
    end;
  end;
begin
  Index := 1;
  GetToken;
  Strings.Text := ParseDataList;
end;           

function LuaDoFile(L: Plua_State): Integer; cdecl;
// dofile ????(arg)?????l?t??
// Lua: DoFile(FileName, Args...)
const
  ArgIdent = 'arg';
var
  FileName: PChar;
  I, N, R: Integer;
  ArgTable, ArgBackup: Integer;
begin
  N := lua_gettop(L);

  // arg, result ??????
  lua_getglobal(L, ArgIdent);
  ArgBackup := lua_gettop(L);

  FileName := luaL_checkstring(L, 1);
  lua_newtable(L);
  ArgTable := lua_gettop(L);
  for I := 2 to N do
  begin
    lua_pushvalue(L, I);
    lua_rawseti(L, ArgTable, I - 1);
  end;
  lua_setglobal(L, ArgIdent);

  Result := lua_gettop(L);
  luaL_loadfile(L, PChar(FileName));
  R := lua_pcall(L, 0, LUA_MULTRET, 0);
  Result := lua_gettop(L) - Result;

  LuaRawSetTableValue(L, LUA_GLOBALSINDEX, ArgIdent, ArgBackup);
  lua_remove(L, ArgBackup);

  if (R <> 0) then
    lua_error(L);
end;

procedure CheckArg(L: Plua_State; N: Integer);
begin
  if (lua_gettop(L) <> N) then
    luaL_error(L, 'BAD parameter call!');
end;


initialization
  DefaultMaxTable := 256;

end.
