unit uTurboCompilerUtils;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  , uTurboConsts
  , uTurboMetaInfo
  , uTurboExecutor
  ;


Type
  //the Compiler option state
  TTurboCompilerOptionState = (cosDefault, cosEnable, cosDisable);

  //uTurboSymbols 还没有完全敲定，所以这些类型定义放在这里.
  TTurboValueRec = packed record
    case Integer of
      0: (
        case TTurboSimpleTypeKind of
          //ttkUByte: (VByte: Byte);
          ttkSByte: (VShortInt: ShortInt);
          //ttkUWord: (VWord: Word);
          ttkSWord: (VSmallInt: SmallInt);
          ttkULong: (VLong: LongWord);
          ttkSLong: (VInteger: Integer);
          ttkSet:        (VSet: Byte);
          ttkLString:    (VAnsiString: Pointer);
          ttkChar:       (VChar: Char);
          ttkWString:    (VWideString: Pointer);
          ttkString:     (VString: PShortString);
          ttkPointer:    (VPointer: Pointer);
          //ttkObject:     (VMeObject: Pointer);
          ttkClass:      (VObject: TObject);
          ttkWChar:      (VWideChar: WideChar);
          ttkVariant:    (VVariant: TVarData);
          ttkInterface:  (VInterface: Pointer);
          ttkInt64:      (VInt64: Int64);
          ttkDynArray:   (VDynBound: Integer; VDynArray: Pointer);
          ttkMethod:     (VCode: Pointer; VData: Pointer);
          //ttkProcedure:  (VCode: Pointer);
             ttkSingle: (VSingle: Single);
             ttkDouble: (VDouble: Double);
             ttkExtended: (VExtended: Extended);
             ttkComp: (VComp: Comp);
             ttkCurr: (VCurr: Currency);
      );
      1: (VBytes: array [0..15] of byte);
      2: (VWords: array [0..7] of word);
      3: (VDWords: array [0..3] of LongWord);
      4: (VInt64s: array [0..1] of Int64);
      5: (VByte: byte);
      6: (VWord: word);
      7: (VLongword: Longword);
  end;

  TTurboLabelDeclarationRec = packed record
    Name: String;
    WordName: String;
    Addr: Integer;
  end;
  

  TTurboSimpleConst = object
  public
    Name: ShortString;
    TypeKind: TTurboSimpleTypeKind;
    Value: TTurboValueRec;
    ValueStr: ShortString;
    Size: Integer;
  public
    procedure SetTypeKind(aValue: TTurboSimpleTypeKind);
    function AssignValueTo(const Source: Pointer): Boolean;
    //根据aValue 如果aTypeKind is ttkUnknown 那么会自动判断其类型
    function AssignValue(const aValue: string; aTypeKind: TTurboSimpleTypeKind = ttkUnknown): Boolean;
    procedure SaveString(const aModule: TCustomTurboModule);
  end;

  TTurboSimpleVar = object(TTurboSimpleConst)
  public
    Visibility: TTurboVisibility;
    Addr: Integer;
  end;

  TTurboSimpleWord = object
    Options: TTurboWordOptions;
    //the Param Field Length
    //该函数主体的长度 
    ParamFieldLength: LongWord;
    CFA: tsInt;//the offset address of the memory.
    Name: ShortString; 
    ModuleName: ShortString;  //for external word
    ModuleType: TTurboModuleType;  //for external word
    ModuleIndex: Integer; //for external word
    Module: TCustomTurboModule; //for external forth word
    ExternalOptions: TTurboExteralMethodOptions; //Exteral Word Options
  end;

  TTurboSimpleModule = object
    Entry: PTurboModuleRefEntry;
    ModuleType: TTurboModuleType;
    Name: ShortString;
    Module: TCustomTurboModule;
  end;

//convert the unit string(100KB, 100MB) to int(byte).
function UnitStrToInt(s: string): integer;
function GetSimpleTurboTypeSize(const aTypeKind: TTurboSimpleTypeKind): Integer;

implementation

function UnitStrToInt(s: string): integer;
var
  vUnit: integer;
begin
  vUnit := 1;
  if Length(s) >= 3 then
  begin
    if UpCase(s[Length(s)]) = 'B' then
    begin
      if UpCase(s[Length(s)-1]) = 'K' then
      begin
        vUnit := 1024;
        Delete(s, Length(s)-2, 2);
      end
      else if UpCase(s[Length(s)-1]) = 'M' then
      begin
        vUnit := 1024 * 1024;
        Delete(s, Length(s)-2, 2);
      end;
    end;
  end;
    Result := StrToIntDef(s, -1);
    if Result > 0 then 
    begin 
      Result := Result * vUnit;
    end;
end;

function GetSimpleTurboTypeSize(const aTypeKind: TTurboSimpleTypeKind): Integer;
begin
  case aTypeKind of
    ttkSByte, ttkUByte, ttkChar, ttkSet: Result := SizeOf(Byte);
    ttkSWord, ttkUWord: Result := SizeOf(Word);
    ttkSingle: Result := SizeOf(Single);
    ttkDouble: Result := SizeOf(Double);
    ttkComp:  Result := SizeOf(Comp);
    ttkExtended: Result := SizeOf(Extended);
    ttkCurr: Result := SizeOf(Currency);
    ttkEnumeration: Result := -1; //can not determine. <=256 elemnts is byte, <=$FFFF elemnts is word.
    ttkQWord, ttkInt64: Result := SizeOf(Int64);
    //ttkShortString, ttkAnsiString, ttkPointer: Result := SizeOf(Pointer); //Pointer = Integer
  else
    Result := SizeOf(Integer);
  end;
end;

function TTurboSimpleConst.AssignValue(const aValue: string; aTypeKind: TTurboSimpleTypeKind): Boolean;
begin
  Result := True;
  //writeln('AssignValue:', Integer(aTypeKind));
  if (aTypeKind = ttkUnknown) and (aValue[1] = '''') then
  begin
       aTypeKind := ttkString;
      {
      if Length(ValueStr) = 1 then
        aTypeKind := ttkChar
      else if Length(ValueStr) < 256 then
      begin
        aTypeKind := ttkString;
      end
      else begin
        aTypeKind := ttkLString;
      end;}
  end;
  Case aTypeKind of
    ttkString, ttkLString: 
    begin
      ValueStr := AnsiDequotedStr(aValue, '''');
    end;
    ttkChar:
    begin
      ValueStr := AnsiDequotedStr(aValue, '''');
      Value.VByte := Ord(ValueStr[1]);
    end;
    ttkSByte, ttkUByte, ttkSWord, ttkUWord, ttkSLong, ttkULong, ttkInt64:
    try
      ValueStr := aValue;
      Value.VInt64 := StrToInt64(aValue);
    except
      aTypeKind := ttkUnknown;
      Result := False;
    end
    else //Case-Else aTypeKind
    begin
      ValueStr := aValue;
      try //is Integer?
        Value.VInt64 := StrToInt64(aValue);
        if (Value.VInt64 >= Low(ShortInt)) and (Value.VInt64<=High(ShortInt)) then
          aTypeKind := ttkSByte
        else if (Value.VInt64 >= Low(Byte)) and (Value.VInt64<=High(Byte)) then
          aTypeKind := ttkUByte
        else if (Value.VInt64 >= Low(SmallInt)) and (Value.VInt64<=High(SmallInt)) then
          aTypeKind := ttkSWord
        else if (Value.VInt64 >= Low(Word)) and (Value.VInt64<=High(Word)) then
          aTypeKind := ttkUWord
        else if (Value.VInt64 >= Low(LongInt)) and (Value.VInt64<=High(LongInt)) then
          aTypeKind := ttkSLong
        else if (Value.VInt64 >= Low(LongWord)) and (Value.VInt64<=High(LongWord)) then
          aTypeKind := ttkULong
        else //if (Value.VInt64 >= Low(Int64)) and (Value.VInt64<=High(Int64)) then
          aTypeKind := ttkInt64;
      except
        aTypeKind := ttkUnknown;
        Result := False;
      end;

      if aTypeKind = ttkUnknown then
      try //is Float?
        Value.VDouble := StrToFloat(aValue);
        {if (Value.VDouble >= Low(Single)) and (Value.VDouble<=High(Single)) then //can not get low..High
          aTypeKind := ttkSingle
        else}
          aTypeKind := ttkDouble;
      except
        aTypeKind := ttkUnknown;
        Result := False;
      end
    end;
  End; //Case

  TypeKind := aTypeKind;
  if aTypeKind <> ttkUnknown then begin
    Result := True;
  end;
  if Result then
    Size := GetSimpleTurboTypeSize(TypeKind);
end;

function TTurboSimpleConst.AssignValueTo(const Source: Pointer): Boolean;
begin
  Result := True;
  //writeln('AssignValueTo:', ValueStr);
  move(Value, Source^, Size);
  //writeln(InttoHex(Value.VInteger, 4));
  {case TypeKind of
    ttkSLong, ttkInterface, ttkProcedure, ttkPointer, ttkString, ttkLString: PInteger(Source)^ := Value.VInteger;
    ttkSByte: PShortInt(Source)^  := Value.VShortInt;
    ttkUByte, ttkChar, ttkSet: PByte(Source)^ := Value.VByte;
    ttkSWord:PSmallInt(Source)^ := Value.VSmallInt;
    ttkUWord: PWord(Source)^ := Value.VWord;
    ttkULong: PLongWord(Source)^ := Value.VLongword;
    ttkQWord, ttkInt64: PInt64(Source)^:= Value.VInt64;
    else 
      Result := False;
  end;//}
  //writeln('TypeKind=', Integer(TypeKind));
  //writeln('PSource=', PInteger(Source)^);
end;

procedure TTurboSimpleConst.SaveString(const aModule: TCustomTurboModule);
begin
    Case TypeKind of
      ttkString: begin
        Value.VInteger := aModule.UsedDataSize;
        aModule.AddByteToData(Length(ValueStr));
        aModule.AddBufferToData(ValueStr[1], Length(ValueStr));
      end;
      ttkLString: begin
        aModule.AddIntToData(-1);
        aModule.AddIntToData(Length(ValueStr));
        Value.VInteger := aModule.UsedDataSize;
        aModule.AddBufferToData(ValueStr[1], Length(ValueStr));
        aModule.AddByteToData(0);
      end;
    end;//case
end;

procedure TTurboSimpleConst.SetTypeKind(aValue: TTurboSimpleTypeKind);
begin
  TypeKind := aValue;
  Size := GetSimpleTurboTypeSize(TypeKind);
end;


end.
