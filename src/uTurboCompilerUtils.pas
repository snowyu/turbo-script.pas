unit uTurboCompilerUtils;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  //, TypInfo
  , uMeTypes
  , uTurboConsts
  , uTurboMetaInfo
  , uTurboExecutor
  ;


Type
  //the Compiler option state
  TTurboCompilerOptionState = (cosDefault, cosEnable, cosDisable);

  //uTurboSymbols 还没有完全敲定，所以这些类型定义放在这里.
  {TTurboValueRec_ = packed record
    case Integer of
      0: (
        case TMeTypeKind of
          //mtkUByte: (VByte: Byte);
          mtkSByte: (VShortInt: ShortInt);
          //mtkUWord: (VWord: Word);
          mtkSWord: (VSmallInt: SmallInt);
          mtkULong: (VLong: LongWord);
          mtkSLong: (VInteger: Integer);
          mtkSet:        (VSet: Byte);
          mtkLString:    (VAnsiString: Pointer);
          mtkChar:       (VChar: Char);
          mtkWString:    (VWideString: Pointer);
          mtkString:     (VString: PShortString);
          mtkPointer:    (VPointer: Pointer);
          //mtkObject:     (VMeObject: Pointer);
          mtkClass:      (VObject: TObject);
          mtkWChar:      (VWideChar: WideChar);
          mtkVariant:    (VVariant: TVarData);
          mtkInterface:  (VInterface: Pointer);
          mtkInt64:      (VInt64: Int64);
          mtkDynArray:   (VDynBound: Integer; VDynArray: Pointer);
          mtkMethod:     (VCode: Pointer; VData: Pointer);
          //mtkProcedure:  (VCode: Pointer);
             mtkSingle: (VSingle: Single);
             mtkDouble: (VDouble: Double);
             mtkExtended: (VExtended: Extended);
             mtkComp: (VComp: Comp);
             mtkCurr: (VCurr: Currency);
      );
      1: (VBytes: array [0..15] of byte);
      2: (VWords: array [0..7] of word);
      3: (VDWords: array [0..3] of LongWord);
      4: (VInt64s: array [0..1] of Int64);
      5: (VByte: byte);
      6: (VWord: word);
      7: (VLongword: Longword);
  end;
//}
  TTurboLabelDeclarationRec = packed record
    Name: String;
    WordName: String;
    Addr: Integer;
  end;
  

  TTurboSimpleConst = object
  public
    Name: ShortString;
    TurboType: PMeType;
    Value: TMeVarRec;
    ValueStr: ShortString;
    Size: Integer;
  public
    procedure SetTurboType(const aValue: PMeType);
    //assign value to mem!
    function AssignValueTo(const Source: Pointer): Boolean;
    //根据aValue 如果aTypeKind is mtkUnknown 那么会自动判断其类型
    function AssignValue(const aValue: string; aType: PMeType = nil): Boolean;
    procedure SaveString(const aModule: TCustomTurboModule);
  end;

  TTurboSimpleVar = object(TTurboSimpleConst)
  public
    Visibility: TTurboVisibility;
    Addr: Integer;
  end;

  PTurboSimpleWord = ^ TTurboSimpleWord;
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
//function GetSimpleTurboTypeSize(const aTypeKind: TMeTypeKind): Integer;

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

{function GetSimpleTurboTypeSize(const aTypeKind: TMeTypeKind): Integer;
begin
  case aTypeKind of
    mtkSByte, mtkUByte, mtkChar, mtkSet: Result := SizeOf(Byte);
    mtkSWord, mtkUWord: Result := SizeOf(Word);
    mtkSingle: Result := SizeOf(Single);
    mtkDouble: Result := SizeOf(Double);
    mtkComp:  Result := SizeOf(Comp);
    mtkExtended: Result := SizeOf(Extended);
    mtkCurr: Result := SizeOf(Currency);
    mtkEnumeration: Result := -1; //can not determine. <=256 elemnts is byte, <=$FFFF elemnts is word.
    mtkQWord, mtkInt64: Result := SizeOf(Int64);
    //mtkShortString, mtkAnsiString, mtkPointer: Result := SizeOf(Pointer); //Pointer = Integer
  else
    Result := SizeOf(Integer);
  end;
end;
}
function TTurboSimpleConst.AssignValue(const aValue: string; aType: PMeType): Boolean;
begin
  Result := True;
  //writeln('AssignValue:', Integer(aTypeKind));
  if (aType = nil) and (aValue[1] = '''') then
  begin
       //aTypeKind := mtkString;
       aType := GetRegisteredTypeByTypeInfo(TypeInfo(ShortString));
  end
  else if Assigned(aType) then
  begin
    Case aType.Kind of
      mtkString, mtkLString: 
      begin
        ValueStr := AnsiDequotedStr(aValue, '''');
      end;
      mtkChar:
      begin
        ValueStr := AnsiDequotedStr(aValue, '''');
        Value.VByte := Ord(ValueStr[1]);
      end;
      mtkInteger, mtkInt64:
      try
        ValueStr := aValue;
        Value.VInt64 := StrToInt64(aValue);
      except
        aType := nil;
        Result := False;
      end;
      mtkFloat:
      try
        ValueStr := aValue;
        Value.VExtended := StrToFloat(aValue);
      except
        aType := nil;
        Result := False;
      end;
    end; //case
  end
  else //try....
  begin
      ValueStr := aValue;
      try //is Integer?
        Value.VInt64 := StrToInt64(aValue);
        if (Value.VInt64 >= Low(ShortInt)) and (Value.VInt64<=High(ShortInt)) then
        begin
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(ShortInt));
        end
        else if (Value.VInt64 >= Low(Byte)) and (Value.VInt64<=High(Byte)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Byte))
        else if (Value.VInt64 >= Low(SmallInt)) and (Value.VInt64<=High(SmallInt)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(SmallInt))
        else if (Value.VInt64 >= Low(Word)) and (Value.VInt64<=High(Word)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Word))
        else if (Value.VInt64 >= Low(LongInt)) and (Value.VInt64<=High(LongInt)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(LongInt))
        else if (Value.VInt64 >= Low(LongWord)) and (Value.VInt64<=High(LongWord)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(LongWord))
        else //if (Value.VInt64 >= Low(Int64)) and (Value.VInt64<=High(Int64)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Int64))
      except
        aType := nil;
        Result := False;
      end;

      if not Assigned(aType) then
      try //is Float?
        Value.VExtended := StrToFloat(aValue);
        if (ABS(Value.VExtended) >= 1.5e-45) and (ABS(Value.VExtended) <= 3.4e38) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Single))
        else if (ABS(Value.VExtended) >= 5e-308) and (ABS(Value.VExtended) <= 1.7e308) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Double))
        else
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Extended))
      except
        aType := nil;
        Result := False;
      end
  end;

  TurboType := aType;
  if Assigned(aType) then begin
    Result := True;
  end;
  if Result then
    Size := GetTypeSize(TurboType);
end;

function TTurboSimpleConst.AssignValueTo(const Source: Pointer): Boolean;
begin
  Result := True;
  //writeln('AssignValueTo:', ValueStr);
  move(Value, Source^, Size);
  //writeln(InttoHex(Value.VInteger, 4));
  //writeln('TurboType=', Integer(TurboType));
  //writeln('PSource=', PInteger(Source)^);
end;

procedure TTurboSimpleConst.SaveString(const aModule: TCustomTurboModule);
begin
  if Assigned(TurboType) then
    Case TurboType.Kind of
      mtkString: begin
        Value.VInteger := aModule.UsedDataSize;
        aModule.AddByteToData(Length(ValueStr));
        aModule.AddBufferToData(ValueStr[1], Length(ValueStr));
      end;
      mtkLString: begin
        aModule.AddIntToData(-1);
        aModule.AddIntToData(Length(ValueStr));
        Value.VInteger := aModule.UsedDataSize;
        aModule.AddBufferToData(ValueStr[1], Length(ValueStr));
        aModule.AddByteToData(0);
      end;
    end;//case
end;

procedure TTurboSimpleConst.SetTurboType(const aValue: PMeType);
begin
  TurboType := aValue;
  Size := GetTypeSize(TurboType);
end;


end.
