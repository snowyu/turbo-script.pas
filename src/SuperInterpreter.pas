unit SuperInterpreter;

interface

{$I Setting.inc}

uses
  SysUtils, Classes
  , uSuperScriptConsts
  , uSuperExecutor 
  ;

resourcestring
  rsVisitMemoryExceed = 'Visit Memory Index Exceed!';
  
const
  cTIBLengthOffset = 0;
  cToINOffset = cTIBLengthOffset + SizeOf(Integer);
  cTIBOffset = cToINOffset + SizeOf(Integer);
  cMAXTIBCount = 1024; //Bytes
  cLastWordEntryOffset = cTIBOffset + cMAXTIBCount;

type
  TVMMethod = procedure of object;
  TMemoryArray = array of byte;
  TStack = TMemoryArray; 
  //TStack = array of Integer;
  TForthMethod = procedure of object;
  PPointer = ^ Pointer;

  {//TForthLibs = array of TForthLib;
  PForthLib = ^ TForthLib; 
  TForthLib = packed record
    PriorLib: Integer; //PForthLib;
    LastWord: Integer; //PForthWord; //前一个单词 0 means 为最前面。
    NameLen: Byte;
    //Name: array[0..NameLen] of char; //it's a PChar, the last char is #0
  end;//}

  //For internal used(not the real memory mirror)
  //the Forth word attributes.
  TForthWordRec = packed record
    Options: TForthWordOptions;
    Name: string;
  end;
  
  TSuperInterpreter = class(TCustomSuperExecutor)
  private
    function GetPLibEntry: PForthWord;
    function GetTIB: string;
    procedure SetMemorySize(Value: Integer);
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetStackSize(const Value: Integer);
    procedure SetTIB(const Value: string);
  protected
    FInstrunction: TForthMethod;
    FInternalProcList: TVMMethodList;
    FIP: Integer;
    FIR: TVMInstruction;
    FLastWordEntryAddress: Integer;
    FLibEntryAddress: Integer;
    FMemory: TMemoryArray;
    FMemorySize: Integer;
    FParameterStack: TStack;
    FParameterStackSize: Integer;
    FPC: Integer;
    FRP: Integer;
    FSP: Integer;
    FStack: TStack;
    FStackSize: Integer;
    FStatus: TForthProcessorStates;
    FTextIndex: Integer;
    FUsedMemory: Integer;
    FWRegister: Integer;
    function ExecuteCFA(const aCFA: Integer): Integer; override;
    procedure Init; override;
    procedure InitProcList;
    procedure iVMAlignMem;
    procedure iVMEnter;
    procedure iVMExit;
    procedure iVMFill(const aValue;  const Size: Integer);
    procedure iVMFillByte(const aValue: Byte);
    procedure iVMFillCountPChar(const aValue: PChar; const aSize: Integer);
    procedure iVMFillForthWordHeader(const aWordAttr: TForthWordRec);
    procedure iVMFillInt(const aValue: Integer);
    procedure iVMFillShortCountPChar(const aValue: PChar; const aSize: Byte);
    procedure iVMFillShortString(const aValue: string);
    procedure iVMFillString(const aValue: string);
    procedure iVMFillWord(const aValue: Word);
    procedure iVMHalt;
    procedure iVMNext;
    procedure iVMRevel;
    procedure vAddInt;
    procedure vAligned;
    procedure vCFetch;
    procedure vCONTEXT;
    procedure vCount;
    procedure vCountShort;
    procedure vCStore;
    procedure vFetch;
    procedure vHERE;
    procedure vLAST;
    procedure vPARSE;
    procedure vPlaceShortString;
    procedure vPlaceString;
    procedure vSetRunning;
    procedure vSkipBlank;
    procedure vStore;
    procedure vSubInt;
    procedure vTIB;
    procedure vTIBNum;
    procedure vToIN;
  public
    constructor Create(const aParamStack: TStack = nil); virtual;
    procedure ExecuteInstruction; overload;
    procedure ExecuteInstruction(const aInstruction: TVMInstruction); overload;
    function GetWordCFA(const aWord: string): Integer; override;
    procedure iForthHeader(const aWordAttr: TForthWordRec);
    procedure LoadFromStream(const aStream: TStream); override;
    procedure SaveToStream(const aStream: TStream); override;
    property Instrunction: TForthMethod read FInstrunction;
    property IP: Integer read FIP write FIP;
    property IR: TVMInstruction read FIR;
    property LibEntryAddress: Integer read FLibEntryAddress;
    property MemorySize: Integer read FMemorySize write SetMemorySize;
    property ParameterStackSize: Integer read FParameterStackSize write
            SetParameterStackSize;
    property PC: Integer read FPC write FPC;
    property PLibEntry: PForthWord read GetPLibEntry;
    property RP: Integer read FRP write FRP;
    property SP: Integer read FSP write FSP;
    property StackSize: Integer read FStackSize write SetStackSize;
    property Status: TForthProcessorStates read FStatus;
    property TIB: string read GetTIB write SetTIB;
    property UsedMemory: Integer read FUsedMemory write FUsedMemory;
    property WRegister: Integer read FWRegister;
  end;
  

implementation

{
****************************** TSuperInterpreter *******************************
}
constructor TSuperInterpreter.Create(const aParamStack: TStack = nil);
begin
  inherited Create;
  StackSize := cDefaultStackSize;
  if aParamStack <> nil then FParameterStack := aParamStack;
  ParameterStackSize := cDefaultParamStackSize;
  TMethod(FInstrunction).Data := Self;
end;

function TSuperInterpreter.ExecuteCFA(const aCFA: Integer): Integer;
begin
  Assert(aCFA < FMemorySize, rsVisitMemoryExceed);
  FIP := aCFA;
  Include(FStatus, psRunning);
  iVMNext;
end;

procedure TSuperInterpreter.ExecuteInstruction;
begin
  while (FPC < FMemorySize) and (psRunning in Status) do
  begin
    FIR := TVMInstruction(FMemory[PC]);
    ExecuteInstruction(FIR);
    Inc(FPC, SizeOf(TVMInstruction));
  end;
end;

procedure TSuperInterpreter.ExecuteInstruction(const aInstruction:
        TVMInstruction);
begin
  TMethod(FInstrunction).Code := FInternalProcList[aInstruction];
  //Run the Instrunction
  FInstrunction;
end;

function TSuperInterpreter.GetPLibEntry: PForthWord;
begin
  Result := PForthWord(@FMemory[LibEntryAddress]);
end;

function TSuperInterpreter.GetTIB: string;
var
  I: Integer;
begin
  i := PInteger(@FMemory[cTIBLengthOffset])^;
  if i > 0 then
  begin
    SetLength(Result, i);
    Move(PChar(@FMemory[cTIBOffset])^, PChar(Result)^, i);
  end
  else
    Result := '';
end;

function TSuperInterpreter.GetWordCFA(const aWord: string): Integer;
begin
  Result := -1;
end;

procedure TSuperInterpreter.iForthHeader(const aWordAttr: TForthWordRec);
begin
  iVMFillForthWordHeader(aWordAttr);
  iVMRevel;
end;

procedure TSuperInterpreter.Init;
begin
  FPC := 0;
  FRP := 0;
  FSP := 0;
  
  FIP := 0;
  UsedMemory := 0;
  
  //FTIBIndex := 1;
  
  //SetLength(FMemory, cLastWordEntryOffset+1);
  MemorySize := cLastWordEntryOffset+1;
  FUsedMemory := MemorySize;
  
  PInteger(@FMemory[cTIBLengthOffset])^ := 0;
  PInteger(@FMemory[cToINOffset])^ := 0;
  PChar(@FMemory[cTIBOffset])^ := #0;
  PChar(@FMemory[cLastWordEntryOffset-1])^ := #0;
end;

procedure TSuperInterpreter.InitProcList;
var
  LVM: TVMMethod;
begin
  LVM := iVMEnter;
  FInternalProcList[inEnter] := TMethod(LVM).Code;
  LVM := iVMNext;
  FInternalProcList[inNext] := TMethod(LVM).Code;
  LVM := iVMHalt;
  FInternalProcList[inHalt] := TMethod(LVM).Code;
  LVM := iVMExit;
  FInternalProcList[inExit] := TMethod(LVM).Code;
  
  //Memory Operation Instruction with Param Stack
  LVM := vFetch;
  FInternalProcList[inFetchInt] := TMethod(LVM).Code;
  LVM := vStore;
  FInternalProcList[inStoreInt] := TMethod(LVM).Code;
  LVM := vCFetch;
  FInternalProcList[inFetchByte] := TMethod(LVM).Code;
  LVM := vCStore;
  FInternalProcList[inStoreByte] := TMethod(LVM).Code;
end;

procedure TSuperInterpreter.iVMAlignMem;
begin
  Inc(FUsedMemory, (SizeOf(Pointer)-1));
  FUsedMemory := FUsedMemory and -SizeOf(Pointer);
end;

procedure TSuperInterpreter.iVMEnter;
begin
  //PUSH IP to Reutrun Stack
  Assert(FSP + SizeOf(Integer) <= Length(FStack), rsReturnStackOverflowError);
  PInteger(@FStack[FSP])^ := IP;
  Inc(FSP, SizeOf(Integer));
  
  Inc(FWRegister, SizeOf(Pointer));
  
  IP := FWRegister;
  
  iVMNext;
end;

procedure TSuperInterpreter.iVMExit;
begin
  //POP IP From RS
  Dec(FSP, SizeOf(Integer));
  Assert(FSP >= 0, rsReturnStackUnderflowError);
  FIP := PInteger(@FStack[SP])^;
  iVMNext;
end;

procedure TSuperInterpreter.iVMFill(const aValue;  const Size: Integer);
begin
  if UsedMemory + Size > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + Size + (SizeOf(Pointer)-1));
  end;
  Move(aValue, FMemory[UsedMemory], Size);
  Inc(FUsedMemory, Size);
end;

procedure TSuperInterpreter.iVMFillByte(const aValue: Byte);
begin
  if UsedMemory + SizeOf(aValue) > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + SizeOf(aValue)+ (SizeOf(Pointer)-1));
  end;
  PByte(@FMemory[UsedMemory])^ := aValue;
  Inc(FUsedMemory, SizeOf(aValue));
end;

procedure TSuperInterpreter.iVMFillCountPChar(const aValue: PChar; const aSize:
        Integer);
begin
  if aSize > 0 then
  begin
    iVMFillInt(aSize+1);
    iVMFill(PChar(aValue)^, aSize);
    iVMFillByte(0);
    //iVMAlignMem;
  end
  else
    iVMFillInt(0);
end;

procedure TSuperInterpreter.iVMFillForthWordHeader(const aWordAttr:
        TForthWordRec);
begin
  //write the integer 0 to header first
  iVMFillInt(0);
  
  //reserved the current defining word address.
  FLastWordEntryAddress := UsedMemory;
  
  if FLibEntryAddress = 0 then
  begin
    //the first words
    FLibEntryAddress := UsedMemory;
    iVMFillInt(0); //No PriorWord
  end
  else begin
    iVMFillInt(FLibEntryAddress); //PriorWord
  end;
  
  iVMFillInt(Integer(aWordAttr.Options));
  iVMFillInt(0); //the Parameter Filed Length(unknown);
  
  //Fill ShortString
  iVMFillShortString(aWordAttr.Name);
end;

procedure TSuperInterpreter.iVMFillInt(const aValue: Integer);
begin
  if UsedMemory + SizeOf(aValue) > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + SizeOf(aValue));
  end;
  Pinteger(@FMemory[UsedMemory])^ := aValue;
  Inc(FUsedMemory, SizeOf(aValue));
end;

procedure TSuperInterpreter.iVMFillShortCountPChar(const aValue: PChar; const
        aSize: Byte);
begin
  if aSize > 0 then
  begin
    iVMFillByte(aSize+1);
    iVMFill(PChar(aValue)^, aSize);
    //if PChar(aValue)[aSize-1]^ <> #0 then
    //if PChar(@FMemory[UsedMemory-1])^ <> #0 then
    //begin
      //Inc(PByte(@FMemory[UsedMemory-1])^, 1);
    iVMFillByte(0);
   // end;
    //iVMAlignMem;
  end
  else
    iVMFillByte(0);
end;

procedure TSuperInterpreter.iVMFillShortString(const aValue: string);
begin
  if Length(aValue) > 0 then
  begin
    iVMFillByte(Length(aValue)+1);
    iVMFill(PChar(aValue)^, Byte(Length(aValue)));
    iVMFillByte(0);
    //iVMAlignMem;
  end
  else
    iVMFillByte(0);
end;

procedure TSuperInterpreter.iVMFillString(const aValue: string);
begin
  if Length(aValue) > 0 then
  begin
    iVMFillInt(Length(aValue)+1);
    iVMFill(PChar(aValue)^, Length(aValue));
    iVMFillByte(0);
    //iVMAlignMem;
  end
  else
    iVMFillInt(0);
end;

procedure TSuperInterpreter.iVMFillWord(const aValue: Word);
begin
  if UsedMemory + SizeOf(aValue) > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + SizeOf(aValue)+ (SizeOf(Pointer)-1));
  end;
  PWord(@FMemory[UsedMemory])^ := aValue;
  Inc(FUsedMemory, SizeOf(aValue));
end;

procedure TSuperInterpreter.iVMHalt;
begin
  Exclude(FStatus, psRunning);
end;

procedure TSuperInterpreter.iVMNext;
begin
  {(IP) -> W  fetch memory pointed by IP into "W" register
                ...W now holds address of the Code Field(CFA)
  IP + 2 -> IP //(assuming 2-byte addresses in the thread)
  (W) -> X  // the machine code address of CFA in W
  JMP (X)
  }
  
  FWRegister := IP;
  Inc(FIP, SizeOf(Integer));
  
  //JMP (X)
  Assert(FWRegister < FMemorySize, rsVisitMemoryExceed);
  FPC := FWRegister;
  //FIR := FMemory[FWRegister];
  ExecuteInstruction;
end;

procedure TSuperInterpreter.iVMRevel;
begin
  FLibEntryAddress := FLastWordEntryAddress;
end;

procedure TSuperInterpreter.LoadFromStream(const aStream: TStream);
var
  I: Integer;
  J: Integer;
  LDumy: LongWord;
  LPriorLibEntry: LongWord;
  LByte: Byte;
  LPriorWordEntry: LongWord;
begin
  inherited LoadFromStream(aStream);
  Init;
  with aStream do
  begin
    Read(LDumy, SizeOf(LDumy));
    if (LDumy > 0) {and (FileType = ftProgram)} then
    begin
      ParameterStackSize := LDumy;
    end;
  
    Read(LDumy, SizeOf(LDumy));
    if (LDumy > 0) {and (FileType = ftProgram)} then
    begin
      StackSize := LDumy;
    end;
  
    UsedMemory := Size - Position + UsedMemory;
    //SetLength(FMemory, UsedMemory + cDefaultFreeMemSize);
    MemorySize := UsedMemory + cDefaultFreeMemSize;
  
    //Read Whole
    Read(FMemory[0], Length(FMemory));
  
    FLibEntryAddress := PLongWord(@FMemory[cLastWordEntryOffset])^;
    //PLibEntry := PForthLib(@FMemory[LDumy]);
    {
      So the Memory Mirror is
      Lib Entry: FMemory[0]
    }
    {//SetLength(FForthLibs, Length(FForthLibs)+1);
    //the Lib Entry
    Read(LDumy, SizeOf(LDumy));
    Seek(LDumy, soFromBeginning);
    I := 0;
    repeat
      SetLength(FForthLibs, I+1);
  
      //the PriorLibEntry
      Read(LPriorLibEntry, SizeOf(LPriorLibEntry));
      //the Lib Name Length
      Read(LByte, SizeOf(LByte));
      with FForthLibs[I] do
      begin
        SetLength(Name, LByte);
        Read(PChar(Name), LByte);
      end;
      //the LastWord Entry
      Read(LDumy, SizeOf(LDumy));
      Seek(LDumy, soFromBeginning);
      J := 0;
      repeat
        SetLength(FForthLibs[I].Words, J+1);
        //the Prior Word Entry
        Read(LPriorWordEntry, SizeOf(LPriorLibEntry));
        //the Lib Name Length
        Read(LByte, SizeOf(LByte));
        with FForthLibs[I].Words[J] do
        begin
          SetLength(Name, LByte);
          Read(PChar(Name), LByte);
          Read(Precedence, SizeOf(Precedence));
          Read(Visibility, SizeOf(Visibility));
          Read(CallStyle, SizeOf(CallStyle));
          Read(CodeFieldStyle, SizeOf(CodeFieldStyle));
          Read(CFA, SizeOf(CFA));
          //the PF Len
          Read(LDumy, Sizeof(LDumy));
          SetLength(ParameterFields, LDumy);
          Read(@ParameterFields[0], LDumy);
        end;
        if LPriorWordEntry <> 0 then
        begin
          Inc(J);
          Seek(LPriorWordEntry, soFromBeginning);
        end;
      unit LPriorWordEntry = 0;
      if LPriorLibEntry <> 0 then
      begin
        Inc(I);
        Seek(LPriorLibEntry, soFromBeginning);
      end;
    until LPriorLibEntry = 0;
  //}
  
  end;
end;

procedure TSuperInterpreter.SaveToStream(const aStream: TStream);
begin
  inherited SaveToStream(aStream);
end;

procedure TSuperInterpreter.SetMemorySize(Value: Integer);
begin
  if FMemorySize <> Value then
  begin
    FMemorySize := Value;
    SetLength(FMemory, FMemorySize);
    {if (FPC >= FPC0) and (FPC0 <> @FMemory[0]) then
    begin
      //the Block has been moved a new address
      // so, i've to update to the PC too.
      FPC := (FPC - FPC0) + @FMemory[0];
    end;
    FPC0 := @FMemory[0]; //}
  end;
end;

procedure TSuperInterpreter.SetParameterStackSize(const Value: Integer);
begin
  if FParameterStackSize <> Value then
  begin
    FParameterStackSize := Value;
    SetLength(FParameterStack, FParameterStackSize);
    if FSP > FParameterStackSize then FSP := FParameterStackSize;
    {if (FSP >= FSP0) and (FSP0 <> @FParameterStack[0]) then
    begin
      //the Block has been moved a new address
      // so, i've to update to the PC too.
      FSP := (FSP - FSP0) + @FParameterStack[0];
    end;
    FSP0 := @FParameterStack[0]; //}
  end;
end;

procedure TSuperInterpreter.SetStackSize(const Value: Integer);
begin
  if FStackSize <> Value then
  begin
    FStackSize := Value;
    SetLength(FStack, FStackSize);
    if FRP > FStackSize then FRP := FStackSize;
    {if (FRP >= FRP0) and (FRP0 <> @FStack[0]) then
    begin
      //the Block has been moved a new address
      // so, i've to update to the PC too.
      FRP := (FRP - FRP0) + @FStack[0];
    end;
    FRP0 := @FStack[0]; //}
  end;
end;

procedure TSuperInterpreter.SetTIB(const Value: string);
var
  I: Integer;
begin
  if Value <> '' then
  begin
    i := Length(Value);
    if i >= cMAXTIBCount then i := cMAXTIBCount-1;
    Move(PChar(Value)^, FMemory[cTIBOffset], i);
    FMemory[cTIBOffset+i] := 0;
    PInteger(@FMemory[cToINOffset])^ := 0;
    PInteger(@FMemory[cTIBLengthOffset])^ := i;
  end
  else begin
    FMemory[cTIBOffset] := 0;
    PInteger(@FMemory[cTIBLengthOffset])^ := 0;
  end;
end;

procedure TSuperInterpreter.vAddInt;
begin
  //至少栈上应该有两个数据
  Assert(FSP>=2*SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  PInteger(@FParameterStack[FSP-SizeOf(Integer)])^ :=
    PInteger(@FParameterStack[FSP-SizeOf(Integer)])^ +
    PInteger(@FParameterStack[FSP])^;
end;

procedure TSuperInterpreter.vAligned;
var
  I: Integer;
begin
  //至少栈上应该有个数据
  I := FSP-SizeOf(Integer);
  Assert(I>=0, rsParamStackUnderflowError);
  
  Inc(PInteger(@FParameterStack[I])^, (SizeOf(Pointer)-1));
  PInteger(@FParameterStack[I])^ := PInteger(@FParameterStack[I])^ and -SizeOf(Pointer);
end;

procedure TSuperInterpreter.vCFetch;
var
  I: Integer;
  LVarAddress: Integer;
begin
  //The Param Stack should not empty
  i := FSP - SizeOf(Integer);
  Assert(i >= 0, rsParamStackUnderflowError);
  LVarAddress := PInteger(@FParameterStack[i])^;
  
  Assert(LVarAddress < FMemorySize, rsVisitMemoryExceed);
  PInteger(@FParameterStack[i])^ := PByte(@FMemory[LVarAddress])^;
end;

procedure TSuperInterpreter.vCONTEXT;
begin
  //Push the CONTEXT Address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  
  PInteger(@FParameterStack[FSP])^ := LibEntryAddress;
  Inc(FSP, SizeOf(Integer));
end;

procedure TSuperInterpreter.vCount;
var
  LStrAddr: Integer;
  LStrCount: Integer;
begin
  //Popup an Address from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  LStrAddr := PInteger(@FParameterStack[FSP])^;
  
  LStrCount := PInteger(@FMemory[LStrAddr])^;
  
  //Push the PChar index address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  //Point to the Pchar
  PInteger(@FParameterStack[FSP])^ := LStrAddr + SizeOf(Integer);
  Inc(FSP, SizeOf(Integer));
  
  //Push the current Str Length to param stack
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  PInteger(@FParameterStack[FSP])^ := LStrCount;
  Inc(FSP, SizeOf(Integer));
end;

procedure TSuperInterpreter.vCountShort;
var
  LStrAddr: Integer;
  LStrCount: Byte;
begin
  //Popup an Address from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  LStrAddr := PInteger(@FParameterStack[FSP])^;
  
  LStrCount := PByte(@FMemory[LStrAddr])^;
  
  //Push the PChar index address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  //Point to the Pchar
  PInteger(@FParameterStack[FSP])^ := LStrAddr + SizeOf(Integer);
  Inc(FSP, SizeOf(Integer));
  
  //Push the current Str Length to param stack
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  PInteger(@FParameterStack[FSP])^ := LStrCount;
  Inc(FSP, SizeOf(Integer));
end;

procedure TSuperInterpreter.vCStore;
var
  I: Integer;
  LVarAddr: Integer;
begin
  //Popup an Address from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  LVarAddr := PInteger(@FParameterStack[FSP])^;
  
  //Popup the Value from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  I := PInteger(@FParameterStack[FSP])^;
  
  //Store the Value
  Assert(LVarAddr < FMemorySize, rsVisitMemoryExceed);
  PInteger(@FMemory[LVarAddr])^ := I;
end;

procedure TSuperInterpreter.vFetch;
var
  I: Integer;
  LVarAddress: Integer;
begin
  //The Param Stack should not empty
  i := FSP - SizeOf(Integer);
  Assert(i >= 0, rsParamStackUnderflowError);
  LVarAddress := PInteger(@FParameterStack[i])^;
  
  Assert(LVarAddress < FMemorySize, rsVisitMemoryExceed);
  PInteger(@FParameterStack[i])^ := PInteger(@FMemory[LVarAddress])^;
end;

procedure TSuperInterpreter.vHERE;
begin
  //Push the current UsedMemory to param stack
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  PInteger(@FParameterStack[FSP])^ := FUsedMemory;
  Inc(FSP, SizeOf(Integer));
end;

procedure TSuperInterpreter.vLAST;
begin
  //Push the LAST-WORD Address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  
  PInteger(@FParameterStack[FSP])^ := FLastWordEntryAddress;
  Inc(FSP, SizeOf(Integer));
end;

procedure TSuperInterpreter.vPARSE;
var
  LTIBIndex: Integer;
  LSepChar: Char;
  LTIBLen: Integer;
  LOldTIBIndex: Integer;
begin
  //Popup a Char(Byte) from Parameter stack
  Assert(FSP>=1, rsParamStackUnderflowError);
  Dec(FSP, 1);
  //分割符
  LSepChar := PChar(@FParameterStack[FSP])^;
  LTIBIndex := PInteger(@FMemory[cToINOffset])^;
  LOldTIBIndex := LTIBIndex;
  LTIBLen := PInteger(@FMemory[cTIBLengthOffset])^;
  
  //Push the current FTIB[FTextIndex] addr to param stack
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  PInteger(@FParameterStack[FSP])^ := cTIBOffset + LOldTIBIndex;
  Inc(FSP, SizeOf(Pointer));
  
  while (LTIBIndex < LTIBLen) and (PChar(@FMemory[cTIBOffset+LTIBIndex])^ <> LSepChar) do
  begin
    Inc(FTextIndex);
    {$IFDEF MBCS_SUPPORT}
    while (LTIBIndex < LTIBLen) and (StrByteType(PChar(@FMemory[cTIBOffset]), LTIBIndex) <> mbSingleByte) do
      Inc(FTextIndex);
    {$ENDIF}
  end;
  
  PInteger(@FMemory[cToINOffset])^ := LTIBIndex;
  
  //Push the current Str Length to param stack
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  PInteger(@FParameterStack[FSP])^ := LTIBIndex - LOldTIBIndex;
  Inc(FSP, SizeOf(Integer));
end;

procedure TSuperInterpreter.vPlaceShortString;
var
  LDstStrAddr: Integer;
  LSrcCount: Byte;
  LSrcPChar: PChar;
begin
  //Popup an Address from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  LDstStrAddr := PInteger(@FParameterStack[FSP])^;
  
  
  //Popup a Count(Byte) from Parameter stack
  Assert(FSP>=SizeOf(Byte), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Byte));
  LSrcCount := PByte(@FParameterStack[FSP])^;
  
  
  //Popup a PChar address from Parameter stack
  Assert(FSP>=SizeOf(PChar), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(PChar));
  LSrcPChar := PChar(@FParameterStack[FSP]);
  
  iVMFillShortCountPChar(LSrcPChar, LSrcCount);
end;

procedure TSuperInterpreter.vPlaceString;
var
  LDstStrAddr: Integer;
  LSrcCount: Integer;
  LSrcPChar: PChar;
begin
  //Popup an Address from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  LDstStrAddr := PInteger(@FParameterStack[FSP])^;
  
  
  //Popup a Count(Integer) from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  LSrcCount := PByte(@FParameterStack[FSP])^;
  
  
  //Popup a PChar address from Parameter stack
  Assert(FSP>=SizeOf(PChar), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(PChar));
  LSrcPChar := PChar(@FParameterStack[FSP]);
  
  iVMFillCountPChar(LSrcPChar, LSrcCount);
end;

procedure TSuperInterpreter.vSetRunning;
begin
  Include(FStatus, psRunning);
end;

procedure TSuperInterpreter.vSkipBlank;
var
  LTIBLen: Integer;
  LTIBIndex: Integer;
begin
  LTIBLen := PInteger(@FMemory[cTIBLengthOffset])^;
  LTIBIndex := PInteger(@FMemory[cToINOffset])^;
  while (LTIBIndex < LTIBLen)
    {$IFDEF MBCS_SUPPORT}
    and (StrByteType(PChar(@FMemory[cTIBOffset]), LTIBIndex) = mbSingleByte)
    {$ENDIF}
    and (PChar(@FMemory[cTIBOffset+LTIBIndex])^ = ' ')
  do begin
    Inc(LTIBIndex);
  end;
  PInteger(@FMemory[cToINOffset])^ := LTIBIndex;
end;

procedure TSuperInterpreter.vStore;
var
  I: Integer;
  LVarAddr: Integer;
begin
  //Popup an Address from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  LVarAddr := PInteger(@FParameterStack[FSP])^;
  
  //Popup the Value from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  I := PInteger(@FParameterStack[FSP])^;
  
  //Store the Value
  Assert(LVarAddr < FMemorySize, rsVisitMemoryExceed);
  PInteger(@FMemory[LVarAddr])^ := I;
end;

procedure TSuperInterpreter.vSubInt;
begin
  //至少栈上应该有两个数据
  Assert(FSP>=2*SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  PInteger(@FParameterStack[FSP-SizeOf(Integer)])^ :=
    PInteger(@FParameterStack[FSP-SizeOf(Integer)])^ -
    PInteger(@FParameterStack[FSP])^;
end;

procedure TSuperInterpreter.vTIB;
begin
  //Push the TIB Address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  
  PInteger(@FParameterStack[FSP])^ := cTIBOffset;
  Inc(FSP, SizeOf(Integer));
end;

procedure TSuperInterpreter.vTIBNum;
begin
  //Push the TIB Length address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  
  PInteger(@FParameterStack[FSP])^ := cTIBLengthOffset;
  Inc(FSP, SizeOf(Integer));
end;

procedure TSuperInterpreter.vToIN;
begin
  //Push the TIB index
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  
  PInteger(@FParameterStack[FSP])^ := cToINOffset;
  Inc(FSP, SizeOf(Integer));
end;


end.
