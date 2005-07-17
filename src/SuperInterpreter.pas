unit SuperInterpreter;

interface

{$I Setting.inc}

uses
  SysUtils, Classes
  , uSuperScriptConsts
  , uSuperExecutor 
  ;

type
  TMemoryArray = array of byte;
  TStack = TMemoryArray;
  TForthMethod = procedure of object;
  PPointer = ^ Pointer;

  TForthVisibility = (fvDefault, fvPrivate, fvProtected, fvPublic, fvPublished);
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
    case Boolean of
     True: (Params: LongWord);
     False: (
      Precedence: Byte; 
      Visibility: Byte; 
      CallStyle: Byte;
      CodeFieldStyle: Byte);
    end;
    Name: string;
  end;
  
  //TForthWords = array of TForthWord;
  PForthWord = ^ TForthWord;
  //For cast the Mem
  TForthWord = packed record //ITC (Indirect Threaded Code)
    PriorWord: Integer; //PForthWord; //前一个单词 0 means 为最前面。
    case Boolean of
     True: (Params: LongWord);
     False: (
      //优先级, 0=low, 1=high equals 1 for an IMMEDIATE word
      //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
      //this can be extent to private, protected, public, etc
      Precedence: Byte; 
      Visibility: Byte; 
      CallStyle: Byte;
      CodeFieldStyle: Byte);
    end;
    //the Param Field Length 
    ParamFieldLength: LongWord;
    NameLen: Byte;
    //Name: String; it's a PChar, the last char is #0
    //the following is ParameterFields   
    //其实就是直接指向的某个单词的PFA，不过那个单词的PFA就是直接执行的机器码而已。
    //CFA = ParameterFields[0]
    //CFA: LongWord;
    //ParameterFields: array of Integer; //大多数情况下是PForthWord，但是少数情况下是数据或VM Codes
  end;
  TForthProcessorState = (psRunning, psCompiling, psFinished, psNoData, psBadData, 
    psBadOp, psDivZero, psOverFlow);
  TForthProcessorStates = set of TProcessorState;

  TSuperInterpreter = class(TCustomSuperExecutor)
  private
    function GetPLibEntry: PForthWord;
    procedure SetMemorySize(Value: Integer);
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetStackSize(const Value: Integer);
    procedure SetText(const Value: string);
  protected
    FInstrunction: TForthMethod;
    FInternalProcList: TVMMethodList;
    FIP: Integer;
    FIR: TInstruction;
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
    FStatus: TProcessorStates;
    FText: string;
    FTextIndex: Integer;
    FUsedMemory: Integer;
    FWRegister: Integer;
    function ExecuteCFA(const aCFA: Integer): Integer; override;
    procedure Init; override;
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
    procedure iVMNext;
    procedure iVMRevel;
    procedure vAddInt;
    procedure vCount;
    procedure vCountShort;
    procedure vHERE;
    procedure vPARSE;
    procedure vPlaceShortString;
    procedure vPlaceString;
    procedure vSetRunning;
    procedure vSkipBlank;
  public
    constructor Create(const aParamStack: TStack = nil); virtual;
    procedure ExecuteInstruction; overload;
    procedure ExecuteInstruction(const aInstruction: TInstruction); overload;
    function GetWordCFA(const aWord: string): Integer; override;
    procedure iForthHeader(const aWordAttr: TForthWordRec);
    procedure InitProcList;
    procedure LoadFromStream(const aStream: TStream); override;
    procedure SaveToStream(const aStream: TStream); override;
    property Instrunction: TForthMethod read FInstrunction;
    property IP: Integer read FIP write FIP;
    property IR: TInstruction read FIR;
    property LibEntryAddress: Integer read FLibEntryAddress;
    property MemorySize: Integer read FMemorySize write SetMemorySize;
    property ParameterStackSize: Integer read FParameterStackSize write
            SetParameterStackSize;
    property PC: Integer read FPC write FPC;
    property PLibEntry: PForthWord read GetPLibEntry;
    property RP: Integer read FRP write FRP;
    property SP: Integer read FSP write FSP;
    property StackSize: Integer read FStackSize write SetStackSize;
    property Status: TProcessorStates read FStatus;
    property Text: string read FText write SetText;
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
  Result := inherited ExecuteCFA(aCFA);
end;

procedure TSuperInterpreter.ExecuteInstruction;
begin
  FIR := TVMInstruction(FMemory[IP]);
  ExecuteInstruction(FIR);
  Inc(FIP, SizeOf(TVMInstruction));
end;

procedure TSuperInterpreter.ExecuteInstruction(const aInstruction:
        TInstruction);
begin
  TMethod(FInstrunction).Code := FInternalProcList[aInstruction];
  //Run the Instrunction
  FInstrunction;
end;

function TSuperInterpreter.GetPLibEntry: PForthWord;
begin
  Result := PForthWord(@FMemory[LibEntryAddress]);
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
  
  FTIBIndex := 1;
end;

procedure TSuperInterpreter.InitProcList;
begin
  FInternalProcList[inEnter] := @iVMEnter;
  FInternalProcList[inNext] := @iVMNext;
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
  PInteger(@FStack[FSP])^ := PC;
  Inc(FSP, SizeOf(Integer));
  //PPointer(SP)^ := PC;
  //Inc(SP, SizeOf(Pointer));
  
  Inc(FWRegister, SizeOf(Pointer));
  
  PC := FWRegister;
  
  iVMNext;
end;

procedure TSuperInterpreter.iVMExit;
begin
  //POP IP From RS
  Dec(FSP, SizeOf(Integer));
  Assert(FSP >= 0, rsReturnStackUnderflowError);
  PC := PInteger(@FStack[SP])^;
  iVMNext;
end;

procedure TSuperInterpreter.iVMFill(const aValue;  const Size: Integer);
begin
  if UsedMemory + Size > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + Size + (SizeOf(Pointer)-1));
  end;
  Move(aValue, @FMemory[UsedMemory], Size);
  Inc(UsedMemory, Size);
end;

procedure TSuperInterpreter.iVMFillByte(const aValue: Byte);
begin
  if UsedMemory + SizeOf(aValue) > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + SizeOf(aValue)+ (SizeOf(Pointer)-1));
  end;
  PByte(@FMemory[UsedMemory])^ := aValue;
  Inc(UsedMemory, SizeOf(aValue));
end;

procedure TSuperInterpreter.iVMFillCountPChar(const aValue: PChar; const aSize:
        Integer);
begin
  iVMFillInt(aSize);
  if Length(aValue) > 0 then
  begin
    iVMFill(PChar(aValue), aSize);
    iVMFillByte(0);
    iVMAlignMem;
  end;
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
  
  iVMFillInt(aWordAttr.Params);
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
  Inc(UsedMemory, SizeOf(aValue));
end;

procedure TSuperInterpreter.iVMFillShortCountPChar(const aValue: PChar; const
        aSize: Byte);
begin
  iVMFillByte(aSize);
  if Length(aValue) > 0 then
  begin
    iVMFill(PChar(aValue), aSize);
    iVMFillByte(0);
    iVMAlignMem;
  end;
end;

procedure TSuperInterpreter.iVMFillShortString(const aValue: string);
begin
  iVMFillByte(Length(aValue));
  if Length(aValue) > 0 then
  begin
    iVMFill(PChar(aValue), Byte(Length(aValue)));
    iVMFillByte(0);
    iVMAlignMem;
  end;
end;

procedure TSuperInterpreter.iVMFillString(const aValue: string);
begin
  iVMFillInt(Length(aValue));
  if Length(aValue) > 0 then
  begin
    iVMFill(PChar(aValue), Length(aValue));
    iVMFillByte(0);
    iVMAlignMem;
  end;
end;

procedure TSuperInterpreter.iVMFillWord(const aValue: Word);
begin
  if UsedMemory + SizeOf(aValue) > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + SizeOf(aValue)+ (SizeOf(Pointer)-1));
  end;
  PWord(@FMemory[UsedMemory])^ := aValue;
  Inc(UsedMemory, SizeOf(aValue));
end;

procedure TSuperInterpreter.iVMNext;
begin
  {(IP) -> W  fetch memory pointed by IP into "W" register
                ...W now holds address of the Code Field(CFA)
  IP + 2 -> IP //(assuming 2-byte addresses in the thread)
  (W) -> X  // the machine code address of CFA in W
  JMP (X)
  }
  
  FWRegister := PC;
  Inc(PC, SizeOf(Integer));
  
  //JMP (X)
  FIP := FMemory[FWRegister];
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
  
    UsedMemory := Size - Poision + Length(FMemory);
    //SetLength(FMemory, UsedMemory + cDefaultFreeMemSize);
    MemorySize := UsedMemory + cDefaultFreeMemSize;
    Read(@FMemory[0], Length(FMemory));
    FLibEntryAddress := PLongWord(@FMemory[0])^;
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
    if FRP > FStackSize then FRP = FStackSize;
    {if (FRP >= FRP0) and (FRP0 <> @FStack[0]) then
    begin
      //the Block has been moved a new address
      // so, i've to update to the PC too.
      FRP := (FRP - FRP0) + @FStack[0];
    end;
    FRP0 := @FStack[0]; //}
  end;
end;

procedure TSuperInterpreter.SetText(const Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    FTIBIndex := 1;
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

procedure TSuperInterpreter.vCount;
var
  LStrAddr: Integer;
  LStrCount: Integer;
begin
  //Popup an Address from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer))
  LStrAddr := PInteger(@FParameterStack[FSP])^;
  
  LStrCount := PInteger(@FParameterStack[LStrAddr])^;
  
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
  LDstStrAddr: Integer;
  LSrcCount: Byte;
  LSrcPChar: PChar;
begin
  //Popup an Address from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer))
  LStrAddr := PInteger(@FParameterStack[FSP])^;
  
  LStrCount := PByte(@FParameterStack[LStrAddr])^;
  
  //Push the PChar index address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  //Point to the Pchar
  PInteger(@FParameterStack[FSP])^ := LStrAddr + SizeOf(Byte);
  Inc(FSP, SizeOf(Integer));
  
  //Push the current Str Length to param stack
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  PByte(@FParameterStack[FSP])^ := LStrCount;
  Inc(FSP, SizeOf(Byte));
end;

procedure TSuperInterpreter.vHERE;
begin
  //Push the current UsedMemory to param stack
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  PInteger(@FParameterStack[FSP])^ := FUsedMemory;
  Inc(FSP, SizeOf(Integer));
end;

procedure TSuperInterpreter.vPARSE;
var
  LSepChar: Char;
  LOldTIBIndex: Integer;
begin
  //Popup a Char(Byte) from Parameter stack
  Assert(FSP>=1, rsParamStackUnderflowError);
  Dec(FSP, 1)
  //分割符
  LSepChar := PChar(@FParameterStack[FSP])^;
  LOldTIBIndex := FTextIndex;
  
  //Push the current FTIBIndex to param stack
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  PInteger(@FParameterStack[FSP])^ := FTextIndex;
  Inc(FSP, SizeOf(Integer));
  
  while (FTextIndex <= Length(FText)) and (FText[FTextIndex] <> LSepChar) do
  begin
    Inc(FTextIndex);
    {$IFDEF MBCS_SUPPORT}
    while (FTextIndex <= Length(FText)) and (ByteType(FText, FTextIndex) <> mbSingleByte) do
      Inc(FTextIndex);
    {$ENDIF}
  end;
  
  //Push the current Str Length to param stack
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  PInteger(@FParameterStack[FSP])^ := FTextIndex - LOldTIBIndex;
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
  Dec(FSP, SizeOf(Integer))
  LDstStrAddr := PInteger(@FParameterStack[FSP])^;
  
  
  //Popup a Count(Byte) from Parameter stack
  Assert(FSP>=SizeOf(Byte), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Byte))
  LSrcCount := PByte(@FParameterStack[FSP])^;
  
  
  //Popup a PChar address from Parameter stack
  Assert(FSP>=SizeOf(PChar), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(PChar))
  LSrcPChar := PChar(@FParameterStack[FSP])^;
  
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
  Dec(FSP, SizeOf(Integer))
  LDstStrAddr := PInteger(@FParameterStack[FSP])^;
  
  
  //Popup a Count(Integer) from Parameter stack
  Assert(FSP>=SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer))
  LSrcCount := PByte(@FParameterStack[FSP])^;
  
  
  //Popup a PChar address from Parameter stack
  Assert(FSP>=SizeOf(PChar), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(PChar))
  LSrcPChar := PChar(@FParameterStack[FSP])^;
  
  iVMFillCountPChar(LSrcPChar, LSrcCount);
end;

procedure TSuperInterpreter.vSetRunning;
begin
  Include(FStatus, psRunning);
end;

procedure TSuperInterpreter.vSkipBlank;
begin
  while (FTextIndex <= Length(FText))
    {$IFDEF MBCS_SUPPORT}
    and (ByteType(FText, FTextIndex) = mbSingleByte)
    {$ENDIF}
    and (FText[FTexrIndex] = ' ')
  do begin
    Inc(FTextIndex);
  end;
end;


end.
