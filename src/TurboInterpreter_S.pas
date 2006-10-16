{: the pure pascal impl. }
unit TurboInterpreter_S;

interface

{$I Setting.inc}

uses
  SysUtils, Classes
  , uTurboScriptConsts
  , uTurboExecutor
  //, uTurboInterpretor 
  ;

resourcestring
  rsVisitMemoryExceed = 'Visit Memory Index Exceed!';
  
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
    LastWord: Integer; //PForthWord; //ǰһ������ 0 means Ϊ��ǰ�档
    NameLen: Byte;
    //Name: array[0..NameLen] of char; //it's a PChar, the last char is #0
  end;//}

  //For internal used(not the real memory mirror)
  //the Forth word attributes.
  TForthWordRec = packed record
    Options: TForthWordOptions;
    Name: string;
  end;

type
  {: The Virtual FORTH Interpreter Class(pure pascal impl) }
  { Description
  Return Stack:
    * SP        Return Stack Pointer
    * StackSize Return Stack Size
    * Stack     Return Stack Array

  Parameter Stack:
    * PSP    Parameter Stack Pointer(TOS: Top Of Stack)
    * ParameterStackSize
    * ParameterStack

  My Stack push is inc, pop is dec.
  The TOS always point to a new cell.

  Push a Integer:
    PInteger(SP)^ := aInt;
    Inc(SP, SizeOf(Integer));
  }
  TTurboInterpreter = class(TCustomTurboModule)
  private
    function GetPLibEntry: PForthWord;
    function GetTIB: string;
    procedure SetMemorySize(Value: Integer);
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetStackSize(const Value: Integer);
    procedure SetTIB(const Value: string);
  protected
    FInstrunction: TForthMethod;
    {: the internal core procedure list }
    FInternalProcList: TVMMethodList;
    FIP: Integer;
    FIR: TVMInstruction;
    {: The LAST in the FORTH }
    { Description
    the defining word.

    ���ڶ���ĵ���.
    }
    FLastWordEntryAddress: Integer;
    FLibEntryAddress: Integer;
    {: The Code Memory }
    FMemory: TMemoryArray;
    FMemorySize: Integer;
    {: : the Parameter Stack }
    FParameterStack: TStack;
    FParameterStackSize: Integer;
    FPC: Integer;
    FProcessorStates: TForthProcessorStates;
    FRP: Integer;
    {: the Parameter Stack(or data stack) Pointer }
    FSP: Integer;
    {: : Return(Proc) Stack }
    { Description
    ���ض�ջ
    }
    FStack: TStack;
    FStackSize: Integer;
    {: The Current TIB Index }
    { Description
    FTIBIndex : Text[FTIBIndex]
    }
    FTextIndex: Integer;
    FUsedMemory: Integer;
    FWRegister: Integer;
    function iExecuteCFA(const aCFA: Integer): Integer; override;
    procedure InitExecution; override;
    procedure InitProcList;
    { Description
    6.1.0705 ALIGN 
    CORE 

            ( -- )

    If the data-space pointer is not aligned, reserve enough space to align it. 
    }
    procedure iVMAlignMem;
    { Description
    Forth subroutine enter procedure.
    }
    procedure iVMEnter;
    { Description
    Forth subroutine exit procedure.
    }
    procedure iVMExit;
    {: Alloc and Fill sValue to the Memory for Compile }
    { Description
    the UsedMemory pointer incresed automaticlly
    (FC)
    }
    procedure iVMFill(const aValue;  const Size: Integer);
    {: Alloc and Fill sValue to the Memory for Compile }
    { Description
    the UsedMemory pointer incresed automaticlly
    (FC)
    }
    procedure iVMFillByte(const aValue: Byte);
    {: Alloc and Fill aValue to the Memory for Compile }
    { Description
    Pls Dont include #0 in PChar length!!!
    }
    procedure iVMFillCountPChar(const aValue: PChar; const aSize: Integer);
    {: add the word name header to free memory. }
    procedure iVMFillForthWordHeader(const aWordAttr: TForthWordRec);
    {: Alloc and Fill sValue to the Memory for Compile }
    { Description
    the UsedMemory pointer incresed automaticlly
    (FC)
    }
    procedure iVMFillInt(const aValue: Integer);
    {: Alloc and Fill aValue to the Memory for Compile }
    { Description
    Pls Dont include #0 in PChar length!!!
    }
    procedure iVMFillShortCountPChar(const aValue: PChar; const aSize: Byte);
    {: Alloc and Fill aValue to the Memory for Compile }
    procedure iVMFillShortString(const aValue: string);
    {: Alloc and Fill aValue to the Memory for Compile }
    procedure iVMFillString(const aValue: string);
    {: Alloc and Fill sValue to the Memory for Compile }
    { Description
    the UsedMemory pointer incresed automaticlly
    (FC)
    }
    procedure iVMFillWord(const aValue: Word);
    procedure iVMHalt;
    procedure iVMNext;
    {: the defining forth word done! }
    { Description
    update the LibEntryAddress
    }
    procedure iVMRevel;
    {: Add the Integer in the param stack }
    { Description
    (n1 n2 -- n3)

    n3 := n1 + n2
    }
    procedure vAddInt;
    {: The CORE FORTH Words: ALIGNED }
    { Description
    6.1.0706 ALIGNED 
    CORE 

            ( addr -- a-addr )

    a-addr is the first aligned address greater than or equal to addr. 

    See: 3.3.3.1 Address alignment, 6.1.0705 ALIGN 
    }
    procedure vAligned;
    {: C@ }
    { Description
    c-fetch CORE 

            ( c-addr -- char )

    Fetch the character stored at c-addr. 
    When the cell size is greater than 
    character size, the unused high-order 
    bits are all zeroes. 

    See: 3.3.3.1 Address alignment 
    }
    procedure vCFetch;
    {: CONTEXT }
    { Description
    CONTEXT CORE EXT 

            ( -- c-addr )

    c-addr is the address of the Words. 

    �ֵ�����ָ��

    the CONTEXT ������Ϊһ���û�����ʵ�֣�
    }
    procedure vCONTEXT;
    {: Get the string Count  }
    { Description
    ( c-addr -- PChar-addr u )

    Return the character string specification for 
    the counted string stored at c-addr. PChar-addr 
    is the address of the first character after 
    c-addr. u is the contents of the character at 
    c-addr, which is the length in characters of 
    the string at PChar-addr. 
    }
    procedure vCount;
    {: Get the shortstring Count  }
    { Description
    ( c-addr1 -- c-addr2 b )

    Return the character string specification for 
    the counted string stored at c-addr1. c-addr2 
    is the address of the first character after 
    c-addr1. b(byte) is the contents of the character at 
    c-addr1, which is the length in characters of 
    the string at c-addr2. 
    }
    procedure vCountShort;
    {: C! }
    { Description
    c-store CORE 

            ( char c-addr -- )

    Store char at c-addr. When character size is 
    smaller than cell size, only the number of 
    low-order bits corresponding to character 
    size are transferred. 

    See: 3.3.3.1 Address alignment 
    }
    procedure vCStore;
    {: @ }
    { Description
    fetch CORE 

            ( a-addr -- x )

    x is the value stored at a-addr. 

    See: 3.3.3.1 Address alignment 
    }
    procedure vFetch;
    {: Push the HERE(FreeMemoryIndex: UsedMemory) to param stack }
    { Description
    }
    procedure vHERE;
    {: LAST }
    { Description
    LAST CORE EXT 

            ( -- c-addr )

    c-addr is the address of the LAST Word. 

    point to �ֵ������һWORD

    the LAST ������Ϊһ���û�����ʵ�֣�
    }
    procedure vLAST;
    {: PARSE }
    { Description
    CORE EXT 

            ( char "ccc<char>" -- c-addr u )

    Parse ccc delimited by the delimiter char. 

    c-addr is the address (within the input buffer) 
    and u is the length of the parsed string. If 
    the parse area was empty, the resulting string 
    has a zero length. 


    �������������vTIB��Text Input Buffer�w��ȡ����һ�����ַ�
    char���ָ���ַ������ҵ����ַ����󣬽������ڵ�ַc-addr��
    ����u���ڶ�ջ�ϡ� 
    }
    procedure vPARSE;
    {: Place the short string to Mem. }
    { Description
    (src-addr bLen dst-addr -- )

    src-addr: �ַ�����ַ(PChar)
    bLen:  �ַ�������(Byte)
    dst-addr: �����Ŀ���ַ
    }
    procedure vPlaceShortString;
    {: Place the string to Mem. }
    { Description
    (src-addr uLen dst-addr -- )

    src-addr: �ַ�����ַ(PChar)
    uLen:  �ַ�������(DWORD)
    dst-addr: �����Ŀ���ַ
    }
    procedure vPlaceString;
    {: change the VM state to running state. }
    procedure vSetRunning;
    {: Skip the blanks in the TIB }
    { Description
    Update the FTextIndex(current TIB Index)
    }
    procedure vSkipBlank;
    {: ! }
    { Description
    store CORE 

            ( x a-addr -- )

    Store x at a-addr. 

    See: 3.3.3.1 Address alignment 
    }
    procedure vStore;
    {: Substract the Integer in the param stack }
    { Description
    (n1 n2 -- n3)

    n3 := n1 - n2
    }
    procedure vSubInt;
    {: TIB }
    { Description
    t-i-b CORE EXT 

            ( -- c-addr )

    c-addr is the address of the terminal input buffer. 

    Note: This word is obsolescent and is included as a concession to existing
    implementations.

    See: A.6.2.2290 TIB , RFI 0006. 


    the TIB ������Ϊһ���û�����ʵ�֣�
    }
    procedure vTIB;
    {: #TIB }
    { Description
    number-t-i-b CORE EXT 

            ( -- a-addr )

    a-addr is the address of a cell containing the number of characters in the
    terminal input buffer.

    Note: This word is obsolescent and is included as a concession to existing
    implementations.

    See: A.6.2.0060 #TIB 


    ���Ҳ������Ϊһ���û�����ʵ�֣�
    }
    procedure vTIBNum;
    {: >IN }
    { Description
    to-in CORE 

            ( -- a-addr )

    a-addr is the address of a cell containing the offset 
    in characters from the start of the input buffer to 
    the start of the parse area. 


    Ҳ������Ϊ�û�����ʵ�֡�
    }
    procedure vToIN;
  public
    constructor Create(const aParamStack: TStack = nil); virtual;
    {: : Only execute the instruction on the current IP  }
    { Description
    Run the Virtual Machine from the PC adress.

    @Param aInstruction execute the aInstruction.


    Note: if the Instruction is multi-Bytes Instruction, then 
        you should add the left bytes size, do not include the SizeOf(
        TInstruction)
    }
    procedure ExecuteInstruction; overload;
    {: : Only execute one virtual machine instruction. }
    { Description
    Run the Virtual Machine from the PC adress.

    @Param aInstruction execute the aInstruction.


    Note: if the Instruction is multi-Bytes Instruction, then 
        you should add the left bytes size, do not include the SizeOf(
        TInstruction)
    }
    procedure ExecuteInstruction(const aInstruction: TVMInstruction); overload;
    {: create a Forth word header by internal }
    { Description
    only used in internal:

    iForthHeader(':');          FillInt(xdcoma);     FillInt(xPEXIT);
    }
    procedure iForthHeader(const aWordAttr: TForthWordRec);
    procedure LoadFromStream(const aStream: TStream); override;
    procedure SaveToStream(const aStream: TStream); override;
    property Instrunction: TForthMethod read FInstrunction;
    {: the VM IP(offset of the FMemory) }
    property IP: Integer read FIP write FIP;
    {: : instruction register(Current instruction). }
    { Description
    instruction register, in which is held 
    the instruction currently being executed. 

    Abondon.
    }
    property IR: TVMInstruction read FIR;
    {: the lib entry address (index) }
    { Description
    it's a index of FMemory.
    }
    property LibEntryAddress: Integer read FLibEntryAddress;
    {: : the Memory Size. }
    { Description
    warining: if in the running status, you may be get trouble!!
    }
    property MemorySize: Integer read FMemorySize write SetMemorySize;
    property ParameterStackSize: Integer read FParameterStackSize write
            SetParameterStackSize;
    {: : program counter. }
    { Description
    program counter, which contains the address 
    in memory of the instruction that is the next 
    to be executed. 
    }
    property PC: Integer read FPC write FPC;
    property PLibEntry: PForthWord read GetPLibEntry;
    {: : the Status of the Processor Register. }
    { Description
    Chinese
      ״̬�Ĵ���
    }
    property ProcessorStates: TForthProcessorStates read FProcessorStates;
    {: : return stack pointer(TOS). }
    { Description
    stack pointer, a register that points to the area 
    in memory utilized as the main return stack.

    the RP0-StackSize <= the stack memory < RP0.
    }
    property RP: Integer read FRP write FRP;
    {: the Parameter Stack(or data stack) Pointer }
    property SP: Integer read FSP write FSP;
    {: : the Stack Size. }
    property StackSize: Integer read FStackSize write SetStackSize;
    {: the script source(TIB) }
    { Description
    TIB: text input Buffer
    }
    property TIB: string read GetTIB write SetTIB;
    {: �Ѿ�ʹ�õ��ڴ� }
    { Description
    Ҳ����ָ�����Ŀ����ڴ棺
    �Ӹõ�ַ����ڴ�δ�ã�FMemory[UsedMemory] 
    }
    property UsedMemory: Integer read FUsedMemory write FUsedMemory;
    property WRegister: Integer read FWRegister;
  end;


implementation

{
****************************** TTurboInterpreter *******************************
}
constructor TTurboInterpreter.Create(const aParamStack: TStack = nil);
begin
  inherited Create;
  StackSize := cDefaultStackSize;
  if aParamStack <> nil then FParameterStack := aParamStack;
  ParameterStackSize := cDefaultParamStackSize;
  TMethod(FInstrunction).Data := Self;
end;

procedure TTurboInterpreter.ExecuteInstruction;
begin
  while (FPC < FMemorySize) and (psRunning in Status) do
  begin
    FIR := TVMInstruction(FMemory[PC]);
    ExecuteInstruction(FIR);
    Inc(FPC, SizeOf(TVMInstruction));
  end;
end;

procedure TTurboInterpreter.ExecuteInstruction(const aInstruction:
        TVMInstruction);
begin
  TMethod(FInstrunction).Code := FInternalProcList[aInstruction];
  //Run the Instrunction
  FInstrunction;
end;

function TTurboInterpreter.GetPLibEntry: PForthWord;
begin
  Result := PForthWord(@FMemory[LibEntryAddress]);
end;

function TTurboInterpreter.GetTIB: string;
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

function TTurboInterpreter.iExecuteCFA(const aCFA: Integer): Integer;
begin
  Assert(aCFA < FMemorySize, rsVisitMemoryExceed);
  FIP := aCFA;
  Include(FProcessorStates, psRunning);
  iVMNext;
end;

procedure TTurboInterpreter.iForthHeader(const aWordAttr: TForthWordRec);
begin
  iVMFillForthWordHeader(aWordAttr);
  iVMRevel;
end;

procedure TTurboInterpreter.InitExecution;
begin
  inherited InitExecution;
  FPC := 0;
  FRP := 0;
  FSP := 0;

  FIP := 0;

  //FTIBIndex := 1;

  //SetLength(FMemory, cLastWordEntryOffset+1);
end;

procedure TTurboInterpreter.InitProcList;
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

procedure TTurboInterpreter.iVMAlignMem;
begin
  Inc(FUsedMemory, (SizeOf(Pointer)-1));
  FUsedMemory := FUsedMemory and -SizeOf(Pointer);
end;

procedure TTurboInterpreter.iVMEnter;
begin
  //PUSH IP to Reutrun Stack
  Assert(FSP + SizeOf(Integer) <= Length(FStack), rsReturnStackOverflowError);
  PInteger(@FStack[FSP])^ := IP;
  Inc(FSP, SizeOf(Integer));

  Inc(FWRegister, SizeOf(Pointer));

  IP := FWRegister;

  iVMNext;
end;

procedure TTurboInterpreter.iVMExit;
begin
  if FSP > 0 then
  begin
    //POP IP From RS
    Dec(FSP, SizeOf(Integer));
    //Assert(FSP >= 0, rsReturnStackUnderflowError);
    FIP := PInteger(@FStack[SP])^;
    iVMNext;
  else begin
    iVMHalt;
  end;
end;

procedure TTurboInterpreter.iVMFill(const aValue;  const Size: Integer);
begin
  if UsedMemory + Size > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + Size + (SizeOf(Pointer)-1));
  end;
  Move(aValue, FMemory[UsedMemory], Size);
  Inc(FUsedMemory, Size);
end;

procedure TTurboInterpreter.iVMFillByte(const aValue: Byte);
begin
  if UsedMemory + SizeOf(aValue) > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + SizeOf(aValue)+ (SizeOf(Pointer)-1));
  end;
  PByte(@FMemory[UsedMemory])^ := aValue;
  Inc(FUsedMemory, SizeOf(aValue));
end;

procedure TTurboInterpreter.iVMFillCountPChar(const aValue: PChar; const aSize:
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

procedure TTurboInterpreter.iVMFillForthWordHeader(const aWordAttr:
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

procedure TTurboInterpreter.iVMFillInt(const aValue: Integer);
begin
  if UsedMemory + SizeOf(aValue) > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + SizeOf(aValue));
  end;
  Pinteger(@FMemory[UsedMemory])^ := aValue;
  Inc(FUsedMemory, SizeOf(aValue));
end;

procedure TTurboInterpreter.iVMFillShortCountPChar(const aValue: PChar; const
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

procedure TTurboInterpreter.iVMFillShortString(const aValue: string);
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

procedure TTurboInterpreter.iVMFillString(const aValue: string);
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

procedure TTurboInterpreter.iVMFillWord(const aValue: Word);
begin
  if UsedMemory + SizeOf(aValue) > Length(FMemory) then
  begin
    SetLength(FMemory, UsedMemory + SizeOf(aValue)+ (SizeOf(Pointer)-1));
  end;
  PWord(@FMemory[UsedMemory])^ := aValue;
  Inc(FUsedMemory, SizeOf(aValue));
end;

procedure TTurboInterpreter.iVMHalt;
begin
  Exclude(FProcessorStates, psRunning);
end;

procedure TTurboInterpreter.iVMNext;
begin
  {(IP) -> W  fetch memory pointed by IP into "W" register
                ...W now holds address of the Code Field(CFA)
  IP + 2 -> IP //(assuming 2-byte addresses in the thread)
  (W) -> X  // the machine code address of CFA in W
  JMP (X)
  }

  FWRegister := PInteger(@FMemory[IP])^;
  Inc(FIP, SizeOf(Integer));

  //JMP (X)
  Assert(FWRegister < FMemorySize, rsVisitMemoryExceed);
  FPC := FWRegister;
  //FIR := FMemory[FWRegister];
  ExecuteInstruction;
end;

procedure TTurboInterpreter.iVMRevel;
begin
  FLibEntryAddress := FLastWordEntryAddress;
end;

procedure TTurboInterpreter.LoadFromStream(const aStream: TStream);
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

procedure TTurboInterpreter.SaveToStream(const aStream: TStream);
begin
  inherited SaveToStream(aStream);
end;

procedure TTurboInterpreter.SetMemorySize(Value: Integer);
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

procedure TTurboInterpreter.SetParameterStackSize(const Value: Integer);
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

procedure TTurboInterpreter.SetStackSize(const Value: Integer);
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

procedure TTurboInterpreter.SetTIB(const Value: string);
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

procedure TTurboInterpreter.vAddInt;
begin
  //����ջ��Ӧ������������
  Assert(FSP>=2*SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  PInteger(@FParameterStack[FSP-SizeOf(Integer)])^ :=
    PInteger(@FParameterStack[FSP-SizeOf(Integer)])^ +
    PInteger(@FParameterStack[FSP])^;
end;

procedure TTurboInterpreter.vAligned;
var
  I: Integer;
begin
  //����ջ��Ӧ���и�����
  I := FSP-SizeOf(Integer);
  Assert(I>=0, rsParamStackUnderflowError);

  Inc(PInteger(@FParameterStack[I])^, (SizeOf(Pointer)-1));
  PInteger(@FParameterStack[I])^ := PInteger(@FParameterStack[I])^ and -SizeOf(Pointer);
end;

procedure TTurboInterpreter.vCFetch;
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

procedure TTurboInterpreter.vCONTEXT;
begin
  //Push the CONTEXT Address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);

  PInteger(@FParameterStack[FSP])^ := LibEntryAddress;
  Inc(FSP, SizeOf(Integer));
end;

procedure TTurboInterpreter.vCount;
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

procedure TTurboInterpreter.vCountShort;
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

procedure TTurboInterpreter.vCStore;
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

procedure TTurboInterpreter.vFetch;
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

procedure TTurboInterpreter.vHERE;
begin
  //Push the current UsedMemory to param stack
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);
  PInteger(@FParameterStack[FSP])^ := FUsedMemory;
  Inc(FSP, SizeOf(Integer));
end;

procedure TTurboInterpreter.vLAST;
begin
  //Push the LAST-WORD Address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);

  PInteger(@FParameterStack[FSP])^ := FLastWordEntryAddress;
  Inc(FSP, SizeOf(Integer));
end;

procedure TTurboInterpreter.vPARSE;
var
  LTIBIndex: Integer;
  LSepChar: Char;
  LTIBLen: Integer;
  LOldTIBIndex: Integer;
begin
  //Popup a Char(Byte) from Parameter stack
  Assert(FSP>=1, rsParamStackUnderflowError);
  Dec(FSP, 1);
  //�ָ��
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

procedure TTurboInterpreter.vPlaceShortString;
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

procedure TTurboInterpreter.vPlaceString;
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

procedure TTurboInterpreter.vSetRunning;
begin
  Include(FProcessorStates, psRunning);
end;

procedure TTurboInterpreter.vSkipBlank;
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

procedure TTurboInterpreter.vStore;
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

procedure TTurboInterpreter.vSubInt;
begin
  //����ջ��Ӧ������������
  Assert(FSP>=2*SizeOf(Integer), rsParamStackUnderflowError);
  Dec(FSP, SizeOf(Integer));
  PInteger(@FParameterStack[FSP-SizeOf(Integer)])^ :=
    PInteger(@FParameterStack[FSP-SizeOf(Integer)])^ -
    PInteger(@FParameterStack[FSP])^;
end;

procedure TTurboInterpreter.vTIB;
begin
  //Push the TIB Address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);

  PInteger(@FParameterStack[FSP])^ := cTIBOffset;
  Inc(FSP, SizeOf(Integer));
end;

procedure TTurboInterpreter.vTIBNum;
begin
  //Push the TIB Length address
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);

  PInteger(@FParameterStack[FSP])^ := cTIBLengthOffset;
  Inc(FSP, SizeOf(Integer));
end;

procedure TTurboInterpreter.vToIN;
begin
  //Push the TIB index
  Assert(FSP + SizeOf(Integer)<= Length(FParameterStack), rsParamStackUnderflowError);

  PInteger(@FParameterStack[FSP])^ := cToINOffset;
  Inc(FSP, SizeOf(Integer));
end;


end.
