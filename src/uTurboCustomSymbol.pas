unit uTurboCustomSymbol;

interface

{$I Setting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , uMeObject
  , uMeTypes
  , uMeProcType
  , uTurboConsts
  ;

Const
  cCompilerSymanticMask = $10000000;
  cCompilerWarningMask  = $20000000;
  cCompilerHintMask     = $30000000;
  cSymbolErrorOk = 0;
  cSymbolErrorNoRefCount = 312 or cCompilerHintMask;

type
  PTurboSymbols = ^ TTurboSymbols;
  PTurboCustomSymbol = ^ TTurboCustomSymbol;

  //the Compiler option state
  TTurboCompilerOptionState = (cosDefault, cosEnable, cosDisable);
  TTurboCompilerErrorEvent = procedure(const Sender: PTurboCustomSymbol; const aErrCode: Integer) of object;


  PTurboUnResolvedRefRec = ^ TTurboUnResolvedRefRec;
  TTurboUnResolvedRefRec = packed record
    Symbol: PTurboCustomSymbol;
    Addr: tsInt; //this should be offset address only!!
  end;

  PTurboUnResolvedRefs = ^ TTurboUnResolvedRefs;
  TTurboUnResolvedRefs = Object(TMeList)
  protected
    function GetItems(Index: Integer): PTurboUnResolvedRefRec;
  public
    destructor Destroy; virtual;(*override;*)
    procedure Clear;
    function Add(): PTurboUnResolvedRefRec;
    procedure Assign(const aSrc: PTurboUnResolvedRefs);
  public
    property Items[Index: Integer]: PTurboUnResolvedRefRec read GetItems;
  end;

  {: abstract Symbol }
  TTurboCustomSymbol = Object(TMeDynamicObject)
  protected
    FRefCount: Longint;
    FIsCompiled: Boolean;
    //store the address need to resolve.
    FUnResolvedRefs: PTurboUnResolvedRefs;
    FOnError: TTurboCompilerErrorEvent;
  protected
    {: 将该符号压入到 aSymbol 中(包括opCode), 如果遇到无法resolve的地址则将其加入到 UnResolvedRefs 列表.}
    function iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer; virtual; abstract;
    function iCompile: Integer; virtual; abstract;
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); virtual;abstract;
    function IsCompileAllowed: Boolean; virtual;
    function GetUnResolvedRefs: PTurboUnResolvedRefs;
    procedure CompileError(const aErrCode: Integer);
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    {: 引用计数+1; 将该符号压入到 aSymbol 中(包括opCode).}
    function ReferenceTo(const aSymbol: PTurboCustomSymbol): Integer;
    function Compile: Integer; //return the result code: cSymbolErrorXXX
    procedure ResolveRefs;
  public
    Name: String;
    //the symbol defined position in the src file:
    Line: Integer;
    Column: Integer;
    //被引用的次数
    property RefCount: Longint read FRefCount;
    property UnResolvedRefs: PTurboUnResolvedRefs read GetUnResolvedRefs;
    property IsCompiled: Boolean read FIsCompiled;
    property OnError: TTurboCompilerErrorEvent read FOnError write FOnError;
  end;

  TTurboSymbols = object(TMeList)
  protected
    function GetItem(Index: Integer): PTurboCustomSymbol;
  public
    destructor Destroy; virtual;{override}
    function Compile: Integer;
    procedure Clear;
    procedure Assign(const aSymbols: PTurboSymbols);
    function IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer; overload;
    function Find(const aName: string): PTurboCustomSymbol;
  public
    property Items[Index: Integer]: PTurboCustomSymbol read GetItem; default;
  end;


function IsSymbolOk(const aErrorCode: Integer): Boolean;

implementation

function IsSymbolOk(const aErrorCode: Integer): Boolean;
begin
  Result := aErrorCode = cSymbolErrorOk;
  if not Result then
    Result := (aErrorCode and cCompilerHintMask) = cCompilerHintMask;
  if not Result then
    Result := (aErrorCode and cCompilerWarningMask) = cCompilerWarningMask;
end;

{TTurboUnResolvedRefs}
destructor TTurboUnResolvedRefs.Destroy;
begin
  Clear;
  Inherited;
end;

procedure TTurboUnResolvedRefs.Clear;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    Dispose(Items[i]);
  Inherited Clear;
end;

procedure TTurboUnResolvedRefs.Assign(const aSrc: PTurboUnResolvedRefs);
var
  i: integer;
  vItem: PTurboUnResolvedRefRec;
begin
  Clear;
  if Assigned(aSrc) then
  begin
    for i := 0 to aSrc.Count - 1 do
    begin
      vItem := Add;
      vItem^ := aSrc.Items[i]^;
    end;
  end;
end;

function TTurboUnResolvedRefs.Add(): PTurboUnResolvedRefRec;
begin
  New(Result);
  if Inherited Add(Result) < 0 then
  begin
    Dispose(Result);
    Result := nil;
  end;
end;

function TTurboUnResolvedRefs.GetItems(Index: Integer): PTurboUnResolvedRefRec;
begin
  Result := Get(Index);
end;

{ TTurboCustomSymbol }
destructor TTurboCustomSymbol.Destroy;
begin
  Name := '';
  Inherited;
end;

procedure TTurboCustomSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboCustomSymbol)) then
  begin
    Name := aSymbol.Name;
    Line := aSymbol.Line;
    Column := aSymbol.Column;
    FRefCount := aSymbol.FRefCount;
    FIsCompiled := aSymbol.FIsCompiled;
    FOnError := aSymbol.FOnError;
    if Assigned(aSymbol.FUnResolvedRefs) then
      UnResolvedRefs.Assign(aSymbol.FUnResolvedRefs)
    else
      MeFreeAndNil(FUnResolvedRefs);
  end;
end;

procedure TTurboCustomSymbol.CompileError(const aErrCode: Integer);
begin
  if Assigned(FOnError) then
    FOnError(@Self, aErrCode);
  if not IsSymbolOk(aErrCode) then
    Raise Exception.Create(Name +  ' ErrorCode: '+ IntTOStr(aErrCode));
end;

function TTurboCustomSymbol.GetUnResolvedRefs: PTurboUnResolvedRefs;
begin
  if not Assigned(FUnResolvedRefs) then
    New(FUnResolvedRefs, Create);
  Result := FUnResolvedRefs;
end;

function TTurboCustomSymbol.Compile: Integer; //return the result code: cSymbolErrorXXX
begin
  Result := cSymbolErrorOk;
  if not FIsCompiled then
  begin
    if IsCompileAllowed then
    begin
      Result := iCompile;
      if IsSymbolOk(Result) then
      begin
        FIsCompiled := True;
        ResolveRefs;
      end;
    end
    else begin
      Result := cSymbolErrorNoRefCount;
      //writeln( 'cSymbolErrorNoRefCount ', Name)
    end;
  end;
  //else
    //Result := cSymbolErrorRedeclaration;
  if Result <> cSymbolErrorOk then
    CompileError(Result);
end;

function TTurboCustomSymbol.ReferenceTo(const aSymbol: PTurboCustomSymbol): Integer;
begin
  Inc(FRefCount);
  Result := iReferenceTo(aSymbol);
end;

procedure TTurboCustomSymbol.ResolveRefs;
var
  i: Integer;
begin
  if FIsCompiled and Assigned(FUnResolvedRefs) then
    with FUnResolvedRefs^ do
    begin
      for i := 0 to Count -1 do
      begin
        ResolveAddr(Items[i]);
      end;
      Clear;
    end;
end;

function TTurboCustomSymbol.IsCompileAllowed: Boolean;
begin
  Result := FRefCount > 0;
end;

{ TTurboSymbols }
destructor TTurboSymbols.Destroy;
begin
  FreeMeObjects;
  inherited;
end;

procedure TTurboSymbols.Clear;
begin
  FreeMeObjects;
  inherited;
end;

procedure TTurboSymbols.Assign(const aSymbols: PTurboSymbols);
var
  i: integer;
begin
  if Assigned(aSymbols) then
  begin
    Clear;
    Count := aSymbols.Count;
    for i := 0 to Count - 1 do
    begin
      Add(NewMeObject(PTurboCustomSymbol(aSymbols.List^[i]).ClassType));
    end;
  end;
end;

function TTurboSymbols.Compile: Integer;
var
  i: Integer;
begin
  Result := cSymbolErrorOk;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].Compile;
    //writeln(Items[i].name, ' Compiled:', Result);
    if not IsSymbolOk(Result) then Exit;
  end;
end;

function TTurboSymbols.GetItem(Index: Integer): PTurboCustomSymbol;
begin
  Result := Inherited Get(Index);
end;

function TTurboSymbols.Find(const aName: string): PTurboCustomSymbol;
var
  i: integer;
begin
  i := IndexOf(aName);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

function TTurboSymbols.IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    if AnsiSameText(aName, Items[Result].Name) then
      exit;
  end;
  Result := -1;
end;

initialization
  SetMeVirtualMethod(TypeOf(TTurboCustomSymbol), ovtVmtParent, TypeOf(TMeDynamicObject));
  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TTurboCustomSymbol), ovtVmtClassName, nil);
  {$ENDIF}
end.
