unit CocoBase;
{Base components for Coco/R for Delphi grammars for use with version 1.3}

{$DEFINE REMOVE_DEPRECATED}

interface

uses
  Classes, SysUtils
  , uStrUtils
  , uTurboConsts
  , uTurboMetaInfo
  , uTurboExecutor
  , uTurboAccessor
  , uTurboModuleFileAccessor
  , uTurboCompilerUtils
  ;

const
  setsize = 16; { sets are stored in 16 bits }

  { Standard Error Types }
  etSyntax = 0;
  etSymantic = 1;
  etWarn = 2;
  etHint = 3;

  chCR = #13;
  chLF = #10;
  chNull = #0;
  chSpace = #32;
  chTab = #9;
  chEOL = chCR + chLF;  { End of line characters for Microsoft Windows }
  chLineSeparator = chCR;

Const
  cMaxMemorySize = 64 * 1024;
  cLabelRedeclarationError = 300;
  cWordNameIsNilDeclarationError = 301;
  cUnknownWordError = 302;
  cVarRedeclarationError = 303;
  cConstRedeclarationError = 304;
  cRedeclarationError = 305;
  cDLLModuleMissError = 306;
  cFileNotFoundError  = 307;
  cWordNotFoundError  = 308;
  cStrToIntCovnertError = 309;
  cInvalidOptionError = 400;
  cInvalidOptionParamError = 401;
  cMessageCompilerOption = 500;

Const
  cReqAlignMemTypes = [ttkUWord, ttkSWord, ttkULong, ttkSLong, ttkPointer, ttkString, ttkLString, ttkInt64, ttkQWord];

type
  ECocoBookmark = class(Exception);
  TCocoStatusType = (cstInvalid, cstBeginParse, cstEndParse, cstLineNum, cstString);
  TCocoError = class(TObject)
  private
    FErrorCode : integer;
    FCol : integer;
    FLine : integer;
    FData : string;
    FErrorType : integer;
    function FixXmlStr(const Str : string) : string;
  public
    function ExportToXMLFragment(
        const ErrorTypeDesc : string;
        const ErrorText : string;
        const ErrorSeverity : string) : string;

    property ErrorType : integer read FErrorType write FErrorType;
    property ErrorCode : integer read FErrorCode write FErrorCode;
    property Line : integer read FLine write FLine;
    property Col : integer read FCol write FCol;
    property Data : string read FData write FData;
  end; {TCocoError}

  TCommentItem = class(TObject)
  private
    fComment: string;
    fLine: integer;
    fColumn: integer;
  public
    property Comment : string read fComment write fComment;
    property Line : integer read fLine write fLine;
    property Column : integer read fColumn write fColumn;
  end; {TCommentItem}

  TCommentList = class(TObject)
  private
    fList : TList;

    function FixComment(const S : string) : string;
    function GetComments(Idx: integer): string;
    procedure SetComments(Idx: integer; const Value: string);
    function GetCount: integer;
    function GetText: string;
    function GetColumn(Idx: integer): integer;
    function GetLine(Idx: integer): integer;
    procedure SetColumn(Idx: integer; const Value: integer);
    procedure SetLine(Idx: integer; const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const S : string; const aLine : integer; const aColumn : integer);
    property Comments[Idx : integer] : string read GetComments write SetComments; default;
    property Line[Idx : integer] : integer read GetLine write SetLine;
    property Column[Idx : integer] : integer read GetColumn write SetColumn;
    property Count : integer read GetCount;
    property Text : string read GetText;
  end; {TCommentList}

  TSymbolPosition = class(TObject)
  private
    fLine : integer;
    fCol : integer;
    fLen : integer;
    fPos : integer;
  public
    procedure Clear;
    procedure Assign(Source : TSymbolPosition);

    property Line : integer read fLine write fLine; {line of symbol}
    property Col : integer read fCol write fCol; {column of symbol}
    property Len : integer read fLen write fLen; {length of symbol}
    property Pos : integer read fPos write fPos; {file position of symbol}
  end; {TSymbolPosition}

  TGenListType = (glNever, glAlways, glOnError);

  TBitSet = set of 0..15;
  PStartTable = ^TStartTable;
  TStartTable = array[0..255] of integer;
  TCharSet = set of char;

  TAfterGenListEvent = procedure(Sender : TObject;
    var PrintErrorCount : boolean) of object;
  TAfterGrammarGetEvent = procedure(Sender : TObject;
    var CurrentInputSymbol : integer) of object;
  TCommentEvent = procedure(Sender : TObject; CommentList : TCommentList) of object;
  TCustomErrorEvent = function(Sender : TObject; const ErrorCode : longint;
    const Data : string) : string of object;
  TErrorEvent = procedure(Sender : TObject; Error : TCocoError) of object;
  TErrorProc = procedure(const ErrorCode : integer; const Symbol : TSymbolPosition;
    const Data : string; const ErrorType : integer) of object;
  TFailureEvent = procedure(Sender : TObject; NumErrors : integer) of object;
  TGetCH = function(pos : longint) : char of object;
  TStatusUpdateProc = procedure(Sender : TObject;
      const StatusType : TCocoStatusType;
      const Status : string;
      const LineNum : integer) of object;
  TFunctionSymbolPosition = function : TSymbolPosition of object;

  TCocoRScanner = class(TObject)
  private
    FbpCurrToken : integer; {position of current token)}
    FBufferPosition : integer; {current position in buf }
    FContextLen : integer; {length of appendix (CONTEXT phrase)}
    FCurrentCh : TGetCH; {procedural variable to get current input character}
    FCurrentSymbol : TSymbolPosition; {position of the current symbol in the source stream}
    FCurrInputCh : char; {current input character}
    FCurrLine : integer; {current input line (may be higher than line)}
    FLastInputCh : char; {the last input character that was read}
    FNextSymbol : TSymbolPosition; {position of the next symbol in the source stream}
    FNumEOLInComment : integer; {number of _EOLs in a comment}
    FOnStatusUpdate : TStatusUpdateProc;
    FScannerError : TErrorProc;
    FSourceLen : integer; {source file size}
    FSrcStream : TMemoryStream; {source memory stream}
    FStartOfLine : integer;
    fLastSymbol: TSymbolPosition;

    function GetNStr(Symbol : TSymbolPosition; ChProc : TGetCh) : string;
    function ExtractBookmarkChar(var aBookmark: string): char;
  protected
    FStartState : TStartTable; {start state for every character}

    function Bookmark : string; virtual;
    procedure GotoBookmark(aBookmark : string); virtual;

    function CapChAt(pos : longint) : char;
    procedure Get(var sym : integer); virtual; abstract;
    procedure NextCh; virtual; abstract;

    function GetStartState : PStartTable;
    procedure SetStartState(aStartTable : PStartTable);

    property bpCurrToken : integer read fbpCurrToken write fbpCurrToken;
    property BufferPosition : integer read fBufferPosition write fBufferPosition;
    property LastSymbol : TSymbolPosition read fLastSymbol write fLastSymbol;
    property CurrentSymbol : TSymbolPosition read fCurrentSymbol write fCurrentSymbol;
    property NextSymbol : TSymbolPosition read fNextSymbol write fNextSymbol;
    property ContextLen : integer read fContextLen write fContextLen;
    property CurrentCh : TGetCh read fCurrentCh write fCurrentCh;
    property CurrInputCh : char read fCurrInputCh write fCurrInputCh;
    property CurrLine : integer read fCurrLine write fCurrLine;
    property LastInputCh : char read fLastInputCh write fLastInputCh;
    property NumEOLInComment : integer read fNumEOLInComment write fNumEOLInComment;
    property OnStatusUpdate : TStatusUpdateProc read FOnStatusUpdate write FOnStatusUpdate;
    property ScannerError : TErrorProc read FScannerError write FScannerError;
    property SourceLen : integer read fSourceLen write fSourceLen;
    property SrcStream : TMemoryStream read fSrcStream write fSrcStream;
    property StartOfLine : integer read fStartOfLine write fStartOfLine;
    property StartState : PStartTable read GetStartState write SetStartState;
  public
    constructor Create;
    destructor Destroy; override;

    function CharAt(pos : longint) : char;
    function GetName(Symbol : TSymbolPosition) : string; // Retrieves name of symbol of length len at position pos in source file
    function GetString(Symbol : TSymbolPosition) : string; // Retrieves exact string of max length len from position pos in source file
    procedure _Reset;
  end; {TCocoRScanner}

  TCocoRGrammar = class(TComponent)
  private
    fAfterGet: TAfterGrammarGetEvent;
    FAfterGenList : TAfterGenListEvent;
    FAfterParse : TNotifyEvent;
    FBeforeGenList : TNotifyEvent;
    FBeforeParse : TNotifyEvent;
    fClearSourceStream : boolean;
    FErrDist : integer; // number of symbols recognized since last error
    FErrorList : TList;
    fGenListWhen : TGenListType;
    FListStream : TMemoryStream;
    FOnCustomError : TCustomErrorEvent;
    FOnError : TErrorEvent;
    FOnFailure : TFailureEvent;
    FOnStatusUpdate : TStatusUpdateProc;
    FOnSuccess : TNotifyEvent;
    FScanner : TCocoRScanner;
    FSourceFileName : string;
    fExtra : integer;

    function GetSourceStream : TMemoryStream;
    procedure SetOnStatusUpdate(const Value : TStatusUpdateProc);
    procedure SetSourceStream(const Value : TMemoryStream);
    function GetLineCount: integer;
    function GetCharacterCount: integer;
  protected
    fCurrentInputSymbol : integer; // current input symbol
  
  protected
    //Common language turbo-compiler properties.
    FTurboGlobalOptions: TTurboGlobalOptions;
    FInitProcCFA: Integer;
    //正在定义的Word,如果存在 
    FDefinedWordEntry: PTurboMethodEntry;
    FLastWordCfa: Integer;
    FModule: TCustomTurboModule;

    FLabels: array of TTurboLabelDeclarationRec;
    FConsts: array of TTurboSimpleConst;
    FVars: array of TTurboSimpleVar;
    FWords: array of TTurboSimpleWord;
    FUsedModules: array of TTurboSimpleModule;

  protected
    procedure Init;
    procedure Final;
    function  DefineWordBegin(var aWord: TTurboSimpleWord): Boolean;
    procedure DefineWordEnd();
    procedure PushString(const aStr: string);
    procedure PushStringSeqToStack(const aStr: string);
    procedure PushInt32(const aStr: string);overload;
    procedure PushInt32(const aInt: tsInt);overload;
    //case the aValue type push the aValue to CodeMem
    procedure PushWordParam(const aValue: string);
    // Add Identifier CFA address into the memory. 
    function AddIdentifierCFA(const aName: String; const aParams: TStringList): Boolean;
    // AddWordCFA address into the memory. 
    function AddWordCFA(aName: String; const aParams: TStringList): Boolean;
    // Add Const address into the memory. 
    function AddConstCFA(const aName: String): Boolean;
    // Add Var address into the memory. 
    function AddVarCFA(const aName: String): Boolean;
  
    function DefineLabel(const aName: string; const aWordName: string =''): Integer;
    //if find retrun label index else -1
    function FindLabel(const aName: string; const aWordName: string =''): Integer;
    //if not find then add new label, return label index else raise error.
    function DefineLabelEx(const aName: string; const aWordName: string =''): Integer;
    function  DefineConst(const aValue: TTurboSimpleConst): Integer;
    //if not find then add new , return index else raise error.
    function  DefineConstEx(const aValue: TTurboSimpleConst): Integer;
    function FindConst(const aName: String): Integer;
    function GetConstValueRec(const aTypeKind: TTurboSimpleTypeKind; const aName: string): TTurboValueRec;
  
    //if not find then add new , return index else raise error.
    function DefineVar(const aValue: TTurboSimpleVar): Integer;
    function FindVar(const aName: String): Integer;
  
    //try find module if not then Generate.
    procedure GenerateModuleEntryForWord(var aWord: TTurboSimpleWord);
    function FindModule(const aName: String; aModuleType: TTurboModuleType): Integer;
    function  AddUsedModule(const aName: String; aModuleType: TTurboModuleType): Integer;
    function FindWord(const aName: String): Integer;
    
    //make sure the indentifier is unique else it will raise the error..
    function IsUniqueIdentifier(const aName: String): Boolean;

  protected
    function Bookmark : string; virtual;
    procedure GotoBookmark(aBookmark : string); virtual;
    function GetSuccessful : boolean; virtual;

    procedure ClearErrors;
    function ErrorStr(const ErrorCode : integer; const Data : string) : string; virtual; abstract;
    procedure Expect(n : integer);
    procedure GenerateListing;
    procedure Get; virtual; abstract;
    procedure PrintErr(line : string; ErrorCode, lnr, col : integer;
      Data : string);
    procedure StoreError(const nr : integer; const Symbol : TSymbolPosition;
      const Data : string; const ErrorType : integer);
    function CurrentBufferPosition : integer;

    //added by riceball
    procedure PrintStr(S : String); //write string to compiler console or output.
    procedure PrintStrLn(S : String);//write string to compiler console or output with line feed.

    procedure DoAfterParse; virtual;
    procedure DoBeforeParse; virtual;

    property ClearSourceStream : boolean read fClearSourceStream write fClearSourceStream default true;
    property CurrentInputSymbol : integer read fCurrentInputSymbol write fCurrentInputSymbol;
    property ErrDist : integer read fErrDist write fErrDist; // number of symbols recognized since last error
    property ErrorList : TList read FErrorList write FErrorList;
    property Extra : integer read fExtra write fExtra;
    property GenListWhen : TGenListType read fGenListWhen write fGenListWhen default glOnError;
    property ListStream : TMemoryStream read FListStream write FListStream;
    property SourceFileName : string read FSourceFileName write FSourceFileName;
    property SourceStream : TMemoryStream read GetSourceStream write SetSourceStream;
    property Successful : boolean read GetSuccessful;

    {Events}
    property AfterParse : TNotifyEvent read fAfterParse write fAfterParse;
    property AfterGenList : TAfterGenListEvent read fAfterGenList write fAfterGenList;
    property AfterGet : TAfterGrammarGetEvent read fAfterGet write fAfterGet;
    property BeforeGenList : TNotifyEvent read fBeforeGenList write fBeforeGenList;
    property BeforeParse : TNotifyEvent read fBeforeParse write fBeforeParse;
    property OnCustomError : TCustomErrorEvent read FOnCustomError write FOnCustomError;
    property OnError : TErrorEvent read fOnError write fOnError;
    property OnFailure : TFailureEvent read FOnFailure write FOnFailure;
    property OnStatusUpdate : TStatusUpdateProc read FOnStatusUpdate write SetOnStatusUpdate;
    property OnSuccess : TNotifyEvent read FOnSuccess write FOnSuccess;

  public
    FileName: string;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    {$IFNDEF REMOVE_DEPRECATED}
    procedure _StreamLine(const s : string); deprecated;
    procedure _StreamLn(const s : string); deprecated;
    {$ENDIF REMOVE_DEPRECATED}

    procedure GetLine(var pos : Integer; var line : string;
      var eof : boolean);
    function LastName: string;
    function LastString: string;
    function LexName : string;
    function LexString : string;
    function LookAheadName : string;
    function LookAheadString : string;
    procedure StreamToListFile(s : string; const AddEndOfLine : boolean);
    procedure ErrorMsg(const errNo : integer; const Data : string; const aErrType: integer);
    procedure SemError(const errNo : integer; const Data : string);
    procedure SynError(const errNo : integer; const Data : string = '');

    property Scanner : TCocoRScanner read fScanner write fScanner;
    property LineCount : integer read GetLineCount;
    property CharacterCount : integer read GetCharacterCount;
  end; {TCocoRGrammar}

const
  _EF = chNull;
  _TAB = #09;
  _CR = chCR;
  _LF = chLF;
  _EL = _CR;
  _EOF = #26; {MS-DOS eof}
  LineEnds : TCharSet = [_CR, _LF, _EF];
  { not only for errors but also for not finished states of scanner analysis }
  minErrDist = 2; { minimal distance (good tokens) between two errors }

function PadL(S : string; ch : char; L : integer) : string;
function StrTok(
    var Text : string;
    const ch : char) : string;

implementation

uses
  TypInfo;

const
  INVALID_CHAR = 'Invalid Coco/R for Delphi bookmark character';
  INVALID_INTEGER = 'Invalid Coco/R for Delphi bookmark integer';
  BOOKMARK_STR_SEPARATOR = ' ';

function PadL(S : string; ch : char; L : integer) : string;
var
  i : integer;
begin
  for i := 1 to L - (Length(s)) do
    s := ch + s;
  Result := s;
end; {PadL}

function StrTok(
    var Text : string;
    const ch : char) : string;
var
  apos : integer;
begin
  apos := Pos(ch, Text);
  if (apos > 0) then
  begin
    Result := Copy(Text, 1, apos - 1);
    Delete(Text, 1, apos);
  end
  else
  begin
    Result := Text;
    Text := '';
  end;
end; {StrTok}

{ TSymbolPosition }

procedure TSymbolPosition.Assign(Source: TSymbolPosition);
begin
  fLine := Source.fLine;
  fCol := Source.fCol;
  fLen := Source.fLen;
  fPos := Source.fPos;
end; {Assign}

procedure TSymbolPosition.Clear;
begin
  fLen := 0;
  fPos := 0;
  fLine := 0;
  fCol := 0;
end; { Clear }

{ TCocoRScanner }

function TCocoRScanner.Bookmark: string;
begin
  Result := IntToStr(bpCurrToken) + BOOKMARK_STR_SEPARATOR
      + IntToStr(BufferPosition) + BOOKMARK_STR_SEPARATOR
      + IntToStr(ContextLen) + BOOKMARK_STR_SEPARATOR
      + IntToStr(CurrLine) + BOOKMARK_STR_SEPARATOR
      + IntToStr(NumEOLInComment) + BOOKMARK_STR_SEPARATOR
      + IntToStr(StartOfLine) + BOOKMARK_STR_SEPARATOR
      + IntToStr(LastSymbol.Line) + BOOKMARK_STR_SEPARATOR
      + IntToStr(LastSymbol.Col) + BOOKMARK_STR_SEPARATOR
      + IntToStr(LastSymbol.Len) + BOOKMARK_STR_SEPARATOR
      + IntToStr(LastSymbol.Pos) + BOOKMARK_STR_SEPARATOR
      + IntToStr(CurrentSymbol.Line) + BOOKMARK_STR_SEPARATOR
      + IntToStr(CurrentSymbol.Col) + BOOKMARK_STR_SEPARATOR
      + IntToStr(CurrentSymbol.Len) + BOOKMARK_STR_SEPARATOR
      + IntToStr(CurrentSymbol.Pos) + BOOKMARK_STR_SEPARATOR
      + IntToStr(NextSymbol.Line) + BOOKMARK_STR_SEPARATOR
      + IntToStr(NextSymbol.Col) + BOOKMARK_STR_SEPARATOR
      + IntToStr(NextSymbol.Len) + BOOKMARK_STR_SEPARATOR
      + IntToStr(NextSymbol.Pos) + BOOKMARK_STR_SEPARATOR
      + CurrInputCh
      + LastInputCh
end; {Bookmark}

function TCocoRScanner.ExtractBookmarkChar(var aBookmark : string) : char;
begin
  if length(aBookmark) > 0 then
    Result := aBookmark[1]
  else
    Raise ECocoBookmark.Create(INVALID_CHAR);
end; {ExtractBookmarkChar}

procedure TCocoRScanner.GotoBookmark(aBookmark: string);
var
  BookmarkToken : string;
begin
  try
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    bpCurrToken := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    BufferPosition := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    ContextLen := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    CurrLine := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    NumEOLInComment := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    StartOfLine := StrToInt(BookmarkToken);

    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    LastSymbol.Line := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    LastSymbol.Col := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    LastSymbol.Len := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    LastSymbol.Pos := StrToInt(BookmarkToken);

    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    CurrentSymbol.Line := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    CurrentSymbol.Col := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    CurrentSymbol.Len := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    CurrentSymbol.Pos := StrToInt(BookmarkToken);

    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    NextSymbol.Line := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    NextSymbol.Col := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    NextSymbol.Len := StrToInt(BookmarkToken);
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    NextSymbol.Pos := StrToInt(BookmarkToken);

    CurrInputCh := ExtractBookmarkChar(aBookmark);
    LastInputCh := ExtractBookmarkChar(aBookmark);
  except
    on EConvertError do
      Raise ECocoBookmark.Create(INVALID_INTEGER);
    else
      Raise;
  end;
end; {GotoBookmark}

constructor TCocoRScanner.Create;
begin
  inherited;
  fSrcStream := TMemoryStream.Create;
  LastSymbol := TSymbolPosition.Create;
  CurrentSymbol := TSymbolPosition.Create;
  NextSymbol := TSymbolPosition.Create;
end; {Create}

destructor TCocoRScanner.Destroy;
begin
  fSrcStream.Free;
  fSrcStream := NIL;
  LastSymbol.Free;
  LastSymbol := NIL;
  CurrentSymbol.Free;
  CurrentSymbol := NIL;
  NextSymbol.Free;
  NextSymbol := NIL;
  inherited;
end; {Destroy}

function TCocoRScanner.CapChAt(pos : longint) : char;
begin
  Result := UpCase(CharAt(pos));
end; {CapCharAt}

function TCocoRScanner.CharAt(pos : longint) : char;
var
  ch : char;
begin
  if pos >= SourceLen then
  begin
    Result := _EF;
    exit;
  end;
  SrcStream.Seek(pos, soFromBeginning);
  SrcStream.ReadBuffer(Ch, 1);
  if ch <> _EOF then
    Result := ch
  else
    Result := _EF
end; {CharAt}

function TCocoRScanner.GetNStr(Symbol : TSymbolPosition; ChProc : TGetCh) : string;
var
  i : integer;
  p : longint;
begin
  SetLength(Result, Symbol.Len);
  p := Symbol.Pos;
  i := 1;
  while i <= Symbol.Len do
  begin
    Result[i] := ChProc(p);
    inc(i);
    inc(p)
  end;
end; {GetNStr}

function TCocoRScanner.GetName(Symbol : TSymbolPosition) : string;
begin
  Result := GetNStr(Symbol, CurrentCh);
end; {GetName}

function TCocoRScanner.GetStartState : PStartTable;
begin
  Result := @fStartState;
end; {GetStartState}

procedure TCocoRScanner.SetStartState(aStartTable : PStartTable);
begin
  fStartState := aStartTable^;
end; {SetStartState}

function TCocoRScanner.GetString(Symbol : TSymbolPosition) : string;
begin
  Result := GetNStr(Symbol, CharAt);
end; {GetString}

procedure TCocoRScanner._Reset;
var
  len : longint;
begin
  { Make sure that the stream has the _EF character at the end. }
  CurrInputCh := _EF;
  SrcStream.Seek(0, soFromEnd);
  SrcStream.WriteBuffer(CurrInputCh, 1);
  SrcStream.Seek(0, soFromBeginning);

  LastInputCh := _EF;
  len := SrcStream.Size;
  SourceLen := len;
  CurrLine := 1;
  StartOfLine := -2;
  BufferPosition := -1;
  LastSymbol.Clear;
  CurrentSymbol.Clear;
  NextSymbol.Clear;
  NumEOLInComment := 0;
  ContextLen := 0;
  NextCh;
end; {_Reset}

{ TCocoRGrammar }

procedure TCocoRGrammar.ClearErrors;
var
  i : integer;
begin
  for i := 0 to fErrorList.Count - 1 do
    TCocoError(fErrorList[i]).Free;
  fErrorList.Clear;
end; {ClearErrors}

constructor TCocoRGrammar.Create(AOwner : TComponent);
begin
  inherited;
  FGenListWhen := glOnError;
  fClearSourceStream := true;
  fListStream := TMemoryStream.Create;
  fErrorList := TList.Create;
  FModule := TCustomTurboModule.Create;
  FModule.GlobalOptions := @FTurboGlobalOptions;
end; {Create}

destructor TCocoRGrammar.Destroy;
begin
  fListStream.Clear;
  fListStream.Free;
  ClearErrors;
  fErrorList.Free;
  FreeAndNil(FModule);
  inherited;
end; {Destroy}

procedure TCocoRGrammar.Expect(n : integer);
begin
  if CurrentInputSymbol = n then
    Get
  else
    SynError(n);
end; {Expect}

procedure TCocoRGrammar.GenerateListing;
  { Generate a source listing with error messages }
var
  i : integer;
  eof : boolean;
  lnr, errC : integer;
  srcPos : longint;
  line : string;
  PrintErrorCount : boolean;
begin
  if Assigned(BeforeGenList) then
    BeforeGenList(Self);
  srcPos := 0;
  GetLine(srcPos, line, eof);
  lnr := 1;
  errC := 0;
  while not eof do
  begin
    StreamToListFile(PadL(IntToStr(lnr), ' ', 5) + '  ' + line, TRUE);
    for i := 0 to ErrorList.Count - 1 do
    begin
      if TCocoError(ErrorList[i]).Line = lnr then
      begin
        PrintErr(line, TCocoError(ErrorList[i]).ErrorCode,
          TCocoError(ErrorList[i]).Line,
          TCocoError(ErrorList[i]).Col,
          TCocoError(ErrorList[i]).Data);
        inc(errC);
      end;
    end;
    GetLine(srcPos, line, eof);
    inc(lnr);
  end;
  // Now take care of the last line.
  for i := 0 to ErrorList.Count - 1 do
  begin
    if TCocoError(ErrorList[i]).Line = lnr then
    begin
      PrintErr(line, TCocoError(ErrorList[i]).ErrorCode,
        TCocoError(ErrorList[i]).Line,
        TCocoError(ErrorList[i]).Col,
        TCocoError(ErrorList[i]).Data);
      inc(errC);
    end;
  end;
  PrintErrorCount := true;
  if Assigned(AfterGenList) then
    AfterGenList(Self, PrintErrorCount);
  if PrintErrorCount then
  begin
    StreamToListFile('', TRUE);
    StreamToListFile(PadL(IntToStr(errC), ' ', 5) + ' error', FALSE);
    if errC <> 1 then
      StreamToListFile('s', TRUE);
  end;
end; {GenerateListing}

procedure TCocoRGrammar.GetLine(var pos : longint;
  var line : string;
  var eof : boolean);
  { Read a source line. Return empty line if eof }
var
  ch : char;
  i : integer;
begin
  i := 1;
  eof := false;
  ch := Scanner.CharAt(pos);
  inc(pos);
  while not (ch in LineEnds) do
  begin
    SetLength(line, length(Line) + 1);
    line[i] := ch;
    inc(i);
    ch := Scanner.CharAt(pos);
    inc(pos);
  end;
  SetLength(line, i - 1);
  eof := (i = 1) and (ch = _EF);
  if ch = _CR then
  begin { check for MsDos end of lines }
    ch := Scanner.CharAt(pos);
    if ch = _LF then
    begin
      inc(pos);
      Extra := 0;
    end;
  end;
end; {GetLine}

function TCocoRGrammar.GetSourceStream : TMemoryStream;
begin
  Result := Scanner.SrcStream;
end; {GetSourceStream}

function TCocoRGrammar.GetSuccessful : boolean;
begin
  Result := ErrorList.Count = 0;
end; {GetSuccessful}

function TCocoRGrammar.LastName : string;
begin
  Result := Scanner.GetName(Scanner.LastSymbol)
end; {LastName}

function TCocoRGrammar.LastString : string;
begin
  Result := Scanner.GetString(Scanner.LastSymbol)
end; {LastString}

function TCocoRGrammar.LexName : string;
begin
  Result := Scanner.GetName(Scanner.CurrentSymbol)
end; {LexName}

function TCocoRGrammar.LexString : string;
begin
  Result := Scanner.GetString(Scanner.CurrentSymbol)
end; {LexString}

function TCocoRGrammar.LookAheadName : string;
begin
  Result := Scanner.GetName(Scanner.NextSymbol)
end; {LookAheadName}

function TCocoRGrammar.LookAheadString : string;
begin
  Result := Scanner.GetString(Scanner.NextSymbol)
end; {LookAheadString}

procedure TCocoRGrammar.PrintErr(line : string; ErrorCode : integer; lnr, col : integer; Data : string);
  { Print an error message }

  procedure DrawErrorPointer;
  var
    i : integer;
  begin
    StreamToListFile('*****  ', FALSE);
    i := 0;
    while i < col + Extra - 2 do
    begin
      if ((length(Line) > 0) and (length(Line) < i)) and (line[i] = _TAB) then
        StreamToListFile(_TAB, FALSE)
      else
        StreamToListFile(' ', FALSE);
      inc(i)
    end;
    StreamToListFile('^ ', FALSE)
  end; {DrawErrorPointer}

var
  S                           : string;
begin {PrintErr}
  DrawErrorPointer;
  StreamToListFile(Copy(Line, 1, Col - 2) + '--------------------------', FALSE);
  //StreamToListFile(ErrorStr(ErrorCode, Data), FALSE);
  //StreamToListFile('', TRUE)
  S := ErrorStr(ErrorCode, Data);
  StreamToListFile(Format('Syntax error at line %d, position %d: %s',
    [lnr, Col - 1, S]), TRUE);
  StreamToListFile('', TRUE)
end; {PrintErr}

procedure TCocoRGrammar.ErrorMsg(const errNo : integer; const Data : string; const aErrType: integer);
begin
  if errDist >= minErrDist then
    Scanner.ScannerError(errNo, Scanner.CurrentSymbol, Data, aErrType);
  errDist := 0;
end; {ErrorMsg}

procedure TCocoRGrammar.SemError(const errNo : integer; const Data : string);
begin
  if errDist >= minErrDist then
    Scanner.ScannerError(errNo, Scanner.CurrentSymbol, Data, etSymantic);
  errDist := 0;
end; {SemError}

{$IFNDEF REMOVE_DEPRECATED}
procedure TCocoRGrammar._StreamLn(const s : string);
begin
  StreamToListFile(s, FALSE);
end; {_StreamLn}

procedure TCocoRGrammar._StreamLine(const s : string);
begin
  StreamToListFile(s, TRUE);
end; {_StreamLine}
{$ENDIF REMOVE_DEPRECATED}

procedure TCocoRGrammar.StreamToListFile(s: string; const AddEndOfLine : boolean);
begin
  if AddEndOfLine then
    s := s + chEOL;
  if length(s) > 0 then
    ListStream.WriteBuffer(s[1], length(s));
end; {StreamToListFile}

procedure TCocoRGrammar.SynError(const errNo : integer; const Data : string);
begin
  if errDist >= minErrDist then
    Scanner.ScannerError(errNo, Scanner.NextSymbol, Data, etSyntax);
  errDist := 0;
end; {SynError}

procedure TCocoRGrammar.SetOnStatusUpdate(const Value : TStatusUpdateProc);
begin
  FOnStatusUpdate := Value;
  Scanner.OnStatusUpdate := Value;
end; {SetOnStatusUpdate}

procedure TCocoRGrammar.SetSourceStream(const Value : TMemoryStream);
begin
  Scanner.SrcStream := Value;
end; {SetSourceStream}

procedure TCocoRGrammar.StoreError(const nr : integer; const Symbol : TSymbolPosition;
  const Data : string; const ErrorType : integer);
  { Store an error message for later printing }
var
  Error : TCocoError;
begin
  Error := TCocoError.Create;
  Error.ErrorCode := nr;
  if Assigned(Symbol) then
  begin
    Error.Line := Symbol.Line;
    Error.Col := Symbol.Col;
  end
  else
  begin
    Error.Line := 0;
    Error.Col := 0;
  end;
  Error.Data := Data;
  Error.ErrorType := ErrorType;
  ErrorList.Add(Error);
  if Assigned(OnError) then
    OnError(self, Error);
end; {StoreError}

function TCocoRGrammar.GetLineCount: integer;
begin
  Result := Scanner.CurrLine;
end; {GetLineCount}

function TCocoRGrammar.GetCharacterCount: integer;
begin
  Result := Scanner.BufferPosition;
end; {GetCharacterCount}

procedure TCocoRGrammar.DoBeforeParse;
begin
  if Assigned(fBeforeParse) then
    fBeforeParse(Self);
  if Assigned(fOnStatusUpdate) then
    fOnStatusUpdate(Self, cstBeginParse, '', -1);
end; {DoBeforeParse}

procedure TCocoRGrammar.DoAfterParse;
begin
  if Assigned(fOnStatusUpdate) then
    fOnStatusUpdate(Self, cstEndParse, '', -1);
  if Assigned(fAfterParse) then
    fAfterParse(Self);
end; {DoAfterParse}

function TCocoRGrammar.Bookmark: string;
begin
  Result :=
        IntToStr(fCurrentInputSymbol) + BOOKMARK_STR_SEPARATOR
      + Scanner.Bookmark;
end; {Bookmark}

procedure TCocoRGrammar.GotoBookmark(aBookmark: string);
var
  BookmarkToken : string;
begin
  try
    BookmarkToken := StrTok(aBookmark, BOOKMARK_STR_SEPARATOR);
    fCurrentInputSymbol := StrToInt(BookmarkToken);
    Scanner.GotoBookmark(aBookmark);
  except
    on EConvertError do
      Raise ECocoBookmark.Create(INVALID_INTEGER);
    else
      Raise;
  end;
end; {GotoBookmark}

function TCocoRGrammar.CurrentBufferPosition: integer;
begin
  Result := Scanner.BufferPosition - Scanner.NextSymbol.Len;
end; {CurrentBufferPosition}

{ TCommentList }

procedure TCommentList.Add(const S : string; const aLine : integer;
    const aColumn : integer);
var
  CommentItem : TCommentItem;
begin
  CommentItem := TCommentItem.Create;
  try
    CommentItem.Comment := FixComment(S);
    CommentItem.Line := aLine;
    CommentItem.Column := aColumn;
    fList.Add(CommentItem);
  except
    CommentItem.Free;
  end;
end; {Add}

procedure TCommentList.Clear;
var
  i : integer;
begin
  for i := 0 to fList.Count - 1 do
    TCommentItem(fList[i]).Free;
  fList.Clear;
end; {Clear}

constructor TCommentList.Create;
begin
  fList := TList.Create;
end; {Create}

destructor TCommentList.Destroy;
begin
  Clear;
  if Assigned(fList) then
  begin
    fList.Free;
    fList := NIL;
  end;
  inherited;
end; {Destroy}

function TCommentList.FixComment(const S: string): string;
begin
  Result := S;
  while (length(Result) > 0) AND (Result[length(Result)] < #32) do
    Delete(Result,Length(Result),1);
end; {FixComment}

function TCommentList.GetColumn(Idx: integer): integer;
begin
  Result := TCommentItem(fList[Idx]).Column;
end; {GetColumn}

function TCommentList.GetComments(Idx: integer): string;
begin
  Result := TCommentItem(fList[Idx]).Comment;
end; {GetComments}

function TCommentList.GetCount: integer;
begin
  Result := fList.Count;
end; {GetCount}

function TCommentList.GetLine(Idx: integer): integer;
begin
  Result := TCommentItem(fList[Idx]).Line;
end; {GetLine}

function TCommentList.GetText: string;
var
  i : integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    Result := Result + Comments[i];
    if i < Count - 1 then
      Result := Result + chEOL;
  end;
end; {GetText}

procedure TCommentList.SetColumn(Idx: integer; const Value: integer);
begin
  TCommentItem(fList[Idx]).Column := Value;
end; {SetColumn}

procedure TCommentList.SetComments(Idx: integer; const Value: string);
begin
  TCommentItem(fList[Idx]).Comment := Value;
end; {SetComments}

procedure TCommentList.SetLine(Idx: integer; const Value: integer);
begin
  TCommentItem(fList[Idx]).Line := Value;
end; {SetLine}

{ TCocoError }

function TCocoError.ExportToXMLFragment(
    const ErrorTypeDesc : string;
    const ErrorText : string;
    const ErrorSeverity : string): string;
begin
  Result := '<Error'
      + ' Severity="' + ErrorSeverity + '"'
      + ' Type="' + IntToStr(ErrorType) + '"'
      + ' Description="' + FixXmlStr(ErrorTypeDesc) + '"'
      + ' Code="' + IntToStr(ErrorCode) + '"'
      + ' Text="' + FixXmlStr(ErrorText) + '"'
      + ' Line="' + IntToStr(Line) + '"'
      + ' Col="' + IntToStr(Col) + '"'
      + ' Data="' + FixXmlStr(Data) + '"'
      + ' />';
end; {ExportToXMLFragment}

function TCocoError.FixXmlStr(const Str: string): string;
begin
  Result := StringReplace(Str, '&', '&amp;', [rfReplaceAll, rfIgnoreCase]); // must be first
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll, rfIgnoreCase]);
end; {FixXmlStr}

{ TCocoRGrammar }
//write string to compiler console or output.
procedure TCocoRGrammar.PrintStr(S : String);
begin
  ListStream.WriteBuffer(S[1],length(S));
end;

//write string to compiler console or output with line feed.
procedure TCocoRGrammar.PrintStrLn(S : String);
begin
  PrintStr(s+#13#10); 
end;

function TCocoRGrammar.DefineWordBegin(var aWord: TTurboSimpleWord): Boolean;
var
  i: Integer;
begin
  //WriteLn('DefineWordBegin:',aWord.Name);
  //FindVar('');
  Result := IsUniqueIdentifier(aWord.Name);
  if Result then
  begin
    //writeln('FModule.UsedDataSize=',InttoHex(FModule.UsedDataSize,4));
    FModule.AlignData;
    //writeln('FModule.UsedDataSize=',InttoHex(FModule.UsedDataSize,4));
    Integer(FDefinedWordEntry) := Integer(FModule.DataMemory) + FModule.UsedDataSize;
    FModule.AllocDataSpace(SizeOf(TTurboMethodEntry));
    FDefinedWordEntry.Prior := FModule.LastWordEntry;
    //FModule.AddIntToData(tsInt(FModule.LastWordEntry));
    //FModule.AllocDataSpace(SizeOf(TTurboWordOptions));
    //FModule.AllocDataSpace(SizeOf(LongWord)); //the ParamFieldLength
    //FModule.AllocDataSpace(SizeOf(Integer)); //the CFA
    if aWord.Options.Visibility >= fvProtected then
    begin
      FDefinedWordEntry.Word.Name := Pointer(FModule.UsedDataSize);
      //FModule.AddByteToData(Length(aWord.Name));
      FModule.AddBufferToData(aWord.Name[1], Length(aWord.Name));
      FModule.AddByteToData(0);
    end
    else
    begin
      //FModule.AddByteToData(0);
      FDefinedWordEntry.Word.Name := nil;
    end;
    FLastWordCfa := FModule.UsedMemory;
    FDefinedWordEntry.Word.CFA := FLastWordCfa;
    aWord.CFA := FLastWordCfa;
    FDefinedWordEntry.Word.Visibility := aWord.Options.Visibility;
    FDefinedWordEntry.Word.CallStyle := aWord.Options.CallStyle;
    FDefinedWordEntry.Word.CodeFieldStyle := aWord.Options.CodeFieldStyle;

    i := Length(FWords);
    SetLength(FWords, i+1);
    FWords[i] := aWord;
  end
  //else FLastWordCfa := 0;
end;

procedure TCocoRGrammar.DefineWordEnd();
var
  vPrior: PTurboMethodEntry;
begin
  if FDefinedWordEntry.Word.Visibility >= fvProtected then
    FModule.AddOpToMem(opExitFar)
  else
    FModule.AddOpToMem(opExit);
  FDefinedWordEntry.Word.ParamFieldLength := FModule.UsedMemory - FLastWordCfa + 1;
  FWords[Length(FWords)-1].ParamFieldLength := FDefinedWordEntry.Word.ParamFieldLength;

  //vPrior := FModule.LastWordEntry;
  FModule.LastWordEntry := Pointer(Integer(FDefinedWordEntry) - Integer(FModule.DataMemory));
  //writeln('dWerdE.Last:',tsInt(FModule.LastWordEntry));
  //FDefinedWordEntry.Prior := vPrior;
end;

function TCocoRGrammar.AddIdentifierCFA(const aName: string; const aParams: TStringList): Boolean;
begin
  if not Assigned(aParams) then
  begin
    Result := AddConstCFA(aName);
    if Result then exit;

    Result := AddVarCFA(aName);
    if Result then exit;
  end;

  Result := AddWordCFA(aName, aParams);
  if Result then exit;

  //writeln('cUnknownWordError:',aName);
  SynError(cUnknownWordError, aName);
end;

function TCocoRGrammar.AddConstCFA(const aName: String): Boolean;
var
  p: Pointer;
  i : Integer;
begin
  i := FindConst(aName);
  Result := i >= 0;
  if Result  then
    with FConsts[i] do
    begin
      if Size <= SizeOf(Integer) then
      begin
        //FModule.AddOpToMem(opPushInt);
        //Size := SizeOf(Integer);
        Case Size of
          SizeOf(Byte): FModule.AddOpToMem(opPushByte);
          SizeOf(Word): FModule.AddOpToMem(opPushWord);
          //SizeOf(tsInt): FModule.AddOpToMem(opPushInt);
          //SizeOf(Int64): FModule.AddOpToMem(opPushInt64);
          else 
          begin
            FModule.AddOpToMem(opPushInt);
            Size := SizeOf(tsInt);
          end;
        end;//case
      end
      else
        FModule.AddOpToMem(opPushInt64);
      Integer(p) := Integer(FModule.Memory) + FModule.UsedMemory;
      FModule.AllocSpace(Size);
      AssignValueTo(p);
    end
end;

function TCocoRGrammar.AddVarCFA(const aName: String): Boolean;
var
  i: Integer;
begin
  i := FindVar(aName);
  Result := i >= 0;
  if Result then
    with FVars[i] do
    begin
      FModule.AddOpToMem(opPushInt);
      FModule.AddIntToMem(Addr);
    end
end;

procedure TCocoRGrammar.PushWordParam(const aValue: string);
begin
  if aValue = '' then exit;
  if (Length(aValue) >= 2) and (aValue[1] = '''') then
  begin
    // it's a string
    PushString(aValue);
  end
  else if StrIsInteger(aValue) then
  begin
    PushInt32(aValue);
  end
  else //暂时不支持内嵌参数的word
    AddIdentifierCFA(aValue, nil);
end;

function TCocoRGrammar.AddWordCFA(aName: string; const aParams: TStringList): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := FindWord(aName);
  if i >= 0 then with FWords[i] do
  begin
    if Assigned(aParams) then
    begin
      //push the parameters
      for i := aParams.Count - 1 downto 0 do
      begin
        PushWordParam(aParams[i]);
      end;
    end;
    case Options.CodeFieldStyle of
      cfsFunction:
      begin
        if Options.Visibility < fvProtected then
        begin
          FModule.AddOpToMem(opEnter);
          FModule.AddIntToMem(CFA);
        end
        else
        begin
          FModule.AddOpToMem(opEnterFar);
          FModule.AddIntToMem(0);
          FModule.AddIntToMem(CFA);
        end;
        Result := True;
      end;
      cfsExternalFunction:
      begin
        case Options.CallStyle of
          csForth:
          begin
            if Assigned(Module) then
            begin
              if ExternalOptions.Name <> '' then aName := ExternalOptions.Name;
              i := Module.GetWordCFA(aName);
              if i <> -1 then
              begin
                FModule.AddOpToMem(opCallFar);
                //writeln(aName, '.ModEntry:',Integer(FUsedModules[ModuleIndex].Entry));
                //point to the TurboModuleInfo
                FModule.AddIntToMem(Integer(FUsedModules[ModuleIndex].Entry)+SizeOf(Pointer)-Integer(FModule.DataMemory));
                FModule.AddIntToMem(i);
                //writeln(aName, '.CFA:',i);
                Result := True;
              end
              else
                SynError(cWordNotFoundError, '"'+ aName + '" CFA not in ' + FUsedModules[ModuleIndex].Name);
            end
            else
              SynError(cWordNotFoundError, '"'+ aName + '" not in ' + FUsedModules[ModuleIndex].Name);
          end;
        end; //case
      end;
    end;//case
  end;
end;

function TCocoRGrammar.IsUniqueIdentifier(const aName: String): Boolean;
begin
  //writeln('IsUniqueIdentifier:', aName);
  Result := FindConst(aName) < 0;
  //writeln('C:',Result);
  if Result then
    Result := FindVar(aName)< 0;
  //writeln('V:',Result);
  if Result then
    Result := FindWord(aName) < 0;
  //writeln('W:',Result);
  if not Result then
  begin
    writeln('RedeclarationError:',aName);
    SynError(cRedeclarationError);
  end;
end;

function TCocoRGrammar.GetConstValueRec(const aTypeKind: TTurboSimpleTypeKind; const aName: String): TTurboValueRec;
var
  i: Integer;
begin
  for i := 0 to Length(FConsts) do 
    with FConsts[i] do
    if (aName = Name) and (aTypeKind = TypeKind) then
    begin
      Result := Value;
      exit;
    end;
  SynError(cUnknownWordError, aName);
end;

function TCocoRGrammar.DefineLabel(const aName: string; const aWordName: string): Integer;
begin
    Result := Length(FLabels);
    SetLength(FLabels, Result+1);
    with FLabels[Result] do
    begin
      Name := aName;
      WordName := aWordName;
      Addr := FModule.UsedMemory;
    end;
end;

function TCocoRGrammar.DefineLabelEx(const aName: string; const aWordName: string): Integer;
begin
  Result := FindLabel(aName, aWordName);
  if Result = -1 then
  begin
    Result := DefineLabel(aName, aWordName);
  end
  else 
    SynError(cLabelRedeclarationError);

end;

function TCocoRGrammar.DefineConstEx(const aValue: TTurboSimpleConst): Integer;
begin
  //WriteLn('ConstDefine:', aValue.Name);
  Result := FindConst(aValue.Name);
  if Result = -1 then
  begin
    Result := DefineConst(aValue);
  end
  else 
    SynError(cConstRedeclarationError);
end;

function TCocoRGrammar.DefineConst(const aValue: TTurboSimpleConst): Integer;
begin
  Result := Length(FConsts);
  SetLength(FConsts, Result+1);
  FConsts[Result] := aValue;
  with FConsts[Result] do
  begin
    //Name := aValue.Name;
    SaveString(FModule); //if this is string 
  end;
end;

function TCocoRGrammar.FindConst(const aName: String): Integer;
begin
  for Result := 0 to Length(FConsts)-1 do
  begin
    //WriteLn(aName,'=', FConsts[Result].Name);
    if aName = FConsts[Result].Name then exit;
  end;
  Result := -1;
end;

function TCocoRGrammar.FindVar(const aName: String): Integer;
begin
  for Result := 0 to Length(FVars)-1 do
  begin
    //if aName = 'Add' then 
    //WriteLn(Result, ':FV:',FVars[Result].Name);
    if aName = FVars[Result].Name then exit;
  end;
  Result := -1;
end;

function TCocoRGrammar.DefineVar(const aValue: TTurboSimpleVar): Integer;
var
  vVaraibleEntry: PTurboVariableEntry;
  vValue: Pointer;
  //vTypeSize: Integer;
begin
  //在前面已经判断了是不是名称重复：
  Result := Length(FVars);
  SetLength(FVars, Result+1);
  FVars[Result] := aValue;
  with FVars[Result] do
  begin
    //WriteLn(Result, ':DefineVar:',Name);
    //Name := aValue.Name; //保证让其ref增加
    //FModule.AligData;
    //vTypeSize := GetSimpleTurboTypeSize(aValue.TypeKind);
    //Size := vTypeSize;
    //WriteLn(Name,'.Visibility=', Integer(Visibility));
    //WriteLn(Name,'.Size=', Size);

    if Visibility >= fvProtected then
    begin
      //FModule.AligData;
      Integer(vVaraibleEntry) := Integer(FModule.DataMemory) + FModule.UsedDataSize;
      FModule.AllocDataSpace(SizeOf(TTurboVariableEntry));
      vVaraibleEntry.Prior := FModule.LastVariableEntry;
      vVaraibleEntry.Variable.Size := Size;
      vVaraibleEntry.Variable.Addr := nil;
      vVaraibleEntry.Variable.TypeInfo := nil;
      //FModule.AddIntToData(Integer(FModule.LastVariableEntry));
      //FModule.AddIntToData(Size);
      //FModule.AddIntToData(0); //preserved.for addr.
      //FModule.AddIntToData(0); //preserved.for TypeInfo.
    end;
  
    if (TypeKind in cReqAlignMemTypes) then FModule.AlignData;
    Addr := FModule.UsedDataSize;
    if Visibility >= fvProtected then
    begin
      Integer(vVaraibleEntry.Variable.Addr) := Addr;
      FModule.LastVariableEntry := Pointer(Integer(vVaraibleEntry) - Integer(FModule.DataMemory));
    end;
    Integer(vValue) := Integer(FModule.DataMemory)  + Addr;
    FModule.AllocDataSpace(Size);
    if ValueStr <> '' then
    begin
      SaveString(FModule); //if this is string 
      AssignValueTo(vValue);
    end;

      if Visibility >= fvPublished then
      begin
        vVaraibleEntry.Variable.Name := Pointer(FModule.UsedDataSize);
        //fill the variable name 
        //FModule.AddByteToData(Length(Name));
        FModule.AddBufferToData(Name[1], Length(Name));
        FModule.AddByteToData(0);
      end
      else if Visibility >= fvProtected then
      begin
        //no name
        //FModule.AddByteToData(0);
        vVaraibleEntry.Variable.Name := nil;
      end;
  end;

  //SetLength(FVars, Length(FVars) + 1);
  //FindVar('');
end;

function TCocoRGrammar.FindWord(const aName: String): Integer;
begin
  for Result := 0 to Length(FWords)-1 do
  begin
    if AnsiSameText(aName, FWords[Result].Name) then exit;
  end;
  Result := -1;
end;

procedure TCocoRGrammar.GenerateModuleEntryForWord(var aWord: TTurboSimpleWord);
var
  i: Integer;
begin
  if aWord.ModuleName <> '' then
  begin
    i := FindModule(aWord.ModuleName, aWord.ModuleType);
    //WriteLn('GenerateModuleEntryForWord:', aWord.ModuleName);
    if i < 0 then i := AddUsedModule(aWord.ModuleName, aWord.ModuleType);
    if i >= 0 then
    begin
      aWord.ExternalOptions.ModuleRef := Pointer(Integer(FUsedModules[i].Entry) + SizeOf(Pointer));
      aWord.ModuleIndex := i;
      aWord.Module := FUsedModules[i].Module;
    end;
  end
  else with aWord.ExternalOptions do 
  begin
    if FModule.LastModuleRefEntry <> nil then
    begin
      ModuleRef := Pointer(Integer(FModule.LastModuleRefEntry) + SizeOf(Pointer));
    end
    else 
      SynError(cDLLModuleMissError, aWord.Name);
  end;
end;

function  TCocoRGrammar.AddUsedModule(const aName: String; aModuleType: TTurboModuleType): Integer;
var
  vModule: TCustomTurboModule;
begin
  //WriteLn('AddUsedModule:', aName);
  Result := -1;
  if aModuleType = mtLib then
  begin
    vModule := FModule.RequireModule(PChar(aName));
    if vModule = nil then
    begin
      SynError(cFileNotFoundError, aName);
      exit;
    end;
  end;
  Result := Length(FUsedModules);
  SetLength(FUsedModules, Result+1);
  with FUsedModules[Result] do
  begin
    Name := aName;
    ModuleType := aModuleType;
    if aModuleType = mtLib then Module := vModule else Module := nil;
    with FModule do 
    begin
      Integer(Entry) := Integer(DataMemory) + UsedDataSize; //the offset address.
      AllocDataSpace(SizeOf(TTurboModuleRefEntry));
      Entry.Prior := LastModuleRefEntry;
      Entry.Module.ModuleType := aModuleType;
      Entry.Module.Revision := vModule.ModuleVersion;
      Entry.Module.BuildDate := vModule.ModuleDate;
      //AddIntToData(tsInt(LastModuleRefEntry));
      //AddByteToData(Byte(aModuleType));
      //AddIntToData(0); //preserved for Module: Pointer
      //AllocDataSpace(SizeOf(LongWord)); //Revision
      //AllocDataSpace(SizeOf(TTimeStamp)); //BuildDate
      Entry.Module.Name := Pointer(UsedDataSize);
      //AddByteToData(Length(aName));
      if Length(aName) > 0 then
        AddBufferToData(aName[1], Length(aName));
      AddByteToData(0);
      LastModuleRefEntry := Pointer(tsInt(Entry) - Integer(DataMemory));
    end;
  end;
end;

function TCocoRGrammar.FindLabel(const aName: string; const aWordName: string): Integer;
begin
  for Result := 0 to length(FLabels) -1 do
  begin
    if (aName = FLabels[Result].Name) and (aWordName = FLabels[Result].WordName) then
    begin
      exit;
    end;
  end;
  Result := -1;
end;

function TCocoRGrammar.FindModule(const aName: String; aModuleType: TTurboModuleType): Integer;
begin
  for Result := 0 to Length(FUsedModules)-1 do
  begin
    if AnsiSameText(aName, FUsedModules[Result].Name) and (aModuleType = FUsedModules[Result].ModuleType) then exit;
  end;
  Result := -1;
end;

procedure TCocoRGrammar.PushInt32(const aInt: tsInt);
begin
  FModule.AddOpToMem(opPushInt);
  FModule.AddIntToMem(aInt);
end;

procedure TCocoRGrammar.PushInt32(const aStr: string);
begin
  try
    PushInt32(StrToInt(aStr));
  except
    SynError(cStrToIntCovnertError, aStr);
  end;
end;

procedure TCocoRGrammar.PushString(const aStr: string);
var
  vConst: TTurboSimpleConst;
  p: Pointer;
begin
  if vConst.AssignValue(aStr) then
  begin
    vConst.SaveString(FModule); //if this is string 
    FModule.AddOpToMem(opPushInt);
    Integer(p) := Integer(FModule.Memory) + FModule.UsedMemory;
    FModule.AllocSpace(SizeOf(Pointer));
    vConst.AssignValueTo(p);
  end
  else
    SynError(cConstRedeclarationError, 'Invalid String:'+ aStr);
end;

procedure TCocoRGrammar.PushStringSeqToStack(const aStr: string);
var
  s: string;
  i: Integer;
begin
  s := AnsiDequotedStr(aStr, '''');
  for i := length(s) downto 1 do
  begin
    FModule.AddOpToMem(opPushInt);
    FModule.AddIntToMem(Ord(s[i])); 
  end;
end;

procedure TCocoRGrammar.Init;
begin
  with FModule do
  begin
    ClearMemory;
    if MemorySize < cMaxMemorySize then
      MemorySize := cMaxMemorySize;
    Status := [psCompiling];
  end;
  SetLength(FLabels, 0);
  SetLength(FConsts, 0);
  SetLength(FVars, 0);
  SetLength(FWords, 0);
  SetLength(FUsedModules, 0);
  //writeln('SourceFileName=', SourceFileName);
  FileName := ExtractFileBaseName(SourceFileName) + cTurboCompiledProgramFileExt;
  //writeln('FileName=', FileName);
end;

procedure TCocoRGrammar.Final;
var
  vStream: TFileStream;
begin
  //writeln('Final');
  if ErrorList.Count = 0 then
  begin
   //writeln('No error');
    //PrintStrLn('The Result: '+FloatToStr(fResult));
    if FileName <> '' then
    begin
      FModule.IsLoaded := True;
      //writeln('InitProcCFA=', FInitProcCFA+SizeOf(TTurboModuleStreamHeader));
      with PTurboPreservedDataMemory(FModule.DataMemory)^ do
      begin
        Integer(InitializeProc) := FInitProcCFA;
      end;
      if LowerCase(ExtractFileExt(FileName)) <> cTurboCompiledProgramFileExt then
        FileName := ChangeFileExt(FileName, cTurboCompiledProgramFileExt);
      vStream := TFileStream.Create(FileName, fmCreate);
      try
        writeln('saving to ' + FileName);
        FModule.SaveToStream(vStream);
      finally
        vStream.Free;
      end;
    end;
  end;
  FModule.ClearMemory;
  SetLength(FLabels, 0);
  SetLength(FConsts, 0);
  SetLength(FVars, 0);
  SetLength(FWords, 0);
  SetLength(FUsedModules, 0);
end;

end.

