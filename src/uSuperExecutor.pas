unit uSuperExecutor;

interface

uses
  SysUtils, Classes
  , uSuperScriptConsts
  ;

type
  TImportModuleRec = record
    Functions: TImportFunctions;
    ModuleType: TModuleType;
    Name: string;
  end;
  
  TImportFunctionRec = record
    Name: string;
    Ordinal: Integer;
    VirtualAddresses: TVirtualAddressTable;
  end;
  
  TRelocationAddress = packed record
    VirtualAddress: Integer;
  end;
  
  TCustomSuperPEFormat = class(TObject)
  private
    FImageBase: Integer;
  protected
    ExportTable: Integer;
    ImportTable: Integer;
    RelocationTable: TVirtualAddressTable;
  public
    property ImageBase: Integer read FImageBase write FImageBase;
  end;
  
  TCustomSuperExecutor = class(TObject)
  private
    FFileDate: LongWord;
    FFileType: TSuperForthFileType;
    FFileVersion: LongWord;
    FName: string;
  protected
    function ExecuteCFA(const aCFA: Integer): Integer; virtual;
    procedure Init; virtual;
  public
    function ExecuteWord(const aWord: string): Integer;
    function GetWordCFA(const aWord: string): Integer; virtual;
    procedure LoadFromFile(const aFileName: String);
    procedure LoadFromStream(const aStream: TStream); virtual;
    procedure SaveToFile(const aFile: String);
    procedure SaveToStream(const aStream: TStream); virtual;
    property FileDate: LongWord read FFileDate write FFileDate;
    property FileType: TSuperForthFileType read FFileType write FFileType;
    property FileVersion: LongWord read FFileVersion write FFileVersion;
    property Name: string read FName write FName;
  end;
  

implementation

{
***************************** TCustomSuperExecutor *****************************
}
function TCustomSuperExecutor.ExecuteCFA(const aCFA: Integer): Integer;
begin
end;

function TCustomSuperExecutor.ExecuteWord(const aWord: string): Integer;
var
  aCFA: Integer;
begin
  Init;
  aCFA := GetWordCFA(aWord);
  if aCFA >=0 then
    Result := ExecuteCFA(aCFA)
  else
    Result := -1;
end;

function TCustomSuperExecutor.GetWordCFA(const aWord: string): Integer;
begin
  Result := -1;
end;

procedure TCustomSuperExecutor.Init;
begin
  //PC := 0;
  //SP := StackSize;
end;

procedure TCustomSuperExecutor.LoadFromFile(const aFileName: String);
var
  aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(aFileStream);
  finally
    aFileStream.Free;
  end;
end;

procedure TCustomSuperExecutor.LoadFromStream(const aStream: TStream);
var
  s: string;
  L: Byte;
begin
  SetLength(s, Length(cFORTHHeaderMagicWord));
  with aStream do
  begin
    Read(@s[1], Length(cFORTHHeaderMagicWord));
    if s <> cFORTHHeaderMagicWord then
      Raise ESuperScriptError.Create(rsMissFileHeaderError);
  
    //Get the Unit(Program) Name.
    Read(L, SizeOf(L));
    SetLength(FName, L);
    if L<>0 then
      Read(PChar(FName), L);
  
    //Read(FFileType, SizeOf(FFileType)); //abondon
    Read(FFileVersion, SizeOf(FFileVersion));
    Read(FFileDate, SizeOf(FFileDate));
  end;
end;

procedure TCustomSuperExecutor.SaveToFile(const aFile: String);
var
  aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFileName, mCreate);
  try
    SaveToStream(aFileStream);
  finally
    aFileStream.Free;
  end;
end;

procedure TCustomSuperExecutor.SaveToStream(const aStream: TStream);
var
  L: Byte;
begin
  aStream.Write(@cFORTHHeaderMagicWord[1], Length(cFORTHHeaderMagicWord));
  //Get the Unit(Program) Name.
  L := Length(FName);
  aStream.Write(L, SizeOf(L));
  if L<>0 then
    aStream.Write(PChar(FName), L);
  
  //aStream.Write(FFileType, SizeOf(FFileType));
  aStream.Write(FFileVersion, SizeOf(FFileVersion));
  aStream.Write(FFileDate, SizeOf(FFileDate));
end;


end.
