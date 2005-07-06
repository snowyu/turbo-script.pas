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
  protected
    function ExecuteCFA(const aCFA: Integer): Integer; virtual;
    procedure Init; virtual;
  public
    function ExecuteWord(const aWord: string): Integer;
    function GetWordCFA(const aWord: string): Integer; virtual;
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


end.
