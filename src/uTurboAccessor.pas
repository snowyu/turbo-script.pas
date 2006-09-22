unit uTurboScriptAccessor;

interface

{$I Setting.inc}

uses
  SysUtils, Classes
  , uTurboScriptConsts
  , uTurboExecutor 
  ;

type
  TTurboScriptAccessor = class;
  {1 模块装入保存机制 }
  {{
  实际的装载、卸载发生在这里，管理从文件或数据库加载模块，模块的唯一性。
  }
  TTurboScriptAccessor = class(TObject)
  public
    {1 : Load the Virtual Machine Codes from the File. }
    procedure LoadFromFile(const aFileName: String);
    {1 : Load the Virtual Machine Codes from the Stream. }
    procedure LoadFromStream(const aStream: TStream); virtual;
    {1 : save the Virtual Machine Codes to file. }
    procedure SaveToFile(const aFileName: String);
    {1 : save the Virtual Machine Codes to Stream. }
    procedure SaveToStream(const aStream: TStream); virtual;
  end;
  

implementation

{
***************************** TTurboScriptAccessor *****************************
}
procedure TTurboScriptAccessor.LoadFromFile(const aFileName: String);
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

procedure TTurboScriptAccessor.LoadFromStream(const aStream: TStream);
var
  Lstr: string;
  L: Byte;
begin
  L := Length(cFORTHHeaderMagicWord);
  SetLength(Lstr, L);
  with aStream do
  begin
    Read(Lstr[1], L);
    if Lstr <> cFORTHHeaderMagicWord then
      Raise ESuperScriptError.Create(rsMissFileHeaderError);
  
    //Get the Unit(Program) Name.
    Read(L, SizeOf(L));
    SetLength(FName, L);
    if L<>0 then
      Read(FName[1], L);
  
    //Read(FModuleType, SizeOf(FModuleType)); //abondon
    Read(FFileVersion, SizeOf(FFileVersion));
    Read(FFileDate, SizeOf(FFileDate));
  end;
end;

procedure TTurboScriptAccessor.SaveToFile(const aFileName: String);
var
  aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(aFileStream);
  finally
    aFileStream.Free;
  end;
end;

procedure TTurboScriptAccessor.SaveToStream(const aStream: TStream);
var
  L: Byte;
begin
  aStream.Write(cFORTHHeaderMagicWord[1], Length(cFORTHHeaderMagicWord));
  //Get the Unit(Program) Name.
  L := Length(FName);
  aStream.Write(L, SizeOf(L));
  if L<>0 then
    aStream.Write(PChar(FName)^, L);
  
  //aStream.Write(FModuleType, SizeOf(FModuleType));
  aStream.Write(FFileVersion, SizeOf(FFileVersion));
  aStream.Write(FFileDate, SizeOf(FFileDate));
end;


end.
