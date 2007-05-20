unit uTurboTypes;

interface

{$I TurboScript.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  {$IFDEF DEBUG}
  , DbugIntf
  {$ENDIF}
  , Variants
  , TypInfo
  , uMeSystem
  , uMeObject
  , uMeTypes
  , uMeProcType
  , uTurboConsts
  ;


type
  {: manage the PMeTypes in the list.}
  TTurboRegisteredTypes = object(TMeTypes)
  protected
    //容器，来保存类型与对应于在流中的偏移量 (PMeType(Items[i]) : FStreamOffsetList[i])
    FStreamOffsetList: array of Int64;
  protected
    function DoOnSaveType(const aStream: TStream; const aType: PMeType): Boolean;
    function DoOnLoadType(const aStream: TStream; var aType: PMeType): Boolean;
    function FindTypeOfOffset(const Offset: Integer): PMeType;
  public
    destructor Destroy; virtual; //override
    procedure LoadFromStream(const aStream: TStream);
    procedure SaveToStream(const aStream: TStream);
  end;

  PTurboVariable = ^ TTurboVariable;
  TTurboVariable = Object(TMeDynamicObject)
  protected
    FDataType: PMeType;
    FValue: TMeVarRec;
  public
    property Value: TMeVarRec read FValue write FValue;
    property DataType: PMeType read FDataType write FDataType;
  end;


implementation

uses
  uTurboExecutor
  ;

destructor TTurboRegisteredTypes.Destroy;
begin
  SetLength(FStreamOffsetList, 0);
  inherited;
end;
function TTurboRegisteredTypes.FindTypeOfOffset(const Offset: Integer): PMeType;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    if FStreamOffsetList[i] = Offset then
    begin
      Result := Items[i];
      exit;
    end;
  end;
  Result := nil;
end;

function TTurboRegisteredTypes.DoOnSaveType(const aStream: TStream; const aType: PMeType): Boolean;
var
  i: Integer;
  j: Int64;
begin
  if Assigned(aType) then
  begin
    j := GRegisteredTypes.IndexOf(aType);
    if j >= 0 then
    begin
      //the internal type
      inc(j);
      aStream.WriteBuffer(j, SizeOf(j))
    end
    else
    begin
      i := IndexOf(aType);
      if i >= 0 then
      begin
        aStream.WriteBuffer(FStreamOffsetList[i], SizeOf(Int64))
      end
      else
        Raise Exception.Create('DoOnSaveType: not found such MeType!');
    end;
  end
  else
  begin
    j := 0;
    aStream.WriteBuffer(j, SizeOf(j))
  end;
  Result := True;
end;

function TTurboRegisteredTypes.DoOnLoadType(const aStream: TStream; var aType: PMeType): Boolean;
var
  i: Int64;
begin
  aStream.ReadBuffer(i, SizeOf(i))
  if i < GRegisteredTypes.Count then
  begin
    //the internal type
    if i=0 then
      aType := nil
    else
      aType := GRegisteredTypes.Items[i-1];
  end
  else
  begin
    aType := FindTypeOfOffset(i);
    i := IndexOf(aType);
    if not Assigned(aType) then
      Raise Exception.Create('DoOnLoadType: not found such MeType!');
  end;
  Result := True;
end;

procedure TTurboRegisteredTypes.LoadFromStream(const aStream: TStream);
var
  i: Integer;
begin
  if TCustomTurboModule(FOwner) is TCustomTurboModule then
  begin
    SetLength(FStreamOffsetList, Count);
    for i := 0 to Count - 1 do
    begin
      vOffsetList[i]:= aStream.Position;
      PMeType(Items[i]).LoadFromStream(aStream, DoOnLoadType);
    end;
  end;
end;

procedure TTurboRegisteredTypes.SaveToStream(const aStream: TStream);
var
  i: Integer;
begin
  if TCustomTurboModule(FOwner) is TCustomTurboModule then
  begin
    SetLength(FStreamOffsetList, Count);
    for i := 0 to Count - 1 do
    begin
      vOffsetList[i]:= aStream.Position;
      PMeType(Items[i]).SaveToStream(aStream, DoOnSaveType);
    end;
  end;
end;


initialization
  //SetMeVirtualMethod(TypeOf(TMeType), ovtVmtParent, TypeOf(TMeDynamicObject));


finalization
end.
