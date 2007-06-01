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
  PTurboRegisteredTypes = ^ TTurboRegisteredTypes;
  {: manage the PMeTypes in the list.}
  {
    TypeId: Int64 for the type in stream!
      TypeId = 0                                        means nill. [impl in the TMeRegisteredTypes]
      TypeId >= 1 and TypeId <= GRegisteredTypes.Count  means the Index(+1) of the GRegisteredTypes.
      TypeId > GRegisteredTypes.Count                   means the offset of the stream. [impl in the TMeRegisteredTypes]
      TypeId < -1                                       means this external reference type!
      TypeId = -1                                       means error!
      
  }
  TTurboRegisteredTypes = object(TMeTypes)
  protected
    function GetTypeByTypeId(const aTypeId: LongInt): PMeType; virtual; {override}
    function GetTypeIdByType(const aType: PMeType): LongInt; virtual;   {override}
  public
    destructor Destroy; virtual; //override
  end;

implementation

uses
  uTurboExecutor
  ;

destructor TTurboRegisteredTypes.Destroy;
begin
  inherited;
end;


function TTurboRegisteredTypes.GetTypeIdByType(const aType: PMeType): LongInt;
begin
  if Assigned(aType) then
  begin
    Result := GRegisteredTypes.IndexOf(aType);
    if Result >= 0 then
    begin
      //the internal type
      Inc(Result);
      Exit;
    end;
  end;
  Result := inherited GetTypeIdByType(aType);
end;

function TTurboRegisteredTypes.GetTypeByTypeId(const aTypeId: LongInt): PMeType;
begin
  if (aTypeId > 0) and (aTypeId <= GRegisteredTypes.Count) then
    Result := GRegisteredTypes.Items[aTypeId-1]
  else if (aTypeId < -1) then  //the external type
  begin
    //TODO:
    //Result := nil;
  end
  else
    Result := inherited GetTypeByTypeId(aTypeId);
end;

initialization
  SetMeVirtualMethod(TypeOf(TTurboRegisteredTypes), ovtVmtParent, TypeOf(TMeTypes));


finalization
end.
