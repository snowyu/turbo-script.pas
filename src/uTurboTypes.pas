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
  {
    TypeId: Int64 for the type in stream!
      TypeId = 0                                        means nill. [impl in the TMeRegisteredTypes]
      TypeId >= 1 and TypeId <= GRegisteredTypes.Count  means the Index(+1) of the GRegisteredTypes.
      TypeId > GRegisteredTypes.Count                   means the offset of the stream. [impl in the TMeRegisteredTypes]
      TypeId < 0                                        means this external reference type!
      
  }
  TTurboRegisteredTypes = object(TMeTypes)
  protected
    function GetTypeByTypeId(const aTypeId: Int64): PMeType; virtual; {override}
    function GetTypeIdByType(const aType: PMeType): Int64; virtual;   {override}
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


function TTurboRegisteredTypes.GetTypeIdByType(const aType: PMeType): Int64;
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

function TTurboRegisteredTypes.GetTypeByTypeId(const aTypeId: Int64): PMeType;
begin
  if aTypeId <= GRegisteredTypes.Count then
  begin
    //the internal type
    if aTypeId = 0 then
      Result := nil
    else if aTypeId > 0 then
      Result := GRegisteredTypes.Items[aTypeId-1]
    else  //i < 0 the external type
    begin
    end;
    Exit;
  end;
  Result := inherited GetTypeByTypeId(aTypeId);
end;

initialization
  SetMeVirtualMethod(TypeOf(TTurboRegisteredTypes), ovtVmtParent, TypeOf(TMeTypes));


finalization
end.
