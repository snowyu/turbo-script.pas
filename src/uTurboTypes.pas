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
    function DoOnSaveType(const aStream: TStream; const aType: PMeType): Boolean; virtual; //override
    function DoOnLoadType(const aStream: TStream; var aType: PMeType): Boolean; virtual; //override
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

function TTurboRegisteredTypes.DoOnSaveType(const aStream: TStream; const aType: PMeType): Boolean;
var
  j: Int64;
begin
  if Assigned(aType) then
  begin
    j := GRegisteredTypes.IndexOf(aType);
    if j >= 0 then
    begin
      //the internal type
      inc(j);
      aStream.WriteBuffer(j, SizeOf(j));
      Result := True;
      Exit;
    end
  end
  Result := inherited DoOnSaveType(aStream, aType);
end;

function TTurboRegisteredTypes.DoOnLoadType(const aStream: TStream; var aType: PMeType): Boolean;
var
  i, j: Int64;
begin
  {TODO: 如果类型在其它单元呢？可以考虑使用负数，如果是负数那么则是引用类型（引用其它单元的）！
    需要建立一个 LastTypeInfoRefEntry
  } 
  j := aStream.Position;
  aStream.ReadBuffer(i, SizeOf(i));
  if i < GRegisteredTypes.Count then
  begin
    //the internal type
    if i = 0 then
      aType := nil
    else if i > 0 then
      aType := GRegisteredTypes.Items[i-1]
    else  //i < 0 the external type
      ;
    Result :=True;
    Exit;
  end;

  //restore the Position.
  aStream.Position := j;
  Result := inherited DoOnLoadType(aStream, aType);
end;

initialization
  SetMeVirtualMethod(TypeOf(TTurboRegisteredTypes), ovtVmtParent, TypeOf(TMeTypes));


finalization
end.
