unit uSuperPEFormat;

interface

uses
  SysUtils, Classes;

type
  { Summary the Module type.}
  { Description
    @param mtExecutor the module registered in the internal Executor.
    @param mtForthDLL the Forth VM(Virtual Mache-code) DLL module
    @param mtDLL the DLL module
  }
  TModuleType = (mtExecutor, mtForthDLL, mtDLL);
  //in fact it is the index No. in the CodeArea array.
  TVirtualAddress = integer;
  TVirtualAddressTable = array of TVirtualAddress;
  TSuperPEFormat = class(TCustomSuperPEFormat)
  end;
  

implementation


end.
