{: The script compiler module. }
{ Description
the abstract parser, compiler class, and the compiler factory here.
}
unit uTurboCompiler;

interface

uses
  SysUtils, Classes;

type
  TTTurboModule = class;
  TCustomTurboCompiler = class;
  TTTurboModule = class(TCustomTurboModule)
  private
    FChilds: TList;
    FParent: TTurboModule;
    FSymbols: TList;
    FUsedModules: TList;
    FVariables: TList;
    FWords: TList;
  public
    { Description
    根据编译开关将这些列表的类编译进入Memory.
    比如如果该模块的Accessor为DBAccessor,那么就只能使用后期绑定的形式编译.
    还有如果该模块的编译开关要求函数分别

    See Also
      Options
    }
    procedure Compile;
    {: 它的子模块列表:被 Parser 或Tree使用 }
    property Childs: TList read FChilds write FChilds;
    {: 它的父亲:被 Parser 或Tree使用. nil means root. }
    property Parent: TTurboModule read FParent write FParent;
    property Symbols: TList read FSymbols write FSymbols;
    property UsedModules: TList read FUsedModules write FUsedModules;
    property Variables: TList read FVariables write FVariables;
    property Words: TList read FWords write FWords;
  end;

  TCustomTurboCompiler = class(TCustomTurboObject)
  end;


implementation

{
******************************** TTTurboModule *********************************
}
procedure TTTurboModule.Compile;
begin
end;


end.
