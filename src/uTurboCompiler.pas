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
    ���ݱ��뿪�ؽ���Щ�б����������Memory.
    ���������ģ���AccessorΪDBAccessor,��ô��ֻ��ʹ�ú��ڰ󶨵���ʽ����.
    ���������ģ��ı��뿪��Ҫ�����ֱ�

    See Also
      Options
    }
    procedure Compile;
    {: ������ģ���б�:�� Parser ��Treeʹ�� }
    property Childs: TList read FChilds write FChilds;
    {: ���ĸ���:�� Parser ��Treeʹ��. nil means root. }
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
