{$IFNDEF TurboScript_INC}
{$DEFINE TurboScript_INC}
{ General Compiler Directives and Compiler Versions Setting }

{------------------------------------------------------------------------------}
{ Please change this to suit your needs                                        }
{------------------------------------------------------------------------------}

{$define Debug}
{.$DEFINE PUREPASCAL}

{$Define TurboScript_Stepping_Supports}
{$Define TurboScript_DataStack_Overflow_Inspect}
{$Define TurboScript_ReturnStack_Overflow_Inspect}
{$Define TurboScript_Memory_Overflow_Inspect}
{$Define TurboScript_ExecTimeOut_Supports}

{Include Common setting File}
{$I Setting.inc}


{$ENDIF ~TurboScript_INC}