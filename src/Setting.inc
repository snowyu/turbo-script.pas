{$IFNDEF SETTING_INC}
{$DEFINE SETTING_INC}
{ General Compiler Directives and Compiler Versions Setting }

{Include Delphi Versions File}
{$I Jedi.inc}

{------------------------------------------------------------------------------}
{ Please change this to suit your needs                                        }
{------------------------------------------------------------------------------}
{.$define Debug}

{------------------------------------------------------------------------------}
{ Common compiler defines                                                      }
{------------------------------------------------------------------------------}
// support for multibyte character sets
{$IFDEF COMPILER3_UP}
  {$DEFINE MBCS_SUPPORT}
{$ENDIF}

{***************** These compiler settings shouldn't be changed ***************}
  {$A+ Force alignment on word/dword boundaries}
  {$S+ stack checking}

  {$B- Short evaluation of boolean values}
  {$H+ Long string support}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$P+ Open string parameters}
  {$J+ Writeable typed constants}
  {$R- No Range checking}
  {$Z1 MIN ENUM SIZE}

{$IFDEF BCB}
  {$OBJEXPORTALL ON}
{$ENDIF}

// additional tests for debugging
{$IFDEF DEBUG}
  {$HINTS ON; WARNINGS ON}
  {$D+,L+,W+,Y+,YD,Q+,T+}
{$ELSE}
  {$HINTS OFF; WARNINGS OFF}
  {$D-,L-,Y-,Q-,W-,T-}
  {$IMAGEBASE $00400000}
{$ENDIF}

{$ENDIF ~SETTING_INC}