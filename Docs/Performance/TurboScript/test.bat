@Echo Off

@Echo Compiling script....
tfcc.exe test.tf
tfcc.exe testAdd.tf
tfcc.exe testSub.tf
tfcc.exe testSubEx.tf
tfcc.exe testSubDll.tf
@Echo Compiled Done!
@Echo --------------

@Echo --------------
@Echo now runing:
@Echo --------------
@Echo test external subroute
tsrun testSubEx -d
pause

@Echo test subroute
tsrun testSub -d
pause

@Echo test add directive
tsrun testAdd -d
pause

@Echo test call DLL subroutine
tsrun testSubDLL -d
pause