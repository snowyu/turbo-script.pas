@Echo Compiling script....
tfcc.exe test.tf
tfcc.exe testAdd.tf
tfcc.exe testSub.tf
tfcc.exe testSubEx.tf

@Echo test external subroute
tsrun testSubEx -d
pause

Echo test subroute
tsrun testSub -d
pause

Echo test add directive
tsrun testAdd -d
pause