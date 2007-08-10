//( test program )
Program test

const
  vCC = $20;
  vBB = $21;
  vB1 = vBB;
  vStr: ShortString =  'Hello word!';

var
  Public named vAA : shortstring = vStr;
  vBC : string = 'BeijingÄãºÃ£¡';

: Public named Add +;
: Public named Add2 2 +;
: Public named LN 10 13 EMIT EMIT;

begin
{  33 67 +
  50 Add Add2
  1 2 3 4 5 6
  7 8 9 10 11 12 -
}
 vAA @ .S 
 LN 
 vBC @ .LS 
 42 43 #'Food' EMIT EMIT EMIT EMIT EMIT EMIT
 LN
 23 32 Add
 45 add
 vCC vBB vB1
 //$7FFFFFF1 $7FFFFFF2 $7FFFFFF3 $7FFFFFF4 $7FFFFFF5 $7FFFFFF6 $7FFFFFF7 $7FFFFFF8 $7FFFFFFF
 //vAA @ .S LN
end.
