( test program )
Program test

const
  vCC = $20;
  vBB = $21;
  vB1 = $333;

var
  Published vAA : shortstring = 'Hello word!';
  vBC : string = 'BeijingÄãºÃ£¡';

: Published Add +;
: Published Add2 2 +;
: Published LN 10 13 EMIT EMIT;

begin
{  33 67 +
  50 Add Add2
  1 2 3 4 5 6
  7 8 9 10 11 12 -
}
 {vCC vBB} vAA @ .S LN vBC @ .LS (10 13 #'Food' EMIT EMIT EMIT EMIT EMIT EMIT)
 23 32 Add
 45 add
 $7FFFFFF1 $7FFFFFF2 $7FFFFFF3 $7FFFFFF4 $7FFFFFF5 $7FFFFFF6 $7FFFFFF7 $7FFFFFF8 $7FFFFFFF
end.
