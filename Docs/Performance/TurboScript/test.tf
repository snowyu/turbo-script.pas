//test lib
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
 vAA @ .S 
 LN 
 vBC @ .LS
 23 32 Add
 45 add
 $7FFFFFF1 $7FFFFFF2 $7FFFFFF3 $7FFFFFF4 $7FFFFFF5 $7FFFFFF6 $7FFFFFF7 $7FFFFFF8 $7FFFFFFF
end.
