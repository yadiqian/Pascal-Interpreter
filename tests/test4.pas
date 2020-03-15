program test4;
(* Test if then else *)
var 
  a, c: real;
  b: boolean;

begin
  a := 5;
  c := 3;
  b := true;
  if a - c = 2 then 
    writeln ('This is true')
  else 
    begin
      a := 7;
      writeln ('This is false');
    end;
  writeln(a);

  if (not b or b) then
    begin
      a := exp(c - a);
      writeln(a);
    end
  else
    writeln (a); 
end.

(*
Expected output:

This is true
5
0.1353352832366127

*)