program test4;
(* Test if then else *)
var a, c: real;
var b: boolean;

begin
  a := 5;
  c := 3;
  b := true;
  if a - c = 2 then 
    // writeln ('This is true')
    writeln(1)
  else 
    begin
      a := 7;
      // writeln ('This is false');
      writeln(-1);
    end;
  writeln(a + 1);

  if (not b or b) then
    begin
      a := exp(c - a);
      writeln(a - 1);
    end
  else
    writeln (a + 1); 
end.

(*
Expected output:

This is true
5
0.1353352832366127

*)