program test4;
(* Test if then else *)
var a, c: real;
var b: boolean;

begin
  a := 5;
  c := 3;
  b := true;
  if a - c = 2 then 
    writeln(1)
  else 
    begin
      a := 7;
      writeln(0);
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

1.0
5.0
0.13533528

*)