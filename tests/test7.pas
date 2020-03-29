program test7;
(* Test arithmetic operations with variables. *)

var
  a, b, c, d: real;

begin
  a := 5;
  b := -10;
  c := 3;
  d := a - c + 1;
  writeln (d);
  d := a + b * (d + a);
  writeln (d);
  d := d - c + (a * (b + (c - d)));
  writeln (d);
  d := a / b * c + 14;
  writeln (d);
  d := d * d / (c / 2) * (a - 1);
end.

(*
Expected output:

3.0
-75.0
262.0
12.5

*)
