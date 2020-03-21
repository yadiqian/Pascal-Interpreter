program test7;
(* Test arithmetic operations with variables. *)

var a, b, c, d, e: real;
var f: boolean;

begin
  a := 5.55;
  b := -18;
  c := 77;
  d := -(a * b) + c - b;
  writeln (d);
  e:= -a + b * (d + a);
  writeln (e);
  writeln(d - c);
  d:= d - c + (a * (b + (c - d)));
  writeln (d);
  e := -exp(d) + -sin(a) / cos(b);
  writeln (e);
  e := a;
  writeln (e);
  f := true;
end.

(*
Expected output:

194.9
-3613.65
117.899994
-636.345
1.0135132
5.55

*)
