progRam test7;
(* Test arithmetic operations with variables. *)

var
  a, b, c, d, e: real;
  f: boolean;

BEGIN
  a := 5.55;
  b := -18;
  c := 77;
  d := -(a * b) + c - b;
  writeln (d);
  e:= -a + b * (d + a);
  writeln (e);
  writeln(d - c + 1);
  d:= d - c + (a * (b + (c - d)));
  writeln (d);
  e := -exp(d) + -sin(a) / cos(b);
  writeln (e);
  e := a;
  writeln (e);
  f := true;
eND.

(*
Expected output:

194.89999999999998
-3613.65
118.89999999999998
-636.3449999999999
1.0135134382043889
5.55

*)
