progRam test1;
(* Test readln. *)

var
  a, b, c, d: real;

BEGIN
  writeln ('Input a real number: ');
  readln(a);
  writeln(a);
  writeln ('Input 3 real numbers: ');
  readln(b, c, d);
  writeln(b, ' ', c, ' ', d);
eND.

(*
Expected output:

Whatever the user inputs should be the output.

*)
