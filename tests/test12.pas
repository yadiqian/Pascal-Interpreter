program test12;
(* Calculates distance formula. *)
var x1, y1, x2, y2, ans: real;

begin
  x1 := 14.11;
  x2 := -6.5;
  y1 := -4.5;
  y2 := 89;
  ans := sqrt((x1 - x1) * (x1 - x1) + (y1 - y2) * (y1 - y2));
  writeln (ans);
end.

(*
Expected output:

93.5

*)