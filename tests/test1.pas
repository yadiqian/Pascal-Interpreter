program test1;
(* Test writeln. *)

begin
  writeln (7);
  writeln (4 * 4);
  writeln (5.5 + 1.7 - 1.5);
  writeln (-13);
  writeln (1, 2, 3, 4, 5);
  writeln ();
  writeln(true);
  writeln (true and (true and false));
  writeln(8.5 > 8);
end.

(*
Expected output:

7.0
16.0
5.7
-13.0
1.0 2.0 3.0 4.0 5.0

True
False
True

*)
