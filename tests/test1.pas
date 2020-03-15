progRam test1;
(* Test writeln. *)

BEGIN
  WRITELN ('Hello world!');
  writeln('My dad''s fish learned how to swim.');
  writeln ('''''''''');
  writeln ();
  writeln (7);
  wrITeln (4 * 4);
  writeln (5.5 + 1.7 - 1.5);
  writeln (-13);
  writeln (1,' ', 2, ' ', 3, ' ', 4, ' ', 5);
  writeln ();
  writeln(true);
  writeln (true and (true and false));
  writeln(8.5 > 8);
eND.

(*
Expected output:

Hello world!
My dad's fish learned how to swim.
''''

7
16
5.7
-13
1 2 3 4 5

TRUE
FALSE
TRUE

*)
