program test20;
(* Test print string in function as params *)

function add (a, b: real): real;
begin
  writeln(1111);
  add := a + b;
end;

begin
  writeln(0.5);
  writeln(add(add(0, 1), add(1, 1)) + add(add(1, 2), add(2, 2)) * add(4, -2));
end.

(*
Expected output:

0.5
1111.0
1111.0
1111.0
1111.0
1111.0
1111.0
1111.0
17.0

*)