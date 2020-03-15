program test2;
(* Test variable declaration and assignment. *)
var 
  a, b: Real;
  c, d: boolean;
var e: real;

begin
  a := 3;
  b := 5.555;
  c := false;
  d := TRUE;
  writeln(a + b);
  writeln(c and d);
  e := a - b;
  writeln (e < a);
end.

(*
Expected Output:

8.555                                              
FALSE                                                                
TRUE

*)