program test9;
(* Test boolean operations with variables. *)

var
  a, b, c: boolean;

begin
  a := true;
  b := false;
  c:= a and b;
  writeln (c);
  c := not b and (a or b);
  writeln (c);
  c := a xor b;
  writeln (c);
  c := a xor true;
  writeln (c);
  c := c xor (b or not (a and b));
  writeln (c);
  c := not (a or b and (c xor b));
  writeln (c);
end.

(*
Expected output:

FALSE                                                                
TRUE                                                                 
TRUE                                                                 
FALSE                                                                
TRUE
FALSE

*)
