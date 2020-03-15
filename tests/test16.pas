program test16;
(* Test procedure *)
var
  a, b, c, min: real;
  test: boolean;

procedure findMin(x, y, z: real; var m: real; var n: boolean); 
(* Finds the minimum of the 3 values *)

begin
  if x < y then
    m:= x
  else
    m:= y;
  
  if z < m then
    m:= z;

  n := true;
end;

begin
  a := 100;
  b := 430;
  c := -11;
  findMin(a, b, c, min, test); (* Procedure call *)
   
  writeln('Minimum: ', min);
  writeln(test);
end.

(*
Expected output:

Minimum: -11
TRUE

*)