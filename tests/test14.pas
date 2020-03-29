program test14;
(* Test procedure *)
var a, b, c, min, rand: real;
var test: boolean;

procedure findMin(x, y, z: real; var m, r: real; var n: boolean); 
(* Finds the minimum of the 3 values *)

begin
  if x < y then
    m:= x
  else
    m:= y;
  
  if z < m then
    m := z;

  n := true;
  r := 2020;
end;

begin
  a := 100;
  b := 430;
  c := -11;
  findMin(a, b, c, min, rand, test); (* Procedure call *)
  
  writeln(a, b, c);
  writeln(min);
  writeln(rand);
  writeln(test);

end.

(*
Expected output:

100.0 430.0 -11.0 
-11.0
2020.0
True

*)