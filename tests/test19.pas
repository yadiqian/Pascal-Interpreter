program test19;
(* Test function and procedure without parameters *)
var returnFive: real;

function returnTrue: boolean;
begin
  returnTrue := true;
end;

function returnFive: real;
begin
  returnFive := 5;
end;

begin
  returnFive := 5 * 5;

  writeln(returnTrue());
  writeln(returnFive());
  writeln(returnFive);
end.

(*
Expected output:

True
5.0
25.0

*)