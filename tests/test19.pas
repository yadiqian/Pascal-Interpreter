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

  writeln('Function return value is: ', returnTrue());
  writeln('Function return value is: ', returnFive());
  writeln('Variable value is: ', returnFive);
end.

(*
Expected output:

Function return value is: TRUE
Function return value is: 5
Variable value is: 25

*)