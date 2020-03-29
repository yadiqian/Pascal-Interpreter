program test15;
(* Test functions *)
var
   a, b, ret : real;

(*function definition *)
function max(num1, num2: real): real;
var
   (* local variable declaration *)
  result: real;
begin
  if (num1 > num2) then
    result := num1
  else
  begin
    result := num2;
  end;
  max := result;
end;

function xorVal(a, b: boolean; c: real): boolean;
begin
  xorVal := a xor b; 
end;

begin
  a := 100.0;
  b := 200.0;
  (* calling a function to get max value *)
  ret := max(400, 1020);
  writeln(ret);

  ret := max(a, b);   
  writeln(ret);

  writeln(xorVal(true, true, max(ret, 1010)));
end.

(*
Expected output:

1020.0
200.0
False

*)