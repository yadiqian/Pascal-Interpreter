program test16;
(* Test static scoping *)
var
  a, b, c: real;

procedure display(a, b, c: real);
begin
  a := a / 10;
  b := b / 10;
  c := c / 10;
  
  writeln(a, b, c);
end;

function displayFunc(a, b, c: real): real;
begin
  a := a / 100;
  b := b / 100;
  c := c / 100;
  
  writeln(a, b, c);

  displayFunc := c;
end;

begin
  a:= 100;
  b:= 200;
  c:= a + b;
  
  writeln(a, b, c);
  display(a, b, c);

  displayFunc(a, b, c);

  c := displayFunc(a, b, c);
  writeln(a, b, c);
end.

(*
Expected output:

100.0 200.0 300.0 
10.0 20.0 30.0 
1.0 2.0 3.0
1.0 2.0 3.0
100.0 200.0 3.0

*)