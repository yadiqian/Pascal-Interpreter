program test22;
(* Test procedure overloading *)

var
  a, b, c, d: real;

procedure doSomething(a, b: real; var ans: real);
begin
  ans := a + b;
end;

procedure doSomething(a, b: real; var ans1: real; var ans2: real);
begin
  ans1 := a + b;
  ans2 := a - b;
end;

begin
  a := 5.5;
  b := 10.5;

  doSomething(a, b, c);
  writeln('a = ', a, ' b = ', b, ' c = ', c, ' d = ', d);
  
  doSomething(a, b, c, d);
  writeln('a = ', a, ' b = ', b, ' c = ', c, ' d = ', d);
end.

(*
Expected output:

a = 5.5 b = 10.5 c = 16 d = 0
a = 5.5 b = 10.5 c = 16 d = -5

*)