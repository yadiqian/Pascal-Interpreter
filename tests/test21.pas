program test21;
(* Test function overloading *)

var
  a, b, c: real;

function max(a, b: real): real;
begin
  if (a > b) then
    max := a
  else
    max := b;
end;

function max(a, b, c: real): real;
begin
  if a > b then
    max := a
  else
    max := b;

  if max < c then
    max := c;
end;

begin
  a := 0.98;
  b := -5;
  c := 4.34;

  writeln(max(a, b));
  writeln(max(b, c));
  writeln(max(a, b, c));
end.

(*
Expected output:

0.98
4.34
4.34

*)