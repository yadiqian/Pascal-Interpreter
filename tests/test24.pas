program test24; 
(* Test loops in procedure and function *)

procedure print;
var i: real;
begin
  for i := 0 to 5 do
    writeln(i);

  while i >= 0 do
  begin
    writeln(i);
    i := i - 1;
  end;
end;

function sum: real;
var i, ret: real;
begin
  for i := 0 to 5 do
  ret := ret + i;

  while i >= 0 do
  begin
    ret := ret + i;
    i := i - 1;
  end;
  sum := ret;
end;

begin
  print();
  writeln(sum());
end.

(*
Expected output:

0.0
1.0
2.0
3.0
4.0
5.0
5.0
4.0
3.0
2.0
1.0
0.0
30.0

*)