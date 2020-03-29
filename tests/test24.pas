program test24; 
(* Test loops in procedure and function *)
var ans: real;

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
  ans := sum();
  writeln(ans);
end.

(*
Expected output:

0                                                                                                                       
1                                                                                                                       
2                                                                                                                       
3                                                                                                                       
4                                                                                                                       
5                                                                                                                       
5                                                                                                                       
4                                                                                                                       
3                                                                                                                       
2                                                                                                                       
1                                                                                                                       
0                                                                                                                       
0                                                                                                                       
1                                                                                                                       
2                                                                                                                       
3                                                                                                                       
4                                                                                                                       
5                                                                                                                       
5                                                                                                                       
4                                                                                                                       
3                                                                                                                       
2                                                                                                                       
1                                                                                                                       
0                                                                                                                       
-1

*)