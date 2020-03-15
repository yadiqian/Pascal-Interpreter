program test24; 
(* Test loops in procedure and function *)
var count: real;

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

function count: real;
var i: real;
begin
  for i := 0 to 5 do
    writeln(i);

  while i >= 0 do
  begin
    writeln(i);
    i := i - 1;
  end;

  count := i;
end;

begin
  print();
  count := count();
  writeln(count);
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