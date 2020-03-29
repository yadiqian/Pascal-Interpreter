program test11;
(* Test while loop *)
var a: real;
var b: boolean;

begin
  a := 10;
  while a < 20 do
  
  begin
    writeln(a);
    a := a + 1;
    while (a < 15) do 
    begin
      writeln(15);
      a := a + 1;
    end;
  end;

  while (a > 8) do
    a := a - 2;
  writeln(a);

  b := true;
  while (b) do
  begin
    if (a > 0) then
      a := a - 1
    else
      b := false;
  end;

  writeln(a);
  writeln(b);

end.

(*
Expected output:

10.0
15.0
15.0
15.0
15.0
15.0
16.0
17.0
18.0
19.0
8.0
0.0
False

*)