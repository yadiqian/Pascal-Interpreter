program test18;
(* Test nested for loops and nested while loop *)
var i, j: real;

begin
  for i := 0 to 5 do
    begin
      for j := 0 to 5 do
        writeln(i, j);
    end;

    writeln();

    while i > 3 do
      begin
        while j >= 3 do
          begin
            writeln(i, j);
            j := j - 1;
          end;
        i := i - 1;
        j := 5;
      end;
end.

(*
Expected output:

0.0 0.0 
0.0 1.0 
0.0 2.0
0.0 3.0
0.0 4.0
0.0 5.0
1.0 0.0
1.0 1.0
1.0 2.0
1.0 3.0
1.0 4.0
1.0 5.0
2.0 0.0
2.0 1.0
2.0 2.0
2.0 3.0
2.0 4.0
2.0 5.0
3.0 0.0
3.0 1.0
3.0 2.0
3.0 3.0
3.0 4.0
3.0 5.0
4.0 0.0 
4.0 1.0
4.0 2.0
4.0 3.0
4.0 4.0
4.0 5.0
5.0 0.0
5.0 1.0
5.0 2.0
5.0 3.0
5.0 4.0
5.0 5.0

5.0 5.0
5.0 4.0
5.0 3.0
4.0 5.0
4.0 4.0
4.0 3.0

*)