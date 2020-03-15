program test20;
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
00
01
02
03
04
05
10
11
12
13
14
15
20
21
22
23
24
25
30
31
32
33
34
35
40
41
42
43
44
45
50
51
52
53
54
55

55                                                                                                                      
54                                                                                                                      
53                                                                                                                                                                                                                                           
45                                                                                                                      
44                                                                                                                      
43

*)