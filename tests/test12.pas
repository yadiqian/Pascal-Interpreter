program test12;
(* Test for loop *)
var
  a: real;

begin
  for a := 10 to 20 do   
    begin
      writeln(a);
    end;

   writeln();

  for a := 10 * 5 downto 20 * 2 do  
    begin
      writeln(a / 2);
    end;
end.

(*
Expected output:

10.0
11.0
12.0
13.0
14.0
15.0
16.0
17.0
18.0
19.0
20.0

25.0
24.5
24.0
23.5
23.0
22.5
22.0
21.5
21.0
20.5
20.0

*)