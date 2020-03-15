program test11;
(* A program that add 2 input numbers. *)
var a, b, c: real;

begin
  writeln ('Please input 2 numbers: ');
  readln (a, b);
  c := a + b;
  writeln ('The sum of the two numbers is: ', c);

  if (c < 100) then 
    writeln ('The sum of your numbers is small.')
  else
    begin
      writeln ('The sum of your numbers is large.');
    end;
end.

(*
Expected output:

Output is dependent on the user input.

Sample output:

Please input 2 numbers:                                                              
123                                                                                  
-98                                                                                  
The sum of the two numbers is: 2.50000000000000E+001                                 
The sum of your numbers is small.

*)