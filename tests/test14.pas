program test14;
(* Test for loop *)
var
  a: real;

begin
  for a := 10 to 20 do   
    begin
      writeln('value of a: ', a);
    end;

   writeln();

  for a := 10 * 5 downto 20 * 2 do  
    begin
      writeln('value of a: ', a / 2);
    end;
end.

(*
Expected output:

value of a: 10                                                                                                          
value of a: 11                                                                                                          
value of a: 12                                                                                                          
value of a: 13                                                                                                          
value of a: 14                                                                                                          
value of a: 15                                                                                                          
value of a: 16                                                                                                          
value of a: 17                                                                                                          
value of a: 18                                                                                                          
value of a: 19                                                                                                          
value of a: 20                                                                                                          
                                                                                                                        
value of a: 25
value of a: 24.5
value of a: 24
value of a: 23.5
value of a: 23
value of a: 22.5
value of a: 22
value of a: 21.5
value of a: 21
value of a: 20.5
value of a: 20
*)