program test13;
(* Test while loop, continue and break keywords in while loop *)
var
  a: real;
  b: boolean;

begin
  a := 10;
  while a < 20 do
  
  begin
    writeln('value of a: ', a);
    a := a + 1;
    while (a < 15) do 
    begin
      writeln('hi');
      a := a + 1;
      continue;
      writeln('You are not supposed to see this!');
    end;
  end;

  while (a > 8) do
    a := a - 2;
  writeln('value of a: ', a);

  b := true;
  while (b) do
  begin
    if (a > 0) then
      a := a - 1
    else
      b := false;
    break;
  end;

  writeln('value of a: ', a);
  writeln('value of b: ', b);

end.

(*
Expected output:

value of a: 10                                                                                                          
hi                                                                                                                      
hi                                                                                                                      
hi                                                                                                                      
hi                                                                                                                      
value of a: 15                                                                                                          
value of a: 16                                                                                                          
value of a: 17                                                                                                          
value of a: 18                                                                                                          
value of a: 19                                                                                                          
value of a: 8                                                                                                           
value of a: 7                                                                                                           
value of b: TRUE

*)