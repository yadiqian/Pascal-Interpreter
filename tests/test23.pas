program test23;
(* Test continue and break keywords *)
var
  i, j: real;

begin
  i := 0;
  while true do
  begin
    if (i = 10) then 
      break;
    i := i + 1;
    writeln(i);
  end;

  while i < 20 do
  begin
    i := i + 1;
    if (i < 15) then continue;
    writeln(i);
  end;
    
    for j := 20 downto 0 do  
    begin
      if (j > 15) then
      begin
          writeln('var j is over 15');
          continue;
      end
      else
      begin
          writeln('var j is 15');
          break;
      end;

      writeln('var j is ', j);
    end;
  
end.

(*
Expected output:

1                                                                                                                       
2                                                                                                                       
3                                                                                                                       
4                                                                                                                       
5                                                                                                                       
6                                                                                                                       
7                                                                                                                       
8                                                                                                                       
9                                                                                                                       
10                                                                                                                      
15                                                                                                                      
16                                                                                                                      
17                                                                                                                      
18                                                                                                                      
19                                                                                                                      
20                                                                                                                      
var j is over 15                                                                                                        
var j is over 15                                                                                                        
var j is over 15                                                                                                        
var j is over 15                                                                                                        
var j is over 15                                                                                                        
var j is 15

*)