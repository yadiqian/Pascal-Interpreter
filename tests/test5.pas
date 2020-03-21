program test5;
(* Test case statements. *)
var
  grade: real;
var value: boolean;

begin
  grade := 5;
  case (grade) of
    5: writeln(5);
    4: writeln(4);
    3: writeln(3);
    2: writeln(2);
    1: writeln(1);
  else
    writeln(1111);
    writeln(2222);
  end; 

  grade := 3;
  case (grade) of
    5: writeln(5);
    4: writeln(4);
    3: writeln(3);
    2: writeln(2);
    1: writeln(1);
  else
    writeln(1111);
    writeln(2222);
  end; 

  grade := -15;
  case (grade) of
    5: writeln(5);
    4: writeln(4);
    3: writeln(3);
    2: writeln(2);
    1: writeln(1);
  else
    writeln(1111);
    writeln(2222);
  end; 

  value := true;
  case (value) of 
    true: writeln(true);
    false: writeln(false);
  else
    writeln(0);
  end;
end.

(*
Expected output (changed grade type from real to integer):

Excellent!                                                                                                                              
You passed                                                           
else1                                                                
else2                                                                
This is true

*)
