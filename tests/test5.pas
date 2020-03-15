program test5;
(* Test case statements. *)
var
  grade: real;
  value: boolean;

begin
  grade := 5;
  case (grade) of
    5: writeln('Excellent!');
    4: writeln('Well done');
    3: writeln('You passed');
    2: writeln('Better try again');
    1: writeln('You failed hard');
  else
    writeln('else1');
    writeln('else2');
  end; 

  grade := 3;
  case (grade) of
    5: writeln('Excellent!');
    4: writeln('Well done');
    3: writeln('You passed');
    2: writeln('Better try again');
    1: writeln('You failed hard');
  else
    writeln('else1');
    writeln('else2');
  end; 

  grade := -15;
  case (grade) of
    5: writeln('Excellent!');
    4: writeln('Well done');
    3: writeln('You passed');
    2: writeln('Better try again');
    1: writeln('You failed hard');
  else
    writeln('else1');
    writeln('else2');
  end; 

  value := TRUE;
  case (value) of 
    true: writeln('This is true');
    false: writeln('This is false');
  else
    writeln('This is neither true or false');
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
