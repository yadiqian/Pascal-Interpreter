program test18;
(* Test static scoping *)
var
  a, b, c: real;

procedure display(a, b, c: real);
begin
  a := a / 10;
  b := b / 10;
  c := c / 10;
  
  writeln('Winthin procedure display');
  writeln('a = ', a , ' b = ',  b, ' c =', c);
end;

function displayFunc(a, b, c: real): real;
begin
  a := a / 100;
  b := b / 100;
  c := c / 100;
  
  writeln('Winthin functnion displayFunc');
  writeln('a = ', a , ' b = ',  b, ' c = ', c);

  displayFunc := c;
end;

begin
  a:= 100;
  b:= 200;
  c:= a + b;
  
  writeln('Winthin program test18');
  writeln('a = ', a , ' b = ',  b, ' c = ', c);
  display(a, b, c);

  displayFunc(a, b, c);

  writeln('Winthin program test18');
  writeln('a = ', a , ' b = ',  b, ' c = ', c);

  c := displayFunc(a, b, c);
  writeln('Winthin program test18');
  writeln('a = ', a , ' b = ',  b, ' c = ', c);

end.

(*
Expected output:

Winthin program test18
a = 100 b = 200 c = 300
Winthin procedure display
a = 10 b = 20 c =30
Winthin functnion displayFunc
a = 1 b = 2 c = 3
Winthin program test18
a = 100 b = 200 c = 300
Winthin functnion displayFunc
a = 1 b = 2 c = 3
Winthin program test18
a = 100 b = 200 c = 3

*)