program fibonacci;
(* Test recursive function call *)
var
  ret, i : real;

function fibonacci(n:real): real;
begin
  if n > 1 then
  fibonacci := fibonacci(n - 1) + fibonacci(n - 2)
  else
    fibonacci := n;
end;

begin
  for i := 0 to 20 do 
  begin
    ret := fibonacci(i);
    writeln(i, ret);
  end;
end.

(* 
Expected output:

0.0 0.0 
1.0 1.0 
2.0 1.0
3.0 2.0
4.0 3.0
5.0 5.0
6.0 8.0
7.0 13.0
8.0 21.0
9.0 34.0
10.0 55.0
11.0 89.0
12.0 144.0
13.0 233.0
14.0 377.0
15.0 610.0 
16.0 987.0
17.0 1597.0
18.0 2584.0 
19.0 4181.0 
20.0 6765.0 

*)