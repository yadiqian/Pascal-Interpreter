program test3;
(* Test special expressions *)

begin
  writeln (sqrt(4 + 12 / 3));
  writeln(sqrt(150));
  writeln (sin(1/2));
  writeln (sin(100));
  writeln (cos(1 - (2 * 3 - 5)));
  writeln (cos(-100.6));
  writeln (ln(1 / 5 + 3));
  writeln (ln(71 - 4 * 2.5));
  writeln (exp(3));
  writeln (exp(4 * (5.5 - 1)));
end.

(*
Expected output:

2.828427
12.247449
0.47942555
-0.50636566
1.0
0.99761814
1.1631508
4.1108737
20.085537
6.5659968e7

*)