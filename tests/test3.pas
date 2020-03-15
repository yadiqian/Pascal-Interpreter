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

2.8284271247461903
12.24744871391589
0.479425538604203
-0.5063656411097588
1
0.9976180247462534
1.1631508098056809
4.110873864173311
20.085536923187668
65659968

*)