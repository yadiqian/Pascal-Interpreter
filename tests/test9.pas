program test9;
(* Test nested code blocks. *)

var
  a, b, c: boolean;

begin
  writeln (1);
  begin
    writeln(2);
  end;
  begin
    writeln(3);
    begin
      writeln (4);
        begin
          writeln (5);
        end;
    end;
  end;
  begin
    writeln(6);
  end;
end.

(*
Expected output:

1.0
2.0
3.0
4.0
5.0
6.0

*)
