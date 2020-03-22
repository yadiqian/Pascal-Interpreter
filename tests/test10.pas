program test7;
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

main block                                                           
A random code block                                                  
Another code block                                                   
A code block within a code block                                    
Another nested block                                                 
The last code block

*)
