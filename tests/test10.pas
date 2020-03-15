program test7;
(* Test nested code blocks. *)

var
  a, b, c: boolean;

begin
  writeln ('main block');
  begin
    writeln('A random code block');
  end;
  begin
    writeln('Another code block');
    begin
      writeln ('A code block within a code block');
        begin
          writeln ('Another nested block');
        end;
    end;
  end;
  begin
    writeln('The last code block');
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
