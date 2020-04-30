program CaseStatement;

var x : real;

begin

    x := 12;

    case x of
        10 : writeln(10);
        23 mod 8 : writeln(101);
        23 + 20 : writeln(101);
        10 + 2 : 
                begin
                    writeln(1);
                    writeln(2);
                end;
    end;

    writeln(505);

    if true then
        begin
            writeln(true);
        end
    else
        writeln(true); 

    case x of
        99 : writeln(505);
    else 
        writeln(101);
    end;

end.