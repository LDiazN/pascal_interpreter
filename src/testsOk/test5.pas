program Scope;

var
 a : real;
 b : real;

begin
    a := 1.0; 
    while(a < 3.0) do
    begin
        b := 5.0;
        writeln(a);
        a := a + 1.0;
    end;

    writeln(b);

end.