program ScopeFunc;

var x : real;

procedure my_own_x();
var x : real;
begin
    x := 10;
    writeln(x);
end;



begin
    x:= -1;

    writeln(x);
    my_own_x();
    writeln(x);
    
end.