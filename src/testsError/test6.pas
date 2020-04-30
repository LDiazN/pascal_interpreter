program FunsNVarsMix;

var v : real;

function foo():real;
begin
    foo := 10;
end;

procedure kung() ;
begin
    
end;

begin
    writeln(v + foo()); // this is ok
    writeln(v + foo);   // this is an error
    writeln(v + kung);  // this is an error
end.