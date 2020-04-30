program ProcNFunsMix;

var x : real;

procedure kung();
begin
    
end;

function foo():real;
begin
    foo := 10;
end;



begin
    
    kung(); //this is ok

    x := 2 + foo(); //This is ok

    x := 2 + kung(); //this is an error

    x := kung() + foo(); //this is an error


end.