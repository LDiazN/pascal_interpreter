program WhileErrors;

var x : real;

function realfun():real;
begin
    realfun := 10;
end;

procedure kung();
begin
end;

begin
    
    while x do      //This is an error since the expression is not a boolean expression
        writeln(x);

    while realfun() do //This is an error since realfun is a real variable
        writeln(101);

    while kung do     //unvalid reference
        writeln(101); 

end.