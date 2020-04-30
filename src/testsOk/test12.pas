program TestFunc;
//return the max between two numbers
function max(a,b:real):real;
begin
    if a > b then 
        max:=a
    else
        max:=b
end;

begin
    
    writeln(max(10,9));
    writeln(max(9,10));
    writeln(max(9,9));

end.