program ForErrors;

var i,j : real;
var b : boolean;

function foo():real;
begin
    foo:=10;
end;

begin

    for i := 0 to 10 do
        i := 2*i;           //Try to modify the iterator

    for i := 0 to 10 do
        for j:=0 to 20 do
            i:=-1;      
    
    for i := 0 to 10 do
        for i:=0 to 20 do
            writeln(i);      
    

    for b:= 0 to 10 do
        writeln(b);         //Try to iterate with a boolean variable

    for foo := 0 to 10 do   //undefined reference since there is no variabled named foo
        writeln(foo);


end.