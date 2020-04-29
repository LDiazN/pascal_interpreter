program AddNums;


var i,b,z:real;

function fibo(x:real) : real;

begin
    if x = 0 then
        fibo := 0
    else if x = 1 then
        fibo := 1
    else 
        fibo := fibo(x-1) + fibo(x-2);
end;


begin
    
    i := ln(2) + 20;
    readln(i);
    writeln(i);
end.