program AddNums;


var i,b,z:boolean;

function fibo(x:real; a,b : boolean) : real;

begin
    if x = 0 then
        fibo := 0
    else if x = 1 then
        fibo := 1
    else 
        fibo := fibo(x-1) + fibo(x-2);
end;


begin
    
    for fibo := 0 to 10 do
    begin
        
    end;
end.