program Recursion;
function fibo(x:real) : real;
begin
    if x = 0 then
        fibo := 0
    else if x = 1 then
        fibo := 1
    else
        fibo := fibo(x-1) + fibo(x-2)
end;

begin
    writeln(fibo(5));
    writeln(fibo(10));
    writeln(fibo(15));
end.