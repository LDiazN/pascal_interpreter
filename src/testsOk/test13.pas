program TestProc;
var global : real;
procedure set_global(x:real);
begin
    global := x;
end;

begin
    writeln(global);
    set_global(10);
    writeln(global);

end.