program LoopControl;

var i,j : real;
var x : real;


begin
    
    for i := 0 to 10 do
        begin
            if (i mod 2) <> 0 then
                continue;
            writeln(i); //print even numbers        
        end;

    for i := 0 to 10 do
        begin
            writeln(101);
            x := 0;
            break;      //stop the disaster
            x := 1/x;
        end;

    x := 0;
    while true do //This will never end
        begin
            x := x + 1;
            if x >= 10 then 
                break; //unless we stop it
        end;
    
    //nested loops 
    for i:=0 to 10 do //The first loop won't break
        for j := 0  to 1 do // The second loop will break
            begin
                if (i mod 2) = 0 then
                    break;
                writeln(i);
            end;

end.