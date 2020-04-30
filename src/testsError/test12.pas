program LoopControlErrors;

var i : real;

begin
    for i := 0 to 10 do
        break; //this is ok

    for i := 0 to 10 do
        continue; //this is ok

    if true then 
        break;  //This is an error

    if false then 
        continue; //this is an error

end.