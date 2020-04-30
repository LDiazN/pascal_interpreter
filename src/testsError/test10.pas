program caseErrors;

var x : real;
var y : real;
begin

    case x of
    //this is an error since the left side of case guards can only be constants
        y + 2: writeln(x)
    end;

end.