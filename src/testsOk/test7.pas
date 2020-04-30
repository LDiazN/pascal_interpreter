program Precedence;

var
    a : real;
    b : real;
    c : real;
    d : real;
    e : real;

begin
   a := 20.0;
   b := 10.0;
   c := 15.0;
   d := 5.0;
    e := (a + b) * c / d; 
    writeln(e);
end.