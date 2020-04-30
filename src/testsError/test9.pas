program FunctionErrors;

function foo(foo : boolean):real;
begin
//A function cannot define a symbol with its own name since
//it can collide with the function return variable
    foo := 10;
end;

function kung():real;
var kung : boolean;
begin
    kung := 9;      
end;


//Every function requires to have a return value, even if it's not reachable
function ret1():real;
begin   //This is an error
end;

function ret2():real;
begin
    if false then
        ret2 := 20; //this is not an error
end;

// This declaration is ok but it fails when we use this function with 
// unmatching arguments
procedure badargs(a,b : real);
begin
end;

begin

    badargs(1);
    badargs(1,2,3);
    badargs(true, false);
    badargs(1+true,false);
    //built in functions can check arguments too
    sin(true);
    cos(1,2);

end.