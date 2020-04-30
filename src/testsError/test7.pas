program ExprErrors;

begin

    writeln(1/0); //This is an error
    //this is an error, but we only evaluate it at runtime since it concerns to 
    //the function execution. Comment the above example to check the second one
    writeln(ln(-1)); 

end.