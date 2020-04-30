About this project 
=====================
This is a simple Pascal interpreter that supports some of the basic features in Pascal made with Haskell (as described below). 

Content of this file
=====================
* Available features
* Running the Project
  * Building
  * Running
  * Use
* Additional information


Available features
====================
### Pascal Features ###

* **One line comments** : Using `//`
* **Multiline comments** : Using `(**)`  
  We decided to accept nested comments like  

        (*
        (*
        *)
* **boolean variables**  : `boolean`
* **real variables**  : `real`
* **Conditional statements like case or if** : `case exp of`, `if exp then`
* **While loop** : `while exp do`
* **For loop** : `for varname := initVal to/downto finalVal do`  
 Note: Since we don't have integer values, we allow real variables to hold the for-loop iterator, and because of that the condition to stop the loop is `varname > finalVal` instead of `varname == finalVal`
 * **Arithmetic Operators** : `* / + - mod`
 * **Relational Operators** : `< > <= >=`
 * **Comparators** : `= <>`
 * **Boolean Operators** : `or and`
 * **Functions** : `function f(args:type) : return_type; begin ... end;`
 * **Procedure** : `procedure p(args:type); begin ... end;`
 * **Numeric built-in functions** : `sin, cos, ln, exp, sqrt`
 * **I/O Operations** : `writeln, reanln`  
  Note: In the case of readln, since we don't have String data type, we try to parse the input depending on the type of the argument variable
 
 ### Interpreter features ###
In addition to the program running, the pascal interpreter provides a few flags to show more information about our pascal  
program. If you invoke the pascal interpreter without arguments, the following help will be shown.
* **-a --ast** : This flag will print a formatted version of the AST as it comes from the lexer if   
this program does not have any lexical or syntactical error
* **-ca --clean-ast** : this flag will print a fromatted version of the AST corrected by the static analyzer if  
this program does not have any lexical, syntactical, or static errors.

Running this project 
======================

### Building ###
You can build this project with cabal or just ghc.  
To make a modest build of the project using just ghc, then go to the the main root folder, then to src folder and run the following command:  

        make   

Or manually:  

        alex Pascal/Lexer.x
        happy Pascal/Parser.y
        ghc --make src/Main.hs -o ./Pascal
        
In case you want to run the project using runhaskell, you have to compile the parser and the lexer:

        alex Pascal/Lexer.x
        happy Pascal/Parser.y



### Running ###
In case you compiled the project with GHC:  

        ./Pascal program.pas 
         
In case you want to run the project with runhaskell:  
        
        runhaskell src/Main.hs program.pas  
        
Note: This may take a while since runhaskell will compile the entire project everytime it's called

### Use ###
If you run the interpreter without arguments, a description about the available flags will be shown.
       
       ./Pascal

The first argument of the command should always be the pascal file to interpret.

Aditional information
=====================

### Test files ###
You can find two sets of test files, working files and error files. The first set, located at `src/TestsOk` contains only working examples of the features implemented. The second one, located at `src/TestsError` contains only programs with errors 
