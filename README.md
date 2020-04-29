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
 Note: Since we don't have integer values, we allow real variables to hold the for-loop iterator, and because of that the condition to stop the loop is varname > finalVal instead of varname == finalVal
 * **Arithmetic Operators** : `* / + - mod`
 * **Relational Operators** : `< > <= >=`
 * **Comparators** : `= <>`
 * **Boolean Operators** : `or and`
 * **Functions** : `function f(args:type) : return_type; begin ... end;`
 * **Procedure** : `procedure p(args:type); begin ... end;`
 
