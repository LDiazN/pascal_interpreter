{
module Pascal.Parser where

import Pascal.Base
import Pascal.Data
import Pascal.Lexer
}

-- Happy definitions
%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TkEOF }

--List of possible tokens 
%token
        -- < Reserved keywords > ------------------------
        begin           { Token _ (TkGen "begin")   }
        end             { Token _ (TkGen "end")     }
        program         { Token _ (TkGen "program") }
        var             { Token _ (TkGen "var")     }
        if              { Token _ (TkGen "if")      }
        else            { Token _ (TkGen "else")    }
        case            { Token _ (TkGen "case")    }
        then            { Token _ (TkGen "then")    }
        of              { Token _ (TkGen "of")      }
        while           { Token _ (TkGen "while" )  }
        do              { Token _ (TkGen "do" )     }
        for             { Token _ (TkGen "for" )    }
        to              { Token _ (TkGen "to" )     }
        break           { Token _ (TkGen "break" )  }
        continue        { Token _ (TkGen "continue")   }
        function        { Token _ (TkGen "function")   }
        procedure       { Token _ (TkGen "procedure")  }

        -- < built in functions > -----------------------
        writeln         { Token _ (TkGen "writeln") }
        readln          { Token _ (TkGen "readln")  }
        sqrt            { Token _ (TkGen "sqrt")    }
        sin             { Token _ (TkGen "sin")     }
        cos             { Token _ (TkGen "cos")     }
        ln              { Token _ (TkGen "ln")      }
        exp             { Token _ (TkGen "exp")     }       

        -- < Supported Data Types > ---------------------
        real            { Token _ (TkGen "real")    }
        boolean         { Token _ (TkGen "boolean") }

        -- < Constants & ids > --------------------------
        name            { Token _ (TkId  $$)        }
        num             { Token _ (TkReal $$)       }
        true            { Token _ (TkGen "true")    }
        false           { Token _ (TkGen "false")   }

        -- < Operators & separators > -------------------
        '+'             { Token _ (TkGen "+")       }
        '-'             { Token _ (TkGen "-")       }
        '*'             { Token _ (TkGen "*")       }
        '/'             { Token _ (TkGen "/")       }
        '%'             { Token _ (TkGen "%")       }              
        '='             { Token _ (TkGen "=")       }
        '>='            { Token _ (TkGen ">=")      }
        '<='            { Token _ (TkGen "<=")      }
        '<'             { Token _ (TkGen "<")       }
        '>'             { Token _ (TkGen ">")       }
        '<>'            { Token _ (TkGen "<>")      }
        and             { Token _ (TkGen "and")     }
        or              { Token _ (TkGen "or")      }
        not             { Token _ (TkGen "not")     }
        ':='            { Token _ (TkGen ":=")      }
        ';'             { Token _ (TkGen ";")       }
        ':'             { Token _ (TkGen ":")       }
        ','             { Token _ (TkGen ",")       }
        '.'             { Token _ (TkGen ".")       }
        '('             { Token _ (TkGen "(")       }
        ')'             { Token _ (TkGen ")")       }

-- associativity of operators in reverse precedence order
%nonassoc '>' '>=' '<' '<=' '==' '<>'
%left '+' '-'
%left '*' '/'
%nonassoc ':='
%%

-- Entry point
MainProgram  :: {MainProgram}
             : program name ';' Program '.'     {($2,$4)}

Program      :: {Program}
             : Declarations Block               { Program $2 $1 }

-- < Execution Statements > ------------------------------------------
Block        :: {Statement}
             :  begin  end                      { Block [] }
             |  begin Statements end            { Block $2 }
                
Statements   :: {[Statement]}
             : Statement                        { [$1] }
             | Statements ';' Statement ';'     {$3 : $1}

Statement    :: {Statement}
             : name '('')'                      { ProcCall $1 [] }
-- < Declaration Statements > ----------------------------------------
Declarations :: {[Declaration]}
             :                                  {[]} --No Declaration
             | Declarations FuncDeclar          {$2:$1}
             | Declarations VarDeclars          { $1 ++ $2 }

FuncDeclar   :: {Declaration} --Declare procedure or function
             : function name '(' FuncArgs ')' ':' DataType ';' Program ';' { Function $2 $4 $7 $9 }
             | procedure name '(' FuncArgs ')' ';' Program ';' {Procedure $2 $4 $7}

FuncArgs     :: {[(String, DataType)]}
             : VarDeclars2                      {$1}
             | FuncArgs ';' VarDeclars2         { $1 ++ $3 }

VarDeclars   :: {[Declaration]}
             : var VarDeclars2 ';'              { [ Variable s t | (s, t) <- $2 ] }

VarDeclars2  :: {[(String, DataType)]}
             : Names ':' DataType               { [(s,$3) | s <- $1] }

Names        :: {[String]}
             : name                              {[$1]}
             | Names ',' name                    {$3:$1} 

-- < Supported Types > -----------------------------------------------
DataType     :: {DataType}
            : real                              { RealT }
            | boolean                           { BooleanT }




{}
