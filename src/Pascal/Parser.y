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
%lexer { lexer }  { Token _ TkEOF }

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
        downto          { Token _ (TkGen "downto" ) }
        break           { Token _ (TkGen "break" )  }
        continue        { Token _ (TkGen "continue")  }
        function        { Token _ (TkGen "function")  }
        procedure       { Token _ (TkGen "procedure") }       

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
%nonassoc then
%nonassoc else
%left 'and' 'or' 
%right 'not'
%nonassoc '>' '>=' '<' '<=' '=' '<>'
%left '+' '-'
%left '*' '/' '%'
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
                
Statements   :: { [Statement] }
             : Statement                        { [$1] }
             | Statements  ';'   Statement      {$3 : $1}
             | Statements  ';'                  {$1}

Statement    :: { Statement }
             : FuncCall                         { uncurry ProcCall $ $1 }
             | Assign                           { $1 }
             | IfStatement                      { $1 }
             | CaseStmnt                        { $1 }             
             | WhileDo                          { $1 }
             | ForDo                            { $1 }
             | Block                            { $1 }
             | break                            { Break }
             | continue                         { Continue }

Assign       :: { Statement }
             : name ':=' Expr                   { Assign $1 $3 }

FuncCall     :: {(String, [Exp])}
             : name'('')'                      { ($1, []) }
             | name'(' FuncArgs ')'            { ($1, $3) }



             

FuncArgs     :: {[Exp]}
             : Expr                             { [$1] }
             | FuncArgs ',' Expr                { $3:$1 } 

IfStatement  :: { Statement } 
             : if Expr then Statement           { If $2 $4 Skip (-1) }
             | if Expr then Statement 
                else Statement                  { If $2 $4 $6 (-1) }

CaseStmnt    :: { Statement }
             : case Expr of Cases end           { Case $2 $4 Skip (-1)}
             

Cases        :: { [(Exp, Statement)] }
             : Case                             { $1 }
             | Cases ';' Case ';'               { $1 ++ $3 }

Case         :: {[(Exp, Statement)]}
             : CaseLabels ':' Statement           { [(e, $3) | e <- $1] }

CaseLabels   :: { [Exp] }
             : Literl                           { [$1] }
             | CaseLabels ',' Literl            { $3:$1 }

WhileDo      :: { Statement }
             : while Expr do Statement          { While $2 $4 0}

ForDo        :: { Statement }
             : for name ':=' Expr to Expr 
                             do Statement       { For $2 $4 $6 $8 "to" (-1) }
             | for name ':=' Expr downto Expr 
                             do Statement       { For $2 $4 $6 $8 "downto" (-1) }

-- < Declaration Statements > ----------------------------------------
Declarations :: {[Declaration]}
             :                                  {[]} --No Declaration
             | Declarations FuncDeclar          {$2:$1}
             | Declarations VarDeclars          { $1 ++ $2 }

FuncDeclar   :: {Declaration} --Declare procedure or function
             : function name '(' FuncArgsDec ')' ':' DataType ';' Program ';' { Function $2 $4 $7 $9 }
             | procedure name '(' FuncArgsDec ')' ';' Program ';' {Procedure $2 $4 $7}

FuncArgsDec  :: {[(String, DataType)]}
             : VarDeclars2                      {$1}
             | FuncArgsDec ';' VarDeclars2      { $1 ++ $3 }

VarDeclars   :: {[Declaration]}
             : var VarDeclars2 ';'              { [ Variable s t | (s, t) <- $2 ] }

VarDeclars2  :: {[(String, DataType)]}
             : Names ':' DataType               { [(s,$3) | s <- $1] }

Names        :: {[String]}
             : name                             {[$1]}
             | Names ',' name                   {$3:$1} 

-- < Supported Types > -----------------------------------------------
DataType     :: {DataType}
             : real                             { RealT }
             | boolean                          { BooleanT }

-- < Expressions > ---------------------------------------------------
Expr         :: { Exp }                         
             : SimpleExpr                       { $1 }
             | SimpleExpr RelOpr SimpleExpr     { BinaryOp $2 $1 $1 }

SimpleExpr   :: { Exp }
             : Term                             { $1 }
             | UnOper Term                      { NumExpr (Op1 $1 $2) }
             | Term AddOpr Term                 { BinaryOp $2 $1 $3 }

Term         :: { Exp }
             : Factor                           { $1 }
             | Factor MulOpr Factor             { BinaryOp $2 $1 $3 }

Factor       :: { Exp }                                 
             :  num                             { NumExpr (NumConst $1) }
             |  name                            { IdExpr $1 }
             |  true                            { BoolExpr TrueC }
             |  false                           { BoolExpr FalseC }
             | '(' Expr ')'                     { $2 }
             | not Factor                       { BoolExpr (Not $2) }
             | FuncCall                         { uncurry FunExpr $ $1 }


RelOpr       :: { String }
             : '='                              { "=" } 
             | '<>'                             { "<>" } 
             | '<='                             { "<=" }   
             | '>='                             { ">=" }
             | '>'                              { ">" }
             | '<'                              { "<" }

UnOper       :: { String }                      
             : '+'                              { "+" }
             | '-'                              { "-" }

AddOpr       :: { String }
             : '+'                              { "+" }
             | '-'                              { "-" }
             | or                               { "or" }

MulOpr       :: { String }
             : '*'                              { "*" }   
             | '/'                              { "/" }   
             | '%'                              { "%" }   
             | and                              { "and" }

Literl       :: { Exp }
             : num                              { NumExpr (NumConst $1) }
             | true                             { BoolExpr (TrueC) }
             | false                            { BoolExpr (FalseC) }
{

}
