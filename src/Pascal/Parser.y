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
        name            { Token _ (TkId  _)        }
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
             : program name ';' Program '.'     {(getId $2,$4)}

Program      :: { Program }
             : Declarations Block               { Program $2  (reverse $1) }

-- < Execution Statements > ------------------------------------------
Block        :: {Statement}
             :  begin  end                      { Block [] }
             |  begin Statements end            { Block (reverse $2) }
                
Statements   :: { [Statement] }
             : Statement                        { [$1] }
             | Statements  ';'   Statement      {$3 : $1}
             | Statements  ';'                  {$1}

Statement    :: { Statement }
             : FuncCall                         { ProcCall (getId . fst $ $1) (snd $1) (getPos . fst $ $1) }
             | Assign                           { $1 }
             | IfStatement                      { $1 }
             | CaseStmnt                        { $1 }             
             | WhileDo                          { $1 }
             | ForDo                            { $1 }
             | Block                            { $1 }
             | break                            { Break (getPos $1)}
             | continue                         { Continue (getPos $1)}

Assign       :: { Statement }
             : name ':=' Expr                   { Assign (getId $1) $3 (getPos $1) }

FuncCall     :: {(Token, [Exp])}
             : name'('')'                      { ($1, []) }
             | name'(' FuncArgs ')'            { ($1, reverse $3) }

FuncArgs     :: {[Exp]}
             : Expr                             { [$1] }
             | FuncArgs ',' Expr                { $3:$1 } 

IfStatement  :: { Statement } 
             : if Expr then Statement           { If $2 $4 Skip (-1) (getPos $1)}
             | if Expr then Statement 
                else Statement                  { If $2 $4 $6 (-1) (getPos $1)}

CaseStmnt    :: { Statement }
             : case Expr of Cases end           { Case $2 $4 Skip (-1) (getPos $1)}
             

Cases        :: { [(Exp, Statement)] }
             : Case                             { $1 }
             | Cases ';' Case ';'               { $1 ++ $3 }

Case         :: {[(Exp, Statement)]}
             : CaseLabels ':' Statement           { [(e, $3) | e <- $1] }

CaseLabels   :: { [Exp] }
             : Expr                           { [$1] }
             | CaseLabels ',' Expr            { $3:$1 }

WhileDo      :: { Statement }
             : while Expr do Statement          { While $2 $4 (-1) (getPos $1)}

ForDo        :: { Statement }
             : for name ':=' Expr to Expr 
                             do Statement       { For (getId $2) $4 $6 $8 "to" (-1) (getPos $1)}
             | for name ':=' Expr downto Expr 
                             do Statement       { For (getId $2) $4 $6 $8 "downto" (-1) (getPos $1)}

-- < Declaration Statements > ----------------------------------------
Declarations :: {[Declaration]}
             :                                  {[]} --No Declaration
             | Declarations FuncDeclar          {$2:$1}
             | Declarations VarDeclars          { $2 ++ $1 }

FuncDeclar   :: {Declaration} --Declare procedure or function
             : function name '(' FuncArgsDec ')' ':' DataType ';' Program ';' { Function (getId $2) $4 $7 $9 (getPos $2)}
             | procedure name '(' FuncArgsDec ')' ';' Program ';' {Function (getId $2) $4 NoneT $7 (getPos $2)}

FuncArgsDec  :: {[(String, DataType)]}
             : VarDeclars2                      {reverse [ (getId t, dt) | (t, dt) <- $1]}
             | FuncArgsDec ';' VarDeclars2      { $1 ++ reverse [ (getId t, dt) | (t, dt) <- $3] }

VarDeclars   :: {[Declaration]}
             : var VarDeclars2 ';'              { [ Variable (getId s) t (getPos s) | (s, t) <- $2 ] }

VarDeclars2  :: {[(Token, DataType)]}
             : Names ':' DataType               { [(s,$3) | s <- $1] }

Names        :: {[Token]}
             : name                             {[$1]}
             | Names ',' name                   {$3:$1} 

-- < Supported Types > -----------------------------------------------
DataType     :: {DataType}
             : real                             { RealT }
             | boolean                          { BooleanT }

-- < Expressions > ---------------------------------------------------
Expr         :: { Exp }                         
             : SimpleExpr                       { $1 }
             | SimpleExpr RelOpr SimpleExpr     { BinaryOp $2 $1 $3 }

SimpleExpr   :: { Exp }
             : UnTerm                           { $1 }
             | AddOprs                          { $1 }

UnTerm       :: { Exp }
             : Term                             { $1 }
             | UnOper UnTerm                    { NumExpr (Op1 $1 $2) }

AddOprs      :: { Exp }
             : UnTerm AddOpr UnTerm             { BinaryOp $2 $1 $3 }
             | AddOprs AddOpr UnTerm            { BinaryOp $2 $1 $3 }

Term         :: { Exp }
             : Factor                           { $1 }
             | MulOprs                          { $1 }

MulOprs      :: { Exp }
             : Factor MulOpr Factor             { BinaryOp $2 $1 $3 }
             | MulOprs MulOpr Factor            { BinaryOp $2 $1 $3 }
             
Factor       :: { Exp }                                 
             :  num                             { NumExpr (NumConst $1) }
             |  name                            { IdExpr (getId $1) (getPos $1) }
             |  true                            { BoolExpr TrueC }
             |  false                           { BoolExpr FalseC }
             | '(' Expr ')'                     { $2 }
             | not Factor                       { BoolExpr (Not $2) }
             | FuncCall                         { FunExpr ( getId . fst $ $1) (snd $1) (getPos . fst $ $1) }


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

{

}
