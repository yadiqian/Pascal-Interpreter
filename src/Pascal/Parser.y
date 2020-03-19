{
module Pascal.Parser where

import Pascal.Base
import Pascal.Data
import Pascal.Lexer
}


%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }

%token
        int             { Token _ (TokenInt $$) }
        float           { Token _ (TokenFloat $$) }
        ID              { Token _ (TokenID $$)  }
        '+'             { Token _ (TokenOp "+")   }
        '-'             { Token _ (TokenOp "-")   }
        '*'             { Token _ (TokenOp "*")   }
        '/'             { Token _ (TokenOp "/")   }
        ':='            { Token _ (TokenOp ":=")   }
        'and'           { Token _ (TokenOp "and") }
        'or'            { Token _ (TokenOp "or") }
        'xor'           { Token _ (TokenOp "xor") }
        'not'           { Token _ (TokenOp "not") }
        '>'             { Token _ (TokenOp ">") }
        '<'             { Token _ (TokenOp "<") }
        '='             { Token _ (TokenOp "=") }
        '>='            { Token _ (TokenOp ">=") }
        '<='            { Token _ (TokenOp "<=") }
        '<>'            { Token _ (TokenOp "<>") }
        '('             { Token _ (TokenK  "(")   }
        ')'             { Token _ (TokenK  ")")   }
        'begin'         { Token _ (TokenK "begin") }
        'end'           { Token _ (TokenK "end")  }
        'true'          { Token _ (TokenK "true") }
        'false'         { Token _ (TokenK "false") }
        'program'       { Token _ (TokenK "program") }
        'writeln'       { Token _ (TokenK "writeln") }
        'var'           { Token _ (TokenK "var") }
        'boolean'       { Token _ (TokenK "boolean") }
        'real'          { Token _ (TokenK "real") }
        'string'        { Token _ (TokenK "string") }
        ';'             { Token _ (TokenK ";") }
        ':'             { Token _ (TokenK ":") }
        ','             { Token _ (TokenK ",") }
        '.'             { Token _ TokenEOF }

-- associativity of operators in reverse precedence order
%nonassoc '>' '>=' '<' '<=' '=' '<>'
%left '+' '-' 'and' 'or' 'xor'
%left '*' '/' 'not'
%nonassoc ':='
%%

-- Entry point
Program :: {Program}
    : 'program' ID ';' Defs Body { $5 }

Defs :: {[Definition]}
    : { [] } -- nothing; make empty list
    | Definition Defs { $1 : $2 } -- put definition as first element of defs

Definition :: {Definition}
    : 'var' ID_list ':' Type ';' { VarDef $2 $4 }

Type :: {VType}
    : 'boolean' { BOOL }
    | 'real' { REAL }

ID_list :: {[String]}
    : ID {[$1]}
    | ID ',' ID_list { $1 : $3 }

-- Main code block
Body :: {Body}
    : 'begin' Statements 'end' { $2 }

-- Expressions
RealExp :: { RealExp }
    : '+' RealExp { $2 } -- ignore Plus
    | '-' RealExp { Op1 "-" $2}
    | RealExp '+' RealExp { Op2 "+" $1 $3 }
    | RealExp '-' RealExp { Op2 "-" $1 $3 }
    | RealExp '*' RealExp { Op2 "*" $1 $3 }
    | RealExp '/' RealExp { Op2 "/" $1 $3 }
    | '(' RealExp ')' { $2 } -- ignore brackets
    | int { Integer $1 }
    | float { Real $1 }

BoolExp :: { BoolExp }
    : 'true' { True_C }
    | 'false' { False_C }
    | 'not' BoolExp { Not $2 }
    | BoolExp 'and' BoolExp { OpB "and" $1 $3 }
    | BoolExp 'or' BoolExp { OpB "or" $1 $3 }
    | BoolExp 'xor' BoolExp { OpB "xor" $1 $3 }
    | '(' BoolExp ')' { $2 } -- ignore brackets
    | RealExp '>' RealExp { Comp ">" $1 $3 }
    | RealExp '<' RealExp { Comp "<" $1 $3 }
    | RealExp '=' RealExp { Comp "=" $1 $3 }
    | RealExp '>=' RealExp { Comp ">=" $1 $3 }
    | RealExp '<=' RealExp { Comp "<=" $1 $3 }
    | RealExp '<>' RealExp { Comp "<>" $1 $3 }

GenExp :: { GenExp }
    : RealExp { FloatExp $1 }
    | BoolExp { BExp $1 }

Statements :: {[Statement]}
    : { [] } -- nothing; make empty list
    | Statement Statements { $1 : $2 } -- put statement as first element of statements

Statement :: {Statement}
    : ID ':=' GenExp ';' { Assign $1 $3 }
    | 'writeln' '(' GenExp ')' ';' { Print $3 }
{}
