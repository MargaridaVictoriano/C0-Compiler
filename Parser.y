{
module Parser where
import Lexer
import AST
}

%name parser
%tokentype { Token }
%error { parseError }

%nonassoc '>' '<' '<=' '>=' '==' '!=' '!'
%left '+' '-'
%left '*' '/' '%'
%left ';' ','
%left '='
%token


-- Types
int                                  { TokenInt  $$ }
bool                                 { TokenBool  $$ }

id                                   { TokenIdentifier $$ }

-- Symbols
';'                                  { TokenSemiC  }
','                                  { TokenComma  }
'('                                  { TokenLPAREN  }
')'                                  { TokenRPAREN  }
'{'                                  { TokenLCB  }
'}'                                  { TokenRCB  }

-- Arithmetic Expressions
'+'                                  { TokenPlus  }
'-'                                  { TokenMinus }
'*'                                  { TokenTimes }
'/'                                  { TokenDiv  }
'%'                                  { TokenMod  }

-- Comparisons

'=='                                 { TokenIsEqual  }
'!='                                 { TokenIsNotEqual }
'<'                                  { TokenLessThan   }
'<='                                 { TokenLessEqual  }
'>'                                  { TokenGreaterThan }
'>='                                 { TokenGreaterEqual }

-- Attributions
'='                                  { TokenAtr  }

-- IFs
if                                   { TokenIf  }
else                                 { TokenElse  }

-- while
while                                { TokenWhile  }

return                               { TokenReturn }

tInt                                 { TokenTypeInt }
tBool                                { TokenTypeBool }

%%

FunList : Fun                        { [$1] }
        | Fun FunList                { ($1 : $2 )}

Fun : TType id '(' Args ')' Stm      { Func $1 $2 $4 $6 }

Args : {- empty -}                   { [] }
     | Arg                           { [$1] }
     | Arg ',' Args                  { ($1 : $3) }

Arg : TType id                       { ArgValue $1 $2}

Stm : Exp ';'                        { Expression $1 }
    | if '(' Exp ')' Stm             { IfStm $3 $5 }
    | if '(' Exp ')' Stm else Stm    { IfElseStm $3 $5 $7 }
    | while '(' Exp ')' Stm          { While $3 $5 }
    | id '=' Exp ';'                 { Atr $1 $3 }
    | '{' Stms '}'                   { Stms $2 }
    | TType id ';'                   { InitVar $1 $2 }
    | return Exp ';'                 { Return $2 }

Stms : {- empty -}                   { [] }
     | Stm Stms                      { ($1 : $2) }

Exps : {- empty  -}                  { [] }
     | Exp                           { [$1] }
     | Exp ',' Exps                  { ($1 : $3)}

Exp : Term                           { $1 }
    | Exp '+' Exp                    { (BinOp Add) $1 $3 }
    | Exp '-' Exp                    { (BinOp Minus) $1 $3 }
    | Exp '*' Exp                    { (BinOp Times) $1 $3 }
    | Exp '/' Exp                    { (BinOp Divide) $1 $3 }
    | Exp '%' Exp                    { (BinOp Mod) $1 $3 }
    | Exp '==' Exp                   { (RelOp Equal) $1 $3 }
    | Exp '<' Exp                    { (RelOp LessThan) $1 $3 }
    | Exp '>' Exp                    { (RelOp GreaterThan) $1 $3 }
    | Exp '<=' Exp                   { (RelOp LessEq) $1 $3 }
    | Exp '>=' Exp                   { (RelOp GreaterEq) $1 $3 }
    | Exp '!=' Exp                   { (RelOp Diff) $1 $3 }
    | id '(' Exps ')'                {  CallFun $1 $3 }


Term : int                           { Num $1 }
     | bool                          { Boolean $1 }
     | id                            { Var $1 }
     | '(' Exp ')'                   { $2 }

TType : tInt                         { TTypeInt }
      | tBool                        { TTypeBool }


{
parseError :: [Token] -> a
parseError toks = error "parse error"
}
