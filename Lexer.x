{
module Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z_]

tokens :-
[\ \t\f\v\r\n]+ ;


-- Attributions
\=                                         { \_ -> TokenAtr }

-- Expressions
\+                                         { \s -> TokenPlus }
\-                                         { \s -> TokenMinus }
\*                                         { \s -> TokenTimes }
\/                                         { \s -> TokenDiv }
\%                                         { \s -> TokenMod }



--Comparations
\==                                        { \s -> TokenIsEqual }
\!=                                        { \s -> TokenIsNotEqual }
\<                                         { \s -> TokenLessThan }
\<=                                        { \s -> TokenLessEqual }
\>                                         { \s -> TokenGreaterThan }
\>=                                        { \s -> TokenGreaterEqual }

--Types and constants
$digit+  	                                 { \s -> TokenInt (read s) }
true                                       { \_ -> TokenBool True }
false                                      { \_ -> TokenBool False }
bool                                       { \_ -> TokenTypeBool  }
int                                        { \_ -> TokenTypeInt  }


--Control Flow
if                                         { \_ -> TokenIf }
else                                       { \_ -> TokenElse }
while                                      { \_ -> TokenWhile }
return                                     { \_ -> TokenReturn }
[$alpha \_] [$alpha $digit \_]*            { \s -> TokenIdentifier s }

--Other Stuff
\(                                         { \_ -> TokenLPAREN }
\)					                               { \_ -> TokenRPAREN }
\{					                               { \_ -> TokenLCB }
\}					                               { \_ -> TokenRCB }
\;					                               { \_ -> TokenSemiC }
\,                                         { \_ -> TokenComma }
{

data Token = TokenInt Int                 -- int value
          | TokenTypeInt                  -- int type
          | TokenTypeBool                 -- bool type
          | TokenBool Bool                -- bool value
          | TokenAtr                      -- =
          | TokenPlus                     -- +
          | TokenMinus                    -- -
          | TokenTimes                    -- *
          | TokenDiv                      -- /
          | TokenMod                      -- %
          | TokenIsEqual                  -- ==
          | TokenIsNotEqual               -- !=
          | TokenLessThan                 -- <
          | TokenLessEqual                -- <=
          | TokenGreaterThan              -- >
          | TokenGreaterEqual             -- >=
          | TokenIdentifier String
          | TokenLPAREN                   -- (
          | TokenRPAREN                   -- )
          | TokenLCB                      -- {
          | TokenRCB                      -- }
          | TokenSemiC                    -- ;
          | TokenComma                    -- ,
          | TokenReturn                   -- return
          | TokenIf                       -- if
          | TokenElse                     -- else
          | TokenWhile                    -- while
          deriving Show

}
