{
module Grammars where

import Lex (Token(..))
}

%name parse Program
%tokentype { Token }
%error { parseError }

%token
      int             { TokenInt }
      if              { TokenIf }
      else            { TokenElse }
      while           { TokenWhile }
      identifier      { TokenIdentifier $$ }
      number          { TokenNumber $$ }
      '=='            { TokenEq }
      '<='            { TokenLeq }
      '='             { TokenAssign }
      '!'             { TokenNot }
      ';'             { TokenSemi }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '('             { TokenLParen }
      ')'             { TokenRParen }
      '{'             { TokenLBrace }
      '}'             { TokenRBrace }

%%

Program
  : Command                              { $1 }

Command
  : SimpleCommand                        { $1 }
  | SimpleCommand Command                { Seq $1 $2 }

SimpleCommand
  : int identifier '=' AExp ';'          { Decl $2 $4 }
  | identifier '=' AExp ';'              { Assign $1 $3 }
  | if AExp '{' Command '}'
      else '{' Command '}'                { If $2 $4 $8 }
  | while AExp '{' Command '}'            { While $2 $4 }

AExp
  : number                               { Num $1 }
  | identifier                           { Id $1 }
  | '(' AExp '+' AExp ')'                { Add $2 $4 }
  | '(' AExp '-' AExp ')'                { Sub $2 $4 }
  | '(' AExp '*' AExp ')'                { Mul $2 $4 }
  | '(' AExp '==' AExp ')'               { Eq $2 $4 }
  | '(' AExp '<=' AExp ')'               { Leq $2 $4 }
  | '(' '!' AExp ')'                     { Not $3 }

{
type Program = Command

data AExp
  = Num Integer
  | Id String
  | Add AExp AExp
  | Sub AExp AExp
  | Mul AExp AExp
  | Eq AExp AExp
  | Leq AExp AExp
  | Not AExp
  deriving (Eq, Show)

data Command
  = Decl String AExp
  | Assign String AExp
  | Seq Command Command
  | If AExp Command Command
  | While AExp Command
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError tokens =
  error ("Error sintactico cerca de " ++ show (take 4 tokens))
}
