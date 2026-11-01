{
module Grammars where

import Lex (Token(..))
}

%name parse Program
%tokentype { Token }
%error { parseError }

%token
      int             { TokenInt }
      identifier      { TokenIdentifier $$ }
      number          { TokenNumber $$ }
      '='             { TokenAssign }
      ';'             { TokenSemi }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '('             { TokenLParen }
      ')'             { TokenRParen }

%%

Program
  : Command                              { $1 }

Command
  : SimpleCommand                        { $1 }
  | SimpleCommand Command                { Seq $1 $2 }

SimpleCommand
  : int identifier '=' AExp ';'          { Decl $2 $4 }
  | identifier '=' AExp ';'              { Assign $1 $3 }

AExp
  : number                               { Num $1 }
  | identifier                           { Id $1 }
  | '(' AExp '+' AExp ')'                { Add $2 $4 }
  | '(' AExp '-' AExp ')'                { Sub $2 $4 }
  | '(' AExp '*' AExp ')'                { Mul $2 $4 }

{
type Program = Command

data AExp
  = Num Integer
  | Id String
  | Add AExp AExp
  | Sub AExp AExp
  | Mul AExp AExp
  deriving (Eq, Show)

data Command
  = Decl String AExp
  | Assign String AExp
  | Seq Command Command
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError tokens =
  error ("Error sintactico cerca de " ++ show (take 4 tokens))
}
