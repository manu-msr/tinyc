{
module Grammars where

import Lex (Token(..))
}

%name parse Program
%tokentype { Token }
%error { parseError }

%token
      int             { TokenInt }
      void            { TokenVoid }
      return          { TokenReturn }
      print           { TokenPrint }
      if              { TokenIf }
      else            { TokenElse }
      while           { TokenWhile }
      identifier      { TokenIdentifier $$ }
      number          { TokenNumber $$ }
      '=='            { TokenEq }
      '<='            { TokenLeq }
      '='             { TokenAssign }
      '!'             { TokenNot }
      '&'             { TokenAmp }
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
  : int identifier '=' Exp ';'           { Decl $2 $4 }
  | int '*' identifier '=' Exp ';'       { PtrDecl $3 $5 }
  | identifier '=' Exp ';'               { Assign $1 $3 }
  | '*' Exp '=' Exp ';'                  { AssignDeref $2 $4 }
  | int identifier '(' Parameter ')' '{'
      Command return Exp ';' '}'          { FunDecl $2 $4 $7 $9 }
  | int '*' identifier '(' Parameter ')' '{'
      Command return Exp ';' '}'          { FunDecl $3 $5 $8 $10 }
  | void identifier '(' Parameter ')' '{'
      Command return ';' '}'              { VoidDecl $2 $4 $7 }
  | identifier '(' Exp ')' ';'           { CallVoid $1 $3 }
  | print '(' Exp ')' ';'                { CallVoid "print" $3 }
  | if Exp '{' Command '}'
      else '{' Command '}'                { If $2 $4 $8 }
  | while Exp '{' Command '}'             { While $2 $4 }

Parameter
  : int identifier                       { $2 }
  | int '*' identifier                   { $3 }

Exp
  : number                               { Num $1 }
  | identifier                           { Id $1 }
  | '&' identifier                       { Addr $2 }
  | '*' Exp                              { Deref $2 }
  | identifier '(' Exp ')'               { Call $1 $3 }
  | '(' Exp '+' Exp ')'                  { Add $2 $4 }
  | '(' Exp '-' Exp ')'                  { Sub $2 $4 }
  | '(' Exp '*' Exp ')'                  { Mul $2 $4 }
  | '(' Exp '==' Exp ')'                 { Eq $2 $4 }
  | '(' Exp '<=' Exp ')'                 { Leq $2 $4 }
  | '(' '!' Exp ')'                      { Not $3 }

{
type Program = Command

data Exp
  = Num Integer
  | Id String
  | Addr String
  | Deref Exp
  | Call String Exp
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Eq Exp Exp
  | Leq Exp Exp
  | Not Exp
  deriving (Eq, Show)

data Command
  = Decl String Exp
  | PtrDecl String Exp
  | Assign String Exp
  | AssignDeref Exp Exp
  | FunDecl String String Command Exp
  | VoidDecl String String Command
  | CallVoid String Exp
  | Seq Command Command
  | If Exp Command Command
  | While Exp Command
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError tokens =
  error ("Error sintactico cerca de " ++ show (take 4 tokens))
}
