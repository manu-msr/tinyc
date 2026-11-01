{
module Lex (Token(..), lexer) where

import Data.Char (isSpace)
}

%wrapper "basic"

$white = [\x20\x09\x0A\x0D\x0C\x0B]
$digit = 0-9
$letter = [A-Za-z]
$idrest = [A-Za-z0-9_]

tokens :-

  $white+               ;

  "int"                 { \_ -> TokenInt }
  "if"                  { \_ -> TokenIf }
  "else"                { \_ -> TokenElse }
  "while"               { \_ -> TokenWhile }

  "=="                  { \_ -> TokenEq }
  "<="                  { \_ -> TokenLeq }
  "="                   { \_ -> TokenAssign }
  "!"                   { \_ -> TokenNot }
  ";"                   { \_ -> TokenSemi }
  "+"                   { \_ -> TokenPlus }
  "-"                   { \_ -> TokenMinus }
  "*"                   { \_ -> TokenTimes }
  "("                   { \_ -> TokenLParen }
  ")"                   { \_ -> TokenRParen }
  "{"                   { \_ -> TokenLBrace }
  "}"                   { \_ -> TokenRBrace }

  $digit+               { \source -> TokenNumber (read source) }
  $letter$idrest*       { \source -> TokenIdentifier source }

  .                     { \source -> error ("Error lexico: caracter no reconocido " ++ show source) }

{
data Token
  = TokenInt
  | TokenIf
  | TokenElse
  | TokenWhile
  | TokenEq
  | TokenLeq
  | TokenAssign
  | TokenNot
  | TokenSemi
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenNumber Integer
  | TokenIdentifier String
  deriving (Eq, Show)

normalizeSpaces :: String -> String
normalizeSpaces = map normalizeSpace

normalizeSpace :: Char -> Char
normalizeSpace character
  | isSpace character = '\x20'
  | otherwise = character

lexer :: String -> [Token]
lexer = alexScanTokens . normalizeSpaces
}
