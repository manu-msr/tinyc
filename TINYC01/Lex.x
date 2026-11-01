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
  "="                   { \_ -> TokenAssign }
  ";"                   { \_ -> TokenSemi }
  "+"                   { \_ -> TokenPlus }
  "-"                   { \_ -> TokenMinus }
  "*"                   { \_ -> TokenTimes }
  "("                   { \_ -> TokenLParen }
  ")"                   { \_ -> TokenRParen }

  $digit+               { \source -> TokenNumber (read source) }
  $letter$idrest*       { \source -> TokenIdentifier source }

  .                     { \source -> error ("Error lexico: caracter no reconocido " ++ show source) }

{
data Token
  = TokenInt
  | TokenAssign
  | TokenSemi
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenLParen
  | TokenRParen
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
