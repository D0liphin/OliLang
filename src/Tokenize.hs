module Tokenize (tokenize, Token, Tok, TokenizeError (..)) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text qualified as T
import Parse

type IText = (Int, T.Text)

type Tokenizer a = Parser TokenizeError IText a

newtype TokenizeError = TokenizeError Int

data Tok
  = LBrace
  | Module
  | Extern
  | New
  | Float
  | Int
  | Class
  | Data
  | RBrace
  | LParen
  | RParen
  | LSquare
  | RSquare
  | Equals
  | DblArrow
  | RightArrow
  | Comma
  | DblColon
  | DblEquals
  | Backtick
  | Lt
  | Gt
  | Leq
  | Geq
  | Pipe
  | Plus
  | Minus
  | Asterisk
  | FSlash
  | Whitespace
  | Colon
  | Newline
  | Ident T.Text
  | Comment
  deriving (Show)

data Token = Token
  { index :: Int,
    len :: Int,
    tok :: Tok
  }

nonNull :: Tokenizer ()
nonNull (i, t)
  | T.null t = Err (TokenizeError i)
  | otherwise = Ok ((i, t), ())

instance Show Token where
  show (Token index len tok) = show tok ++ " [" ++ show index ++ ":" ++ show (index + len - 1) ++ "]"

token tok len index = Token {tok, len, index}

parseKeyword :: String -> Tok -> Tokenizer Token
parseKeyword str tk (i, text)
  | T.pack str `T.isPrefixOf` text =
      let len = length str
       in Ok ((i + len, T.drop len text), token tk len i)
  | otherwise = Err (TokenizeError i)

isWhitespace ' ' = True
isWhitespace c = False

parseWhitespace :: Tokenizer Token
parseWhitespace (i, t)
  | isWhitespace (T.head t) =
      let (w, t') = T.span isWhitespace t
          len = T.length w
       in Ok ((i + len, t'), token Whitespace len i)
  | otherwise = Err (TokenizeError i)

isIdentHead = isAlpha

isIdentTail = isAlphaNum

parseIdent :: Tokenizer Token
parseIdent (i, t)
  | isIdentHead (T.head t) =
      let (ident', t') = T.span isIdentTail (T.tail t)
          ident = T.head t `T.cons` ident'
          len = T.length ident
       in Ok ((i + len, t'), token (Ident ident) len i)
  | otherwise = Err (TokenizeError i)

parseComment :: Tokenizer Token
parseComment (i, t)
  | T.pack "--" `T.isPrefixOf` t =
      let (h, t') = T.span (/= '\n') t
          len = T.length h
       in Ok ((i + len, t'), token Comment len i)
  | otherwise = Err (TokenizeError i)

tokenizers =
  [
    parseKeyword "mdl" Module,
    parseKeyword "ext" Extern,
    parseKeyword "cls" Class,
    parseKeyword "new" New,
    parseKeyword "flt" Float,
    parseKeyword "dat" Data,
    parseKeyword "int" Int,
    parseKeyword "::" DblColon,
    parseKeyword "=>" DblArrow,
    parseKeyword "->" RightArrow,
    parseKeyword "<=" Leq,
    parseKeyword ">=" Geq,
    parseKeyword "<" Lt,
    parseKeyword ">" Gt,
    parseKeyword "==" DblEquals,
    parseKeyword "{" LBrace,
    parseKeyword "}" RBrace,
    parseKeyword "(" LParen,
    parseKeyword ")" RParen,
    parseKeyword "[" LSquare,
    parseKeyword "]" RSquare,
    parseKeyword "\n" Newline,
    parseKeyword "," Comma,
    parseKeyword "=" Equals,
    parseKeyword ":" Colon,
    parseKeyword "`" Backtick,
    parseKeyword "+" Plus,
    parseKeyword "-" Minus,
    parseKeyword "/" FSlash,
    parseKeyword "|" Pipe,
    parseIdent,
    parseWhitespace,
    parseComment
  ]

tokenize t =
  let ((i, t'), toks) = star (nonNull `discard` alt tokenizers) t
   in if T.null t' then Ok toks else Err i