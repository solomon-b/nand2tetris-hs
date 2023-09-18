{
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module Parser.Grammar where

import Control.Monad.State (gets)
import Data.Coerce
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Parser.Error
import Parser.Monad
import Parser.Span
import Parser.Syntax
import Parser.Token
}

--------------------------------------------------------------------------------

%name parseChip chip
%name parseName name
%tokentype { Token }
%monad { Parser }
%error { failure }

%right LOW
%right '{'

%token

--------------------------------------------------------------------------------

-- Reserved Words
'CHIP'  { TokIdentifier (Loc $$ "CHIP") }
'IN'    { TokIdentifier (Loc $$ "IN") }
'OUT'   { TokIdentifier (Loc $$ "OUT") }
'PARTS' { TokIdentifier (Loc $$ "PARTS") }

-- Symbols
'='            { TokSymbol (Loc $$ SymEq) }
'{'            { TokSymbol (Loc $$ SymCurlyOpen) }
'}'            { TokSymbol (Loc $$ SymCurlyClose) }
'['            { TokSymbol (Loc $$ SymSquareOpen) }
']'            { TokSymbol (Loc $$ SymSquareClose) }
'('            { TokSymbol (Loc $$ SymParenOpen) }
')'            { TokSymbol (Loc $$ SymParenClose) }
':'            { TokSymbol (Loc $$ SymColon) }
';'            { TokSymbol (Loc $$ SymSemicolon) }
','            { TokSymbol (Loc $$ SymComma) }

-- Scalars
int            { TokIntLit $$ }
ident          { TokIdentifier $$ }

%%

--------------------------------------------------------------------------------

name :: { Loc Name }
name
  : ident { Loc (locate $1) (unsafeMkName (unLoc $1)) }

chip :: { Chip }
chip
  : 'CHIP' ident '{' 'IN' binders 'OUT' binders 'PARTS' ':' parts '}' { Chip (locate $1 <> locate $7) (unLoc $2) (unLoc $5) (unLoc $7) $10 }

parts :: { [Part] }
parts
  : part ';' { [$1] }
  | part ';' parts { $1 : $3 }

part :: { Part }
part
  : ident '(' bindings ')' { Part (locate $1 <> locate $4) (unLoc $1) (unLoc $3) }

bindings :: { Loc [Binding] }
bindings
  : ident '=' ident { Loc (locate $1 <> locate $3) [Binding (unLoc $1) (unLoc $3)] }
  | ident '=' ident ',' bindings { Loc (locate $1 <> locate $5) (Binding (unLoc $1) (unLoc $3) : unLoc $5) }

binders :: { Loc [Binder] }
binders
  : ident ';' { Loc (locate $1 <> locate $2) [Atom (unLoc $1)] }
  | ident ',' binders { Loc (locate $1 <> locate $3) (Atom (unLoc $1) : unLoc $3 ) }
  
--------------------------------------------------------------------------------

{
failure :: [Token] -> Parser a
failure [] = do
  sp <- location
  src <- gets parseSource
  parseError $ EmptyTokenStream sp src
failure (tok:_) = do
  src <- gets parseSource
  -- TODO: fix source position capture here. I think we need the prior span.
  parseError $ UnexpectedToken tok src
}
