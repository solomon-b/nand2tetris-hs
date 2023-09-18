{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module Parser.Token where

--------------------------------------------------------------------------------

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Parser.Span (Loc (..), Located (..), Span)
import Prettyprinter (Pretty (..))

--------------------------------------------------------------------------------

data Symbol
  = SymComma
  | SymColon
  | SymSemicolon
  | SymCurlyClose
  | SymCurlyOpen
  | SymEq
  | SymParenClose
  | SymParenOpen
  | SymSquareClose
  | SymSquareOpen
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Pretty Symbol where
  pretty = \case
    SymComma -> ","
    SymColon -> ":"
    SymSemicolon -> ";"
    SymCurlyClose -> "}"
    SymCurlyOpen -> "{"
    SymEq -> "="
    SymParenClose -> ")"
    SymParenOpen -> "("
    SymSquareClose -> "]"
    SymSquareOpen -> "["

data Token
  = TokSymbol (Loc Symbol)
  | TokIdentifier (Loc Text)
  | TokIntLit (Loc Integer)
  | EOF Span
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Located Token where
  locate = \case
    TokSymbol sym -> locate sym
    TokIdentifier iden -> locate iden
    TokIntLit int -> locate int
    EOF sp -> sp

overLoc :: (forall a. Loc a -> Loc a) -> Token -> Token
overLoc f (TokSymbol loc) = TokSymbol $ f loc
overLoc f (TokIdentifier loc) = TokIdentifier $ f loc
overLoc f (TokIntLit loc) = TokIntLit $ f loc
overLoc _ (EOF sp) = EOF sp

instance Pretty Token where
  pretty = \case
    TokSymbol sym -> pretty sym
    TokIdentifier iden -> pretty iden
    TokIntLit i -> pretty i
    EOF _ -> mempty
