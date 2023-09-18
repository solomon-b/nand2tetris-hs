{
module Parser.Lexer where

import Control.Monad.State (gets)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Parser.Error
import Parser.Monad
import Parser.Token
}

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]
$hex = [A-Fa-f0-9]

tokens :-

-- Whitespace insensitive
<0> $white+                             ;

-- Comments
<0> "//".*                               ;

-- Symbols
<0> \,                                  { symbol SymComma }
<0> \:                                  { symbol SymColon }
<0> \;                                  { symbol SymSemicolon }
<0> \=                                  { symbol SymEq }
<0> \{                                  { symbol SymCurlyOpen }
<0> \}                                  { symbol SymCurlyClose }
<0> \(                                  { symbol SymParenOpen }
<0> \)                                  { symbol SymParenClose }
<0> \[                                  { symbol SymSquareOpen }
<0> \]                                  { symbol SymSquareClose }

-- Numbers
<0> \-? $digit+                                       { token (\loc -> TokIntLit (read . T.unpack <$> loc)) }

-- Identifiers
<0> [\_ $alpha] [\_ $alpha $digit]* { token TokIdentifier }

{

stripStart :: B.ByteString -> B.ByteString -> B.ByteString
stripStart pre bs =
  case B.stripPrefix pre bs of
    Nothing -> bs
    Just bs' -> bs'

stripEnd :: B.ByteString -> B.ByteString -> B.ByteString
stripEnd suff bs =
  case B.stripSuffix suff bs of
    Nothing -> bs
    Just bs' -> bs'

stripStartEnd :: B.ByteString -> B.ByteString -> B.ByteString
stripStartEnd pat bs = stripEnd pat $ stripStart pat bs

-- | The monadic wrapper for 'alexScan'. The 'Parser' type is defined
-- in 'Parser.Monad' as a transformer stack of 'StateT' and
-- 'Except'. The Start Code Stack, current 'Span', and 'AlexInput' are
-- tracked in 'StateT'.
--
-- 'scan' recursively consumes the consumes the 'AlexInput' from state
-- until it produces a 'Token', an error, or reaches the end of the
-- file.
scan :: Parser Token
scan = do
  input <- getInput
  code <- startCode
  src <- gets parseSource
  sp <- location
  case alexScan input code of
    AlexEOF -> pure (EOF sp)
    AlexError (AlexInput pos _ _ _) ->
      parseError $ InvalidLexeme pos src
    AlexSkip rest _ -> do
      advance rest
      scan
    AlexToken rest nbytes action -> do
      advance rest
      action (slice nbytes input)

-- | The entry point to the lexer. Recursively calls 'scan' to yield
-- tokens unti we hit EOF.
lexer :: Parser [Token]
lexer = do
  tok <- scan
  case tok of
    EOF _ -> pure []
    x -> (x :) <$> lexer
}
