module Parser
  ( module T,
    module Syntax,
    module M,
    module S,
    module E,
    parseChip,
    parseName,
  )
where

--------------------------------------------------------------------------------

import Data.ByteString qualified as B
import Parser.Error as E
import Parser.Grammar qualified as P
import Parser.Lexer qualified as L
import Parser.Monad as M
import Parser.Span as S
import Parser.Syntax as Syntax
import Parser.Token as T

--------------------------------------------------------------------------------

parseChip :: B.ByteString -> Either ParseError Chip
parseChip bs = M.runParser [] bs $ do
  toks <- L.lexer
  P.parseChip toks

parseName :: MonadFail m => B.ByteString -> m Name
parseName bs =
  let result = M.runParser [] bs $ do
        toks <- L.lexer
        fmap unLoc $ P.parseName toks
      errorMessage = show $ bs <> " is not valid GraphQL name"
   in either (\_ -> fail errorMessage) pure result
