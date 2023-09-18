


-- | Quasiquotation for 'Parser.IR' types.
--
-- These quasiquoters can be used to construct GraphQL literal values at
-- compile-time.
module Parser.QQ
  ( chip,
  )
where

-------------------------------------------------------------------------------

import Data.ByteString.Char8 qualified as Char8
import Parser qualified as P
import Parser.Syntax qualified as Syntax
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)

-------------------------------------------------------------------------------

-- | Construct 'Syntax.Document' literals at compile time via
-- quasiquotation.
chip :: QuasiQuoter
chip =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "document does not support quoting patterns"
    quoteType _ = error "document does not support quoting types"
    quoteDec _ = error "document does not support quoting declarations"
    quoteExp s = case P.parseChip (Char8.pack s) of
      Left err -> fail $ show @P.ParseError err
      Right result -> lift @_ @Syntax.Chip result

