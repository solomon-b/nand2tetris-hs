module Parser.Generator
  ( -- * Generator
    generate,

    -- * Helpers
    mkList,
    mkListNonEmpty,
    dummySpan,
  )
where

-------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty qualified as NE
import Parser
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude

-------------------------------------------------------------------------------

generate :: MonadIO m => Gen a -> m a
generate = Gen.sample

-------------------------------------------------------------------------------
-- Helpers

mkList :: Gen a -> Gen [a]
mkList = Gen.list $ Range.linear 0 11

mkListNonEmpty :: Gen a -> Gen (NE.NonEmpty a)
mkListNonEmpty = Gen.nonEmpty $ Range.linear 1 11

-- | Placeholder span for generators
dummySpan :: Span
dummySpan = Span (AlexSourcePos 1 1) (AlexSourcePos 1 2)
