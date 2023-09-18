-- | GraphQL IR
module Parser.Syntax
  (
    Chip (..),
    Part (..),
    Binder (..),
    Binding (..),
    Name,
    mkName,
    unName,
    unsafeMkName,
    renderPretty,
    renderPrettyBS,
  )
where

--------------------------------------------------------------------------------

import Data.ByteString.Char8 qualified as B
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Parser.Span (Span)
import Parser.Span qualified as S
import Parser.Syntax.Name
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------

-- ghci> pretty $ Chip (Span alexStartPos alexStartPos) "AND" ["a", "b"] ["out"] [Part "NAND" [Binding "a" "a", Binding "b" "b", Binding "out" "NandOut"] (Span alexStartPos alexStartPos), Part "NOT" [Binding "in" "NandOut", Binding "out" "out"] (Span alexStartPos alexStartPos )]
-- CHIP AND {
--     IN
--         a, b;
--     OUT
--         out;
--     PARTS:
--         NAND (a=a, b=b, out=NandOut);
--         NOT (in=NandOut, out=out);
-- }


data Chip = Chip Span Text [Binder] [Binder] [Part]
  deriving stock (Show, Lift)

data Binder = Atom Text | Array Text Int
  deriving stock (Show, Lift)

instance PP.Pretty Binder where
  pretty (Atom bndr) = PP.pretty bndr
  pretty (Array bndr bits) = PP.pretty bndr <> PP.enclose "[" "]" (PP.pretty bits)

instance PP.Pretty Chip where
  pretty (Chip _ bndr input output parts) =
    let input' = "IN" <> PP.nest 4 (PP.line <> PP.sep (PP.punctuate "," $ fmap PP.pretty input) <> ";")
        output' = "OUT" <> PP.nest 4 (PP.line <> PP.sep (PP.punctuate "," $ fmap PP.pretty output) <> ";")
        parts' = "PARTS:" <> PP.nest 4 (PP.line <> PP.align (PP.vsep (fmap PP.pretty parts)))
        decl = "CHIP" PP.<+> PP.pretty bndr
    in
    decl PP.<+> PP.enclose (PP.lbrace <> PP.line) (PP.line <> PP.rbrace)
      (PP.indent 4 $ PP.align $ PP.vsep [input', output', parts'])

data Part = Part { partSpan :: Span, name :: Text, bindings :: [Binding] }
  deriving stock (Show, Lift)

instance S.Located Part where
  locate = partSpan

instance PP.Pretty Part where
  pretty Part {..} = PP.pretty name PP.<+> PP.tupled (fmap PP.pretty bindings) <> ";"

data Binding = Binding Text Text
  deriving stock (Show, Lift)

instance PP.Pretty Binding where
  pretty (Binding x y) = PP.pretty x <> "=" <> PP.pretty y

--------------------------------------------------------------------------------
-- Pretty helpers

renderDoc :: PP.Doc ann -> Text
renderDoc = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

renderPretty :: PP.Pretty a => a -> Text
renderPretty = renderDoc . PP.pretty

renderPrettyBS :: PP.Pretty a => a -> B.ByteString
renderPrettyBS = TE.encodeUtf8 . renderDoc . PP.pretty
