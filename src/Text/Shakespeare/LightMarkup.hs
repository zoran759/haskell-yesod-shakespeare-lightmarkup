{-# LANGUAGE PackageImports, TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Text.Shakespeare.LightMarkup (
  lm,
  lmFile,
  lmFileReload,
  markdownToHtmlUrl,
  rstToHtmlUrl,
  textileToHtmlUrl        
) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import "shakespeare" Text.Shakespeare 
import qualified "text" Data.Text.Lazy as TL
import qualified "text" Data.Text as TS
import "text" Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import "blaze-html" Text.Blaze.Html (Html)
import Text.Blaze.Html       (preEscapedToMarkup)
import Text.Pandoc
import Text.HTML.SanitizeXSS (sanitizeBalance)

import Data.Int (Int32, Int64)

renderText :: Builder -> TL.Text
renderText = toLazyText

renderTextUrl :: RenderUrl url -> TextUrl url -> TL.Text
renderTextUrl r s = renderText $ s r

type TextUrl url = RenderUrl url -> Builder

class ToText a where
    toText :: a -> Builder
    
instance ToText [Char ] where toText = fromLazyText . TL.pack
instance ToText TS.Text where toText = fromText
instance ToText TL.Text where toText = fromLazyText

instance ToText Int32 where toText = toText . show
instance ToText Int64 where toText = toText . show

markdownSettings :: Q ShakespeareSettings
markdownSettings = do
  toMdExp <- [|toText|]
  wrapExp <- [|id|]
  unWrapExp <- [|id|]
  return $ defaultShakespeareSettings { toBuilder = toMdExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  }

lm :: QuasiQuoter
lm = (QuasiQuoter undefined undefined undefined undefined) { quoteExp = \s -> do
    rs <- markdownSettings
    quoteExp (shakespeare rs) s
    }

lmFile :: FilePath -> Q Exp
lmFile fp = do
    rs <- markdownSettings
    shakespeareFile rs fp


lmFileReload :: FilePath -> Q Exp
lmFileReload fp = do
    rs <- markdownSettings
    shakespeareFileReload rs fp


newtype RST = RST String
   deriving (Eq, Ord, Show, Read)   

newtype Markdown = Markdown String
    deriving (Eq, Ord, Show, Read)

newtype Textile = Textile String
    deriving (Eq, Ord, Show, Read)
    
-- | use in a widget monad
--    toWidget $ markdownToHtmlUrl $(lmFile "templates/myfile.markdown")

markdownToHtmlUrl :: TextUrl url -> RenderUrl url -> Html
markdownToHtmlUrl txt rurl = (markdownToHtml . Markdown . TL.unpack . renderTextUrl rurl) txt

rstToHtmlUrl :: TextUrl url -> RenderUrl url -> Html
rstToHtmlUrl txt rurl = (rstToHtml . RST . TL.unpack . renderTextUrl rurl) txt

textileToHtmlUrl :: TextUrl url -> RenderUrl url -> Html
textileToHtmlUrl txt rurl = (textileToHtml . Textile . TL.unpack . renderTextUrl rurl) txt

rstToHtml :: RST -> Html
rstToHtml = writePandoc yesodDefaultWriterOptions
          . parseRST yesodDefaultParserState

markdownToHtml :: Markdown -> Html
markdownToHtml = writePandoc yesodDefaultWriterOptions
               . parseMarkdown yesodDefaultParserState

textileToHtml :: Textile -> Html
textileToHtml = writePandoc yesodDefaultWriterOptions
               . parseTextile yesodDefaultParserState
               
writePandoc :: WriterOptions -> Pandoc -> Html
writePandoc wo = preEscapedToMarkup . sanitizeBalance . TS.pack . writeHtmlString wo

parseMarkdown :: ParserState -> Markdown -> Pandoc
parseMarkdown ro (Markdown m) = readMarkdown ro m

parseRST :: ParserState -> RST -> Pandoc
parseRST ro (RST m) = readRST ro m

parseTextile :: ParserState -> Textile -> Pandoc
parseTextile ro (Textile m) = readTextile ro m


yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = defaultWriterOptions
  { writerHtml5    = True
  , writerWrapText = False
  }

yesodDefaultParserState :: ParserState
yesodDefaultParserState = defaultParserState
    { stateSmart    = True
    , stateParseRaw = True
    }
   
