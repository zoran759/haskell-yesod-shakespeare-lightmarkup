##Light Markup templates

Markdown example:

```haskell
import qualified Text.Shakespeare.LightMarkup as LM


getCreditsR :: Handler RepHtml
getCreditsR = do
    ...
    defaultLayout $ do
        ...
        setTitle "Credits"
        toWidget $ LM.markdownToHtmlUrl $(LM.lmFile "templates/credits.markdown")
```

Textile and RST have similar treatement.
