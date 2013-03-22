##Light Markup templates

Markdown example:

```haskell
import qualified Text.Shakespeare.LightMarkup as LM

setTitle "Credits"
toWidget $ LM.markdownToHtmlUrl $(LM.lmFile "templates/credits.markdown")
```

Textile and RST have similar treatement, although not fully tested.
