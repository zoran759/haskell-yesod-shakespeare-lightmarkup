##Light Markup templates

Markdown example:

```haskell

setTitle "Credits"
toWidget $ LM.markdownToHtmlUrl $(LM.lmFile "templates/credits.markdown")
```

Textile and RST have similar treatement, although not fully tested.
