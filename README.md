##Light Markup templates

Markdown example:

    setTitle "Credits"
    toWidget $ LM.markdownToHtmlUrl $(LM.lmFile "templates/credits.markdown")
