{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Control.Monad (forM_)

import Hakyll
-- import Hakyll.Web.Pandoc

mathJaxWriterOptions = defaultHakyllWriterOptions{writerHTMLMathMethod   =  MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML-full" }

myRender ::  Compiler (Page String) (Page String)
myRender  =
    pageRenderPandocWith defaultHakyllParserState defaultHakyllWriterOptions
main :: IO ()
main = hakyll $ do
    route   "css/*" idRoute
    compile "css/*" compressCssCompiler

    compile "templates/*" templateCompiler
    forM_ ["research.markdown", "index.markdown", "other stkjdkfj"] $ \page -> do
        route   page $ setExtension "html"
        compile page $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler