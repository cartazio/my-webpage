{-# LANGUAGE OverloadedStrings #-}
module Main  where

import Text.Pandoc
import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat)

import Hakyll

-- import Hakyll.Web.Pandoc

mathJaxWriterOptions = defaultHakyllWriterOptions{writerHTMLMathMethod   =  MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML-full" }

myRender ::  Compiler (Page String) (Page String)
myRender  =
    pageRenderPandocWith defaultHakyllParserState mathJaxWriterOptions

-- main :: IO ()
-- main = hakyll $ do
--     route   "css/*" idRoute
--     compile "css/*" compressCssCompiler
-- 
--     compile "templates/*" templateCompiler
--     forM_ ["research.markdown", "index.markdown", "other stkjdkfj"] $ \page -> do
--         route   page $ setExtension "html"
--         compile page $ pageCompiler
--             >>> applyTemplateCompiler "templates/default.html"
--             >>> relativizeUrlsCompiler



main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
            >>> myRender

    -- Render posts list
    match  "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
        >>> myRender

    -- Index
    match  "home.markdown" $ route idRoute
    route $ setExtension "html"
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . sortByBaseName) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
        >>> myRender

    -- Render RSS feed
    match  "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ "posts/*" >>> renderRss feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Carter's Web Page"
    , feedDescription = "Research and Blog Site"
    , feedAuthorName  = "Carter Tazio Schonwald"
    , feedRoot        = "http://example.com"
    }