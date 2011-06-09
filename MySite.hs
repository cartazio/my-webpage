{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Pandoc
import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat)

import Hakyll

-- import Hakyll.Web.Pandoc

mathJaxWriterOptions = defaultHakyllWriterOptions{writerHTMLMathMethod   =  MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML-full" }

mathPageCompiler ::  Compiler Resource (Page String)
mathPageCompiler =
   pageCompilerWith  defaultHakyllParserState mathJaxWriterOptions




main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ mathPageCompiler
            -- >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
     

    -- Render posts list
    match  "posts.markdown" $ route idRoute
    route $ setExtension "html"
    create "posts.html" $ constA mempty
         >>> arr (setField "title" "All posts")
         >>> requireAllA "posts/*" addPostList
         >>> applyTemplateCompiler "templates/posts.html"
         >>> applyTemplateCompiler "templates/default.html"
         -- >>> mathPageCompiler
         >>> relativizeUrlsCompiler

 

    -- Index
    match  "home.markdown" $ route idRoute
    route $ setExtension "html"
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . chronological) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

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
    arr (reverse . chronological)
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