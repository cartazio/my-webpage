module Main where

import Control.Arrow ((>>>))
import Text.Hakyll (hakyllWithConfiguration,defaultHakyllConfiguration)
import Text.Hakyll.Render
import Text.Hakyll.Context
import Text.Hakyll.File (getRecursiveContents, directory)
import Text.Hakyll.CreateContext (createPage, createCustomPage, createListing,addField)
import Text.Hakyll.ContextManipulations (copyValue)
import Text.Hakyll.Feed (FeedConfiguration (..), renderRss)
import Data.List (sort)
import Control.Monad (forM_, liftM)
import Control.Monad.Reader (liftIO)
import Text.Hakyll.CreateContext    
import Text.Hakyll.Page(readPageAction)

import Data.Either (Either(..))
import Text.Pandoc.Shared
-- import Text.Hamlet
import Text.Hakyll.HakyllMonad
import Text.Pandoc.Parsing


myHakyllConfig :: String -> HakyllConfiguration
myHakyllConfig  absoluteUrl =  
                let r1 =   (defaultHakyllConfiguration absoluteUrl) --{pandocParserState   =  defaultParserState{stateParseRaw  = False}}
                    r2 = r1{pandocWriterOptions =   
                                defaultWriterOptions {writerHTMLMathMethod   =  LaTeXMathML Nothing }} 
                    in 
                       r2


main = hakyllWithConfiguration  (myHakyllConfig "/Users/carter/carter-web") $ do
    -- Static directory.
    directory css "css"
    directory static "static"
    -- Find all post paths.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let postPages = map createPage postPaths


    -- Render index, including recent posts.
    let home = createPage "home.markdown"
    let index = createListing "index.html" ["templates/postitem.html"]
                              (take 5 postPages) [("title", Left "Home")]
    renderChain ["index.html", "templates/default.html"] $ combine index home
                        -- uses combine trick to add blurb

    -- find all research pages
    researchPaths <- liftM (reverse . sort) $ getRecursiveContents "research"
    let researchPages = map createPage researchPaths
    
    -- Render all research list
    let research  = createListing "research.html" ["templates/summary-item.html"]
                                    researchPages [("title",Left "My Research")]
                                    
    renderChain ["research.html","templates/default.html"] research 
    
    codePaths <- liftM (reverse . sort) $ getRecursiveContents "Code"
    let codePages = map createPage codePaths
    
    let code = createListing "code.html" ["template/summary-item"]
                                    codePages [("title", Left "My Code")]
    
    -- Render all posts list.
    let posts = createListing "posts.html" ["templates/postitem.html"]
                              postPages [("title", Left "All posts")]
    renderChain ["posts.html", "templates/default.html"] posts

    -- Render all posts.
    liftIO $ putStrLn "Generating posts..."
    forM_ postPages $ renderChain [ "templates/post.html"
                                  , "templates/default.html"
                                  ]

    -- Render RSS feed.
    renderRss myFeedConfiguration $
        map (>>> copyValue "body" "description") (take 3 postPages)

myFeedConfiguration = FeedConfiguration
    { feedUrl         = "feed.xml"
    , feedTitle       = "Carter Schonwald's Blog"
    , feedDescription = "My occaional thoughts on matters technical or otherwise"
    , feedAuthorName  = "Carter Tazio Schonwald"
    }
