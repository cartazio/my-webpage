module Main where

import Control.Arrow ((>>>))
import Text.Hakyll (hakyll)
import Text.Hakyll.Render
import Text.Hakyll.Context
import Text.Hakyll.File (getRecursiveContents, directory)
import Text.Hakyll.CreateContext (createPage, createCustomPage, createListing)
import Text.Hakyll.ContextManipulations (copyValue)
import Text.Hakyll.Feed (FeedConfiguration (..), renderRss)
import Data.List (sort)
import Control.Monad (forM_, liftM)
import Control.Monad.Reader (liftIO)
import Data.Either (Either(..))

myHakyllConfig  absoluteUrl' = HakyllConfiguration
    { absoluteUrl         = absoluteUrl'
    , additionalContext   = mempty
    , siteDirectory       = "_site"
    , cacheDirectory      = "_cache"
    , enableIndexUrl      = False
    , previewMode         = BuildOnRequest
    , pandocParserState   = defaultPandocParserState{stateParseRaw  = True}
    , pandocWriterOptions = defaultPandocWriterOptions{writerHTMLMathMethod   = }
    , hamletSettings      = defaultHamletSettings
    }


main = myHakyllConfig "http://www.cs.dartmouth.edu/~carter" $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let postPages = map createPage postPaths

    -- Render index, including recent posts.
    let index = createListing "index.html" ["templates/postitem.html"]
                              (take 3 postPages) [("title", Left "Home")]
    renderChain ["index.html", "templates/default.html"] index

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
