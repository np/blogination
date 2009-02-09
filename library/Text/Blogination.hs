{-# LANGUAGE RecordWildCards #-}
module Text.Blogination 
    (Blog(..)
    ,Blogination
    ,runBloginator
    ,buildBlog
    ,ensureProperState
    ,renderIndex
    ,renderEntries
    ,pageToHtml
    ,highlight)
    where

import Control.Arrow
import Control.Monad.State
import Control.Monad.Error
import Data.Char hiding (Space)
import Data.List.Higher
import Data.Maybe
import Text.Highlighting.Kate
import Text.Pandoc
import Text.Printf
import Text.XHtml.Strict
import System.Directory
import System.FilePath
import System.Time

data Blog = Blog
    { blogName     :: String -- e.g. Chris Done's Blog
    , blogRoot     :: String -- /blog
    , blogCSS      :: [String] -- e.g. ["style.css","highlight.css"]
    , blogEntries  :: FilePath
    , blogHtml     :: FilePath
    , blogAuthor   :: String
    , blogForce    :: Bool
    } deriving (Read,Show)

type Blogination = ErrorT String (StateT Blog IO)

runBloginator :: Blogination a -> Blog -> IO (Either String a)
runBloginator m blog = evalStateT (runErrorT m) blog

buildBlog :: Blogination ()
buildBlog = do ensureProperState
               anyChanges <- renderEntries
               when anyChanges renderIndex

renderIndex :: Blogination ()
renderIndex = do
  blog@Blog{..} <- lift get
  links <- mapM getEntryLink =<< getEntryNames
  let html = toHtml [title,name,menu]
      title = thetitle << blogName
      name = h1 << blogName
      menu = ulist << map ((li<<) . showLink) links
  liftIO $ writeFile "index.html" $ showHtml html
      where showLink (url,title) = hotlink url << title

getEntryLink :: FilePath -> Blogination (URL,String)
getEntryLink path = do
  blog@Blog{..} <- lift get
  liftIO $ do
    contents <- readFile (blogEntries</>path)
    return (blogRoot++"/"++blogHtml++"/"++path++".html"
           ,getTitle $ read $ contents)
        where read = readMarkdown defaultParserState

renderEntries :: Blogination Bool
renderEntries = do
  blog@Blog{..} <- lift get
  names <- getEntryNames
  let times dir = liftIO $ mapM (getModificationTime' . dir) names
  entryTimes <- times (blogEntries</>)
  htmlTimes <- times ((blogHtml</>).(++".html"))
  let updated = catMaybes $ zipWith compare names $ zip entryTimes htmlTimes
      compare name (entry,html) | entry > html = Just name
                                | otherwise    = Nothing
      toChange | blogForce = names
               | otherwise = updated
  mapM_ renderEntry toChange
  return $ not $ null toChange

getModificationTime' :: FilePath -> IO (Maybe ClockTime)
getModificationTime' path = do
  exists <- doesFileExist path
  if exists
     then Just `fmap` getModificationTime path
     else return Nothing

renderEntry :: FilePath -> Blogination ()
renderEntry path = do
  blog@Blog{..} <- lift get
  liftIO $ do
    contents <- readFile (blogEntries</>path)
    writeFile (blogHtml</>path++".html") $ showHtml $ pageToHtml blog contents

getEntryNames :: Blogination [FilePath]
getEntryNames = do
  Blog{..} <- lift get
  return . clean =<< liftIO (getDirectoryContents blogEntries) 
    where clean = reverse . sort . filter (not . all (=='.'))

ensureProperState :: Blogination ()
ensureProperState = do
  Blog{..} <- lift get
  entries <- liftIO $ doesDirectoryExist blogEntries
  when (not entries) $ do 
    throwError $ printf "Blog entries directory \"%s\" does not exist."
                        blogEntries
    return ()
  liftIO $ createDirectoryIfMissing False blogHtml

pageToHtml :: Blog -> String -> Html
pageToHtml blog = html . second write . (getTitle &&& highlight) . read where
    read = readMarkdown defaultParserState
    write = writeHtml defaultWriterOptions
    html = template blog

template :: Blog -> (String,Html) -> Html
template Blog{..} (title,html) = toHtml [head,thebody] where
    head = header << [toHtml $ map style blogCSS
                     ,meta ! [httpequiv "Content-Type"
                             ,content "text/html; charset=utf-8"]
                     ,thetitle << title]
    thebody = body << [back,hr,html,hr,back]
    back = toHtml $ p << hotlink blogRoot << ("« Back to " ++ blogName)
    style css = thelink ! [rel "stylesheet",href (blogRoot++"/"++css)]
                << noHtml

getTitle :: Pandoc -> String
getTitle (Pandoc meta blocks) = title blocks where
    title = list "" head . catMaybes . map getHeading
    getHeading (Header 1 parts) = Just $ join $ map getPart parts
    getHeading _ = Nothing
    getPart (Str str) = str
    getPart Space = " "
    getPart _ = " "

highlight :: Pandoc -> Pandoc
highlight (Pandoc meta blocks) = Pandoc meta newblocks where
    newblocks = map tryHighlight blocks
    tryHighlight (CodeBlock opts ('$':rest))
        | take 1 rest == "$" = CodeBlock opts rest
        | otherwise = 
            case highlightOpts ('$':rest) of
              Just (lang,code) -> highlightWith lang code
              Nothing          -> CodeBlock opts ('$':rest)
    tryHighlight other = other

highlightOpts :: String -> Maybe (String,String)
highlightOpts = langAndCode . break isSpace where
    langAndCode ('$':lang,code) = Just (lang,drop 1 code)
    langAndCode _               = Nothing

highlightWith :: String -> String -> Block
highlightWith lang code = RawHtml $ showHtml html where
    html = either (const def) format highlight
    format = formatAsXHtml [] lang
    highlight = highlightAs lang code
    def = pre << code
