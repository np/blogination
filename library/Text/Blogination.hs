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

import Control.Arrow hiding ((+++))
import Control.Monad.State
import Control.Monad.Error
import Data.Char hiding (Space)
import Data.List.Higher
import Data.Maybe
import Data.Time.Format
import Data.Time.Clock
import Prelude hiding (readFile,writeFile)
import Text.Highlighting.Kate
import Text.Pandoc
import Text.Printf
import Text.RSS.Export
import Text.RSS.Syntax
import Text.XHtml.Strict
import Text.XML.Light.Output
import System.Directory
import System.FilePath
import System.IO.UTF8 (readFile,writeFile)
import System.Time
import System.Locale
import Data.Monoid

data Blog = Blog
    { blogName     :: String -- e.g. Chris Done's Blog
    , blogRoot     :: String -- /blog
    , blogCSS      :: [String] -- e.g. ["style.css","highlight.css"]
    , blogEntries  :: FilePath
    , blogHtml     :: FilePath
    , blogAuthor   :: String
    , blogForce    :: Bool
    , blogDate     :: String -- date format e.g. 
                             -- "%A %d %b, %Y" makes "Tuesday 10 Feb, 2009"
    , blogTags     :: FilePath
    , blogURL      :: String -- e.g. "http://chrisdone.com/blog"
     } deriving (Read,Show)

type Blogination = ErrorT String (StateT Blog IO)

runBloginator :: Blogination a -> Blog -> IO (Either String a)
runBloginator m blog = evalStateT (runErrorT m) blog

buildBlog :: Blogination ()
buildBlog = do ensureProperState
               changed <- renderEntries
               when (not $ null changed) $ do
                 renderTags changed
                 renderIndex
                 renderIndexRSS

renderIndexRSS :: Blogination ()
renderIndexRSS = do
  getEntryNames >>= renderEntriesRSS . take 5 
                >>= liftIO . writeFile "rss.xml"

renderTags :: [FilePath] -> Blogination ()
renderTags entries = mapM_ (renderTag entries) =<< getTags

renderTag :: [FilePath] -> FilePath -> Blogination ()
renderTag entries tag = do 
  changedInThisTag <- (intersect entries) `fmap` getTagEntryNames tag
  when (not $ null changedInThisTag) $ do
    getTagEntryNames tag >>= renderEntriesRSS . take 5
    renderTagHtml tag

renderEntriesRSS :: [FilePath] -> Blogination String
renderEntriesRSS names = do
  channel <- renderToRSS names
  let rss = RSS "2.0" [] channel []
      xml = xmlRSS rss
      string = showElement xml
  return string

renderToRSS :: [FilePath] -> Blogination RSSChannel
renderToRSS names = do
  blog@Blog{..} <- lift get
  items <- mapM entryToItem names
  return $ (nullChannel blogName blogURL) { rssItems = items }

entryToItem :: FilePath -> Blogination RSSItem
entryToItem path = do
  blog@Blog{..} <- lift get
  let get = liftIO . readFile . (blogEntries</>)
  fmap (item . (getTitle &&& write) . read) . get $ path where
    read = readMarkdown defaultParserState
    write = writeHtml defaultWriterOptions
    item (title,content) = (nullItem title) 
                           { rssItemDescription =
                                 Just $ showHtmlFragment content 
                           , rssItemPubDate = show `fmap` makeDate path }

renderTagHtml :: FilePath -> Blogination ()
renderTagHtml tag = do
  blog@Blog{..} <- lift get
  links <- mapM getEntryLink =<< getTagEntryNames tag
  let html = [head,thebody]
      head = header << [toHtml $ map style blogCSS
                       ,encoding
                       ,thetitle << title
                       ,rss]
      thebody = body << [back,hr,name,menu,hr,back]
      title = tag ++ " - " ++ blogName
      name = h2 << ("Tag: " ++ tag)
      menu = ulist << map ((li<<) . showLink) links
      back = toHtml $ p << hotlink blogRoot << ("« Back to " ++ blogName)
      style css = thelink ! [rel "stylesheet",href (blogRoot++css)]
                  << noHtml
      rss = thelink ! [rel "alternate",thetype "application/rss+xml"
                      ,href $ blogRoot++"tags/"++tag++".xml"] << noHtml
  liftIO $ writeFile (blogTags</>tag++".html") $ showHtml html
      where showLink (url,title) = hotlink url << title  

getTagEntryNames :: FilePath -> Blogination [FilePath]
getTagEntryNames path = do
  blog@Blog{..} <- lift get
  names <- lines `fmap` liftIO (readFile (blogTags</>path))
  liftIO $ fmap (reverse . sort) $ filterM (doesFileExist . (blogEntries</>)) names

renderIndex :: Blogination ()
renderIndex = do
  blog@Blog{..} <- lift get
  links <- mapM getEntryLink =<< getEntryNames
  alltags <- getTags
  let html = toHtml [header<<[title,encoding,rss]
                    ,body<<[name,menu,tags]]
      title = thetitle << blogName
      name = h1 << blogName
      menu = h2 << "Posts" +++ ul (map showLink links)
      tags = h2 << "Tags" +++ ul (map (mkTagLink blog) alltags)
      ul l = ulist << map (li<<) l
      rss = thelink ! [rel "alternate",thetype "application/rss+xml"
                      ,href $ blogRoot++"rss.xml"] << noHtml
  liftIO $ writeFile "index.html" $ showHtml html
      where showLink (url,title) = hotlink url << title

getEntryLink :: FilePath -> Blogination (URL,String)
getEntryLink path = do
  blog@Blog{..} <- lift get
  liftIO $ do
    contents <- readFile (blogEntries</>path)
    return (blogRoot++blogHtml++"/"++path++".html"
           ,getTitle $ read $ contents)
        where read = readMarkdown defaultParserState

renderEntries :: Blogination [FilePath]
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
  return toChange

getModificationTime' :: FilePath -> IO (Maybe ClockTime)
getModificationTime' path = do
  exists <- doesFileExist path
  if exists
     then Just `fmap` getModificationTime path
     else return Nothing

renderEntry :: FilePath -> Blogination ()
renderEntry path = do
  blog@Blog{..} <- lift get
  alltags <- getTags
  tagEntries <- mapM getTagEntryNames alltags
  let tags = map fst . filter (any (==path) . snd) $ zip alltags tagEntries
  liftIO $ do
    contents <- readFile (blogEntries</>path)
    writeFile (blogHtml</>path++".html") $ 
       showHtml $ pageToHtml blog path tags contents
    where match = map fst . filter (any (== path) . snd)

getEntryNames :: Blogination [FilePath]
getEntryNames = do
  Blog{..} <- lift get
  fileClean `fmap` liftIO (getDirectoryContents blogEntries) 

getTags :: Blogination [FilePath]
getTags = do 
  Blog{..} <- lift get
  (fileClean . filterPlain) `fmap` liftIO (getDirectoryContents blogTags)

filterPlain = filter (all (flip any [isLetter,isSpace,isDigit] . flip ($)))

fileClean = reverse . sort . filter (not . all (=='.'))

ensureProperState :: Blogination ()
ensureProperState = do
  Blog{..} <- lift get
  entries <- liftIO $ doesDirectoryExist blogEntries
  when (not entries) $ do 
    throwError $ printf "Blog entries directory \"%s\" does not exist."
                        blogEntries
    return ()
  liftIO $ createDirectoryIfMissing False blogHtml
  fixBlogRoot

fixBlogRoot :: Blogination ()
fixBlogRoot = do
  Blog{..} <- lift get
  modify $ \s -> s { blogRoot = fix blogRoot }
      where fix = (++"/") . reverse . dropWhile (=='/') . reverse

pageToHtml :: Blog -> FilePath -> [String] -> String -> Html
pageToHtml blog fname tags = 
    html . second write . (getTitle &&& highlight) . read where
    read = readMarkdown defaultParserState
    write = writeHtml defaultWriterOptions
    html = template blog fname tags

template :: Blog -> FilePath -> [String] -> (String,Html) -> Html
template blog@Blog{..} path tags (title,html) = toHtml [head,thebody] where
    head = header << [toHtml $ map style blogCSS
                     ,encoding
                     ,thetitle << title]
    thebody = body << [back,hr,tagndate,html,hr,back]
    back = toHtml $ p << hotlink blogRoot << ("« Back to " ++ blogName)
    style css = thelink ! [rel "stylesheet",href (blogRoot++css)]
                << noHtml
    tagndate = p << (small << (date +++ ", " +++ tag))
    date = "Date: " +++ (maybe noHtml showTime $ makeDate path)
    tag = "Tags: " +++ (mconcat $ intersperse (toHtml ", ") taglinks)
    taglinks = map (mkTagLink blog) tags
    showTime = toHtml . formatTime defaultTimeLocale blogDate

mkTagLink :: Blog -> FilePath -> Html
mkTagLink Blog{..} tag = 
    toHtml $ hotlink (blogRoot++"tags/"++tag++".html") << tag

makeDate :: FilePath -> Maybe UTCTime
makeDate path = 
    parse $ take (length "0000-00-00") path
    where parse = parseTime defaultTimeLocale "%Y-%m-%d"

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

encoding = meta ! [httpequiv "Content-Type"
                  ,content "text/html; charset=utf-8"]