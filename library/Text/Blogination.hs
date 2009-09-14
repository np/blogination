{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-missing-signatures
            -fno-warn-name-shadowing
            -fno-warn-unused-matches #-}
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

import Control.Applicative
import Control.Arrow hiding ((+++))
import Control.Monad.State
import Control.Monad.Error
import Data.Char hiding (Space)
import Data.List.Higher
import Data.Maybe
import Data.Monoid
import Data.Ord
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
    , blogURL      :: URL -- e.g. "http://chrisdone.com/blog"
    , blogAnalytics :: Maybe String -- google analytics ID e.g. UA-7443395-1
    , blogHome     :: Maybe URL -- home page e.g. "http://chrisdone.com/"
    , blogHomeName :: Maybe String -- home page e.g. Chris Done's Home Page
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
renderTags entries = do
  mapM_ (renderTag entries) =<< getTags

renderTag :: [FilePath] -> FilePath -> Blogination ()
renderTag entries tag = do 
  blog@Blog{..} <- lift get
  changedInThisTag <- intersect entries <$> getTagEntryNames tag
  when (not $ null changedInThisTag) $ do
    getTagEntryNames tag >>= renderEntriesRSS . take 5
                         >>= liftIO . writeFile (blogTags</>tag++".xml")
    renderTagHtml tag

renderEntriesRSS :: [FilePath] -> Blogination String
renderEntriesRSS names =
  showElement . xmlRSS . flip (RSS "2.0" []) [] <$> renderToRSS names

renderToRSS :: [FilePath] -> Blogination RSSChannel
renderToRSS names = do
  blog@Blog{..} <- lift get
  items <- mapM entryToItem names
  return $ (nullChannel blogName blogURL) { rssItems = items }

entryToItem :: FilePath -> Blogination RSSItem
entryToItem path = do
  blog@Blog{..} <- lift get
  let get = liftIO . readFile . (blogEntries</>)
      item (title,content) = (nullItem title) 
        { rssItemDescription =
              Just $ showHtmlFragment content 
        , rssItemPubDate = show `fmap` makeDate path 
        , rssItemLink = Just $ blogURL ++ "/html/" ++ path ++ ".html" }
  fmap (item . (getTitle &&& (write . hideTitle)) . read) . get $ path where
    read = readMarkdown defaultParserState
    write = writeHtml defaultWriterOptions

hideTitle :: Pandoc -> Pandoc
hideTitle (Pandoc meta blocks) = Pandoc meta newblocks where
    newblocks = case blocks of
                  (Header 1 _:content) -> content
                  content -> content

renderTagHtml :: FilePath -> Blogination ()
renderTagHtml tag = do
  blog@Blog{..} <- lift get
  tagEntries <- getTagEntryNames tag
  alltags <- getTags
  links <- mapM getEntryLink tagEntries
  tags <- mapM (entryTags alltags) tagEntries
  let html = htmlTemplate blog theheader (back +++ hr) (hr +++ back) thecontent "tag"
      theheader = toHtml [thetitle << title
                         ,rss $ blogRoot++"tags/"++tag++".xml"]
      thecontent = name +++ menu
      title = tag ++ " - " ++ blogName
      name = h1 << ("Tag: " ++ tag)
      menu = ulist << (map ((li<<) . showLink blog) $ zip links tags)
      back = backlink blogRoot blogName
  liftIO $ writeFile (blogTags</>tag++".html") $ showHtml html

getTagEntryNames :: FilePath -> Blogination [FilePath]
getTagEntryNames path = do
  blog@Blog{..} <- lift get
  names <- lines `fmap` liftIO (readFile (blogTags</>path))
  liftIO $ fmap dateSort $ filterM (doesFileExist . (blogEntries</>)) names

renderIndex :: Blogination ()
renderIndex = do
  blog@Blog{..} <- lift get
  entries <- getEntryNames
  links <- mapM getEntryLink entries
  alltags <- getTags
  tagEntries <- mapM getTagEntryNames alltags
  entryTags <- mapM (entryTags alltags) entries
  let html = htmlTemplate blog theheader (back +++ hr) (hr +++ back) thecontent "index"
      theheader = toHtml [thetitle << blogName
                         ,rss $ blogRoot++"rss.xml"]
      thecontent = name +++ menu +++ tags
      name = h1 << blogName
      menu = thediv ! [theclass "indexposts"]
             << (h2 << "Posts" +++ ul (map (showLink blog) $ zip links entryTags))
      tags = thediv ! [theclass "indextags"]
             << (h2 << "Tags" +++ ul (map (mkTagLink blog) $ zip alltags tagEntries))
      ul l = ulist << map (li<<) l
      back = fromMaybe noHtml $ do
                url <- blogHome
                name <- blogHomeName
                return $ backlink url name
  liftIO $ writeFile "index.html" $ showHtml html

showLink :: Blog -> ((URL,String,UTCTime,ClockTime),[FilePath]) -> Html
showLink blog@Blog{..} ((url,name,created,modified),tags) = toHtml
    [p ! [theclass "link"] << hotlink url << name
    ,p ! [theclass "dates"]
     << [small << ("Created: " +++ showTime created)
        ,toHtml ", "
        ,small << ("Modified: " +++ showTime modified')]
    ,p ! [theclass "tagged"]
           << small << tag]
    where showTime = formatTime defaultTimeLocale blogDate
          modified' = clockToUTCTime modified
          tag = list noHtml (("Tags: " +++) . mconcat . intersperse (toHtml ", ")) taglinks
          taglinks = map (mkTagLink blog) $ zip tags (repeat [])

clockToUTCTime :: ClockTime -> UTCTime
clockToUTCTime = readTime l "%Y%m%d%H%M%S" 
                 . formatCalendarTime l "%Y%m%d%H%M%S" . toUTCTime
                     where l = defaultTimeLocale

getEntryLink :: FilePath -> Blogination (URL,String,UTCTime,ClockTime)
getEntryLink path = do
  blog@Blog{..} <- lift get
  liftIO $ do
    contents <- readFile (blogEntries</>path)
    modified <- getModificationTime (blogEntries</>path)
    return (blogRoot++blogHtml++"/"++path++".html"
           ,getTitle $ read $ contents
           ,fromMaybe undefined $ makeDate path
           ,modified)
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
  tags <- entryTags alltags path
  liftIO $ do
    contents <- readFile (blogEntries</>path)
    writeFile (blogHtml</>path++".html") $ 
       showHtml $ pageToHtml blog path tags contents
    where match = map fst . filter (any (== path) . snd)

entryTags :: [FilePath] -> FilePath -> Blogination [FilePath]
entryTags tags path = do
  tagEntries <- mapM getTagEntryNames tags
  return $ map fst . filter (any (==path) . snd) $ zip tags tagEntries

getEntryNames :: Blogination [FilePath]
getEntryNames = do
  Blog{..} <- lift get
  fileClean `fmap` liftIO (getDirectoryContents blogEntries) 

getTags :: Blogination [FilePath]
getTags = do 
  Blog{..} <- lift get
  (fileClean . filterPlain) `fmap` liftIO (getDirectoryContents blogTags)

filterPlain = filter (all (flip any [isLetter,isSpace,isDigit] . flip ($)))

fileClean = dateSort . filter (not . all (=='.'))

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
template blog@Blog{..} path tags (title,html) =
    htmlTemplate blog theheader (back +++ hr) (hr +++ back) thecontent "post" where
    theheader = thetitle << title
    thecontent = tagndate +++ html
    back = backlink blogRoot blogName
    tagndate = p ! [theclass "tagndate"] << (small << (date +++ tag))
    date = "Date: " +++ (maybe noHtml showTime $ makeDate path)
    tag = list noHtml ((", Tags: " +++) . mconcat . intersperse (toHtml ", ")) taglinks
    taglinks = map (mkTagLink blog) $ zip tags (repeat [])
    showTime = toHtml . formatTime defaultTimeLocale blogDate

analytics :: Blogination Html
analytics = do
  Blog{..} <- lift get
  return $ analyticsScript blogAnalytics

analyticsScript :: Maybe String -> Html
analyticsScript = maybe noHtml script where
    script blogAnalytics = primHtml $
     "<script type=\"text/javascript\">\
    \var gaJsHost = ((\"https:\" == document.location.protocol) ? \
    \ \"https://ssl.\" : \"http://www.\"); \
    \ document.write(unescape(\"%3Cscript src='\" + gaJsHost + \
    \ \"google-analytics.com/ga.js' \
    \ type='text/javascript'%3E%3C/script%3E\")); </script> \
    \ <script type=\"text/javascript\"> try { \
    \ var pageTracker = _gat._getTracker(\"" ++ blogAnalytics ++ "\"); \
    \ pageTracker._trackPageview(); } catch(err) {}</script>"

mkTagLink :: Blog -> (FilePath,[FilePath]) -> Html
mkTagLink Blog{..} (tag,entries) = 
    toHtml $ tagLink ! [theclass "taglink"] where
    tagLink = hotlink (blogRoot++"tags/"++tag++".html") << tag +++
                      if null entries
                         then noHtml
                         else toHtml $ " (" ++ show (length entries) ++ ")"

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
highlightOpts  ('$':rest) = langAndCode $ break (=='$') $ rest where
    langAndCode (lang,'$':code) = Just (lang,dropWhile isSpace code)
    langAndCode _ = Nothing
highlightOpts _ = Nothing

highlightWith :: String -> String -> Block
highlightWith lang code = RawHtml $ showHtmlFragment html where
    html = either (const def) format highlight
    format = formatAsXHtml [] lang
    highlight = highlightAs lang code
    def = pre << code

htmlTemplate :: Blog -> Html -> Html -> Html -> Html -> String -> Html
htmlTemplate blog@Blog{..} head bhead bfoot bcont bcclass = toHtml [theheader,thebody] where
    theheader = header << [toHtml $ map style $ blogCSS
                          ,encoding
                          ,head]
    thebody = body << [thebodyheader, thebodycontent, thebodyfooter, anal]
    thebodyheader  = thediv ! [theclass "header"] << bhead
    thebodycontent = thediv ! [theclass bcclass] << bcont
    thebodyfooter  = thediv ! [theclass "footer"] << bfoot
    anal = analyticsScript blogAnalytics
    style css = thelink ! [rel "stylesheet",thetype "text/css"
                          ,href (blogRoot++css)] << noHtml

rss path = thelink ! [rel "alternate",thetype "application/rss+xml"
                     ,href path] << noHtml

backlink path name = toHtml $ p ! [theclass "backto"]
                              << hotlink path << ("« Back to " ++ name)

encoding = meta ! [httpequiv "Content-Type"
                  ,content "text/html; charset=utf-8"]

dateSort :: [String] -> [String]
dateSort = sortBy (flip $ comparing makeDate)
