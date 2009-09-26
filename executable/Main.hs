{-# LANGUAGE FlexibleInstances #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Prelude hiding (readFile,writeFile)
import System
import System.IO.UTF8 (readFile,writeFile)
import Text.Blogination
import qualified Data.ConfigFile as C
import Data.List

main = do
  args <- getArgs
  let confFile =
        case delete "--force" args of
          [conf] -> conf
          []     -> "blog.conf"
          _      -> error "Usage: bloginator [--force] [<blog-config>]"
  conf <- getConf confFile
  case conf of
    Left err -> error $ show err
    Right blog -> runBloginator buildBlog blog { blogForce = "--force" `elem` args }

instance Monad m => Applicative (ErrorT C.CPError m) where
    pure = return; (<*>) = ap

instance Monad m => Alternative (ErrorT C.CPError m) where
    empty = mzero; (<|>) = mplus

getConf :: FilePath -> IO (Either (C.CPErrorData,String) Blog)
getConf filePath = runErrorT $ do
  contents <- liftIO $ readFile filePath
  config <- C.readstring C.emptyCP contents
  let get = C.get config "BLOG"
      optional key = Just <$> get key <|> return Nothing
  Blog <$> get "name" 
       <*> get "root"
       <*> (read <$> get "css") 
       <*> get "entries" 
       <*> get "html"
       <*> get "author"
       <*> return False
       <*> get "date"
       <*> return "tags"
       <*> get "url"
       <*> optional "analytics"
       <*> optional "home"
       <*> optional "home-name"
