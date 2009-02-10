{-# LANGUAGE FlexibleInstances #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Prelude hiding (readFile,writeFile)
import System
import System.IO.UTF8 (readFile,writeFile)
import Text.Blogination
import qualified Data.ConfigFile as C

main = do
  args <- getArgs
  case args of
    (conf:k) -> do 
       conf <- getConf conf
       case conf of
         Left err -> error $ show err
         Right blog -> runBloginator buildBlog blog { blogForce = not $ null k }
    _ -> getConf "blog.conf" >>= either (error . show) (runBloginator buildBlog)

instance Monad m => Applicative (ErrorT C.CPError m) where
    pure = return; (<*>) = ap

getConf :: FilePath -> IO (Either (C.CPErrorData,String) Blog)
getConf filePath = runErrorT $ do
  contents <- liftIO $ readFile filePath
  config <- C.readstring C.emptyCP contents
  let get = C.get config "BLOG"
  Blog <$> get "name" <*> get "root"
       <*> (read <$> get "css") <*> get "entries" <*> get "html"
       <*> get "author"
       <*> (return False)
       <*> get "date"