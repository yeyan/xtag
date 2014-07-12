{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Control.Monad.Reader
import           Data.Aeson                           hiding (json)
import           Data.String
import qualified Data.Text.Lazy                       as T
import           Text.Blaze.Html.Renderer.Utf8        (renderHtml)
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import           Text.Read                            (readMaybe)

import           Database.Persist.Sqlite              hiding (get)

import           Model.Book
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty.Trans

type DBScottyM = ScottyT T.Text (ReaderT ConnectionPool IO)
type DBActionM = ActionT T.Text (ReaderT ConnectionPool IO)

type Port = Int

runScotty :: Port -> ConnectionPool -> DBScottyM () -> IO ()
runScotty port pool = scottyT port initM runM
    where
        runM :: ReaderT ConnectionPool IO a -> IO a
        runM m = runReaderT m pool

        initM m = runM $ do
            pool <- ask
            liftIO $ liftIO $ runResourceT $ runNoLoggingT $ flip runSqlPool pool $ do
                initDB
                scanDirectory "data"
            m

runDB m = do
    pool <- lift ask
    let  runDB' m = liftIO $ runResourceT $ runNoLoggingT $ runSqlPool m pool
    runDB' m

app :: DBScottyM ()
app = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "public")
    get "/" $ do
        redirect "/index.html"

    get "/api/list/:page" $ do
        index <- parse "page"
        bookList <- runDB $ listBooks 16 index
        json $ bookList

    get "/api/book/:book" $ do
        bookId <- parse "book"
        book <- runDB $ getBook bookId
        case book of
            Nothing -> raise "404"
            Just vl -> json vl

    get "/api/book/:book/:page" $ do
        book <- parse "book"
        page <- parse "page"
        info <- runDB $ getPage book page
        case info of
            Just entity -> file $ pagePath $ entityVal entity
            Nothing -> raise "404"
    where
        parse name = do
            value <- liftM readMaybe $ param name
            case value of
                Nothing -> raise $ T.concat ["Can't parse param ", name]
                Just vl -> return vl

main = do
    withSqlitePool "xtag.db3" 10 $ \pool -> do
        runScotty 3000 pool app
