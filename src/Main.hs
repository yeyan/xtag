{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Data.Aeson                           hiding (json)
import           Data.String

import qualified Data.ByteString.Char8                as C
import qualified Data.Text.Encoding                   as E
import qualified Data.Text.Lazy                       as T
import           Text.Read                            (readMaybe)

import           Control.Monad.Reader
import           Model.Book

import           Network.HTTP.Types
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty.Trans

type XTagScottyM = ScottyT T.Text (ReaderT Connection IO)
type XTagActionM = ActionT T.Text (ReaderT Connection IO)

type Port = Int

runScotty :: Port -> Connection -> XTagScottyM () -> IO ()
runScotty port pool = scottyT port initApp runM
    where
        runM :: ReaderT Connection IO a -> IO a
        runM m = runReaderT m pool

        initApp m = runM $ do
            conn <- ask
            rslt <- liftIO $ runRedis conn $ scanBooks "../data"
            liftIO $ print rslt
            m

runDB :: Redis a -> XTagActionM a
runDB m = do
    pool <- lift ask
    rslt <- liftIO $ runRedis pool m
    case rslt of
        Left _ -> raise "backend error"
        Right value -> return value

app :: XTagScottyM ()
app = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "../public")
    get "/" $ do
        redirect "/index.html"

    get "/api/book/index" $ do
        index <- pageP :: XTagActionM Integer
        bookList <- runDB $ getBookIndex 16 index
        json $ bookList

    get "/api/book/:book" $ do
        bookId <- bookIdP
        book <- runDB $ getBook bookId
        case book of
            Just vl -> json vl
            Nothing -> resp404

    get "/api/book/:book/page/:page/thumb" $ do
        book <- bookIdP
        page <- pageIndexP
        maybePath <- runDB $ getThumbnail book page
        case maybePath of
            Just path -> file $ C.unpack path
            Nothing -> resp404

    get "/api/book/:book/page/:page/content" $ do
        book <- bookIdP
        page <- pageIndexP
        maybePath <- runDB $ getPage book page
        case maybePath of
            Just path -> file $ C.unpack path
            Nothing -> resp404

    where
        parse name = do
            value <- liftM readMaybe $ param name
            case value of
                Nothing -> raise $ T.concat ["Can't parse param ", name]
                Just vl -> return vl
        pageP =
            param "page" `rescue` \_ -> return 1

        bookIdP = param "book"
        pageIndexP = param "page"

        resp404 = do
            status status404
            text "Resource not found"

main = do
    pool <- connect defaultConnectInfo
    runScotty 3000 pool app
