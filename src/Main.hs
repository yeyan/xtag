{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Control.Applicative
import           Data.Aeson                           hiding (json)
import           Data.String

import qualified Data.ByteString.Char8                as C
import qualified Data.Text.Encoding                   as E
import qualified Data.Text.Lazy                       as T
import           Text.Read                            (readMaybe)

import           Control.Lens
import           Data.Maybe
import           System.Environment.FindBin
import           XTag.Config

import           Control.Monad.Reader
import           XTag.Model.Book

import           Network.HTTP.Types
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty.Trans

data AppData = AppData
    { _appConfig  :: Config
    , _connection :: Connection
    }

makeLenses ''AppData

type XTagScottyM = ScottyT T.Text (ReaderT AppData IO)
type XTagActionM = ActionT T.Text (ReaderT AppData IO)

runScotty :: AppData -> XTagScottyM () -> IO ()
runScotty appData = do
    scottyT (appData ^. appConfig . configPort) initApp runM
    where
        runM :: ReaderT AppData IO a -> IO a
        runM m = runReaderT m appData

        initApp m = runM $ do
            appData <- ask
            let conn  = appData ^. connection
                repos = appData ^. appConfig . configRepos
            result <- liftIO $ runRedis conn $ mapM doScan repos
            liftIO $ print result
            m

        doScan path = do
            liftIO $ putStrLn $ "Scanning repository " ++ path
            scanBooks path

runDB :: Redis a -> XTagActionM a
runDB m = do
    app  <- lift ask
    rslt <- liftIO $ runRedis (app ^. connection) m
    case rslt of
        Left _ -> raise "backend error"
        Right value -> return value

loadConfig file = do
    result <- readConfig file
    let errMsg = result ^. _1 . _Left
    when (not $ null errMsg) $
        putStrLn $ "Warning: " ++ errMsg
    return $ result ^. _2

main = do
    appData <- AppData <$> loadConfig "../xtag.config" <*> connect defaultConnectInfo
    runScotty appData app

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
