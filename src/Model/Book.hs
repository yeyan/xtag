{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Model.Book
    ( Book(..)
    , scanBooks
    , getBookIndex
    , getBook
    , getPage
    , getThumbnail
    , defaultConnectInfo
    , connect
    , runRedis
    , Connection
    , Redis
    , Reply(..)
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.IO.Class

import           Data.Either
import           Data.Maybe

import           Data.UUID
import           System.UUID.V4

import qualified Data.ByteString.Char8  as C
import qualified Data.Pool              as P
import           Database.Redis.Simple

import           Data.Aeson

import           System.Directory
import           System.Process

listDirectory d = do
    isDir <- doesDirectoryExist d
    if isDir
        then do
            map toSub <$> filter noHidden <$> getDirectoryContents d
        else
            return []
    where
        noHidden f = head f /= '.'
        toSub f = d ++ "/" ++ f

findLeafDirectories d = do
    isDir <- doesDirectoryExist d
    if isDir
        then do
            fs <- listDirectory d
            leafs <- concat <$> mapM findLeafDirectories fs
            if null leafs
                then return [d]
                else return leafs
        else
            return []

runTest action = do
    conn <- connect defaultConnectInfo
    r <- runRedis conn action
    print r

-- book operations

newId name = do
    uuid' <- C.pack <$> show <$> uuid
    return $ C.concat [name , ":", uuid']

bookIndex = "book:index"
bookPath = "book:path"

toAttrId id name =
    C.concat [id, ":", name]

setAttr id name value =
    set (toAttrId id name) value

getAttr id name =
    get $ toAttrId id name

type BookId = C.ByteString

data Book = Book
    { bookId        :: C.ByteString
    , bookName      :: C.ByteString
    , bookPageCount :: Integer
    }
    deriving Show

data BookIndex = BookIndex
    { index :: Integer
    , total :: Integer
    , books :: [Book]
    }
    deriving (Show)

instance ToJSON C.ByteString where
    toJSON str =
        toJSON $ C.unpack str

instance ToJSON Book where
    toJSON (Book id name total) =
        object ["id" .= id, "name" .= name, "total" .= total]

instance ToJSON BookIndex where
    toJSON (BookIndex index total books) =
        object ["index" .= index, "total" .= total, "books" .= books]

createBook path = do
    (nId, name, path, pages) <- prepare path
    eId <- hGet bookPath path
    -- liftIO $ print $ C.unpack $ "scanning " `C.append` path
    if isJust eId
        then return $ fromJust eId
        else insert nId name path pages
    where
        insert id name path pages = do
            setAttr id "name" name
            setAttr id "path" path
            rPush (toAttrId id "pages") pages
            -- register book to book index
            rPush bookIndex [id]
            -- register path to book path
            hSet bookPath path id
            return id
        prepare p =
            liftIO $ do
                path <- canonicalizePath p
                name <- return $ reverse . takeWhile (\x -> x /= '/') . reverse $ p
                bkid <- newId "book"
                pags <- listDirectory path
                return (bkid, C.pack name,C.pack path, map C.pack pags)

scanBooks path = do
    bookPaths <- liftIO $ findLeafDirectories path
    mapM_ createBook bookPaths

getBook bookId = do
    name <- getAttr bookId "name"
    pageCount <- lLen $ toAttrId bookId "pages"
    return $ Book <$> Just bookId <*> name <*> Just pageCount

getBookIndex limit page = do
    let start = limit * page
        stop  = limit * (page + 1) - 1
    indexes <- lRange bookIndex start stop
    total   <- lLen bookIndex
    books   <- mapM getBook indexes
    let total' = ceiling $ fromIntegral total / fromIntegral page
        books' = catMaybes books
    return $ BookIndex page total' books'

getPage bookId pageId =
    lIndex (toAttrId bookId "pages") $ pageId - 1

testScanBook =
    runTest $ do
        liftIO $ print "A"
        scanBooks "../data"
        liftIO $ print "B"
        books <- getBookIndex 0 10
        liftIO $ print "C"

getThumbnail bookId pageId = do
    eThumb <- getAttr bookId "thumbnail"
    if isJust eThumb
        then return eThumb
        else newThumbnail
    where
        newThumbnail = do
            page <- getPage bookId pageId
            case page of
                Nothing -> return Nothing
                Just vl -> do
                    thumb <- liftIO $ createThumbnail "../cache" vl
                    setAttr bookId "thumbnail" thumb
                    return $ Just thumb

createThumbnail dir path= do
    thumb <- (\x y -> x ++ "/" ++ y ++ ".jpg")
        <$> canonicalizePath (C.unpack dir)<*> liftA show uuid
    --rawSystem "convert" [pagePath, "-thumbnail", "200x200", thumbnailPath]
    (_, _, _, hProc) <- createProcess
        (proc "./convert" [(C.unpack path), "-thumbnail", "200x200", thumb])
    waitForProcess hProc
    return $ C.pack thumb

close conn = void $ runRedis conn quit
