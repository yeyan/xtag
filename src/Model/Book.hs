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

import qualified Data.ByteString.Char8  as CC
import qualified Data.ByteString.UTF8   as C
import qualified Data.Pool              as P
import           Database.Redis.Simple

import           Model.Type
import           Model.Util

runTest action = do
    conn <- connect defaultConnectInfo
    r <- runRedis conn action
    print r

testScanBook =
    runTest $ do
        liftIO $ print "A"
        scanBooks "../data"
        liftIO $ print "B"
        books <- getBookIndex 0 10
        liftIO $ print "C"

newId name = do
    uuid' <- C.fromString <$> show <$> uuid
    return $ CC.concat [name , ":", uuid']

bookIndex = "book:index"
bookPath  = "book:path"

toAttrId id name =
    CC.concat [id, ":", name]

setAttr id name value =
    set (toAttrId id name) value

getAttr id name =
    get $ toAttrId id name

createBook (name, path, pages) = do
    eId  <- hGet bookPath path
    if isJust eId
        then return eId
        else do
            nId <- liftIO $ newId "book"
            Just <$> insert nId name path pages
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

scanBooks path = do
    books <- findBooks path
    mapM_ createBook books

getBook bookId = do
    name <- getAttr bookId "name"
    pageCount <- lLen $ toAttrId bookId "pages"
    return $ Book <$> Just bookId <*> name <*> Just pageCount

getBookIndex limit page = do
    let start = limit * (page - 1)
        stop  = limit * page - 1
    indexes <- lRange bookIndex start stop
    total   <- lLen bookIndex
    books   <- mapM getBook indexes
    let total' = ceiling $ fromIntegral total / fromIntegral limit
        books' = catMaybes books
    return $ BookIndex page total' books'

getPage bookId pageId =
    lIndex (toAttrId bookId "pages") $ pageId - 1

getThumbnail bookId pageNumber = do
    eThumb <- hGet thumbId pageId --getAttr bookId "thumbnail"
    if isJust eThumb
        then return eThumb
        else newThumbnail
    where
        newThumbnail = do
            page <- getPage bookId pageNumber
            case page of
                Nothing -> return Nothing
                Just vl -> do
                    thumb <- liftIO $ createThumbnail "../cache" vl 200 200
                    --setAttr bookId "thumbnail" thumb
                    hSet thumbId pageId thumb
                    return $ Just thumb
        thumbId = toAttrId bookId "thumbnail"
        pageId  = C.fromString $ show pageNumber
