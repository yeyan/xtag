{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Model.Book(
    module Model.Type,
    module Control.Monad.IO.Class,
    module Control.Monad.Logger,
    module Control.Monad.Trans.Resource,
    module Database.Persist.Sqlite,
    initDB,
    scanDirectory,
    listBooks,
    getBook,
    getThumbnail,
    getPage)  where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import qualified Data.Text                    as T
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite      (runMigrationSilent, runSqlConn,
                                               withSqliteConn)
import           Model.Type
import           System.Cmd
import           System.Directory

import           Data.UUID
import           System.UUID.V4

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

createBook dir = do
    bookId <- getBy $ UniqueBookPath dir
    case bookId of
        Just vl -> return $ entityKey vl
        Nothing -> do
            pagePaths <- liftIO $ listDirectory dir
            let name = reverse . takeWhile (\x -> x /= '/') . reverse $ dir
            bookId <- insert $ Book name dir $ length pagePaths
            forM_ (zip [1..] pagePaths) $ \(index, path) -> do
                insert $ Page index path bookId
            return bookId

scanDirectory dir = do
    path <- liftIO $ canonicalizePath dir
    dirs <- liftIO $ findLeafDirectories path
    mapM_ createBook dirs
    deleteInvalidBooks path dirs

deleteInvalidBooks dir bookDirs = do
    rawExecute initTable []
    --rawExecute emptyTable []
    forM_ bookDirs $ \dir -> do
        let value = PersistText $ T.pack dir
        rawExecute "insert into directory values (null, ?)" [value]
    let like = PersistText $ T.pack $ dir ++ "%"
    bookIds <- rawQuery selectInvalidBook [like] $= CL.map (Key . head) $$ CL.consume
    forM_ bookIds $ \bookId -> do
        delete bookId
        deleteWhere [PageBookId ==. bookId]
    where
        initTable =
            "create temp table if not exists directory (id integer auto increment, path text not null); delete from directory;"
        --emptyTable =
        --    "delete from directory"
        selectInvalidBook =
            "select id from book where path like ? and path not in (select path from directory)"

listBooks limit index = do
    total <- count [BookPageCount >=. 0]
    books <- selectList [] [LimitTo limit, OffsetBy $ (index - 1) * limit]
    let maxPage = (ceiling $ fromIntegral total/fromIntegral limit)
    return $ BookList index maxPage books

toBookId :: Integer -> BookId
toBookId = Key . PersistInt64 . fromIntegral

getBook bookId = do
    let key = toBookId bookId
    book <- get key
    return $ Entity key <$> book

getPage bookId index = do
    page <- selectFirst [PageBookId ==. (toBookId bookId), PageIndex ==. index] []
    return $ pagePath <$> entityVal <$> page

createThumbnail cacheDir pagePath = do
    thumbnailPath <- (\x y -> x ++ "/" ++ y ++ ".jpg")
        <$> canonicalizePath cacheDir <*> liftA show uuid
    rawSystem "convert" [pagePath, "-thumbnail", "200x200", thumbnailPath]
    return thumbnailPath

genThumbnail cacheDir bookId index = do
    path <- getPage bookId index
    case path of
        Nothing -> return Nothing
        Just pt -> do
            path <- liftIO $ createThumbnail cacheDir pt
            return $ Just path

getThumbnail cacheDir bookId index = do
    thumb <- selectFirst [ThumbnailBookId ==. (toBookId bookId),
        ThumbnailIndex ==. index] []
    path <- genThumbnail cacheDir bookId index
    return $ (thumbnailPath <$> entityVal <$> thumb) <|> path

initDB = do
    runMigrationSilent migrateAll
    --rawExecute "delete from thumbnail" []

runTest query = do
    withSqliteConn "testXTag.db3" $ \conn -> do
        runResourceT $ runStdoutLoggingT $ runSqlConn query conn

testScanDir =
    runTest $ do
        initDB
        scanDirectory "data"
