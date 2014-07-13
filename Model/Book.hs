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
    getPage)  where


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
import           System.Directory

listDirectory d = do
    isDir <- doesDirectoryExist d
    if isDir
        then do
            fs <- getDirectoryContents d
            return $ map toSub $ filter noHidden fs
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
            leafs <- liftM concat $ mapM findLeafDirectories fs
            if null leafs
                then return [d]
                else return leafs
        else
            return []

fileName = reverse . takeWhile (\x -> x /= '/') . reverse

createBook dir = do
    bookId <- getBy $ UniqueBookPath dir
    case bookId of
        Just vl -> return $ entityKey vl
        Nothing -> do
            pagePaths <- liftIO $ listDirectory dir
            bookId <- insert $ Book (fileName dir) dir $ length pagePaths
            forM_ (zip [1..] pagePaths) $ \(index, path) -> do
                insert $ Page index path bookId
            return bookId

--listBooks :: PersistQuery m =>
--    Int -> Int -> m [Entity (BookGeneric (PersistMonadBackend m))]
--listBooks limit page = do
--    selectList [] [LimitTo limit, OffsetBy $ (page - 2) * limit]
--
--countBooks = do
--    count [BookPageCount >=. 0]

listBooks limit index = do
    total <- count [BookPageCount >=. 0]
    books <- selectList [] [LimitTo limit, OffsetBy $ (index - 1) * limit]
    let maxPage = (ceiling $ fromIntegral total/fromIntegral limit)
    return $ BookList index maxPage books

toBookId :: Integral n => n -> BookId
toBookId = Key . PersistInt64 . fromIntegral

getBook bookId = do
    get $ toBookId bookId

getPage bookId index = do
    selectFirst [PageBookId ==. (toBookId bookId), PageIndex ==. index] []

scanDirectory dir = do
    path <- liftIO $ canonicalizePath dir
    dirs <- liftIO $ findLeafDirectories path
    mapM_ createBook dirs
    deleteInvalidBooks path dirs

initDB = runMigrationSilent migrateAll

runTest query = do
    withSqliteConn "xtag.db3" $ \conn -> do
        runResourceT $ runStdoutLoggingT $ runSqlConn query conn

--deleteInvalidBooks :: MonadSqlPersist m => String -> [String] -> m ()
deleteInvalidBooks dir bookDirs = do
    rawExecute createTable []
    rawExecute emptyTable []
    forM_ bookDirs $ \dir -> do
        let value = PersistText $ T.pack dir
        rawExecute "insert into directory values (null, ?)" [value]
    let like = PersistText $ T.pack $ dir ++ "%"
    bookIds <- rawQuery selectInvalidBook [like] $= CL.map (Key . head) $$ CL.consume
    forM_ bookIds $ \bookId -> do
        delete bookId
        deleteWhere [PageBookId ==. bookId]
    where
        createTable =
            "create temp table if not exists directory (id integer auto increment, path text not null)"
        emptyTable =
            "delete from directory"
        selectInvalidBook =
            "select id from book where path like ? and path not in (select path from directory)"

testScanDir =
    runTest $ do
        initDB
        scanDirectory "data"
