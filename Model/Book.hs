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
import           Database.Persist
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
    books <- selectList [] [LimitTo limit, OffsetBy $ (index -2) * limit]
    let maxPage = (ceiling $ fromIntegral total/fromIntegral limit) + 1
    return $ BookList index maxPage books

toBookId :: Integral n => n -> BookId
toBookId = Key . PersistInt64 . fromIntegral

getBook bookId = do
    get $ toBookId bookId

getPage bookId index = do
    selectFirst [PageBookId ==. (toBookId bookId), PageIndex ==. index] []

scanDirectory dir = do
    bookDirs <- liftIO $ findLeafDirectories dir
    mapM_ createBook bookDirs

initDB = runMigrationSilent migrateAll
