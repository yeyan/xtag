module Model.Util
    ( createThumbnail
    , findBooks
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Maybe
import           System.Directory

import qualified Data.ByteString.UTF8   as C
import           Data.UUID
import           System.Process
import           System.UUID.V4

findLeafDirectories dir = do
    isDir <- liftIO $ doesDirectoryExist dir
    if isDir
        then do
            contents <- liftIO $ transform <$> getDirectoryContents dir
            leafs <- concat <$> mapM findLeafDirectories contents
            if null leafs
                then return [(dir, contents)]
                else return leafs
        else
            return []
    where
        transform = map subpath . filter noDot
        noDot x = head x /= '.'
        subpath x = concat [dir, "/", x]

isImage p =
    return $ extension p `elem` ["jpg", "png", "jpeg"]
    where
        extension =
            map toLower . reverse . takeWhile (\x -> x /= '.') . reverse

toBook (path, pages) = do
    pages <- filterM isImage pages
    if null pages
        then return Nothing
        else return $ Just
            (C.fromString path, map C.fromString pages)

findBooks dir = do
    dir <- liftIO $ canonicalizePath dir
    catMaybes <$> (findLeafDirectories dir >>= mapM toBook)

createThumbnail dir path width height = do
    thumb <- (\x y -> x ++ "/" ++ y ++ ".jpg")
        <$> canonicalizePath (C.toString dir)<*> liftA show uuid
    let size = concat [show width, "x", show height]
    (_, _, _, hProc) <- createProcess
        (proc "./convert" [(C.toString path), "-thumbnail", size, thumb])
    waitForProcess hProc
    return $ C.fromString thumb
