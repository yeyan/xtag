module Model.Util
    ( listDirectory
    , findLeafDirectories
    , createThumbnail
    , isImage
    , extension
    , module System.Directory
    ) where

import           Control.Applicative

import           System.Directory
import           System.Process

import qualified Data.ByteString.UTF8 as C
import           Data.Char
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

createThumbnail dir path width height = do
    thumb <- (\x y -> x ++ "/" ++ y ++ ".jpg")
        <$> canonicalizePath (C.toString dir)<*> liftA show uuid
    let size = concat [show width, "x", show height]
    (_, _, _, hProc) <- createProcess
        (proc "./convert" [(C.toString path), "-thumbnail", size, thumb])
    waitForProcess hProc
    return $ C.fromString thumb

isImage p = extension p `elem` ["jpg", "jpeg", "png"]

extension = map toLower . reverse . takeWhile (\x -> x /= '.') . reverse
