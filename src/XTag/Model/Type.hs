{-# LANGUAGE OverloadedStrings #-}

module XTag.Model.Type where

import           Data.Aeson
import qualified Data.ByteString.UTF8 as C

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
        toJSON $ C.toString str

instance ToJSON Book where
    toJSON (Book id name total) =
        object ["id" .= id, "name" .= name, "total" .= total]

instance ToJSON BookIndex where
    toJSON (BookIndex index total books) =
        object ["index" .= index, "total" .= total, "items" .= books]

