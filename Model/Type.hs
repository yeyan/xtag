{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Model.Type where

import           Data.Aeson
import           Database.Persist
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Book
    name String
    path String
    pageCount Int
    UniqueBookPath path
    deriving Show
Page
    index Int
    path String
    bookId BookId
    UniquePagePath path
    deriving Show
Thumbnail
    index Int
    path String
    bookId BookId
    UniquePreviewPath path
    deriving Show
|]

data BookList = BookList
    { bookListIndex :: Int
    , bookListTotal :: Int
    , bookListBooks :: [Entity Book]
    }
    deriving (Show)

instance ToJSON Book where
    toJSON (Book name _ count) = object
        [ "name" .= name
        , "total" .= count
        ]

instance ToJSON (Entity Book) where
    toJSON (Entity uid (Book name _ count)) = object
        [ "id" .= uid
        , "name" .= name
        , "total" .= count
        ]

instance ToJSON BookList where
    toJSON (BookList index total books) = object
        [ "index" .= index
        , "total" .= total
        , "books" .= toJSON books
        ]
