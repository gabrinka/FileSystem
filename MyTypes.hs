
{-# LANGUAGE OverloadedStrings #-}
module MyTypes where

    import Prelude hiding (FilePath)
    import Data.String (String)
    import System.IO hiding (FilePath)

    type FileName = String
    type FileSize = Int
    type FilePath = String
    type FileContent = String

    data FSItem = File FileName FileContent FileSize | Directory FileName [FSItem] deriving (Eq,Show)
    data FileSystem = FileSystem { systemRoot::FSItem }  deriving (Show, Eq)
