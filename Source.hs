{-# LANGUAGE OverloadedStrings #-}

module Main where
    
import FileSystemImpl
import Data.String  
import Commands 

main :: IO () --start program by typing main in the terminal
main = start "/" fileSystem