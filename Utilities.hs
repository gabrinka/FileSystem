{-# LANGUAGE OverloadedStrings #-}
module Utilities where
    import Prelude hiding (FilePath)
    import MyTypes
    import Data.List
    import Data.String
    import System.IO hiding (FilePath)

    split     :: (Char -> Bool) -> String -> [String]
    split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

    getParent :: FilePath -> FilePath
    getParent "/" = "/"    
    getParent path = let parent = reverse $ tail $  dropWhile (/='/') $ reverse path
     in if parent == "" then "/" else parent

    isValidDir :: FilePath -> FilePath -> FileSystem -> Bool
    isValidDir path currPath fs = let directory = findDir path currPath fs -- path is where we want to go
     in if directory == Nothing then False else True

    findDir :: FilePath -> FilePath -> FileSystem -> Maybe FSItem
    findDir path currPath fst = let pathList = getAbsPath path currPath 
                                    fs = [systemRoot fst]
     in comparePaths ( ["/"] ++ (split (=='/') pathList)) fs

    getAbsPath :: FilePath -> FilePath -> FilePath
    getAbsPath fp currp 
     | ( isPrefixOf "/" fp ) = fp 
     | currp == "/" = "/" ++ fp
     | otherwise = currp ++ ['/'] ++ fp

    comparePaths :: [FilePath] -> [FSItem] -> Maybe FSItem
    comparePaths _ [] = Nothing
    comparePaths _ [File _ _ _] = Nothing
    comparePaths [dirName] [dir@(Directory name _)]
     | dirName == name = Just dir
     | otherwise       = Nothing
    comparePaths [dirName] (dir@(Directory name _):fs)
     | dirName == name = Just dir
     | otherwise       = comparePaths [dirName] fs
    comparePaths (dirName:dirs) ((Directory name children):fs)
     | dirName == name = comparePaths dirs children
     | otherwise       = comparePaths (dirName:dirs) fs
    comparePaths dirs (_:fs)    = comparePaths dirs fs

    showContent :: FilePath -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
    showContent filePath currPath fs = 
     case findDir filePath currPath fs of
        Just dir -> let (Directory _ kids) = dir in showContent' kids >> return (currPath, fs)
        Nothing  -> do putStrLn $ "ls: cannot open directory '.': Permission denied"  
                       return (currPath, fs)

    showContent' :: [FSItem] -> IO ()
    showContent' [] =  return ()
    showContent' [Directory name _]       = putStrLn $ name
    showContent' [File name _ size]      = putStrLn name
    showContent' ((File name _ size):fs) = do
     putStr $ name ++ " "
     showContent' fs
    showContent' ((Directory name _):fs) = do
     putStr $ name ++ " "
     showContent' fs

    readFromConsole :: IO ()
    readFromConsole = do 
        input <- getLine
        if input == "." then return () 
        else do
            putStrLn input
            readFromConsole

    readFileData :: [FilePath] -> [FSItem] -> IO FileContent
    readFileData [arg] []    = do
     putStr $  "cat: " ++ arg ++ ": cannot find a directory or file with this name!"
     return ""
    readFileData [arg] ((File name text _):fs)
     | arg == name = return text
     | otherwise   = readFileData [arg] fs
    readFileData [arg] ((Directory _ kids):fs) = readFileData [arg] fs
    readFileData (arg:args) ((Directory name kids):fs)
     | arg == name = readFileData args kids
     | otherwise   = readFileData (arg:args) fs
    readFileData args (_:fs) = readFileData args fs     

    createFile :: [FilePath] -> [FSItem] -> [FSItem]
    createFile args fs = createFile' args "" fs

    getSize :: FileContent -> FileSize
    getSize content = length content

    createFile' :: [FilePath] -> FileContent-> [FSItem] -> [FSItem]
    createFile' [arg] content []                         = [File arg content $ getSize content]
    createFile' [arg] content [dir@(Directory _ _)]      = dir : [File arg content $ getSize content]
    createFile' [arg] content (dir@(Directory _ _):fs)   = dir : (createFile' [arg] content fs)
    createFile' [arg] content [file@(File name _ _)]
     | arg == name = [File arg content $ getSize content]
     | otherwise   = file : [File arg content $ getSize content]
    createFile' [arg] content (file@(File name _ _):fs)
     | arg == name = [File name content $ getSize content] ++ fs
     | otherwise   = file : (createFile' [arg] content fs)
    createFile' (arg:args) content (file@(File name _ _):fs) = file : (createFile' (arg:args) content fs)
    createFile' (arg:args) content ((Directory name lst):fs)
     | arg == name = [Directory name $ createFile' args content lst] ++ fs
     | otherwise   = [Directory name lst] ++ (createFile' (arg:args) content fs)


    concatAll :: [FilePath] -> FilePath -> FilePath -> FileSystem -> IO FileSystem
    concatAll [] _ _ fs = return fs
    concatAll toRead toWrite currPath fs = do
    text <- readAll toRead currPath [systemRoot fs]
    let path = getAbsPath toWrite currPath
    let newFs = head $ createFile' ( ["/"] ++ (split (=='/') path)) text [systemRoot fs]
    return $ FileSystem newFs
    where
        readAll [] _ _                 = return ""
        readAll (x:xs) currPath fs = do
            let path'= getAbsPath x currPath
            cont <- readFileData ( ["/"] ++ (split (=='/') path')) fs
            allcont <- readAll xs currPath fs
            return $ cont ++ allcont     


    isValidFile :: FilePath -> FilePath -> FileSystem -> Bool
    isValidFile filePath currPath fs = Nothing /= findFile filePath currPath fs  

    findFile :: FilePath -> FilePath -> FileSystem -> Maybe FSItem
    findFile path currPath fst = let pathList = getAbsPath path currPath 
                                     fs = [systemRoot fst]
                                     getFileName = last $ ["/"] ++ (split (=='/') pathList)
                                     getdir = ["/"] ++ split (=='/') (getParent pathList)  
     in case (comparePaths getdir fs) of 
         Just dir -> let (Directory _ kids) = dir in
             compareFiles kids getFileName
         Nothing  -> Nothing


    compareFiles :: [FSItem] -> FileContent -> Maybe FSItem
    compareFiles [] _ =  Nothing
    compareFiles _ "" = Nothing
    compareFiles ((Directory _ _ ):xs) name = compareFiles xs name
    compareFiles ((file@(File fname _ _)):xs) name
     | name == fname = Just file
     | otherwise = compareFiles xs name

    
     
    
    