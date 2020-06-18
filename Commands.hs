{-# LANGUAGE OverloadedStrings #-}
module Commands where
    import Prelude hiding (FilePath)
    import MyTypes
    import Utilities
    


    start :: FilePath -> FileSystem -> IO ()
    start fp fs = do 
           cmd <- getLine
           if cmd == ""
            then start fp fs
            else if cmd == "x"
            then return ()
            else do
             (newfp,newfs) <- splitCommand cmd fp fs --binding the reuslt of an IO action to new filepath and new directory
             --putStrLn "sucess"
             putStrLn newfp
             start newfp newfs

        

    splitCommand :: String -> FilePath -> FileSystem -> IO (FilePath,FileSystem)
    splitCommand cmd fp fst = processCommand (head $ words cmd) (tail $ words cmd) fp fst       

    processCommand :: String -> [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
    processCommand "pwd" _ currPath fs      = pwd currPath fs
    processCommand "cd" args currPath fs    = cd args currPath fs
    processCommand "ls" args currPath fs    = ls args currPath fs
    processCommand "cat" args currPath fs   = cat args currPath fs
    processCommand "rm" args currPath fs    = rm args currPath fs
    processCommand cmd _ currPath fs        = do putStrLn "command not found" 
                                                 return (currPath,fs)

    pwd :: FilePath -> FileSystem -> IO (FilePath, FileSystem)
    pwd currPath fs = do
     putStrLn currPath
     return (currPath, fs)

    cd :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
    cd [] currPath fs = return (currPath, fs)
    cd ["/"] _ fs = return ("/", fs)
    cd [".."] currPath fs = return (getParent currPath, fs)
    cd [arg] currPath fs
      | not $ isValidDir arg currPath fs =do putStrLn "No such file or directory"
                                             return (currPath,fs)
      | otherwise = return (getAbsPath arg currPath, fs)
    cd _ currPath fs = do putStrLn "too many arguments" 
                          return (currPath,fs)

    ls :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
    ls [] currPath fs    = showContent currPath currPath fs
    ls [arg] currPath fs
     | isValidDir arg currPath fs  = showContent arg currPath fs 
     | otherwise                = do putStrLn "No such directory"
                                     return (currPath,fs)
    ls _ currPath fs = do putStrLn "too many arguments"
                          return (currPath,fs)     

    cat :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
    cat [] currPath fs = do readFromConsole
                            return (currPath, fs)
    cat [arg] currPath fs = let pathList = getAbsPath arg currPath in do 
          text <- readFileData ( ["/"] ++ (split (=='/') pathList)) [systemRoot fs]
          putStrLn text
          return (currPath, fs)
    cat (">":args) currPath fs = do
     cont <- getLine
     let path = getAbsPath (head args) currPath
     let rootNewFs = head $ createFile' (["/"] ++ (split (=='/') path)) cont [systemRoot fs]
     let newFs = FileSystem rootNewFs
     return (currPath, newFs)    
    cat args currPath fs = do
     let readFiles = takeWhile (/= ">") args
     let writeFile = dropWhile (/= ">") args !! 1
     newFs <- concatAll readFiles writeFile currPath fs
     return (currPath, newFs)                                     

    rm :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
    rm [] currPath fs   = do putStrLn "rm: error"
                             return ( currPath , fs)
    rm args currPath fs = do
     newFs <- removeFiles args currPath fs
     return (currPath, newFs)

    removeFiles :: [FilePath] -> FilePath -> FileSystem -> IO FileSystem
    removeFiles [] _ fs = return fs
    removeFiles (arg:args) currPath fs
     | (isValidFile arg currPath fs) == False      = do  
        putStrLn $  "rm: failed to remove " ++ arg ++ ": No such file or directory"
        removeFiles args currPath fs
     | otherwise = do 
       let path = getAbsPath arg currPath
       let newFs = FileSystem $ head $ removeFile (["/"] ++ (split (=='/') path)) [systemRoot fs]
       removeFiles args currPath newFs

    removeFile :: [FilePath] -> [FSItem] -> [FSItem]
    removeFile _ []                       = []
    removeFile [arg] [dir@(Directory _ _)] = [dir]
    removeFile (arg:args) (dir@(Directory name lst):fs)
     | arg == name = [Directory name $ removeFile args lst] ++ fs
     | otherwise   = dir : (removeFile (arg:args) fs)
    removeFile (arg:args) (file@(File name _ _):fs)
     | arg == name && args == [] = fs
     | otherwise                 = file : (removeFile (arg:args) fs) 