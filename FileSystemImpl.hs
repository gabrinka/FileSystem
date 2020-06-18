{-# LANGUAGE OverloadedStrings #-}
module FileSystemImpl where
    import Prelude hiding (FilePath)
    import MyTypes
    import Data.String (String)

    data1 :: FileContent
    data1 = "This is the content for file1."

    
    data2 :: FileContent
    data2 = "This is the content for file2."

    
    data3 :: FileContent
    data3 = "This is the content for file3."

    
    data4 :: FileContent
    data4 = "This is the content for file4."

    
    root :: FSItem
    root = Directory "/" [
        Directory "folder1" [
            Directory "folder11" [] , 
            File "file1" data1 $ length data1
        ] ,
        Directory "folder2" [
            Directory "folder3" [
                Directory "folder4" [
                    File "file2" data2 $ length data2
                ]
            ] ,
            File "file3" data3 $ length data3 ,
            File "file4" data4 $ length data4
  
          ]
     ]  

    fileSystem :: FileSystem
    fileSystem = FileSystem root 

    

   
     


 
        