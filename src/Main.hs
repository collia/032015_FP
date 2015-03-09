{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Options
import Data.List
import Data.List.Split
import System.IO
import Data.Char
import Control.Exception
import System.IO.Error

data MainOptions = MainOptions
     { optDatabase  :: Maybe String,
       optAddFiles  :: Maybe String,
       optFrequency :: Maybe String,
       optStates    :: Maybe String
     }

instance Options MainOptions where
         defineOptions = pure MainOptions
              <*> simpleOption "output" Nothing
                   "Fife to add new bugs"
              <*> simpleOption "add" Nothing
                  "File or files (in quotes) with a bug info. Example --add \"1.txt 2.txt\""
              <*> simpleOption "freq" Nothing
                  "File with frequency info"
              <*> simpleOption "state" Nothing
                  "File with state info"


data TableRow = TableRow {rowName :: String, rowData :: [String]} deriving (Show)
data Table = Table { columnNames :: [String], rows :: [TableRow]} deriving (Show)

data TableRowInt = TableRowInt {rowNameInt :: String, rowDataInt :: [Int]} deriving (Show)
data TableInt = TableInt { columnNamesInt :: [String], rowsInt :: [TableRowInt]} deriving (Show)
     
type  InfoTable = [(String, Int)]
type  TableIntData = [[Int]]      

trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

     
parseCSV :: String -> Table
parseCSV "" = Table { columnNames = [], rows = []}
parseCSV dt = let lns = lines dt in
                   Table {
                         columnNames = tail (splitOn ";" (head lns)),
                         rows = map (\line -> let splitedLine=  splitOn ";" line in
                                              TableRow {rowName = (head splitedLine), rowData = (tail splitedLine)})
                                    (tail lns)}

generateCSV :: Table-> String
generateCSV table = let arrToCSV :: [String] -> String
                        arrToCSV a = (concat (map (\x -> x ++ ";") (init a))) ++ (last a)
                        firstLine = ";" ++ (arrToCSV (columnNames table))
                        buildRow x = (rowName x) ++";"++ (arrToCSV (rowData x)) in
                    unlines $ [firstLine] ++ (map buildRow (rows table))


parseDatLine :: String -> [TableRow]
parseDatLine line = let value = head (splitOn ":" line)
                        rowNames = (map trim (splitOn "," (last (splitOn ":" line)))) in
                    map (\name -> TableRow {rowName = name, rowData=[value]}) rowNames
            
parseDatFile :: String -> Table
parseDatFile dt =  let lns = lines dt
                       colName = trim $ head lns
                       -- delete second, empty line
                       content = tail $ tail lns in
                   Table {columnNames = [colName],
                          rows = concat (map parseDatLine content)}

parseSpFile :: String -> InfoTable
parseSpFile dt = let lns = lines dt in
                    map (\ ln -> let number = read $ takeWhile (/=' ') ln
                                     text = trim $ dropWhile (/=' ') ln in
                                 (text, number)) lns
            
loadDatabase :: String -> IO (Table)
loadDatabase filename = 
              catchIOError (do  
                              handle <- openFile filename ReadMode  
                              contents <- hGetContents handle
                              let value = parseCSV contents
                              putStrLn $ "Loaded " ++ show (length (columnNames value)) ++ " titles from database"
                              hClose handle
                              return value)
                            (\ _ -> do putStrLn ("Cannot open file " ++ filename ++ ". Creating new")
                                       return Table { columnNames = [], rows = []} )

storeDatabase :: String -> Table -> IO()
storeDatabase filename table = do
              writeFile filename $ generateCSV table


loadDatFile :: String -> IO (Table)
loadDatFile filename = do  
             contents <- readFile filename
             return $ parseDatFile contents

loadSpFile :: String -> IO (InfoTable)
loadSpFile filename = do
                      contents <- readFile filename
                      return $ parseSpFile contents

concatRows :: [TableRow] -> [TableRow] -> [TableRow]
concatRows [] r2 = r2
concatRows r1 [] = r1
concatRows r1 r2 = let findValue x rows = case (find (\ri -> (rowName ri) == x) rows) of
                                          Nothing -> ""
                                          Just x -> head (rowData x)
                       newElements = deleteFirstsBy (\ rr1 rr2 -> ((rowName rr1) == (rowName rr2))) r2 r1
                       newElementsPaddingLength = length (rowData (head r1)) in
                   map (\rx -> TableRow { rowName = (rowName rx), rowData= ((rowData rx)++[(findValue (rowName rx) r2)])}) r1  ++
                   map (\rx -> TableRow { rowName = (rowName rx), rowData= (take newElementsPaddingLength (repeat "")) ++ (rowData rx)}) newElements

deleteColumn :: Table -> String -> Table
deleteColumn table name = let delIndex = elemIndex name (columnNames table)
                              delByIndex index array = fst (splitAt index array) ++ (tail (snd (splitAt index array))) in
                          case delIndex of
                               Nothing -> table
                               Just index -> Table {
                                             columnNames = delByIndex index (columnNames table),
                                             rows = map (\ row -> TableRow { rowName = (rowName row),
                                                                             rowData = (delByIndex index (rowData row))}) (rows table)}

deleteColumns :: Table -> [String] -> Table
deleteColumns table [] = table              
deleteColumns table (col:cols) = deleteColumns (deleteColumn table col) cols
             
concat2Tables :: Table -> Table -> Table
concat2Tables t1 t2 = let repeatCol = intersect (columnNames t1) (columnNames t2)
                          updatedTable = (deleteColumns t1 repeatCol)
                          colNames = (columnNames updatedTable) ++ (columnNames t2) in
                    Table { columnNames = colNames, rows = (concatRows (rows updatedTable) (rows t2))}

            
concatTables :: [Table] -> Table
concatTables t = foldl1 concat2Tables t

getIntInfo :: InfoTable -> String -> Int
getIntInfo ftable text = let mayRes = find (\ test -> fst test == text) ftable in
                         case mayRes of
                         Nothing -> 0
                         Just val -> snd val
             
convertDatabase :: Table -> InfoTable -> TableInt
convertDatabase table freq = let convertRow row = TableRowInt { rowNameInt = (rowName row),
                                                                rowDataInt = map (getIntInfo freq) (rowData row)} in
                             TableInt { columnNamesInt = (columnNames table),
                                        rowsInt = map  convertRow (rows table)}
                                 
calculateGenFreq :: TableInt -> InfoTable
calculateGenFreq table = map (\ x -> ( (rowNameInt x), sum (rowDataInt x))) (rowsInt table)

formatGenInfoTable :: InfoTable -> String
formatGenInfoTable table = foldl (\ t x -> t ++ "\n" ++ (fst x) ++ " - " ++ (show (snd x))) "" table

getDatabaseWithState :: TableInt -> InfoTable -> TableInt
getDatabaseWithState table state = let convertRow row = TableRowInt { rowNameInt = (rowNameInt row),
                                                                      rowDataInt = map (\ x -> x * (getIntInfo state (rowNameInt row))) (rowDataInt row)} in
                                   TableInt { columnNamesInt = (columnNamesInt table),
                                              rowsInt = map  convertRow (rowsInt table)}                     

getDatabaseData :: TableInt -> TableIntData
getDatabaseData table = let rowsDt = rowsInt table in
                        map (\ x -> rowDataInt x) rowsDt


calculateGenState :: TableInt -> InfoTable -> InfoTable
calculateGenState table states = let dbWithState = getDatabaseWithState table states
                                     dbData = getDatabaseData dbWithState
                                     transDbData = transpose dbData
                                     sumDbColumns = map sum transDbData in
                                 zip (columnNamesInt table) sumDbColumns


                          
main ::IO()
main = runCommand $ \opts args -> do
          case (optDatabase opts) of
               Nothing -> putStrLn "Error - no database file" >> return()
               Just name -> case (optAddFiles opts) of
                              Just files -> do
                                            database <- (loadDatabase name)
                                            value <- mapM  loadDatFile (words files)
                                            storeDatabase  name $ concatTables ([database] ++ value)
                              Nothing -> case (optFrequency opts) of
                                         Nothing -> putStrLn "Error - no operation" >> return()
                                         Just freqFile -> case (optStates opts) of
                                                          Nothing -> do
                                                                     database <- (loadDatabase name)
                                                                     freqInfo <- loadSpFile freqFile
                                                                     let intDatabase = convertDatabase database freqInfo
                                                                     putStrLn $ formatGenInfoTable $ calculateGenFreq intDatabase
                                                          Just stateFile -> do
                                                                     database <- (loadDatabase name)
                                                                     freqInfo <- loadSpFile freqFile
                                                                     stateInfo <- loadSpFile stateFile
                                                                     let intDatabase = convertDatabase database freqInfo
                                                                     putStrLn $ formatGenInfoTable $ calculateGenState intDatabase stateInfo
                                                                     
