{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data
import Init
import Util

import Control.Monad.State
-- import System.Directory.Internal
import Data.Typeable
import Data.Maybe
import System.IO
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Control.Monad.Reader
-- import Data.Text
-- import qualified Data.Text.IO as Text

    -- function f() {

    --     x = 1                   LOAD_VAL 1
    --                             WRITE_VAR â€˜xâ€™

    --     y = 2                   LOAD_VAL 2
    --                             WRITE_VAR â€˜yâ€™

    --     return (x + 1) * y      READ_VAR â€˜xâ€™
    --                             LOAD_VAL 1
    --                             ADD

    --                             READ_VAR â€˜yâ€™
    --                             MULTIPLY

    --                             RETURN_VALUE
-- ex . 2 
-- x = 1, y = 3, z = 5, return ( x * y) + z

-- incorrect assingment - 
-- load_val 1
-- write_var x
-- add               -- only value so far so cant add. 


-- the only combinations/rules allowed is 
-- 1) Load can be on its own - only check if its Int as variable. but later we can put in Maybe or so.. 
-- 1) write need to precede with having a load right before. Also need to check variable is string and also need to store that value.
-- 3) read means that var was written before. So go back and find a read with Variable matching and bring the value forward in.
-- 4) Add -- will add 2 prior Load/Reads. and also keep the value for subsequent - 
-- 5) Multiple is same. 
-- 6) Return -- should be last one. and prior to that should only be ADD or Multiply




transformBytecodeAddIndex :: Int -> [ByteCode Int String] -> [ByteCodeIndexed]
transformBytecodeAddIndex _ [] = []
transformBytecodeAddIndex i [bc] = [(bc,i)]
transformBytecodeAddIndex i (bc:bcs) = (bc,i) : transformBytecodeAddIndex (i+1) bcs

bytecodeIndexList :: MonadIO m => [ByteCode Int String] -> Int -> m [ByteCodeIndexed]
bytecodeIndexList asgList i = return (transformBytecodeAddIndex i asgList)


-- we will now take the BytecodeIndexed and reverse it first --
-- goal is to check the Write - take the variable and create a variable list from it.
-- also will check next one is Load - if not error. 
-- also if we write this to Variable list we will just drop that Write and Load combo. 
-- now if i have to write the Variable list then i need to carry State now into this Function.
-- i dont want to still use Reader since this function will first get rid of the Write and Load combos.


-- writeVar :: (MonadState StateStack m) => StateStack -> String -> Int -> m StateStack 
-- writeVar ss var val = put { ss 


-- ver 2.0
-- checkLoad is called with Index and a copy of whole bytcodeList so we can match index and if it does then 
    -- Return True with Load value if successful
    -- if not return False with value _ does not matter 0. 
checkLoad :: [ByteCodeIndexed] -> Int -> (Bool, Int) 
checkLoad [] indSeek = (False,0)
checkLoad [((Load_val val), ind)] indSeek = if (ind == indSeek) 
                                          then (True, val)
                                          else (False, 0)
checkLoad [ (_ , ind)] indSeek = (False, 0)                                    

checkLoad (((Load_val val), ind) : bcs) indSeek = if (ind == indSeek) 
                                          then (True, val)
                                          else checkLoad bcs indSeek
checkLoad (( _ , ind) : bcs) indSeek = checkLoad bcs indSeek


strAsg :: [String] -> IO (ByteCode Int String)
strAsg [w] 
   | (w == "Add") = return Add
   | (w == "Multiply") = return Multiply
   | (w == "Return_value") = return Return_value
strAsg (w:w2:ws) 
   | (w == "Load_val") = return (Load_val (read w2))
   | (w == "Write_var") = return (Write_var w2)
   | (w == "Read_var") = return (Read_var w2)
   | otherwise = return Zero    -- for now --- later have some error messages

-- strByte :: [String] -> (ByteCode Int String)
-- strByte [w] 
--    | (w == "Add") = Add
--    | (w == "Multiply") = Multiply
--    | (w == "Return_value") = Return_value
-- strByte (w:w2:ws) 
--    | (w == "Load_val") = (Load_val (read w2))
--    | (w == "Write_var") = (Write_var w2)
--    | (w == "Read_var") = (Read_var w2)
--    -- | otherwise = return Zero    -- for now --- later have some error messages




strAsgEx1 = strAsg (words "Load_val 23")
strAsgEx2 = strAsg (words "Read_var X")


-- ioStrAsg = getLine >>= \x ->  
-- ver 1.5 - not using
readDataFrom fileHandle = 
    do 
        isFileEnd <- hIsEOF fileHandle
        if isFileEnd 
            then
                return ("")
            else
                do
                    info <- hGetLine  fileHandle
                    putStrLn info 
                    strAsg (words info)
                    readDataFrom fileHandle


-- lsIO :: [Text.Text] -> IO [String]
-- lsIO lsStr = return lsStr

-- unPack :: [Text.Text] -> [String]
-- unpack [] = []
-- unPack [t] = [Text.unpack t]
-- unPack (t : ts) = (Text.unpack t) : unPack ts

-- wordsStringAsg :: [String] -> [ByteCode Int String]
-- wordsStringAsg [] = []
-- wordsStringAsg [s] = [strByte (words s)]
-- wordsStringAsg (str: strs) = (strByte (words str)) : wordsStringAsg strs





-- ver 2.0
handleLoad :: (MonadIO m, MonadState StateStack m) => [ByteCodeIndexed] -> m ()
handleLoad [] = return ()

handleLoad [((Load_val v),i)] = do 
                  ss <- get
                  put ( ss { -- errorMessage = ("In Others1 Write" : errorMessage ss), 
                             loadIndex = (i,v): loadIndex ss})
                  s2 <- get
                  return ()

handleLoad [bc] = do 
                  return ()


handleLoad (((Load_val v),i) : bcs ) = do 
                  ss <- get
                  put ( ss { -- errorMessage = ("In Others1 Write" : errorMessage ss), 
                             loadIndex = (i,v) : loadIndex ss})
                  s2 <- get
                  handleLoad bcs
                  return ()
handleLoad (bc : bcs ) = do 
                --   ss <- get
                --   -- return $ putStrLn " in PutStrLn BLANK write list "
                --   -- put ( ss { errorMessage = ("Not Write" : errorMessage ss)})
                --   s2 <- get
                  handleLoad bcs
                  return ()

-- ver 2.0
-- First we put update state writeIndex with all the writes we have
handleWrite :: (MonadIO m, MonadState StateStack m) => [ByteCodeIndexed] -> m ()
handleWrite [] = return ()
handleWrite (((Write_var s),i) : bcs ) = do 
                  ss <- get
                  --return $ putStrLn " in PutStrLn BLANK write list "
                  put ( ss { -- errorMessage = ("In Others1 Write" : errorMessage ss), 
                             writesIndex = i: writesIndex ss})
                  s2 <- get
                  handleWrite bcs
                  return ()
handleWrite (bc : bcs ) = do 
                  ss <- get
                  -- return $ putStrLn " in PutStrLn BLANK write list "
                  -- put ( ss { errorMessage = ("Not Write" : errorMessage ss)})
                  s2 <- get
                  handleWrite bcs
                  return ()



-- ver 2.0
handleVariables :: (MonadIO m, MonadState StateStack m) => [ByteCodeIndexed] -> m ()
handleVariables [] = return ()
handleVariables (((Write_var s),i) : bcs ) = do 
                  ss <- get
                  let loadList = loadIndex ss
                  let (bl1, val, indL) = getVar loadList i
                  let delLIndex = deleteLoad loadList indL
                  if (bl1 == True)
                  then  
                      put ( ss { 
                                   variables = (s, val) : variables ss, 
                                   loadIndex = delLIndex        -- here overwrite updated LoadIndex
                                } )     
                      
                      
                  else
                      put ( ss { errorMessage = "Write does not have Load " : errorMessage ss } )
                  s2 <- get
                  handleVariables bcs
                  return ()
handleVariables (bc : bcs ) = do 
                  handleVariables bcs
                  return ()

getVar :: [(Int,Int)] -> Int -> (Bool, Int, Int)
getVar [] indW = (False, 0, 0)
getVar [(indL, valL)] indW = if (indL == (indW -1))
                             then (True, valL,indL) 
                             else (False, 0, 0)
getVar ((indL, valL) : tupL ) indW = if (indL == (indW -1))
                                then (True, valL, indL) 
                                else getVar tupL indW


deleteLoad :: [(Int,Int)] -> Int -> [(Int,Int)]
deleteLoad [] indD = []
deleteLoad [(indL, valL)] indD = if (indL == indD)
                             then []                  -- remove this as its writeLoad combo one.
                             else [(indL, valL)]
deleteLoad ((indL, valL) : tupL ) indD = if (indL == indD)
                                then []               -- remove this as its writeLoad combo one.
                                else (indL, valL) : deleteLoad tupL indD

compute :: (MonadIO m, MonadReader [ByteCodeIndexed] m, MonadState StateStack m) => [ByteCodeIndexed] -> m ()
compute [] = return ()
compute [(Return_value,i)]  = do 
                                  --io . putStrLn $ " io test from Return - "
                                  ss <- get
                                  let errM =  errorMessage ss
                                  let stackCompSS = stackComputation ss
                                  let errF = errorF ss
                                  let logM = logMessage ss
                                  if ((length stackCompSS) /= 1)
                                  then 
                                      do 
                                          put ( ss { errorMessage = "Return error has more than 1 computation left " : show i:  errM }  )
                                          return ()
                                  else 
                                      do 
                                          put ( ss { logMessage = "i am in Return in compute with Index " : show i:  logM }  )
                                          return ()


-- need to Multiply the first 2 in StackComputation list and remove those 2 and insert this new value. 
compute ((Multiply,i) : bcs ) = 
                        -- do 
                        --    ss <- get 
                        --    let errM =  errorMessage ss
                        --    put ( ss { errorMessage = "i am in Multiply in compute " : errM }  )
                        --    compute bcs
                        do 
                           ss <- get 
                           let errM =  errorMessage ss
                           let errF = errorF ss
                           let logM = logMessage ss
                           let stackCompSS = stackComputation ss
                           let (boolA, stackA) = multVal stackCompSS i 
                           if (boolA == True)
                           then 
                               do 
                                   put ( ss {  stackComputation = stackA,
                                               logMessage = "i am in MULT in compute " : show stackA : logM }  )
                                   compute bcs
                           else 
                               do 
                                   put ( ss { errorMessage = "Error in mutiply " : show stackA : errM,
                                              errorF = True: errF }  )
                                   compute bcs




-- need to add the first 2 in StackComputation list and remove those 2 and insert this new value. 
compute ((Add,i) : bcs ) = 
                        do 
                           ss <- get 
                           let errM =  errorMessage ss
                           let errF = errorF ss
                           let logM = logMessage ss
                           let stackCompSS = stackComputation ss
                           let (boolA, stackA) = addVal stackCompSS i 
                           if (boolA == True)
                           then 
                               do 
                                   put ( ss {  stackComputation = stackA,
                                               logMessage = "i am in Add in compute, showing stack:  " : show stackA : logM }  )
                                   compute bcs
                           else 
                               do 
                                   put ( ss { errorMessage = "ADD error " : show stackA : errM,
                                              errorF = True: errF  }  )
                                   compute bcs

-- Read Var - we need to read the variable put in the stack computation for consumption
compute (((Read_var s),i) : bcs ) = 
                        do 
                           ss <- get 
                           let errM =  errorMessage ss
                           let errF = errorF ss
                           let logM = logMessage ss
                           let stackCompSS = stackComputation ss
                           let varSS = variables ss
                           let (boolR, valR) = readVar varSS s 
                           if (boolR == True )
                           then 
                               do 
                                    put ( ss { stackComputation = (i,valR) : stackCompSS, 
                                        logMessage = "i am in Read in compute " : logM }  )
                                    compute bcs
                           else 
                               do
                                   put ( ss { errorMessage = "READ error "  : errM,
                                              errorF = True: errF  }  )
                                   compute bcs

-- in compute Stack there is not much to do with Write. We just ignore since read will later read it.
compute (((Write_var s),i) : bcs ) = 
                        do 
                           ss <- get 
                           let errM =  errorMessage ss
                           let errF = errorF ss
                           let logM = logMessage ss
                           put ( ss { logMessage = "i am in Write in compute " : logM }  )
                           compute bcs
                        
compute (((Load_val v),i) : bcs ) =                      -- Push into ComputeStack if load index exists.
                       do 
                           -- io . putStrLn $ "going to compute load now "
                           ss <- get
                           let stackCompCur = stackComputation ss
                           let errF = errorF ss
                           let logM = logMessage ss
                           let errM =  errorMessage ss
                           let boolL = pushLoad (loadIndex ss) i 
                           if (boolL == True)
                           then 
                               do 
                                   put ( ss { stackComputation = (i,v) : stackCompCur, 
                                              logMessage = "i am in Load in compute with index " : show i : logM }  )
                                   compute bcs
                           else
                               do 
                                   put ( ss {  
                                              logMessage = "i am in Ignored Load in compute with index  " : show i : logM }  )
                                   compute bcs
                           

compute (((Return_value,i)) : bcs)  = do 
                                  -- io . putStrLn $ " io test from Return - "
                                  ss <- get
                                  let errM =  errorMessage ss
                                  let errF = errorF ss
                                  let logM = logMessage ss
                                  put ( ss { logMessage = "i am in Return in compute with Index " : show i:  logM,
                                             errorMessage = "RETURN error - has to be last "  : errM,
                                              errorF = True: errF  }  )
                                  -- compute bcs



addVal :: [(Index,Int)] -> Int -> (Bool,[(Index,Int)])
addVal ((indA,a):(indB,b):rest) ind = (True, ((ind,(a+b)): rest) )
addVal _ _ = (False, [])



multVal :: [(Index,Int)] -> Int -> (Bool,[(Index,Int)])
multVal ((indA,a):(indB,b):rest) ind = (True, ((ind,(a*b)): rest) )
multVal _ _ = (False, [])


readVar :: [(String, Int)] -> String -> (Bool, Int)
readVar [] strR = (False, 0)
readVar [(strV, val)] strR = do 
                               if (strV == strR)
                               then (True, val)
                               else (False, 0)
readVar ((strV, val) : vars) strR = 
                             do 
                                if (strV == strR)
                                then (True, val)
                                else (readVar vars strR)


pushLoad :: [(Int,Int)] -> Int -> Bool
pushLoad [] indC = False
pushLoad [(indL, valL)] indC = if (indL == indC)
                               then True
                               else False
pushLoad ((indL, valL): lcs) indC = if (indL == indC)
                               then True
                               else pushLoad lcs indC                             

printList :: (MonadIO m, MonadReader [ByteCodeIndexed] m, MonadState StateStack m) => [String] -> m ()
printList [] = io . putStrLn $ "emptyList - "
printList [a] = io . putStrLn $ show a
printList (a:as) = do 
                       (io . putStrLn $ show a) 
                       printList as


renderState :: (MonadIO m, MonadReader [ByteCodeIndexed] m, MonadState StateStack m) => m ()
renderState = do
    stateCur <- get
    -- io . putStrLn $ " io test- " 
    io . putStrLn $ "errorFlags - " ++ (show . errorF $ stateCur)
    io . putStrLn $ "Message - " ++ (show . errorMessage $ stateCur)
    io . putStrLn $ "WriteIndexes - " ++ (show . writesIndex $ stateCur)
    io . putStrLn $ "LoadIndexes - " ++ (show . loadIndex $ stateCur)
    io . putStrLn $ "Variables - " ++ (show . variables $ stateCur)
    io . putStrLn $ "stackComputation - " ++ (show . stackComputation $ stateCur)
    -- io . putStrLn $ "Log Message - " ++ (show . logMessage $ stateCur)
    io . putStrLn $ "Log Messages: - "
    printList (logMessage stateCur)


-- uses Monad Reader to get the ByteCodeIndexed
-- calls handleWrite which indexes all the Write instructions into State writesIndex
-- calls handleLoad which indexed all the Load instructions into the State loadIndex
-- calls handleVariables - this uses Write to get preceding Load to store as variables List 
--      which can be accessed later as Read. Also removes the Load index since we wont use that Load fo consumption 
--                       in future as these will be accessed with Read.
-- calls compute which parses instruction one by one and either pushes in to Stack computation 
--        or uses for consumption and stores in stack after value is computed.
operations :: (MonadIO m, MonadReader [ByteCodeIndexed] m, MonadState StateStack m) => m ()
operations = do 
    byteAsgIndexed <- ask
    handleWrite byteAsgIndexed  
    handleLoad byteAsgIndexed  
    -- renderState
    handleVariables byteAsgIndexed 
    -- renderState
    io . putStrLn $ "*******************  Final State "
    compute byteAsgIndexed 
    renderState

main :: IO ()
main = 
    do
        -- putStrLn "Enter file name (Including full path) to read"
        byteFileList <- initByte
        byteAsgIndexed <- bytecodeIndexList byteFileList 1
        -- return byteAsgIndexed    -- gave error for now places it in a putStrLn
        --putStrLn (show byteAsgIndexed)
        (bool1, val1) <- return (checkLoad byteAsgIndexed 7)
        --putStrLn (show (bool1, val1))
        ss <- initState
        putStrLn "*******************   Assignment with index: "
        putStrLn (show byteAsgIndexed)
        -- printList byteAsgIndexed
        -- putStrLn "*******************  "
        -- (a, s2) <- runStateT (handleWrite byteAsgIndexed) ss
        runReaderT (runStateT operations ss) byteAsgIndexed
        -- stateCur <- get
        -- renderState
        -- putStrLn ("After Write run " ++ show s2)
        -- (a2, s3) <- runStateT (handleLoad byteAsgIndexed) ss
        -- hl <- handleLoad byteAsgIndexed
        -- s3 <- get
        -- putStrLn ("After Load run ")
        -- renderState
        -- runStateT (renderState ) s2
        return ()
        -- putStrLn (show byteAsgIndexed)
        -- fileName <- getLine
        -- fileHandle <- openFile fileName ReadMode
        -- readDataFrom fileHandle
        -- ls <- fmap Text.lines (Text.readFile "src/assg1.txt")
        -- -- putStrLn (show ls)
        -- -- putStrLn (show ( (unPack ls)))
        -- putStrLn (show  (wordsStringAsg (unPack ls)) )
        

-- Reader for instructions
-- State for the stack of compuation
-- list of variables...    
-- can combine stack and List of var... 