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



-- data ByteCode i var = Load_val i 
--               | Write_var var
--               | Read_var var
--               | Add
--               | Multiply
--               | Return_value
--               | Zero
--               deriving (Show, Eq)


-- 

-- assignment :: [Bytecode]
line1 = Load_val 1
line2 = Write_var "X"
line3 = Load_val 22
line4 = Write_var "Y"
line5 = Read_var "X"
line6 = Load_val 5
line7 = Add
line8 = Read_var "Y"
line9 = Multiply
line10 = Return_value

assignment :: [ByteCode Int String]

-- assignment = [line1, line2, line3,line4,line5,line6,line7,line8, line9, line10]

-- assignment = [line1, line2, line3,line4,line5,line6,line7,line7, line9, line10]

assignment = [line2, line3,line4,line5,line6,line7,line8, line9, line10]


bytecodeTup :: (ByteCode Int String) -> (ByteCode Int String, Int)
bytecodeTup a = (a, 1)
-- simple test with Applicative
assignmentTupAppl = [bytecodeTup] <*> assignment


-- assignmentTup :: [(ByteCode Int String, Int)]
-- assignmentTup = convertAsgTup assignment
-- asg_length = length assignment 

-- convertAsgTup :: assignment -> assignmentTup
-- -- convertAsgTup [] = [(,)]
-- convertAsgTup [asg] = [asg,1]


result_assignment :: [ByteCode Int String] -> Bool
result_assignment []  = False

-- type Index = Int 
-- convAsgTup :: assignment -> State Index assignment
-- convAsgTup [as] = [as] >>= \a -> get >>= \i -> (return (a, i ) )

list1 = [1,2,3]
listMonad list1 = list1 >>= \l -> return (l+1)
-- *Main> listMonad list1
-- [2,3,4]


-- type AssignSt = 
-- convAsgTup assignment = assignment >>= \l -> return (l,1)


-- example2 :: StateT Int [] ()
-- example2 = do 
--   v <- get
--   lift.putStrLn $ "The state is " ++ show v
--   liftIO $ putStrLn ("Enter the instruction: ")
--   instruction <- liftIO $ getLine 
--   case instruction of 
--     "i" -> do 
--       put (v+1)
--       example2     -- will keep running 
--     "d" -> do
--       put (v-1)
--       example2
--     "x" -> return ()


-- convAsgTup assignment = do 
--   i <- get 
--   modify (+1)
--   return (a,i)

-- sum' :: ListT (State Int) Int
-- sum' = do
--     lift $ put 0
--     x <- ListT $ return [1,2,3]
--     lift $ modify (+x)
--     return $ x + 1

-- testAs :: Int -> [(assignment, Int)]
-- testAs = runStateT $ do
--     a <- lift assignment
--     modify (+a)
--     return a

test :: Int -> [(Int, Int)]
test = runStateT $ do
    a <- lift [1..10]
    modify (+a)
    return a


type Val = Int

type Consumed = Bool
-- Number each of the ByteCode instruction read
-- for now just doing with simple Function to tranform read list of ByteCode into a list ByteCode as tuple with its sequence number.

-- ver 1.0
type ByteCodeTup = (ByteCode Int String, Index, Val, Bool, Consumed)
assgConvertTupFunc :: Int -> [ByteCode Int String] -> [ByteCodeTup]
assgConvertTupFunc _ [] = []
assgConvertTupFunc i [bc] = [(bc,i, 0, True, False)]
assgConvertTupFunc i (bc:bcs) = (bc,i,0, True, False) : assgConvertTupFunc (i+1) bcs

assignmentTupList = assgConvertTupFunc 1 assignment
-- *Main> assignmentTupList
-- [(Load_val 1,1,0,True),(Write_var "X",2,0,True),(Load_val 2,3,0,True),(Write_var "Y",4,0,True),(Read_var "X",5,0,True),(Load_val 1,6,0,True),(Add,7,0,True),(Read_var "Y",8,0,True),(Multiply,9,0,True),(Return_value,10,0,True)]

consAssignmentTupList = assignmentTupList


-- ver 2.0

transformBytecodeAddIndex :: Int -> [ByteCode Int String] -> [ByteCodeIndexed]
transformBytecodeAddIndex _ [] = []
transformBytecodeAddIndex i [bc] = [(bc,i)]
transformBytecodeAddIndex i (bc:bcs) = (bc,i) : transformBytecodeAddIndex (i+1) bcs

bytecodeIndexList :: MonadIO m => [ByteCode Int String] -> Int -> m [ByteCodeIndexed]
bytecodeIndexList asgList i = return (transformBytecodeAddIndex i asgList)



-- ver 2.0
-- we will now take the BytecodeIndexed and reverse it first --
-- goal is to check the Write - take the variable and create a variable list from it.
-- also will check next one is Load - if not error. 
-- also if we write this to Variable list we will just drop that Write and Load combo. 
-- now if i have to write the Variable list then i need to carry State now into this Function.
-- i dont want to still use Reader since this function will first get rid of the Write and Load combos.


-- handleWrite :: (MonadState StateStack m) => [ByteCodeIndexed] -> m [ByteCodeIndexed]


-- handleWrite []  = do 
--                     return $ putStrLn " in write list "
--                     return ()
-- handleWrite [((Write_var s), i)] = 
--                             do 
--                               ss <- get
--                               return $ putStrLn " in write list "
--                               put ( ss { errorMessage = " in write " : errorMessage ss})
-- handleWrite [(bc, i)] = do 
--                           return $ putStrLn " in write list "
--                           return ()

-- handleWrite (((Write_var s), i) : bcs) = 
--                             do 
--                               return $ putStrLn " in write list "
--                               ss <- get
--                               put ( ss { errorMessage = " in write " : errorMessage ss})
--                               -- return ( mconcat ( return ((Write_var s), i) : handleWrite bcs ) )
-- handleWrite ((bc, i) : bcs ) = do 
--                   ss <- get
--                   put ( ss { errorMessage = ["In Other list Write"]})
--                   return ()



-- test...
handleWrite2 :: (MonadIO m, MonadState StateStack m) => [ByteCodeIndexed] -> m ()
handleWrite2 _ = do 
                  ss <- get
                  put ( ss { errorMessage = ("In Others2 Write" : errorMessage ss)})
                  return ()
-- notes -- copied from canPlay in snake.
-- handleWriteLoadCombo :: (MonadState StateStack m) => [ByteCodeIndexed] -> [ByteCodeIndexed] -> m [ByteCodeIndexed]
-- -- handleWriteLoadCombo :: [ByteCodeIndexed] -> [ByteCodeIndexed] -> State StateStack [ByteCodeIndexed]
-- handleWriteLoadCombo [] bcC = do
--   stateCur <- get
--   return []
-- handleWriteLoadCombo [((Write_var s), i)] bcC = 
--                 do 
--                     ss <- get 
--                     if (fst (checkLoad bcC (i+1)) == True) 
--                     then         -- load is found right after Write , add the variables list, mark this write in writeIndex.
--                         return $ put ( ss { stackComputation = stackComputation ss , 
--                                    variables = (s, (snd (checkLoad bcC (i+1)))) : (variables ss), 
--                                    errorF = errorF ss,
--                                    errorMessage = errorMessage ss,
--                                    writesIndex = i : (writesIndex ss)} )
--                         return [] 
--                     else     -- report error cause load has to be there before write.  
--                         put ( ss { stackComputation = stackComputation ss , 
--                                    variables = variables ss, 
--                                    errorF = (False : errorF ss) ,
--                                    errorMessage = " Write does not have Load" : errorMessage ss,
--                                    writesIndex = writesIndex ss})
--                         return []                        
-- handleWriteLoadCombo [ (bc,i)] bcC = return [ (bc,i)]


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



  







-- write will check earlier index and if its Load then just keep that value and also return 
-- 
writeCheck ::   [ByteCodeTup] -> [ByteCodeTup]
-- if its a write 
writeCheck [(Write_var var,ind, val, booll, bools)]  = [(Write_var var, ind, snd (getLoadFound (consAssignmentTupList) (ind-1)), fst (getLoadFound (consAssignmentTupList) (ind-1)), bools)]
writeCheck [bt]  = [bt]         -- others

writeCheck ((Write_var var,ind, val, booll, bools):bts) = (Write_var var, ind, snd (getLoadFound (consAssignmentTupList) (ind-1)), fst (getLoadFound ( consAssignmentTupList) (ind-1)), bools) : writeCheck bts
writeCheck (bt :bts) = bt : writeCheck bts


afterWriteAssg = writeCheck consAssignmentTupList

-- *Main> writeCheck consAssignmentTupList 
-- [(Load_val 1,1,0,True),
-- (Write_var "X",2,1,True),     -- value 1 correct
-- (Load_val 2,3,0,True),
-- (Write_var "Y",4,2,True),     -- Value 2 correct
-- (Read_var "X",5,0,True),
-- (Load_val 1,6,0,True),
-- (Add,7,0,True),
-- (Read_var "Y",8,0,True),
-- (Multiply,9,0,True),
-- (Return_value,10,0,True)]


-- temp.. 
getLoadVal consAssignmentTupList _ = 1

-- this Function finds the Load_val based on Index and fills the 3rd tuple with that value and found status.
getLoadFound :: [ByteCodeTup] -> Int -> (Bool, Int)
getLoadFound [(Load_val ldVal, ind, val, bool1, bools) ] indf = if ind == indf 
                                                    then (True, ldVal)
                                                    else (False, 0)
getLoadFound [_,_,_,_, _] indf = (False, 0)
getLoadFound ((Load_val ldVal, ind, val, bool1, bools): bts) indf = (if ind == indf 
                                                    then (True, ldVal)
                                                    else ( False, 0 ) )
                                                    `aggTup` (getLoadFound bts indf)
getLoadFound (_: bts) indf =  (False,0)  `aggTup`  (getLoadFound bts indf)

-- *Main> getLoadFound consAssignmentTupList 3
-- (True,2)
-- *Main> getLoadFound consAssignmentTupList 1
-- (True,1)
-- *Main> getLoadFound consAssignmentTupList 6
-- (True,1)
-- *Main> getLoadFound consAssignmentTupList 2
-- (False,0)


aggTup :: (Bool, Int) -> (Bool, Int) -> (Bool, Int)
aggTup (b1, i1) (b2,i2) = ((b1 || b2), (i1 + i2))


aggTupMult :: (Bool, Int) -> (Bool, Int) -> (Bool, Int)
aggTupMult (b1, i1) (b2,i2) = ((b1 || b2), (i1 * i2))

-- get the variable value 
getWriteFound :: [ByteCodeTup] -> String -> (Bool, Int)
getWriteFound [(Write_var var, ind, val, bool1, bools) ] varf = if var == varf 
                                                    then (True, val)
                                                    else (False, 0)
getWriteFound [_,_,_,_] varf = (False, 0)     -- if its not write dont do anything
getWriteFound ((Write_var var, ind, val, bool1, bools): bts) varf = (if var == varf 
                                                    then (True, val)
                                                    else ( False, 0 ) )
                                                    `aggTup` (getWriteFound bts varf)
getWriteFound (_: bts) varf =  (False,0)  `aggTup`  (getWriteFound bts varf)

-- *Main> getWriteFound afterWriteAssg "Y"
-- (True,2)
-- *Main> getWriteFound afterWriteAssg "X"
-- (True,1)
-- *Main> getWriteFound afterWriteAssg "Z"
-- (False,0)
-- *Main> getWriteFound afterWriteAssg "A"
-- (False,0)




readCheck ::   [ByteCodeTup] -> [ByteCodeTup]
-- if its a write 
readCheck [(Read_var var,ind, val, booll, bools)]  = [(Read_var var, ind, snd (getWriteFound afterWriteAssg var), fst (getWriteFound afterWriteAssg var), bools)]
readCheck [bt]  = [bt]         -- others

readCheck ((Read_var var,ind, val, booll, bools):bts) = (Read_var var, ind, snd (getWriteFound afterWriteAssg var), fst (getWriteFound afterWriteAssg var), bools) : readCheck bts
readCheck (bt :bts) = bt : readCheck bts       -- first element is NOT read. 

-- *Main> readCheck afterWriteAssg 
-- [(Load_val 1,1,0,True),
-- (Write_var "X",2,1,True),      -- X = 1
-- (Load_val 2,3,0,True),
-- (Write_var "Y",4,2,True),      -- Y = 2
-- (Read_var "X",5,1,True),       -- read X = 1
-- (Load_val 1,6,0,True), 
-- (Add,7,0,True),
-- (Read_var "Y",8,2,True),       -- Read 
-- (Multiply,9,0,True),
-- (Return_value,10,0,True)]

afterReadAsg = readCheck afterWriteAssg


-- Add needs to go back to get 2 matches on Load or Read - any combinations.
type Seed = Int

getForAdd :: [ByteCodeTup] -> Int -> Int -> (Bool, Int)
getForAdd [(Read_var var, ind, val, bool1, bools) ] i indp = (False, 0)
getForAdd [(Load_val ldVal, ind, val, bool1, bools) ] i indp = (False, 0)
getForAdd ((Read_var var, ind, val, bool1, bools): bts) i indp = if ( i == 2 && ind <= indp )
                                                      then (True, val) `aggTup` (getForAdd bts 1 indp)
                                                      else if (i  == 1 && ind <= indp)
                                                           then (True, val) 
                                                           else if (ind > indp) 
                                                                then (False, 0) `aggTup` (getForAdd bts i indp)
                                                                else (False, 0)
getForAdd ((Load_val ldVal, ind, val, bool1, bools): bts) i indp = if ( i == 2 && ind <= indp)
                                                            then (True, ldVal) `aggTup` (getForAdd bts 1 indp)
                                                            else if (i  == 1 && ind <= indp)
                                                                 then (True, ldVal) 
                                                                 else if (ind > indp) 
                                                                      then (False, 0) `aggTup` (getForAdd bts i indp)
                                                                       else (False, 0)
-- should not get another Add in the next 2 values prior. 
-- getForAdd (((Add, ind, val, bool1, bools)) : bts) i indp = if ((i == 1 || i == 2) && ind <= indp) 
--                                             then (False, 0)
--                                             else (False, 0)
getForAdd (_: bts) i indp =  (False,0)  `aggTup`  (getForAdd bts i indp)
-- getForAdd [_,_,_,_,_] i indp = (False, 0) 

-- *Main> getForAdd (reverse afterReadAsg ) 2 7
-- (True,2)


-- 2 adds in a row will still fail.. 
 

-- addCheck ::   [ByteCodeTup] -> [ByteCodeTup]
-- -- if its a write 
-- addCheck [(Add,ind, val, booll)]  = [(Add, ind, snd (getForAdd (reverse afterReadAsg) 2 (ind-1)), fst (getForAdd (reverse afterReadAsg) 2 (ind-1)))]
-- addCheck [bt]  = [bt]         -- others

-- addCheck ((Add,ind, val, booll):bts) = (Add , ind, snd (getForAdd ( reverse afterReadAsg) 2 (ind-1)), fst (getForAdd ( reverse afterReadAsg ) 2 (ind-1))) : addCheck bts
-- addCheck (bt :bts) = bt : addCheck bts       -- first element is NOT read. 


addCheck ::   [ByteCodeTup] -> [ByteCodeTup]
-- if its a write 
addCheck [(Add,ind, val, booll, bools)]  = [(Add, ind, snd (getForAdd (reverse (take (ind-1) afterReadAsg)) 2 (ind-1)), fst (getForAdd (reverse (take (ind-1) afterReadAsg)) 2 (ind-1)), True)]
addCheck [bt]  = [bt]         -- others

addCheck ((Add,ind, val, booll, bools):bts) = (Add , ind, snd (getForAdd ( reverse (take (ind-1) afterReadAsg)) 2 (ind-1)), fst (getForAdd ( reverse (take (ind-1) afterReadAsg )) 2 (ind-1)), True) : addCheck bts
addCheck (bt :bts) = bt : addCheck bts       -- first element is NOT read. 





afterAddAsg = addCheck (afterReadAsg )

-- *Main> afterAddAsg 
-- [(Load_val 1,1,0,True),
-- (Write_var "X",2,1,True),
-- (Load_val 22,3,0,True),
-- (Write_var "Y",4,22,True),
-- (Read_var "X",5,1,True),
-- (Load_val 5,6,0,True),
-- (Add,7,6,True),
-- (Read_var "Y",8,22,True),
-- (Multiply,9,0,True),
-- (Return_value,10,0,True)]







getForMult :: [ByteCodeTup] -> Int -> Int -> (Bool, Int)
getForMult [(Read_var var, ind, val, bool1, bools) ] i indp = (False, 1)
getForMult [(Load_val ldVal, ind, val, bool1, bools) ] i indp = (False, 1)
getForMult [_,_,_,_] i indp = (False, 1)   

getForMult ((Read_var var, ind, val, bool1, bools): bts) i indp = if ( i == 2 && ind <= indp )
                                                      then (True, val) `aggTupMult` (getForMult bts 1 indp)
                                                      else if (i  == 1 && ind <= indp)
                                                           then (True, val) 
                                                           else if (ind > indp) 
                                                                then (False, 1) `aggTupMult` (getForMult bts i indp)
                                                                else (False, 1)
getForMult ((Load_val ldVal, ind, val, bool1, bools): bts) i indp = if ( i == 2 && ind <= indp)
                                                            then (True, ldVal) `aggTupMult` (getForMult bts 1 indp)
                                                            else if (i  == 1 && ind <= indp)
                                                                 then (True, ldVal) 
                                                                 else if (ind > indp) 
                                                                      then (False, 1) `aggTupMult` (getForMult bts i indp)
                                                                       else (False, 1)


getForMult ((Add, ind, val, bool1, bools): bts) i indp = if ( i == 2 && ind <= indp)
                                                            then (True, val) `aggTupMult` (getForMult bts 1 indp)
                                                            else if (i  == 1 && ind <= indp)
                                                                 then (True, val) 
                                                                 else if (ind > indp) 
                                                                      then (False, 1) `aggTupMult` (getForMult bts i indp)
                                                                       else (False, 1)



getForMult (_: bts) i indp =  (False,1)  `aggTupMult`  (getForMult bts i indp)

-- *Main> getForMult (reverse afterAddAsg ) 2 9
-- (True,44)



addMult ::   [ByteCodeTup] -> [ByteCodeTup]
-- if its a write 
addMult [(Multiply,ind, val, booll, bools)]  = [(Multiply, ind, snd (getForMult (reverse afterAddAsg) 2 (ind-1)), fst (getForMult (reverse afterAddAsg) 2 (ind-1)), True)]
addMult [bt]  = [bt]         -- others

addMult ((Multiply,ind, val, booll, bools):bts) = (Multiply , ind, snd (getForMult ( reverse afterAddAsg) 2 (ind-1)), fst (getForMult ( reverse afterAddAsg ) 2 (ind-1)), True) : addMult bts
addMult (bt :bts) = bt : addMult bts 


-- *Main> addMult (afterAddAsg)
-- [(Load_val 1,1,0,True,False),
-- (Write_var "X",2,1,True,False),
-- (Load_val 22,3,0,True,False),
-- (Write_var "Y",4,22,True,False),
-- (Read_var "X",5,1,True,False),
-- (Load_val 5,6,0,True,False),
-- (Add,7,6,True,True),
-- (Read_var "Y",8,22,True,False),
-- (Multiply,9,132,True,True),
-- (Return_value,10,0,True,False)]


-- handle Multiple and Add in any sequence 
combAddMult ::   [ByteCodeTup] -> [ByteCodeTup] -> Int -> [ByteCodeTup]
-- if its a write 
combAddMult [(Multiply,ind, val, booll, bools)]  btc indp = if (ind == indp )
                                                        then [(Multiply, ind, snd (getForMult (reverse btc) 2 (ind-1)), fst (getForMult (reverse btc) 2 (ind-1)), True)]
                                                        else [(Multiply,ind, val, booll, bools)]

combAddMult [(Add,ind, val, booll, bools)] btc indp  = if (ind == indp) 
                                                   then [(Add, ind, snd (getForAdd (reverse (take (ind-1) btc)) 2 (ind-1)), fst (getForAdd (reverse (take (ind-1) btc)) 2 (ind-1)), True)]
                                                   else [(Add,ind, val, booll, bools)]

combAddMult [bt] bt' indp  = [bt]     -- others


combAddMult ((Multiply,ind, val, booll, bools):bts) btc indp = if (ind == indp) 
                                                           then (Multiply , ind, snd (getForMult ( reverse btc) 2 (ind-1)), fst (getForMult ( reverse btc ) 2 (ind-1)), True) : combAddMult bts btc indp
                                                           else ((Multiply,ind, val, booll, bools): combAddMult bts btc indp)

combAddMult ((Add,ind, val, booll, bools):bts) btc indp = if (ind == indp) 
                                                      then (Add , ind, (snd (getForAdd ( reverse (take (ind-1) btc)) 2 (ind-1)) ), fst (getForAdd ( reverse (take (ind-1) btc )) 2 (ind-1)), True) : combAddMult bts btc indp
                                                      else ((Add,ind, val, booll, bools):combAddMult bts btc indp)

combAddMult (bt :bts) btc indp = bt : combAddMult bts btc indp

-- function takes length. 
-- take a Monad bytecodeTup >>= \bytecode -> Monad bytecode.

byteCodeJust :: [ByteCodeTup] -> Int -> Maybe ([ByteCodeTup], Int)
byteCodeJust bt i = Just (bt, i)

justBytecodeWStatus = byteCodeJust (afterReadAsg) (length afterReadAsg)

finalByteCode = (justBytecodeWStatus) >>= \(bt, i) -> processAssg bt 1 

processAssg :: [ByteCodeTup] -> Int -> Maybe (([ByteCodeTup], Int))
processAssg bt i  = if (i == (length bt)) 
                   then return (bt,i)
                   else (return (combAddMult bt bt i, i )) >>= \(bt',i') -> processAssg bt' (i'+1) 





-- bindMaybe1 = Just 3 >>= (\x -> Just 5 >>= \y -> Just (x + y))
-- -- *Main> bindMaybe1
-- -- Just 8




-- issues - what if we reverse the order or Multiply and Add.
-- need to have another wrapper to call which ever comes first.

-- we also have issue of 2 adds in a row. -- maybe 
-- also when i pass List along need to do Take so i dont have to reverse etc.






-- *Main> afterReadAsg
-- [(Load_val 1,1,0,True,False),
-- (Write_var "X",2,1,True,False),
-- (Load_val 22,3,0,True,False),
-- (Write_var "Y",4,22,True,False),
-- (Read_var "X",5,1,True,False),
-- (Load_val 5,6,0,True,False),
-- (Add,7,0,True,False),
-- (Read_var "Y",8,22,True,False),
-- (Multiply,9,0,True,False),
-- (Return_value,10,0,True,False)]

-- *Main> combAddMult afterReadAsg 7
-- [(Load_val 1,1,0,True,False),
-- (Write_var "X",2,1,True,False),
-- (Load_val 22,3,0,True,False),
-- (Write_var "Y",4,22,True,False),
-- (Read_var "X",5,1,True,False),
-- (Load_val 5,6,0,True,False),
-- (Add,7,0,True,False),
-- (Read_var "Y",8,22,True,False),
-- (Multiply,9,0,True,False),
-- (Return_value,10,0,True,False)]



-- *Main> finalByteCode
-- Just ([
-- (Load_val 1,1,0,True,False),
-- (Write_var "X",2,1,True,False),
-- (Load_val 22,3,0,True,False),
-- (Write_var "Y",4,22,True,False),
-- (Read_var "X",5,1,True,False),
-- (Load_val 5,6,0,True,False),
-- (Add,7,6,True,True),
-- (Read_var "Y",8,22,True,False),
-- (Multiply,9,132,True,True),
-- (Return_value,10,0,True,False)],10)


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
handleWrite :: (MonadIO m, MonadState StateStack m) => [ByteCodeIndexed] -> m ()
handleWrite [] = return ()
handleWrite (((Write_var s),i) : bcs ) = do 
                  ss <- get
                  return $ putStrLn " in PutStrLn BLANK write list "
                  put ( ss { -- errorMessage = ("In Others1 Write" : errorMessage ss), 
                             writesIndex = i: writesIndex ss})
                  s2 <- get
                  handleWrite bcs
                  return ()
handleWrite (bc : bcs ) = do 
                  ss <- get
                  return $ putStrLn " in PutStrLn BLANK write list "
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
                                  io . putStrLn $ " io test from Return - "
                                  ss <- get
                                  let errM =  errorMessage ss
                                  let errF = errorF ss
                                  let logM = logMessage ss
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
    io . putStrLn $ " io test- " 
    io . putStrLn $ "errorFlags - " ++ (show . errorF $ stateCur)
    io . putStrLn $ "Message - " ++ (show . errorMessage $ stateCur)
    io . putStrLn $ "WriteIndexes - " ++ (show . writesIndex $ stateCur)
    io . putStrLn $ "LoadIndexes - " ++ (show . loadIndex $ stateCur)
    io . putStrLn $ "Variables - " ++ (show . variables $ stateCur)
    io . putStrLn $ "stackComputation - " ++ (show . stackComputation $ stateCur)
    -- io . putStrLn $ "Log Message - " ++ (show . logMessage $ stateCur)
    printList (logMessage stateCur)

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