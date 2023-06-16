module Init where

import Control.Monad.IO.Class
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data
import Util

unPack :: [Text.Text] -> [String]
unpack [] = []
unPack [t] = [Text.unpack t]
unPack (t : ts) = Text.unpack t : unPack ts

wordsStringAsg :: [String] -> [ByteCode Int String]
wordsStringAsg [] = []
wordsStringAsg [s] = [strByte (words s)]
wordsStringAsg (str: strs) = (strByte (words str)) : wordsStringAsg strs

strByte :: [String] -> (ByteCode Int String)
strByte [w]
   | (w == "Add") = Add
   | (w == "Multiply") = Multiply
   | (w == "Return_value") = Return_value
strByte (w:w2:ws)
   | w == "Load_val" = Load_val (read w2)
   | w == "Write_var" = Write_var w2
   | w == "Read_var" = Read_var w2
-- | otherwise = return Zero    -- for now --- later have some error messages




-- Reads the file and gets Text.Text
-- then we unpack it to String using `unPack` 
-- then we convert to ByteCode data type with `wordsStringAsg`

initByte :: MonadIO m => m [ByteCode Int String]
initByte = do
  ls <- io (fmap Text.lines (Text.readFile "src/assg1.txt"))
  bytecodeFile <- return (wordsStringAsg (unPack ls))
  return bytecodeFile

-- initByte :: MonadIO m => m  [ByteCodeIndexed]
--   ls <- io (fmap Text.lines (Text.readFile "src/assg2Err.txt"))
--   ls <- io (fmap Text.lines (Text.readFile "src/assg3AddErr.txt"))
--  ls <- io (fmap Text.lines (Text.readFile "src/assg4.txt"))
  -- ls <- io (fmap Text.lines (Text.readFile "src/assg5ReturnErr.txt"))
  -- ls <- io (fmap Text.lines (Text.readFile "src/assg6ErrReturnComp.txt"))
  -- bytecodeIndexList <- transformBytecodeAddIndex bytecodeFile


-- we start with empty state
initState :: MonadIO m => m ( StateStack)
initState = do
  return $ StateStack {
    stackComputation = [],
    variables = [],
    errorF = [],
    errorMessage = [],
    logMessage = [],
    writesIndex = [],
    loadIndex = []
  }


-- initConfig :: MonadIO m => m Config
-- initConfig = do
--   s <- io $ getTerminalSize
--   return $ Config
--     {
--       screenSize = fromJust s,
--       headChar   = 'o',
--       bodyChar   = 'x',
--       foodChar   = '&',
--       tickRate   = 200,
--       blinkRate  = 500
--     }