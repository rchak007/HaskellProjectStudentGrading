module Data where 

-- this will be Reader but as a List
data ByteCode i var = Load_val i 
              | Write_var var
              | Read_var var
              | Add
              | Multiply
              | Return_value
              | Zero
              deriving (Show, Eq)
type Index = Int

type ByteCodeIndexed = ((ByteCode Int String), Index)

-- it will be an integer computation stack -
-- so i have a load i put that Int here 
-- but if there is Write just after Load i take it out as now its a variable.
-- but assuming variables were taken care...

type StackComputation = [(Index,Int)]     -- keeps Index of what was pushed and value 
type Variables = [(String, Int)]
data StateStack  =  StateStack {
    stackComputation :: StackComputation,
    variables        :: Variables,
    errorF           :: [Bool],
    errorMessage     :: [String],
    logMessage       :: [String],
    writesIndex      :: [Int],
    loadIndex        :: [(Int,Int)]
} deriving Show


-- rules - 
-- if i have a load and write -- then now its only available from Read to use the loaded value.

-- add 
    -- will pop out 2 computations from computational stack or if its read it will get one from there
    -- i can add either a loaded value from Stack (thats not written to variable )
    -- i can add a Read variable
    -- i can also add from Stack - means some previoud add exists too as computation. 

-- multiply is same too 

-- load, read, add, load, add --- valid
-- load, read, add, add -- invalid  -- as 1st add already consumed earlier load and read 
                     -- .. 2nd add does not have 2nd argument

-- i first make 1 pass over the Reader (full assigment) and create variable and value list. 
--     so that combination of load and Write are not there anymore on the stack 
--       also 


-- VALIDATIONS - 
-- write has to have load earlier. and both dont go into Stack. 
-- 

-- LOAD/WRITE combo
-- If i get load i push into stack. -- this is a list with tuple (load, val, err). 
-- If i get write then i write to variable list , and also pop the last Load as value. 
    -- I will give error if i dont find load in stack.
-- LOAD (and not write) -- will push into Stack as Tuple and will stay
-- READ - will read the variable list and value and put that now in Stack with Push.
-- ADD -- will pop out 2 from Stack and Push value of computation back on to Stack.
-- Multiply - same as Add. and if there is only 1 or 0 in stack that's an error. (mult, 0, err)
 

-- line1 = Load_val 1       --- push stack 
-- line2 = Write_var "X"    --  write to variable list with value, and POP the load out  -- so nothing in stack
-- line3 = Load_val 22
-- line4 = Write_var "Y"    --- same nothign in stack now.
-- line5 = Read_var "X"     --  now stack will have 1 value of X.
-- line6 = Load_val 5       --- push stack -- now has 2 values in stack.
-- line7 = Add              --   will pop last 2  and push this value .. now stack has only 1 value.
-- line8 = Read_var "Y"     --   push value of read into stack.. now has 2 
-- line9 = Multiply         -- will pop out last 2 and now push multiply value -- stack has only 1
-- line10 = Return_value    -- return - ends after checking no more and also stack should have only 1 value.





