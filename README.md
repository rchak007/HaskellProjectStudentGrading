# Project StudentGrading
Automate Student Grading using Haskell





![image-20230615221528672](Images/image-20230615221528672.png)



[Haskell main program - Main.hs](https://github.com/rchak007/HaskellProjectStudentGrading/blob/main/bytecode/src/Main.hs) 



## Summary

This project takes a Pseudo code for Students assignment in a file which has some basic Add/Multiply set of expressions and helps the Professor pre-screen by running students assigment to see the completeness of Assignment.

It will give error with messages if somethign is wrong. 

If its correct there is no error messages and flags and final Stack will have the final computation.



### Sample Initial file:

This below is valid assignment.

| Bytecode File |
| ------------- |
| LOAD_VAL 1    |
| WRITE_VAR X   |
| LOAD_VAL 2    |
| WRITE_VAR  Y  |
| READ_VAR X    |
| LOAD_VAL 1    |
| ADD           |
| READ_VAR Y    |
| MULTIPLY      |
| RETURN_VALUE  |





### Ground Rules 

| Rules                                                        |
| ------------------------------------------------------------ |
| Return_value will be last. Anything other place has to error |
| Write - every write needs to have preceeding Load so that the variable is stored. Otherwise there will be error. |
| Load without Write right after means its ready to be used in Add/Mutiply |
| Add/Multiply will operate on 2 stored valus on stack. If its missing that's an error. Once its computed the prior 2 values are erased as its consumed and current value will be stored for future consumption. |
|                                                              |





### Implementation 

#### Monads used 

MonadIO m, MonadReader [ByteCodeIndexed] m, MonadState StateStack m



#### Implementation 

we start by reading the student's assignment file and unpacking each instruction into our custom data type ByteCode.

```haskell
-- sample Student assignment
Load_val 1
Write_var X
Load_val 22
Write_var Y
Read_var X
Load_val 5
Add
Read_var Y
Multiply
Return_value
```



```haskell
-- Reads the file and gets Text.Text
-- then we unpack it to String using `unPack` 
-- then we convert to ByteCode data type with `wordsStringAsg`
initByte :: MonadIO m => m [ByteCode Int String]
initByte = do
  ls <- io (fmap Text.lines (Text.readFile "src/assg1.txt"))
  bytecodeFile <- return (wordsStringAsg (unPack ls))
  return bytecodeFile
```



Then due to the nature of looking back to see if Write has a preceding Load etc. we now move the ByteCode to index each instruction into ByteCodeIndexed list. This way when there is a Write instruction we make immediately preceding there is a Load instruction. 

```haskell
transformBytecodeAddIndex :: Int -> [ByteCode Int String] -> [ByteCodeIndexed]
transformBytecodeAddIndex _ [] = []
transformBytecodeAddIndex i [bc] = [(bc,i)]
transformBytecodeAddIndex i (bc:bcs) = (bc,i) : transformBytecodeAddIndex (i+1) bcs

bytecodeIndexList :: MonadIO m => [ByteCode Int String] -> Int -> m [ByteCodeIndexed]
bytecodeIndexList asgList i = return (transformBytecodeAddIndex i asgList)

```



Assignment is converted to:

```haskell
*******************   Assignment with index: 
[(Load_val 1,1),(Write_var "X",2),(Load_val 22,3),(Write_var "Y",4),(Read_var "X",5),(Load_val 5,6),(Add,7),(Read_var "Y",8),(Multiply,9),(Return_value,10)]
```





First we update the state writeIndex with all the writes we have. 

Then we process all the Load instructions and put indexs on each Load. 

Next step we create the Variables list for the writes but also do error handling to make sure Write precedes with Load. At this point we have all our varibles.

Now we execute the `compute` function that reads each instruction and starts building and consuming the `StackComputation`

```haskell
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
```



A Load (only the non Load/write combo are considered) or Read will result is Pushing the value into the stack computation to be consumed later.

An Add or Multiply will consume 2 from the stack and will be popped out of the stack but now its current calculation is pushed onto the stack for furhter computation. 



### Results

#### Result 1 - correct assignment 

![image-20220702135120780](Images/asg1Result.png)







#### Result 1 - Incorrect assignment 

![image-20220702135337178](Images/asg3AddErr.png)

















