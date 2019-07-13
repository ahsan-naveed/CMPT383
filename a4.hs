data Token = Num Double | Op String
    deriving (Eq, Read)


instance Show Token where
    show (Num n)  = "<" ++ (show n) ++ ">"
    show (Op str) = "<" ++ str ++ ">"

    
-- Returns True when input is one of the operators
is_op :: [Char] -> Bool 
is_op x 
      | x `elem` ops = True
      | otherwise    = False
        where ops = [
                        "+", "-", "*", "/", "swap", "cos", 
                        "clear", "pop", "inv", "dec", "inc",
                        "sin", "dup", "sqrt", "*all", "+all"
                    ]


-- Converts input to an equivalent Token type
to_token :: String -> Token
to_token x
         | is_op x   = Op x
         | otherwise = Num (read x::Double)


-- Prints out tokenized stack
calcStack :: String -> String
calcStack input_stream = unwords [show . to_token $ x | x <- (words input_stream)]


-- displays error messages when executing faulty unary operations
handleError :: String -> [Double]
handleError "dec"   = error "dec: empty stack"
handleError "inc"   = error "inc: empty stack"
handleError "swap"  = error "swap: empty stack"
handleError "pop"   = error "pop: empty stack"
handleError "clear" = error "clear: empty stack"
handleError "inv"   = error "inv: empty stack"
handleError "cos"   = error "cos: empty stack"
handleError "sin"   = error "sin: empty stack"
handleError "dup"   = error "dup: empty stack"
handleError "sqrt"  = error "sqrt: empty stack"


-- Evaluates the input stream 
-- Note: this functino also handles error with binary operators
operator :: [Double] -> String -> [Double]  
-- binary operators and their error handling
operator (x:y:stack) "*"    = (x * y):stack
operator (x:stack) "*"      = case stack of [] -> error "*: not enough args"

operator (x:y:stack) "+"    = (x + y):stack
operator (x:stack) "+"      = case stack of [] -> error "+: not enough args"

operator (x:y:stack) "-"    = (y - x):stack
operator (x:stack) "-"      = case stack of [] -> error "-: not enough args"

operator (x:y:stack) "/"    = (y / x):stack
operator (x:stack) "/"      = case stack of [] -> error "/: not enough args"

operator stack "+all"       = [sum stack]             
operator stack "*all"       = [foldl (*) 1 stack]   

-- unary operators
operator (x:stack) "sqrt"   = (sqrt x):stack          
operator (x:stack) "dup"    =  x:x:stack                
operator (x:stack) "sin"  = (sin x):stack             
operator (x:stack) "cos"    = (cos x):stack           
operator (x:stack) "inc"    = (x + 1):stack           
operator (x:stack) "dec"    = (x - 1):stack           
operator (x:stack) "inv"    = (1 / x):stack           
operator (x:stack) "pop"    = stack                   
operator stack "clear"      = []                      
operator (x:y:stack) "swap" = y:x:stack      

-- uneary operators error handling
operator stack numOrOp      = if (numOrOp `elem` ops) then handleError numOrOp else read numOrOp:stack
                                where ops = [
                                                "swap", "clear", "pop", "inv", "dec",
                                                "cos", "sin", "dup", "sqrt", "inc"
                                            ]


-- invokes operator function on list of tokens                                          
evaluator :: String -> [Double] 
evaluator input_stream = foldl operator [] (words input_stream)

calc :: String -> String
calc input_stream
    | null result = "empty stack"
    | otherwise   = show . head $ result
        where result = evaluator input_stream


-- References:
-- 1. http://learnyouahaskell.com/syntax-in-functions
-- 2. http://www.sfu.ca/~tjd/383summer2019/haskell_comp_and_app_lhs.html