-- Name: Ahsan Naveed
-- Course: CMPT 383 Comparative Programming Languages
-- Assignment 3


-- Question 1
-- Reference: https://wiki.haskell.org/Syntactic_sugar/Cons
snoc :: a -> [a] -> [a]
snoc elem []     = [elem]
snoc elem (x:xs) = x : (snoc elem xs)

--------------------------------------------------------------------

-- Question 2
myappend :: [a] -> [a] -> [a]
myappend elem []         = elem
myappend elem (x:xs)     = (myappend (snoc x elem) xs)

--------------------------------------------------------------------
                                    
-- Question 3
-- Reference: http://www.sfu.ca/~tjd/383summer2019/haskell_functions_lhs.html
len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + (len xs)

myreverse :: [a] -> [a]
myreverse lst
    | (len lst) <= 1 = lst
    | otherwise      = (myappend
                           (myreverse (tail lst))
                           [head lst])

--------------------------------------------------------------------

-- Question 4
-- Reference: http://www.sfu.ca/~tjd/383summer2019/haskell_functions_lhs.html
smallest_divisor :: Integer -> Integer
smallest_divisor n
    | n < 0     = error "n must be >= 0"
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = head (dropWhile (\x -> n `mod` x /= 0) [2..n])

-- Reference: http://www.sfu.ca/~tjd/383summer2019/haskell_functions_lhs.html
is_prime :: Integer -> Bool
is_prime n | n < 2     = False
           | otherwise = (smallest_divisor n) == n

-- Reference: https://stackoverflow.com/questions/19725292/how-to-reverse-an-integer-in-haskell
reverse_int :: Integer -> Integer
reverse_int x | x < 0    = 0 - (read . myreverse . tail . show $ x)
             | otherwise = read . myreverse . show $ x

is_emirp :: Integer -> Bool
is_emirp n
    | n == reverse_int n = False
    | otherwise          = ((is_prime (reverse_int n)) && is_prime n)

count_emirps :: Integer -> Integer
count_emirps n
    | n < 13     = 0
    | is_emirp n = (1 + (count_emirps (n - 1)))
    | otherwise  = (count_emirps (n - 1))

--------------------------------------------------------------------

-- Question 5
f_maximizer :: (a -> Int) -> a ->[a] -> a
f_maximizer f x xs
    | null xs           = x
    | f x > f (head xs) = f_maximizer f x (tail xs)
    | otherwise         = f_maximizer f (head xs) (tail xs)

biggest_sum :: [[Int]] -> [Int]
biggest_sum (x:xs) = f_maximizer sum x xs

--------------------------------------------------------------------

--Question 6
greatest :: (a -> Int) -> [a] -> a
greatest f (x:xs) = f_maximizer f x xs

--------------------------------------------------------------------

-- Question 7
is_bit :: Int -> Bool
is_bit x
    | (x == 1 || x == 0) = True
    | otherwise          = False

--------------------------------------------------------------------

-- Question 8
flip_bit :: Int -> Int
flip_bit x
    | x == 1    = 0
    | x == 0    = 1
    | otherwise = error "x should be either 1 or 0"

--------------------------------------------------------------------

-- Question 9-a
is_bit_seq1 :: [Int] -> Bool
is_bit_seq1 lst
    | null lst  = True
    | otherwise = (is_bit (head lst)) && (is_bit_seq1 (tail lst))

-- Question 9-b
is_bit_seq2 :: [Int] -> Bool
is_bit_seq2 lst = if (null lst) then True
                  else (is_bit (head lst)) && (is_bit_seq2 (tail lst))

-- Question 9-c
-- Reference: http://learnyouahaskell.com/modules
is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 []  = True
is_bit_seq3 lst = all is_bit lst

--------------------------------------------------------------------

-- Question 10-a
invert_bits1 :: [Int] -> [Int]
invert_bits1 lst
    | null lst  = []
    | otherwise = (flip_bit (head lst)) : (invert_bits1 (tail lst))

-- Question 10-b
-- Reference: http://learnyouahaskell.com/higher-order-functionss
invert_bits2 :: [Int] -> [Int]
invert_bits2 lst = map flip_bit lst

-- Question 10-c
-- Reference: http://learnyouahaskell.com/starting-out#im-a-list-comprehension
invert_bits3 :: [Int] -> [Int]
invert_bits3 lst = [(flip_bit x) | x <- lst]

--------------------------------------------------------------------

-- Question 11
bit_count :: [Int] -> (Int, Int)
bit_count []  = (0, 0)
bit_count lst = (len [x | x <- lst, x == 0], len [x | x <- lst, x == 1])

--------------------------------------------------------------------

-- Question 12
prefix_bit :: Int -> [[Int]] -> [[Int]]
prefix_bit bit lst = [bit : x | x <- lst]

all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs n
    | n < 0     = error "n must be >= 0"
    | n == 0    = []
    | n == 1    = [[0], [1]]
    | otherwise = (myappend 
                    (prefix_bit 0 (all_basic_bit_seqs (n - 1)))
                    (prefix_bit 1 (all_basic_bit_seqs (n - 1))))

--------------------------------------------------------------------

-- Question 13
-- Reference: http://www.sfu.ca/~tjd/383summer2019/a3.html#a-custom-list-data-type
data List a = Empty | Cons a (List a)
    deriving Show

toList :: [a] -> List a
toList []  = Empty
toList (x:xs) = (Cons x (toList xs))

--------------------------------------------------------------------

-- Question 14
toHaskellList :: List a -> [a]
toHaskellList Empty            = []
toHaskellList (Cons x xs) = x : (toHaskellList xs)

--------------------------------------------------------------------

-- Question 15
snoc_List :: a -> List a -> List a
snoc_List elem Empty       = (Cons elem Empty)
snoc_List elem (Cons x xs) = (Cons x (snoc_List elem xs))

append :: List a -> List a -> List a
append elem Empty       = elem
append elem (Cons x xs) = (append (snoc_List x elem) xs)

--------------------------------------------------------------------

-- Question 16
removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty       = Empty
removeAll f (Cons x xs) = if (f x) then (removeAll f xs) else (Cons x (removeAll f xs))

--------------------------------------------------------------------

-- Question 17
-- Ord a => means that the type a must be usable with comparisons functions such as <, <=, ==, etc.
-- Reference: http://www.sfu.ca/~tjd/383summer2019/haskell_functions_lhs.html
sort :: Ord a => List a -> List a
sort Empty  = Empty
sort (Cons x xs) = (append (append smalls middle) bigs)
                   where smalls = sort (removeAll (\e -> e > x) xs)
                         middle = (Cons x Empty)
                         bigs   = sort (removeAll (\e -> e <= x) xs)



