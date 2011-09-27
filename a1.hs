-- isNum returns true if the given string contains only numbers, i.e. 
-- each character is in the range of 0 to 9, by recursively checking 
-- the first element of the string.
isNum :: String -> Bool
isNum s
	| null s					= True
	| head s `elem` ['0'..'9']	= isNum (tail s)
	| otherwise 				= False

-- isNum' returns true if the given string contains only numbers, i.e.
-- each character is in the range of 0 to 9, by checking the length 
-- of the string and the length of a new character list generated with 
-- list comprehensions.
isNum' :: String -> Bool
isNum' s =
	if length [x | x <- s, x `elem` ['0'..'9']] == length s
		then True
		else False
	
-- Test cases for isNum1
{- 
isNum "5" == True
isNum "9123456780" == True
isNum "1234a" == False 
-}

-- Test cases for isNum2
{-
isNum' "090182340186710384"
isNum' "0901823401867a10384"
-}

-- divisors returns a list of integers that divide the given argument. 
-- This function calls a helper funtion to abstract unnecessary 
-- complexity from the user.
divisors :: Int -> [Int]
divisors n = divisorsHelper n 1 []

-- divisorsHelper returns a list of integers that divide the given 
-- argument. The second argument is the potential divisor to be 
-- tested on the current recursive call, and the third argument 
-- is a list of divisors.
divisorsHelper :: Int -> Int -> [Int] -> [Int]
divisorsHelper num div lst
	| div == num			= num:lst
	| num `mod` div /= 0	= divisorsHelper num (div + 1) lst
	| otherwise				= div : divisorsHelper num (div + 1) lst

-- Tests for divisors
{-
divisors 1 == [1]
divisors 6 == [1,2,3,6]
divisors 7 == [1,7]
-}

-- isPrime returns true if the given argument is a prime number. This
-- function uses the divisors function to check if the number of 
-- divisors is two, i.e. 1 and the argument.
isPrime :: Int -> Bool
isPrime n
	| n <= 0					= False
	| length (divisors n) == 2	= True
	| otherwise					= False

-- Tests for isPrime
{-
isPrime 0 == False
isPrime 1 == False
isPrime 2 == True
isPrime 3 == True
isPrime 12 == False
isPrime 17 == True
-}

-- isPrimeList takes a list of potential prime numbers and returns a
-- list of boolean values that correspond with the primality of each 
-- number.
isPrimeList :: [Int] -> [Bool]
isPrimeList lst =
	if null lst
		then []
		else isPrime (head lst) : isPrimeList (tail lst)

-- isPrimeList tests
{-
isPrimeList [] == []
isPrimeList [1] == [False]
isPrimeList  [1..5] == [False, True, True, False, True]
-}

-- removeDups takes a list and returns a list containing all the same
-- elements but without duplicates. The helper function returns the 
-- right list but in the reverse order, so flip it back.
removeDups :: (Eq a) => [a] -> [a]
removeDups lst = reverse (removeDupsHelper lst [])

-- removeDupsHelper removes duplicates from a list of a comparable type.
-- There's probably a slick way to do this with higher order functions
-- but this is a simple solution. To return the list in the right order,
-- the calling function simply reverses this list.
removeDupsHelper :: (Eq a) => [a] -> [a] -> [a]
removeDupsHelper x y
	| null x			= y
	| head x `elem` y	= removeDupsHelper (tail x) y
	| otherwise 		= removeDupsHelper (tail x) (head x : y)

-- Test cases for removeDups
{-
removeDups [] == []
removeDups [1] == [1]
removeDups [1,1,2,3,6,2] == [1,2,3,6]
-}

-- My simple database with a few students is a list of tuples. Each
-- tuple stores the student's name and a list of classes. Nice and
-- simple.
studentDB = [
	("sally", 	["cpsc110", "cpsc312", "cpsc204"]),
	("jim", 	["cpsc110", "cpsc313"]),
	("bob", 	["cpsc121", "cpsc303", "cpsc212"]),
	("frank", 	["cpsc110", "cpsc212", "cpsc204"]),
	("billy", 	["cpsc312", "cpsc236"]),
	("jane", 	["cpsc121"]),
	("larry", 	["cpsc411", "cpsc236"]) ]

-- studentClasses takes a name and returns the classes the student has
-- taken by accessing the "snd" part of the database tuple.
studentClasses :: [Char] -> [[Char]]
studentClasses s = head [snd x | x <- studentDB, fst x == s]

-- Test cases for studentClasses
{-
studentClasses "larry" = ["cpsc411", "cpsc236"]
studentClasses "jane" = ["cpsc121"]
-}

-- countClasses takes a name and returns the number of classes the student is taking.
countClasses :: [Char] -> Int
countClasses s = length (studentClasses s)

-- Test cases for countClasses
{-
countClasses "larry" == 2
countClasses "jane" == 1
countClasses "bob" = 3
-}

-- countStudents takes a class name and returns the number of students enrolled.
countStudents :: [Char] -> Int
countStudents c = countStudentsHelper c studentDB 0

-- countStudentsHelper walks the list of students and counts how many are in given class.
countStudentsHelper :: (Num a2, Eq a) => a -> [(a1, [a])] -> a2 -> a2
countStudentsHelper c d a
	| null d					= a
	| c `elem` snd (head (d)) 	= countStudentsHelper c (tail d) (a + 1)
	| otherwise 				= countStudentsHelper c (tail d) a

-- Test cases for countClasses
{-
countStudents "cpsc411" == 1
countStudents "cpsc121" == 2
-}

-- romanDigit takes a single character in ['0'..'9'] and returns itsroman numeral.
romanDigit :: Char -> String
romanDigit num
	| num == '0' = ""
	| num == '9' = "IX"
	| num == '8' = "VIII"
	| num == '7' = "VII"
	| num == '6' = "VI"
	| num == '5' = "V"
	| num == '4' = "IV"
	| num == '3' = "III"
	| num == '2' = "II"
	| num == '1' = "I"

-- Test cases for romanDigit
{-
romanDigit '0' == ""
romanDigit '4' == "IV"
romanDigit '9' == "IX"
-}

-- multiply takes two non-negative integers and multiplies them without using tail recursion.
multiply :: Int -> Int -> Int
multiply n1 n2
	| n1 == 0 	= 0
	| n2 == 0 	= 0
	| n2 == 1 	= n1
	| otherwise = n1 + multiply n1 (n2 - 1)

-- Test cases for multiply
{-
multiply 1 0 == 0
multiply 0 1 == 0
multiply 2 1 == 2
multiply 1 2 == 2
multiply 3 4 == 12
multiply 4 2 == 8
-}

-- multiply_tr takes two non-negative integers and multiplies them using tail recursion.
multiply_tr :: Int -> Int -> Int
multiply_tr n1 n2
	| n1 == 0  = 0
	| n2 == 0  = 0
	| n1 == 1  = n2
	| n2 == 1  = n1
	|otherwise = multiply_tr_helper n1 n2 0

-- multiply_tr_helper takes two numbers and stores the product through repeated addition
-- using an accumulator.
multiply_tr_helper :: Int -> Int -> Int -> Int
multiply_tr_helper n1 n2 acc
	| n2 == 0 	= acc
	| otherwise = multiply_tr_helper n1 (n2 - 1) (n1 + acc)
	
-- Test cases for multiply_tr
{-
multiply_tr 1 0 == 0
multiply_tr 0 1 == 0
multiply_tr 2 1 == 2
multiply_tr 1 2 == 2
multiply_tr 3 4 == 12
multiply_tr 4 2 == 8
-}

-- power raises the first argument to second without using tail recursion.
power :: Int -> Int -> Int
power b e
	| b == 0	= 0
	| e == 0	= 1
	| e == 1	= b
	| otherwise = multiply b (power b (e - 1))
	
-- Test cases for power
{-
power 0 1 == 0
power 1 0 == 1
power 0 2 == 0
power 2 0 == 1
power 1 3 == 1
power 3 1 == 3
power 2 3 == 8
-}

-- power_tr raises the first argument to second using tail recursion.
power_tr :: Int -> Int -> Int
power_tr b e
	| b == 0	= 0
	| e == 0	= 1
	| otherwise = power_tr_helper b e 1
	
-- power_tr_helper computes a power using tail recursion with the help of an accumulator.
power_tr_helper :: Int -> Int -> Int -> Int
power_tr_helper b e a
	| e == 0	= a
	| otherwise = power_tr_helper b (e - 1) (multiply b a)
	
-- Test cases for power_tr
{-
power_tr 0 1 == 0
power_tr 1 0 == 1
power_tr 0 2 == 0
power_tr 2 0 == 1
power_tr 1 3 == 1
power_tr 3 1 == 3
power_tr 2 3 == 8
-}