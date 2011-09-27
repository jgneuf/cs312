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
divisors n =
	divisorsHelper n 1 []

-- divisorsHelper returns a list of integers that divide the given 
-- argument. The second argument is the potential divisor to be 
-- tested on the current recursive call, and the third argument 
-- is a list of divisors.
divisorsHelper :: Int -> Int -> [Int] -> [Int]
divisorsHelper num div lst
	| div == num			= num:lst
	| num `mod` div /= 0 	= divisorsHelper num (div + 1) lst
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
isPrimeList lst = map isPrime lst

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
{-
let studentDB = [
				 ("sally", 	["cpsc110", "cpsc312", "cpsc204"]),
				 ("jim", 	["cpsc110", "cpsc313"]),
				 ("bob", 	["cpsc121", "cpsc303", "cpsc212"]),
				 ("frank", 	["cpsc110", "cpsc212", "cpsc204"]),
				 ("billy", 	["cpsc312", "cpsc236"]),
				 ("jane", 	["cpsc121"]),
				 ("larry", 	["cpsc411", "cpsc236"]) ]
-}

-- studentClasses takes a name and returns the classes the student has
-- taken by accessing the "snd" part of the database tuple.
--studentClasses :: [Char] -> [Char]
studentClasses s d = head [snd x | x <- d, fst x == s]

