{-
	Student Name: 	Jonathan Neufeld
	Student ID: 	30671093
	CS ID:			p9d8
-}

-- Bring in HUnit for some simple unit testing.
import Test.HUnit

-- Simple unit tests, everything runs but you're free to check it ;)
main =  runTestTT (TestList [
	(TestCase (assertEqual ""	True	(isNum "5"))),
	(TestCase (assertEqual ""	True	(isNum "109876654423"))),
	(TestCase (assertEqual ""	False	(isNum "111a0002"))),
	(TestCase (assertEqual ""	True	(isNum' "5"))),
	(TestCase (assertEqual ""	True	(isNum' "109876654423"))),
	(TestCase (assertEqual ""	False	(isNum' "111a0002"))),
	(TestCase (assertEqual ""	[1]			(divisors 1))),
	(TestCase (assertEqual ""	[1,2,3,6]	(divisors 6))),
	(TestCase (assertEqual ""	[1,7]		(divisors 7))),
	(TestCase (assertEqual ""	False	(isPrime 1))),
	(TestCase (assertEqual ""	True	(isPrime 2))),
	(TestCase (assertEqual ""	True	(isPrime 3))),
	(TestCase (assertEqual ""	False	(isPrime 4))),
	(TestCase (assertEqual ""	True	(isPrime 5))),
	(TestCase (assertEqual ""	[False,True,True,False,True] 
										(isPrimeList [1..5]))),
	(TestCase (assertEqual ""	[1]		(removeDups[1,1]))),
	(TestCase (assertEqual ""	[1,2,3]	(removeDups [1,1,2,3,2]))),
	(TestCase (assertEqual ""	["cpsc411","cpsc236"]
									(	studentClasses "larry"))),
	(TestCase (assertEqual ""	["cpsc121"]	
										(studentClasses "jane"))),
	(TestCase (assertEqual ""	2		(countClasses "larry"))),
	(TestCase (assertEqual ""	3		(countClasses "bob"))),
	(TestCase (assertEqual ""	1		(countStudents "cpsc411"))),
	(TestCase (assertEqual ""	2		(countStudents "cpsc121"))),
	(TestCase (assertEqual ""	"V"		(romanDigit '5'))),
	(TestCase (assertEqual ""	"IX"	(romanDigit '9'))),
	(TestCase (assertEqual ""	0	(multiply 0 1))),
	(TestCase (assertEqual ""	0	(multiply 1 0))),
	(TestCase (assertEqual ""	2	(multiply 1 2))),
	(TestCase (assertEqual ""	2	(multiply 2 1))),
	(TestCase (assertEqual ""	12	(multiply 4 3))),
	(TestCase (assertEqual ""	12	(multiply 3 4))),
	(TestCase (assertEqual ""	0	(multiply_tr 0 1))),
	(TestCase (assertEqual ""	0	(multiply_tr 1 0))),
	(TestCase (assertEqual ""	2	(multiply_tr 1 2))),
	(TestCase (assertEqual ""	2	(multiply_tr 2 1))),
	(TestCase (assertEqual ""	12	(multiply_tr 4 3))),
	(TestCase (assertEqual ""	12	(multiply_tr 3 4))),
	(TestCase (assertEqual ""	0	(power 0 1))),
	(TestCase (assertEqual ""	1	(power 1 0))),
	(TestCase (assertEqual ""	1	(power 1 2))),
	(TestCase (assertEqual ""	2	(power 2 1))),
	(TestCase (assertEqual ""	8	(power 2 3))),
	(TestCase (assertEqual ""	243	(power 3 5))),
	(TestCase (assertEqual ""	0	(power_tr 0 1))),
	(TestCase (assertEqual ""	1	(power_tr 1 0))),
	(TestCase (assertEqual ""	1	(power_tr 1 2))),
	(TestCase (assertEqual ""	2	(power_tr 2 1))),
	(TestCase (assertEqual ""	8	(power_tr 2 3))),
	(TestCase (assertEqual ""	243	(power_tr 3 5)))
		])

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

-- isPrime returns true if the given argument is a prime number. This
-- function uses the divisors function to check if the number of 
-- divisors is two, i.e. 1 and the argument.
isPrime :: Int -> Bool
isPrime n
	| n <= 0					= False
	| length (divisors n) == 2	= True
	| otherwise					= False

-- isPrimeList takes a list of potential prime numbers and returns a
-- list of boolean values that correspond with the primality of each 
-- number.
isPrimeList :: [Int] -> [Bool]
isPrimeList lst =
	if null lst
		then []
		else isPrime (head lst) : isPrimeList (tail lst)

-- removeDups takes a list and returns a list containing all the same
-- elements but without duplicates. The helper function returns the 
-- right list but in the reverse order, so flip it back.
removeDups :: (Eq a) => [a] -> [a]
removeDups lst = 
	if null lst
		then []
		else reverse (removeDupsHelper lst [])

-- removeDupsHelper removes duplicates from a list of a comparable type.
-- There's probably a slick way to do this with higher order functions
-- but this is a simple solution. To return the list in the right order,
-- the calling function simply reverses this list.
removeDupsHelper :: (Eq a) => [a] -> [a] -> [a]
removeDupsHelper x y
	| null x			= y
	| head x `elem` y	= removeDupsHelper (tail x) y
	| otherwise 		= removeDupsHelper (tail x) (head x : y)

-- My simple database with a few students is a list of tuples. Each
-- tuple stores the student's name and a list of classes. Nice and
-- simple.
studentDB :: [([Char], [[Char]])]
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

-- countClasses takes a name and returns the number of classes the student is taking.
countClasses :: [Char] -> Int
countClasses s = length (studentClasses s)

-- countStudents takes a class name and returns the number of students enrolled.
countStudents :: [Char] -> Int
countStudents c = countStudentsHelper c studentDB 0

-- countStudentsHelper walks the list of students and counts how many are in given class.
countStudentsHelper :: (Num a2, Eq a) => a -> [(a1, [a])] -> a2 -> a2
countStudentsHelper c d a
	| null d					= a
	| c `elem` snd (head (d)) 	= countStudentsHelper c (tail d) (a + 1)
	| otherwise 				= countStudentsHelper c (tail d) a

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

-- multiply takes two non-negative integers and multiplies them without using tail recursion.
multiply :: Int -> Int -> Int
multiply n1 n2
	| n1 == 0 	= 0
	| n2 == 0 	= 0
	| n2 == 1 	= n1
	| otherwise = n1 + multiply n1 (n2 - 1)

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

-- power raises the first argument to second without using tail recursion.
power :: Int -> Int -> Int
power b e
	| b == 0	= 0
	| e == 0	= 1
	| e == 1	= b
	| otherwise = multiply b (power b (e - 1))

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
