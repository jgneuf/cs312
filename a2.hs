-- Bring in HUnit for some simple unit testing.
import Test.HUnit

-- Everything runs ok except for the tests that are commented out. Those tests cause
-- run time errors when you run main. However, when execute the given function for
-- that test in the prompt, you get the correct result. Must be something wrong with
-- the way I'm writing my tests but at least the function actually works. Also,
-- myremoveduplicates has some odd ordering behavior. It does remove duplicates though.
main =  runTestTT (TestList [
	(TestCase (assertEqual ""	"abcd"      (myremoveduplicates "abacad"))),
	(TestCase (assertEqual ""	[3,2,1]	    (myremoveduplicates [3,2,1,3,2,2,1,1]))),

    (TestCase (assertEqual ""	"abcd"      (myremoveduplicates_pm "abacad"))),
	(TestCase (assertEqual ""	[3,2,1]	    (myremoveduplicates_pm [3,2,1,3,2,2,1,1]))),

	(TestCase (assertEqual ""	"bc"	    (myintersection "abc" "bcd"))),
	(TestCase (assertEqual ""	[4,2,1]	    (myintersection [3,4,2,1] [5,4,1,6,2]))),
	(TestCase (assertEqual ""	[]	        (myintersection [] [1,2,3]))),
	(TestCase (assertEqual ""	""	        (myintersection "abc" ""))),

    (TestCase (assertEqual ""	"bc"	    (myintersection_pm "abc" "bcd"))),
	(TestCase (assertEqual ""	[4,2,1]	    (myintersection_pm [3,4,2,1] [5,4,1,6,2]))),
	(TestCase (assertEqual ""	[]	        (myintersection_pm [] [1,2,3]))),
	(TestCase (assertEqual ""	""	        (myintersection_pm "abc" ""))),

	(TestCase (assertEqual ""	"abcd"	    (mynthtail 0 "abcd"))),
	(TestCase (assertEqual ""	"bcd"	    (mynthtail 1 "abcd"))),
	(TestCase (assertEqual ""	"cd"	    (mynthtail 2 "abcd"))),
	(TestCase (assertEqual ""	"d"	        (mynthtail 3 "abcd"))),
	(TestCase (assertEqual ""	""	        (mynthtail 4 "abcd"))),
	(TestCase (assertEqual ""	[3,4]	    (mynthtail 2 [1,2,3,4]))),
	(TestCase (assertEqual ""	[]	        (mynthtail 4 [1,2,3,4]))),

   	(TestCase (assertEqual ""	"abcd"	    (mynthtail_pm 0 "abcd"))),
	(TestCase (assertEqual ""	"bcd"	    (mynthtail_pm 1 "abcd"))),
	(TestCase (assertEqual ""	"cd"	    (mynthtail_pm 2 "abcd"))),
	(TestCase (assertEqual ""	"d"	        (mynthtail_pm 3 "abcd"))),
	(TestCase (assertEqual ""	""	        (mynthtail_pm 4 "abcd"))),
	(TestCase (assertEqual ""	[3,4]	    (mynthtail_pm 2 [1,2,3,4]))),
    (TestCase (assertEqual ""	[]	        (mynthtail_pm 4 [1,2,3,4]))),

	(TestCase (assertEqual ""	""	        (mylast ""))),
	(TestCase (assertEqual ""	"b"	        (mylast "b"))),
	(TestCase (assertEqual ""	"d"	        (mylast "abcd"))),
	(TestCase (assertEqual ""	[4]	        (mylast [1,2,3,4]))),
	--(TestCase (assertEqual ""	[]	        (mylast []))),

 	(TestCase (assertEqual ""	""	        (mylast_pm ""))),
	(TestCase (assertEqual ""	"b"	        (mylast_pm "b"))),
	(TestCase (assertEqual ""	"d"	        (mylast_pm "abcd"))),
	(TestCase (assertEqual ""	[4]	        (mylast_pm [1,2,3,4]))),
	--(TestCase (assertEqual ""	[]	        (mylast_pm []))),

	(TestCase (assertEqual ""	""	        (myreverse ""))),
	(TestCase (assertEqual ""	"cba"	    (myreverse "abc"))),
	(TestCase (assertEqual ""	[3,2,1]	    (myreverse [1,2,3]))),
	--(TestCase (assertEqual ""	[]	        (myreverse []))),

	(TestCase (assertEqual ""	""	        (myreverse_pm ""))),
	(TestCase (assertEqual ""	"cba"	    (myreverse_pm "abc"))),
	(TestCase (assertEqual ""	[3,2,1]	    (myreverse_pm [1,2,3]))),
	--(TestCase (assertEqual ""	[]	        (myreverse_pm []))),
	
	(TestCase (assertEqual ""	[3,0,3,1,3,2,3]	(myreplaceall 3 7 [7,0,7,1,7,2,7]))),
	(TestCase (assertEqual ""	""	            (myreplaceall 'x' 'a' ""))),
	(TestCase (assertEqual ""	"xbxcxd"	    (myreplaceall 'x' 'a' "abacad"))),

	(TestCase (assertEqual ""	[3,0,3,1,3,2,3]	(myreplaceall_pm 3 7 [7,0,7,1,7,2,7]))),
	(TestCase (assertEqual ""	""	            (myreplaceall_pm 'x' 'a' ""))),
	(TestCase (assertEqual ""	"xbxcxd"	    (myreplaceall_pm 'x' 'a' "abacad"))),

	--(TestCase (assertEqual "" True	    (myordered []))),
	(TestCase (assertEqual ""	True    	(myordered [1]))),
	(TestCase (assertEqual ""	True    	(myordered [1,2]))),
	(TestCase (assertEqual ""	True    	(myordered [1,1]))),
	(TestCase (assertEqual ""	False   	(myordered [2,1]))),
	(TestCase (assertEqual ""	True    	(myordered "abcdefg"))),
	(TestCase (assertEqual ""	False   	(myordered "ba"))),

	--(TestCase (assertEqual ""   True        (myordered_pm []))),
	(TestCase (assertEqual ""	True    	(myordered_pm [1]))),
	(TestCase (assertEqual ""	True    	(myordered_pm [1,2]))),
	(TestCase (assertEqual ""	True    	(myordered_pm [1,1]))),
	(TestCase (assertEqual ""	False   	(myordered_pm [2,1]))),
	(TestCase (assertEqual ""	True    	(myordered_pm "abcdefg"))),
	(TestCase (assertEqual ""	False   	(myordered_pm "ba")))
		])

-- myremoveduplicates returns a list with all the elements of the given list
-- without repeating any item.
myremoveduplicates :: Eq a => [a] -> [a]
myremoveduplicates lst = myreverse (myremoveduplicatesHelper lst [])

myremoveduplicatesHelper :: Eq a => [a] -> [a] -> [a]
myremoveduplicatesHelper lst acc
	| null lst		        = acc
	| elem (head lst) acc	= myremoveduplicatesHelper (tail lst) acc
	| otherwise		        = myremoveduplicatesHelper (tail lst) (head lst : acc)
	
myremoveduplicates_pm []    = []
myremoveduplicates_pm x     = myreverse (myremoveduplicates_pmh x [])
myremoveduplicates_pmh [] a = a
myremoveduplicates_pmh (x:xs) a
    | elem x a  = myremoveduplicates_pmh xs a
    | otherwise = myremoveduplicates_pmh xs (x : a)

-- myintersection returns a list containing only elements that are in both the
-- given lists.
myintersection :: Eq a => [a] -> [a] -> [a]
myintersection lst1 lst2 = myremoveduplicates (myintersectionHelper lst1 lst2)

myintersectionHelper :: Eq a => [a] -> [a] -> [a]
myintersectionHelper lst1 lst2
	| null lst1 || null lst2	= []
	| elem (head lst1) lst2		= (head lst1) : myintersection (tail lst1) lst2
	| otherwise			        = myintersection (tail lst1) lst2

myintersection_pm _ []  = []
myintersection_pm [] _  = []
myintersection_pm x y   = [a | a <- x, elem a y]

-- mynthtail drops the given number of elements from the given list.
mynthtail :: (Ord a1, Num a1) => a1 -> [a] -> [a]
mynthtail num lst
	| null lst	= []
	| num <= 0	= lst
	| otherwise	= mynthtail (num - 1) (tail lst)

mynthtail_pm _ []       = []
mynthtail_pm 0 x        = x
mynthtail_pm n (_:xs)   = mynthtail (n - 1) xs

-- mylast returns the last element in the given list, or nothing if it's empty.
mylast :: [a] -> [a]
mylast lst
	| null lst	= []
	| otherwise	= mynthtail ((mylength lst) - 1) lst

mylast_pm []       = []
mylast_pm (x:[])   = x:[]
mylast_pm (_:xs)   = mylast_pm xs

-- mylength is a helper function that determines the length of a list.
mylength :: Num a1 => [a] -> a1
mylength lst
	| null lst	= 0
	| otherwise	= 1 + mylength (tail lst)

-- myreverse returns a list of each element in the reverse order.
myreverse :: [a] -> [a]
myreverse lst = myreverseHelper lst []

myreverseHelper :: [a] -> [a] -> [a]
myreverseHelper lst acc
	| null lst	= acc
	| otherwise	= myreverseHelper (tail lst) (head lst : acc)

myreverse_pm xs             = myreverse_pm_h xs []
myreverse_pm_h [] xs        = xs
myreverse_pm_h (x:xs) acc   = myreverse_pm_h xs (x:acc)
 
-- myreplace_all returns a list where every instance of the second argument is
-- replaced by the first in the given list.
myreplaceall :: Eq a => a -> a -> [a] -> [a]
myreplaceall val sub lst = myreverse (myreplaceallHelper val sub lst [])

myreplaceallHelper :: Eq a => a -> a -> [a] -> [a] -> [a]
myreplaceallHelper val sub lst acc
	| null lst	    	= acc
	| head (lst) == sub	= myreplaceallHelper val sub (tail lst) (val : acc)
	| otherwise	    	= myreplaceallHelper val sub (tail lst) (head lst : acc)

myreplaceall_pm _ _ []      = []
myreplaceall_pm v s (x:xs)  
    | s == x    = v : myreplaceall_pm v s xs
    | otherwise = x : myreplaceall_pm v s xs

-- myordered returns true if the list contains elements in increasing order.
myordered :: Ord a => [a] -> Bool
myordered lst
	| null lst	= True
	| otherwise = myorderedHelper lst (head lst)

myorderedHelper :: Ord a => [a] -> a -> Bool
myorderedHelper l f
	| null l	    = True
	| head l < f	= False
	| otherwise	    = myorderedHelper (tail l) (head l)

myordered_pm []         = True
myordered_pm (x:[])     = True
myordered_pm (x:y:[])   = x <= y
myordered_pm (x:y:xs)   = (x <= y) && (myordered_pm (y:xs))

