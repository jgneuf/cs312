-- Drop the given number of elements from the given list.
mynthtail num lst
	| null lst	= []
	| num <= 0	= lst
	| otherwise = mynthtail (num - 1) (tail lst)

-- Drop the given number of elements from the given list.
mynthtail_pm 0 lst = lst
mynthtail_pm n (_:xs) = mynthtail (n - 1) xs

-- Return a list in reverse order.
myreverse :: [a] -> [a]
myreverse lst = myreverseHelper lst []

-- Use recursion to reverse the elements of a list using accumulator.
myreverseHelper :: [a] -> [a] -> [a]
myreverseHelper lst acc
	| null lst	= acc
	| otherwise = myreverseHelper (tail lst) (head lst : acc)

myreverse_pm [] 	= []

mylast lst 
	| null lst  = []
	| otherwise = head (myreverse lst)

mylast_pm []	 = []
mylast_pm (_:xs) = mylast_pm xs

-- Replace all of second arg with first arg (ugly signature).
myreplaceall x y lst
	| null lst			= []
	| (head lst) == y	= x : myreplaceall x y (tail lst)
	| otherwise 		= (head lst) : myreplaceall x y (tail lst)

myreplaceall_pm x y [] 		= []
myreplaceall_pm x a (a:lst) = x : myreplaceall_pm x a lst
myreplaceall_pm x a (b:lst) = b : myreplaceall_pm x a lst
