import System.Random
import System.IO.Unsafe



users = [ "user1" ,"user2" , "user3" , "user4"]
items = [ "item1" , "item2" , "item3" , "item4" , "item5" , "item6"]
purchasesHistory = [("user1",[["item1","item2","item3"],["item1","item2","item4"]]) ,("user2",[["item2" , "item5"] , ["item4" , "item5"]]),("user3" , [[ "item3" , "item2"]]) ,("user4" ,[])]
--cart =  ["item2", "item4", "item6"]

createEmptyFreqList [] = []
createEmptyFreqList (x:xs) = helper_cefl x : createEmptyFreqList xs
helper_cefl x = (x,[])

getAllUserStats :: Num a => [(b,[[[Char]]])] -> [([Char],[([Char],[([Char],a)])])]
getAllUserStats ((s,x):xs) = getAllUserStatsHelper users ((s,x):xs)
getAllUserStatsHelper (u:us) ((s,x):xs) = (pop_user u items (createEmptyFreqList items) x):(getAllUserStatsHelper us xs)
getAllUserStatsHelper [] _ = []
getAllUserStatsHelper _ [] = []

pop_helper [] [] _ = []
pop_helper (item:restitems) (x:xs) h = ((item,(pop_helper1 x h)):(pop_helper restitems xs h))
pop_helper1 (i,[]) h = counta i items h

pop_user u (item:restitems) (x:xs) h = (u,(pop_helper (item:restitems) (x:xs) h))

counta _ [] _ = []
counta i (x:xs) xx = if i /= x then clean ((countb i x xx 0):(counta i xs xx)) else clean (counta i xs xx)
countb a b [] c = (b,c)
countb a b (x:xs) c = if (elem a x) then  countb a b xs (c+occ b x )  else countb a b xs c

occ i [] = 0
occ i (x:xs)  = if i ==x then 1+occ i xs else occ i xs

clean [] = []
clean ((s,c):xs) = if c == 0 then clean xs else ((s,c):clean xs)

purchasesIntersection :: (Num a, Eq b) => [(c,[(b,a)])] -> [(d,[(e,[(b,a)])])] -> [(c,[(b,a)])]
purchasesIntersection _ [] = []						 
purchasesIntersection ((it,[]):its) ((user,((it1,((i1,n1):is1)):its1)):us) = (intersectionHelperLoop ((it,[]):its) ((it1,((i1,n1):is1)):its1))++(purchasesIntersection  ((it,[]):its) (us))
purchasesIntersection ((it,((i,n):is)):its) ((user,((it1,[]):its1)):us) = (intersectionHelperLoop ((it,((i,n):is)):its) ((it1,[]):its1))++(purchasesIntersection  ((it,((i,n):is)):its) (us))
purchasesIntersection ((it,[]):its) ((user,((it1,[]):its1)):us) = (intersectionHelperLoop ((it,[]):its) ((it1,[]):its1))++(purchasesIntersection  ((it,[]):its) (us))
purchasesIntersection ((it,((i,n):is)):its) ((user,((it1,((i1,n1):is1)):its1)):us) = 
         (intersectionHelperLoop ((it,((i,n):is)):its) ((it1,((i1,n1):is1)):its1))++(purchasesIntersection  ((it,((i,n):is)):its) (us))

intersectionHelper ((i,n):is) [] = 	((i,n):is)						 
intersectionHelper ((i,n):is) ((i1,n1):is1) = if (itemPresent ((i,n):is) (i1,n1)== True )
                                             then intersectionHelper (replaceItem ((i,n):is) (itemResult ((i,n):is) (i1,n1))) is1
                                             else intersectionHelper ((i1,n1):(i,n):is) is1


intersectionHelperLoop ((it,[]):its) ((it1,((i1,n1):is1)):its1) = intersectionHelperLoop its its1
intersectionHelperLoop ((it,((i,n):is)):its) ((it1,[]):its1) = intersectionHelperLoop its its1
intersectionHelperLoop ((it,[]):its) ((it1,[]):its1) = intersectionHelperLoop its its1
intersectionHelperLoop _ [] = []
intersectionHelperLoop [] _ = []		 
intersectionHelperLoop ((it,((i,n):is)):its) ((it1,((i1,n1):is1)):its1) = (it,(intersectionHelper ((i,n):is) ((i1,n1):is1))):(intersectionHelperLoop its its1)


itemPresent [] _ = False
itemPresent ((i,n):is) (i1,n1) = if i == i1 then True else itemPresent is (i1,n1)

--No base case because the array will not be empty unless (i1,n1) is found
itemResult ((i,n):is) (i1,n1) = if i == i1 then (i,(n+n1)) else itemResult is (i1,n1)

replaceItem ((i,n):is) (i1,n1) = if i == i1 then ((i1,n1):is) else (i,n):(replaceItem is (i1,n1))

freqListUsers :: Num a => [Char] -> [([Char],a)]
freqListUsers user = cleanList (freqListHelper items (purchasesIntersection (getUserInput user (getAllUserStats purchasesHistory)) (excludeUser user (getAllUserStats purchasesHistory))))
freqListHelper [] _ = []
freqListHelper _ [] = []
-- freqListHelper takes the form ((it,((i,n):is)):its) and returns only (i,n) of all items with n's summed
freqListHelper (item:restItems) x = (item, (total item (cleanedItems x))):(freqListHelper restItems x)

cleanedItems1 [] = []
cleanedItems1 ((it,x):its) = (x:cleanedItems1 its)
cleanedItems x = concat (cleanedItems1 x)

total _ [] = 0
total item ((i,n):is) = if item == i then (n+(total item is)) else total item is 

cleanList [] = []
cleanList ((i,n):is) = if n == 0 then cleanList is else (i,n):cleanList is


getUserInput _ [] = error "User doesn't exist"
getUserInput u ((user,x):us) = if u==user then x else getUserInput u us

excludeUser _ [] = []
excludeUser u ((user,x):us) = if u==user then excludeUser u us else (user,x):(excludeUser u us)

fasas [] = []
fasas ((i,0):is) = fasas is
fasas ((i,n):is) = (i):(fasas ((i,n-1):is))

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

recommendBasedOnUsers :: [Char] -> [Char]
recommendBasedOnUsers user = if (length(fasas(freqListUsers user))) > 0 then pickRandom (fasas (freqListUsers user)) (randomZeroToX (length(fasas(freqListUsers user))-1)) else []

pickRandom (x:xs) 0 = x
pickRandom (x:xs) i = pickRandom xs (i-1)

freqListItems :: Num a => [Char] -> [([Char],a)]
freqListItems user = cleanList (freqListHelper items (getUserInput user (getAllUserStats purchasesHistory)))

freqListCartInput [] _ = []
freqListCartInput ((it,x):its) cartItem = if it == cartItem then [(it,x)] else freqListCartInput its cartItem
freqListCartInputLoop1 _ [] = []
freqListCartInputLoop1 x (cartItem:restCartItems) = (freqListCartInput x cartItem):(freqListCartInputLoop1 x restCartItems)
freqListCartInputLoop x (cartItem:restCartItems) = concat (freqListCartInputLoop1 x (cartItem:restCartItems))
freqListCart user cart  = cleanList (freqListHelper items (freqListCartInputLoop (getUserInput user (getAllUserStats purchasesHistory)) cart))

freqListCartAndItems :: Num a => [Char] -> [[Char]] -> [([Char],a)]
freqListCartAndItems user cart = cleanList (freqListHelper (items) ((getUserInput user (getAllUserStats purchasesHistory))++(freqListCartInputLoop (getUserInput user (getAllUserStats purchasesHistory)) cart)))

recommendEmptyCart :: [Char] -> [Char]
recommendEmptyCart user = if (length(fasas(freqListItems user))) > 0 then pickRandom (fasas (freqListItems user)) (randomZeroToX (length(fasas(freqListItems user))-1)) else []

recommendBasedOnItemsInCart :: [Char] -> [[Char]] -> [Char]
recommendBasedOnItemsInCart user cart =  if (length(fasas(freqListItems user))) > 0 then pickRandom ((fasas (freqListItems user))++cart) (randomZeroToX ((length(fasas(freqListItems user)))+length(cart))) else []

recommend :: [Char] -> [[Char]] -> [Char]
recommend user cart = if (length(fasas(freqListItems user))) > 0 then pickRandom ((fasas (freqListItems user))++cart++(fasas (freqListUsers user))) (randomZeroToX ((length(fasas(freqListItems user)))+length(freqListUsers user))) else []























