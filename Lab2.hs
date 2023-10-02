-- CptS 355 - Lab 2 (Haskell) - Fall 2023
-- Name: 


module Lab2
     where


-- 1
{- (a) merge2 -}

merge2 iL iL2 = reverse (merge2helper iL iL2 [])

merge2helper [] [] buffer = buffer
merge2helper [] (x:xs) buffer = merge2helper [] xs (x:buffer)
merge2helper (x:xs) [] buffer = merge2helper xs [] (x:buffer)
merge2helper (x:xs) (y:ys) buffer = merge2helper xs ys (y:(x:buffer))

{- (b) merge2Tail -}

merge2tail iL iL2 = reverse (merge2helper iL iL2 [])


{- (c) mergeN -}

mergeN iL = foldl merge2tail [] iL


-- 2
{- (a) count -}

count val iL = length (filter (==val) iL) 




{- (b) histogram  -}

histogram iL = deleteduplicates (merge2tuple iL (map (histogramcounthelper iL) iL))

merge2tuple iL iL2 = reverse (merge2tuplehelper iL iL2 [])

merge2tuplehelper [] [] buffer = buffer
merge2tuplehelper [] (x:xs) buffer = error("lists are not of same length")
merge2tuplehelper (x:xs) [] buffer = error("lists are not of same length")
merge2tuplehelper (x:xs) (y:ys) buffer = merge2tuplehelper xs ys ((x,y):buffer)

histogramcounthelper iL val = count val iL

deleteduplicates iL = reverse (deleteduplicateshelper iL [] [])

deleteduplicateshelper [] buff buff2 = buff
deleteduplicateshelper (x:xs) buff buff2 | ((count x buff2) /= 0) = deleteduplicateshelper xs buff buff2
                                         | otherwise = deleteduplicateshelper xs (x:buff) (x:buff2)



-- 3                
{- (a) concatAll -}




{- (b) concat2Either -}               
--data AnEither  = AString String | AnInt Int
--                deriving (Show, Read, Eq)



-- 4      
{-  concat2Str -}               




-- data Op = Add | Sub | Mul | Pow
--           deriving (Show, Read, Eq)

-- evaluate:: Op -> Int -> Int -> Int
-- evaluate Add x y =  x+y
-- evaluate Sub   x y =  x-y
-- evaluate Mul x y =  x*y
-- evaluate Pow x y = x^y

-- data ExprTree a = ELEAF a | ENODE Op (ExprTree a) (ExprTree a)
--                   deriving (Show, Read, Eq)

-- 5 
{- evaluateTree -}



-- 6
{- printInfix -}



--7
{- createRTree -}
-- data ResultTree a  = RLEAF a | RNODE a (ResultTree a) (ResultTree a)
--                      deriving (Show, Read, Eq)






