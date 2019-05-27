import Data.List 

data Ex = Ex Float Float String String deriving Show
data NewSt = NewSt Float Float String deriving Show
data Dist = Dist Float NewSt Ex deriving Show

euclidean :: NewSt -> Ex -> Dist

euclidean (NewSt midterm1 quiz1 name1) (Ex midterm2 quiz2 name2 label2) = (Dist  (sqrt( (midterm1-midterm2)*(midterm1-midterm2) + (quiz1-quiz2)*(quiz1-quiz2))) (NewSt midterm1 quiz1 name1) (Ex midterm2 quiz2 name2 label2))

manhattan :: NewSt -> Ex -> Dist

manhattan (NewSt midterm1 quiz1 name1) (Ex midterm2 quiz2 name2 label2) = (Dist  (abs (midterm1-midterm2)+ abs (quiz1-quiz2)) (NewSt midterm1 quiz1 name1) (Ex midterm2 quiz2 name2 label2))




dist function news ex = function news ex

all_dists function news [] = []
all_dists function news (x:t) = (function news x):all_dists function news t


takeN :: Num a => a -> [b] -> [b]

takeN _ [] = []
takeN 0 _ = []
takeN n (x:t) = (x:takeN (n-1) t)


compareDist (Dist dist1 news1 ex1) (Dist dist2 news2 ex2) = if  (dist1>dist2) then GT else LT

closest function n list newS = takeN n sol where sol = sortBy compareDist sol1 where sol1 = all_dists function newS list

-- helper 
takePass [] = []
takePass ((Dist dist newS (Ex mid quiz name label)):t) = if (label == "pass") then ((Dist dist newS (Ex mid quiz name label)): takePass t) else takePass t
-- helper 
takeFail [] = []
takeFail ((Dist dist newS (Ex mid quiz name label)):t) = if (label == "fail") then ((Dist dist newS (Ex mid quiz name label)): takeFail t) else takeFail t

grouped_dists function n list newS = [ takePass list1,takeFail list1] where list1 = closest function n list newS

--helper
modeValue list = if (length(l1)>length(l2)) then "pass" else "fail" where (l1,l2) = (takePass list, takeFail list)

mode function n list newS = if (modeValue list1 == "pass") then takePass list1 else takeFail list1 where list1 = closest function n list newS

labelOf ((Dist dist (NewSt midterm1 quiz1 name1) (Ex mid quiz name label)):t) = (name1,label)

classify function n list newS = labelOf listDist where listDist = mode function n list newS 

classify_all function n list [] = [] 
classify_all function n list (newS:t) = (classify function n list newS):(classify_all function n list t)

