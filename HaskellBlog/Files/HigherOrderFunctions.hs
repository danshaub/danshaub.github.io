addFourNum :: (Num a) => a -> a -> a -> a -> a
addFourNum x y z w = x + y + z + w

fivePlusThreeNum :: (Num a) => a -> a -> a -> a
fivePlusThreeNum = addFourNum 5

eightPlus2Num :: (Num a) => a -> a -> a
eightPlus2Num = fivePlusThreeNum 3

add12 :: (Num a) => a -> a
add12 = eightPlus2Num 4

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

map' :: (a -> b) -> [a] -> [b]
map' f (x : xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) = if f x then x : filter f xs else filter f xs