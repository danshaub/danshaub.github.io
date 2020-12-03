import qualified Data.Maybe as M
import qualified Data.Text as T

toRoman :: Int -> T.Text
toRoman x
  | x >= 1000 = T.cons 'M' (toRoman (x - 1000))
  | x >= 900 = T.append (T.pack "CM") (toRoman (x - 900))
  | x >= 500 = T.cons 'D' (toRoman (x - 500))
  | x >= 400 = T.append (T.pack "CD") (toRoman (x - 400))
  | x >= 100 = T.cons 'C' (toRoman (x - 100))
  | x >= 90 = T.append (T.pack "XC") (toRoman (x - 90))
  | x >= 50 = T.cons 'L' (toRoman (x - 50))
  | x >= 40 = T.append (T.pack "XL") (toRoman (x - 40))
  | x >= 10 = T.cons 'X' (toRoman (x - 10))
  | x >= 9 = T.append (T.pack "IX") (toRoman (x - 9))
  | x >= 5 = T.cons 'V' (toRoman (x - 5))
  | x >= 4 = T.append (T.pack "IV") (toRoman (x - 4))
  | x >= 1 = T.cons 'I' (toRoman (x - 1))
  | otherwise = T.empty

fromRoman :: T.Text -> Int
fromRoman x
  | T.isPrefixOf (T.singleton 'M') x = 1000 + fromRoman (T.tail x)
  | T.isPrefixOf (T.pack "CM") x = 900 + fromRoman (T.tail (T.tail x))
  | T.isPrefixOf (T.singleton 'D') x = 500 + fromRoman (T.tail x)
  | T.isPrefixOf (T.pack "CD") x = 400 + fromRoman (T.tail (T.tail x))
  | T.isPrefixOf (T.singleton 'C') x = 100 + fromRoman (T.tail x)
  | T.isPrefixOf (T.pack "XC") x = 90 + fromRoman (T.tail (T.tail x))
  | T.isPrefixOf (T.singleton 'L') x = 50 + fromRoman (T.tail x)
  | T.isPrefixOf (T.pack "XL") x = 40 + fromRoman (T.tail (T.tail x))
  | T.isPrefixOf (T.singleton 'X') x = 10 + fromRoman (T.tail x)
  | T.isPrefixOf (T.pack "IX") x = 9 + fromRoman (T.tail (T.tail x))
  | T.isPrefixOf (T.singleton 'V') x = 5 + fromRoman (T.tail x)
  | T.isPrefixOf (T.pack "IV") x = 4 + fromRoman (T.tail (T.tail x))
  | T.isPrefixOf (T.singleton 'I') x = 1 + fromRoman (T.tail x)
  | otherwise = 0

expand :: T.Text -> T.Text
expand x
  | T.isPrefixOf (T.pack "CM") x = T.append (T.pack "DCCCC") (expand (T.drop 2 x))
  | T.isPrefixOf (T.pack "CD") x = T.append (T.pack "CCCC") (expand (T.drop 2 x))
  | T.isPrefixOf (T.pack "XC") x = T.append (T.pack "LXXXX") (expand (T.drop 2 x))
  | T.isPrefixOf (T.pack "XL") x = T.append (T.pack "XXXX") (expand (T.drop 2 x))
  | T.isPrefixOf (T.pack "IX") x = T.append (T.pack "VIIII") (expand (T.drop 2 x))
  | T.isPrefixOf (T.pack "IV") x = T.append (T.pack "IIII") (expand (T.drop 2 x))
  | not (T.null x) = T.cons (T.head x) (expand (T.tail x))
  | otherwise = T.empty

contract :: T.Text -> T.Text
contract x
  | T.isSuffixOf (T.pack "VV") x = contract (order (T.append (T.dropEnd 2 x) (T.pack "X")))
  | T.isSuffixOf (T.pack "IIIII") x = contract (order (T.append (T.dropEnd 5 x) (T.pack "V")))
  | T.isSuffixOf (T.pack "VIIII") x = contract (T.append (T.dropEnd 5 x) (T.pack "IX"))
  | T.isSuffixOf (T.pack "IIII") x = contract (T.append (T.dropEnd 4 x) (T.pack "IV"))
  | T.isSuffixOf (T.pack "VIX") x = contract (T.append (T.dropEnd 3 x) (T.pack "XIV"))
  | T.isSuffixOf (T.pack "LL") x = contract (order (T.append (T.dropEnd 2 x) (T.pack "C")))
  | T.isSuffixOf (T.pack "XXXXX") x = contract (order (T.append (T.dropEnd 5 x) (T.pack "L")))
  | T.isSuffixOf (T.pack "LXXXX") x = contract (T.append (T.dropEnd 5 x) (T.pack "XC"))
  | T.isSuffixOf (T.pack "XXXX") x = contract (T.append (T.dropEnd 4 x) (T.pack "XL"))
  | T.isSuffixOf (T.pack "LXC") x = contract (T.append (T.dropEnd 3 x) (T.pack "CLX"))
  | T.isSuffixOf (T.pack "DD") x = contract (order (T.append (T.dropEnd 2 x) (T.pack "M")))
  | T.isSuffixOf (T.pack "CCCCC") x = contract (order (T.append (T.dropEnd 5 x) (T.pack "D")))
  | T.isSuffixOf (T.pack "DCCCC") x = contract (T.append (T.dropEnd 5 x) (T.pack "CM"))
  | T.isSuffixOf (T.pack "CCCC") x = contract (T.append (T.dropEnd 4 x) (T.pack "CD"))
  | T.isSuffixOf (T.pack "DCM") x = contract (T.append (T.dropEnd 3 x) (T.pack "MCD"))
  | not (T.null x) = T.snoc (contract (T.init x)) (T.last x)
  | otherwise = T.empty

order :: T.Text -> T.Text
order x
  | let ind = M.fromJust (T.findIndex ('I' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'I') x = T.snoc (order (T.append bef (T.tail aft))) 'I'
  | let ind = M.fromJust (T.findIndex ('V' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'V') x = T.snoc (order (T.append bef (T.tail aft))) 'V'
  | let ind = M.fromJust (T.findIndex ('X' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'X') x = T.snoc (order (T.append bef (T.tail aft))) 'X'
  | let ind = M.fromJust (T.findIndex ('L' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'L') x = T.snoc (order (T.append bef (T.tail aft))) 'L'
  | let ind = M.fromJust (T.findIndex ('C' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'C') x = T.snoc (order (T.append bef (T.tail aft))) 'C'
  | let ind = M.fromJust (T.findIndex ('D' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'D') x = T.snoc (order (T.append bef (T.tail aft))) 'D'
  | let ind = M.fromJust (T.findIndex ('M' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'M') x = T.snoc (order (T.append bef (T.tail aft))) 'M'
  | otherwise = T.empty

minusOneRN :: T.Text -> T.Text
minusOneRN x
  | T.isSuffixOf (T.pack "I") x = T.dropEnd 1 x
  | T.isSuffixOf (T.pack "IV") x = T.append (T.dropEnd 2 x) (T.pack "III")
  | T.isSuffixOf (T.pack "V") x = T.append (T.dropEnd 1 x) (T.pack "IV")
  | T.isSuffixOf (T.pack "IX") x = T.append (T.dropEnd 2 x) (T.pack "VIII")
  | T.isSuffixOf (T.pack "X") x = T.append (T.dropEnd 1 x) (T.pack "VIV")
  | T.isSuffixOf (T.pack "XL") x = T.append (T.dropEnd 2 x) (T.pack "XXXIX")
  | T.isSuffixOf (T.pack "L") x = T.append (T.dropEnd 1 x) (T.pack "XLIX")
  | T.isSuffixOf (T.pack "XC") x = T.append (T.dropEnd 2 x) (T.pack "LXXXIX")
  | T.isSuffixOf (T.pack "C") x = T.append (T.dropEnd 1 x) (T.pack "XCIX")
  | T.isSuffixOf (T.pack "CD") x = T.append (T.dropEnd 2 x) (T.pack "CCCXCIX")
  | T.isSuffixOf (T.pack "D") x = T.append (T.dropEnd 1 x) (T.pack "CDXCIX")
  | T.isSuffixOf (T.pack "CM") x = T.append (T.dropEnd 2 x) (T.pack "DCCCXCIX")
  | T.isSuffixOf (T.pack "M") x = T.append (T.dropEnd 1 x) (T.pack "CMXCIX")
  | otherwise = T.empty

addRN :: T.Text -> T.Text -> T.Text
addRN m n = contract (order (T.append (expand m) (expand n)))

subtractRN :: T.Text -> T.Text -> T.Text
subtractRN m n
  | m == T.empty = T.empty
  | n == T.empty = m
  | otherwise = subtractRN (minusOneRN m) (minusOneRN n)

multRN :: T.Text -> T.Text -> T.Text
multRN m n
  | (m == T.empty) || (n == T.empty) = T.empty
  | m == T.pack "I" = n
  | n == T.pack "I" = m
  | otherwise = addRN m (multRN m (minusOneRN n))
