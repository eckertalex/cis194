-- Exercise 1

-- | Converts a positive integer to a list of its digits.
-- | For 0 or negative inputs, it returns an empty list.
--
-- Examples:
-- >>> toDigits 1234
-- [1, 2, 3, 4]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []
toDigits :: Integer -> [Integer]
toDigits n
  | n < 0 = []
  | n `div` 10 == 0 = [n]
  | otherwise = toDigits (n `div` 10) ++ toDigits (n `mod` 10)

-- | Converts a positive integer to a list of its digits in reverse order.
-- | For 0 or negative inputs, it returns an empty list.
--
-- Examples:
-- >>> toDigitsRev 1234
-- [4, 3, 2, 1]
-- >>> toDigitsRev 0
-- []
-- >>> toDigitsRev (-17)
-- []
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Exercise 2

-- | Doubles every other number in a list, starting from the right.
-- | The second-to-last, fourth-to-last, and so on are doubled.
--
-- Examples:
-- >>> doubleEveryOther [8, 7, 6, 5]
-- [16, 7, 12, 5]
-- >>> doubleEveryOther [1, 2, 3]
-- [1, 4, 3]
-- >>> doubleEveryOther []
-- []
-- >>> doubleEveryOther [5]
-- [5]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ go (reverse xs)
  where
    go :: [Integer] -> [Integer]
    go [] = []
    go [x] = [x]
    go (x : y : xs) = [x, 2 * y] ++ go xs

-- Exercise 3

-- | Calculates the sum of all digits in a list of integers.
-- | If an integer in the list has multiple digits, each digit is summed individually.
--
-- Examples:
-- >>> sumDigits [16, 7, 12, 5]
-- 22   -- (1 + 6 + 7 + 1 + 2 + 5)
-- >>> sumDigits []
-- 0
-- >>> sumDigits [10, 5, 123]
-- 12   -- (1 + 0 + 5 + 1 + 2 + 3)
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concatMap toDigits xs

-- Exercise 4

-- | Validates whether a given integer could be a valid credit card number.
-- | The validation is based on the Luhn algorithm, which involves:
-- |   - Converting the number to a list of digits.
-- |   - Doubling every other digit starting from the right.
-- |   - Summing all digits (including the digits of any doubled values).
-- |   - The sum must be divisible by 10 for the number to be valid.
--
-- Examples:
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False
-- >>> validate 1234567812345670
-- True
-- >>> validate 1234567812345671
-- False
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0
