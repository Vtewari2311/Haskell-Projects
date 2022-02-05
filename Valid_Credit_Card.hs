--Validation Algorithm
--Double the value of every second digit beginning from the right
--Add the digits of changed and unchanged numbers
--Calculate the remainder when the sum is divided by 10
--toDigits :: Integer -> [Integer]
--convert +ve ints to list of ints, if -ve ints or 0 then return empty list
--Example: toDigits 1234 == [1,2,3,4]
--Example: toDigits 0 == []
--Example: toDigits (-17) == []
toDigits n =
  if (n <= 0)
    then []
    else toDigits (div n 10) ++ [mod n 10]

--toDigitsRev :: Integer -> [Integer]
--Same functionality as toDigits, returns reversed list
--Example: toDigitsRev 1234 == [4,3,2,1]
toDigitsRev n = reverse (toDigits n)

--doubleEveryOther :: [Integer] -> [Integer]
--Doubles every second-to-last digit from the rightmost digit
--Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
--Example: doubleEveryOther [1,2,3] == [1,4,3]
--Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- helper functions
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x : xs) =
  if mod (length xs) 2 == 0
    then 2 * x : (doubleEveryOther xs)
    else x : doubleEveryOther xs

--sumDigits :: [Integer] -> Integer
--Calculates sum of all digits
--Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits [] = 0
sumDigits [x] = if x < 10 then x else sumDigits (toDigits x)
sumDigits (x : xs) = x + sumDigits xs

--validate :: Integer -> Bool
--Calculates remainder and checks to see if number is valid, if 0 then valid
--Example: validate 4012888888881881 = True
--Example: validate 4012888888881882 = False
validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0

main :: IO ()
main = do
  putStrLn $ show $ validate 4012888888881881
  putStrLn $ show $ validate 4012888888881882
