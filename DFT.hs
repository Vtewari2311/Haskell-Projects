---Defining a function range from to count
---Returns a list of count of equally-spaced numbers from and to (from <= count < to)
---Example: range 0 1 10 returns
---[0.0,0.1,0.2,0.30000000000000004,0.4,0.5,0.6,0.7,0.7999999999999999,0.8999999999999999]
--- range :: (Ord a, Fractional a) => a -> a -> t -> [a]
range a b c
  | a >= b = []
  | otherwise = a : range (a + 0.1) b c

---Defining a function rd n x and a helper round_to_digit
---Rounds a list x to nth digits of precision
---Example: rd 1 (range 0 1 10) returns
---[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
---round_to_digit :: (RealFrac a1, Integral b, Fractional a2) => a1 -> b -> a2
--- rd :: (RealFrac a1, Integral b1, Fractional b2) => b1 -> [a1] -> [b2]
round_to_digit a b = fromInteger (round (a * 10 ^ b)) / 10 ^ b

rd n x = map (\d -> round_to_digit d n) x

---For the next two questions, we use a 2-tuple to represent a complex number
---For example, 1 + i ·2 is written as (1, 2)

---Defining a function absolute to compute the absolute value of a list of tuples
---Example: absolute [(3.0, 4.0), (4.0, 3.0)] returns
---[5.0,5.0]
---absolute :: Floating a => [(a, a)] -> [a]
absolute [] = []
absolute ((a, b) : xs) = sqrt (a ^ 2 + b ^ 2) : absolute xs

---Defining a function dft to compute the Discrete Fourier Transform (DFT)
---of a list of reals {x0,x1,...,xN }. You should use the following equation
---to compute each element of the DFT.
---In Haskell, pi, sin, and cos are predefined.
---list of reals -> list of 2-tuples
---helper function: list_tuple converts a list into a list of tuples
---list_tuple :: [b] -> [(b, b)]
---dft :: Floating b => [b] -> [(b, b)]
dft x =
  let n = fromIntegral $ length x
      index = range 0 n n
      xn = x `zip` index
      f [] = []
      f (k:rest) = (sum r, sum i) : f rest
        where (r, i) = unzip $ factor xn
              factor [] = []
              factor ((xi, j) : rest) = (xi * cos y, -xi * sin y) : factor rest
                where y = 2 * pi * j * k / n             
  in
      f index

---dft x = 
---   let large_n = fromIntegral $ length x
---       index = range 0 large_n n
---       xs = zip x index

---       helper [] = []
---       helper (k : xs) = index * (index*cos(2*pi*k*index / large_n)- a*sin(2*pi*k*index / large_n))) : helper (xs)--compute x_k :h (rest)
---   in
---       helper index

main = do
  let n = 64
  let s = map (\t -> sin (10 * 2 * pi * t) + sin (20 * 2 * pi * t) / 2) $ range 0 1 n
  let result = map (\x -> x / n) $ absolute $ dft s 
  print (rd 3 s)
  print (rd 2 result)
