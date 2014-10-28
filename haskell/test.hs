double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns


n = a `div` length xs
    where
       a  = 10
       xs = [1,2,3,4,5]

second xs = head (tail xs)

swap (x, y) = (y, x)

pair x y = (x, y)

palindrome xs = reverse xs == xs

twice f x = f (f x)

f xs = take 3 (reverse xs)
