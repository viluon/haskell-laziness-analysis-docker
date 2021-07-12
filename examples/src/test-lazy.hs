evens []  = []
evens [x] = []
evens (n:x:xs) = x : evens xs

undef = error "oops"

ys :: [Int]
ys = undef : 0 : ys

foo xs = xs ++ xs

bar x y z w True  = undef
bar x y z w False = undef

main = print . foo . evens $ take 10 ys
