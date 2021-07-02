evens []  = []
evens [x] = [x]
evens (x:_:xs) = x : evens xs

main = print $ evens [1]
