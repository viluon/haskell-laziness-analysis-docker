
main = putStrLn $ choose True "hi" undefined

{-# NOINLINE choose #-}
choose True  x _ = x
choose False _ y = y
