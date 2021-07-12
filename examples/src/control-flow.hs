{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (print, error, ($))

snd (x, y) = y

main = print $ snd (error "oops!", 3)
