module MyLib (someFunc, bar) where

bar str = length str

someFunc :: IO ()
someFunc = putStrLn $ "someFunc" ++ show (bar "foo")
