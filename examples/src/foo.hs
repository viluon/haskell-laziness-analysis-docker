
tsum :: Int -> String
tsum 0 = "\\Sigma"
tsum n = tsum 0 ++ "_{" ++ next ++ "}^{" ++ next ++ "}"
    where next = tsum $ n - 1

main :: IO ()
main = do
       n <- readLn
       putStrLn $ tsum n

