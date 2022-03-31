module Lib
    ( meMama
    , meEsquece
    , dividePoPai
    , multiplicaPoPai
    , falaCmg
    , printa
    , clonaCartao
    , mapzada
    , naMira
    , crime
    ) where

meMama :: Num a => a -> a -> a
meMama x y = x + y

meEsquece :: Num a => a -> a -> a
meEsquece x y = x - y

dividePoPai :: (Fractional a, Num a) => a -> a -> a
dividePoPai x y = x / y

multiplicaPoPai :: Num a => a -> a -> a
multiplicaPoPai x y = x * y

falaCmg :: String -> IO ()
falaCmg = putStrLn

printa :: Show a => a -> IO ()
printa x = putStrLn (show x)

clonaCartao :: Int -> a -> [a]
clonaCartao a b = if a > 0 then b : Lib.clonaCartao (a-1) b else []

mapzada :: (a -> b) -> [a] -> [b]
mapzada fn [] = []
mapzada fn (x:xs) = fn x : Lib.mapzada fn xs

naMira :: [a] -> Int -> Maybe a
naMira [] a = Nothing
naMira (x:xs) a = if a == 0 then Just x else naMira xs (a-1)

crime :: [Char] -> Int -> [Char]
crime x y = x ++ show y

someFunc :: IO ()
someFunc = putStrLn "someFunc"
