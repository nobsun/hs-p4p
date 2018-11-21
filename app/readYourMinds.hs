module Main where

import Data.Bool
import Data.List
import System.IO
import YouCanReadMinds

main :: IO ()
main = assistant >>= magician

formats :: [String]
formats = map showCard deck

assistant :: IO [Card]
assistant = do
  { putStrLn "カードは以下のように文字列で表現します．"
  ; putStrLn "順序は以下のとおり．"
  ; putStr (showDeck deck)
  ; order <$> foldl takeCard (return []) [1 .. 5]
  } 


takeCard :: IO [Card] -> Int -> IO [Card]
takeCard mc i = do
  { cs <- mc
  ; c <- getCard i cs
  ; return (c:cs)
  }

prompt :: String -> IO ()
prompt s = hSetBuffering stdout NoBuffering >> putStr s

getCard :: Int -> [Card] -> IO Card
getCard i cs = do
  { prompt (show i ++ "番目のカードを上の形式で下さい: ")
  ; s <- getLine
  ; bool (putStrLn "フォーマットが違います" >> getCard i cs)
         (bool (return (readCard s))
               (putStrLn "そのカードはすでに取られています" >> getCard i cs)
               (elem s (map showCard cs)))
         (elem s formats)
  }

magician :: [Card] -> IO ()
magician cs = do
  { prompt "隠したカードを除いた4枚のカードを教えてください: "
  ; putStrLn (intercalate " " (map showCard cs))
  ; putStrLn ("隠れているカードは " ++ showCard (guess cs) ++ " ですね．")
  }

