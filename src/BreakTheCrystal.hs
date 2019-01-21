{-# LANGUAGE NPlusKPatterns #-}
module BreakTheCrystal where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Bool
import Numeric.Natural

import Debug.Trace

type Ball  = Natural
type Stage = Natural
type Trial = Natural

cover :: Ball -> Trial -> Stage
cover d k = sum (firstStage d <$> [1 .. k])

firstStage :: Ball -> Trial -> Stage
firstStage 0 _ = 0
firstStage d k = succ (cover (pred d) (pred k))

upperBound :: Stage -> Ball -> Trial
upperBound n d = iter 0
  where
    iter k = bool k (iter (succ k)) (n > cover d k)

data ExState = ExState
  { nball   :: Natural
  , cball   :: Natural
  , count   :: Natural
  , current :: Natural
  , bottom  :: Natural
  , toplim  :: Natural
  }

type Experiment = StateT ExState IO

initState :: Natural -> Natural -> ExState
initState n d = ExState
  { nball   = d
  , cball   = 1
  , count   = 0
  , current = 0
  , bottom  = 0
  , toplim  = n
  }

next :: Experiment ()
next = do
  { s0@(ExState d b cnt cur bot top) <- get
  ; liftIO (putStrLn ("現在の考慮区間: " ++ show (bot, top)))
  ; put (s0 {current = bot + firstStage d (upperBound (top - bot) d)})
  }

dropBall :: Experiment (Maybe Natural)
dropBall = do
  { s0@(ExState d b cnt cur bot top) <- get
  ; liftIO (putStrLn (show cur ++ "階から" ++ show b ++ "番の玉を落します"))
  ; put (s0 {count = cnt + 1})
  ; liftIO (putStr "玉は壊れましたか(yes/no)？ ")
  ; res <- liftIO getLine
  ; bool notbroken broken (res == "yes")
  }
  
notbroken :: Experiment (Maybe Natural)
notbroken = do
  { s0@(ExState d b cnt cur bot top) <- get
  ; put (s0 {bottom = cur})
  ; return (bool Nothing (Just cur) (cur == top))
  }

broken :: Experiment (Maybe Natural)
broken = do
  { s0@(ExState d b cnt cur bot top) <- get
  ; put (s0 {nball = d - 1, cball = b + 1, toplim = cur - 1})
  ; return (bool Nothing (Just (cur - 1)) (cur == top))
  }

trial :: Experiment (Maybe Natural)
trial = next >> dropBall

experiment :: Experiment ()
experiment = trial >>= maybe experiment endproc

endproc :: Natural -> Experiment ()
endproc h = do
  { s0@(ExState d b cnt cur bot top) <- get
  ; liftIO (putStr (unlines ["硬度    : " ++ show h
                            ,"試行回数: " ++ show cnt
                            ,"破壊玉数: " ++ show (b - 1)
                            ]))
  }

