{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import SDL
import Foreign.C.Types
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import System.Random

run :: IO ()
run = do
  gen <- initStdGen
  initializeAll
  window <- createWindow "Snek" defaultWindow { windowInitialSize = V2 1200 800 }
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer $ initialGameState gen

data GameState
  = GameState
  { x          :: CInt,
    y          :: CInt,
    dx         :: CInt,
    dy         :: CInt,
    tailLength :: Int,
    tailElems  :: Seq (V2 CInt),
    fruit      :: V2 CInt,
    berry      :: V2 CInt,
    rndGen     :: StdGen,
    rndGen1    :: StdGen,
    speed      :: Int,
    score      :: Int,
    flag       :: Int,
    flag2      :: Int
  }

initialGameState :: StdGen -> GameState
initialGameState gen
  = GameState { x = 4,
                y = 10,
                dx = 0,
                dy = 1,
                tailLength = 3,
                tailElems = mempty,
                fruit = V2 20 23,
                berry = V2 300 200,
                rndGen = gen,
                rndGen1 = gen,
                speed = 200000,
                score = 0,
                flag = 0,
                flag2 = 0
              }

appLoop :: Renderer -> GameState -> IO()
appLoop renderer state@GameState {..} = do
  events <- pollEvents
  let eventIsKeyPressed keyCode event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
          _ -> False
      qPressed = any (eventIsKeyPressed KeycodeEscape) events
      upBtnPressed = any (eventIsKeyPressed KeycodeUp) events
      downBtnPressed = any (eventIsKeyPressed KeycodeDown) events
      leftBtnPressed = any (eventIsKeyPressed KeycodeLeft) events
      rightBtnPressed = any (eventIsKeyPressed KeycodeRight) events
  let (dx', dy' ) =
        case (compare dx 0, compare dy 0) of
          (EQ, EQ) | upBtnPressed ->  (0, -1)
          (EQ, EQ) | downBtnPressed ->  (0, 1)
          (EQ, EQ) | leftBtnPressed ->  (-1, 0)
          (EQ, EQ) | rightBtnPressed ->  (1, 0)
          (EQ, EQ) -> (0, 0)
          (LT, EQ) | upBtnPressed -> (0, -1)
          (LT, EQ) | downBtnPressed -> (0, 1)
          (LT, EQ) -> (dx, dy)
          (EQ, LT) | leftBtnPressed -> (-1, 0)
          (EQ, LT) | rightBtnPressed -> (1, 0)
          (EQ, LT) -> (dx, dy)
          (GT, EQ) | upBtnPressed -> (0, -1)
          (GT, EQ) | downBtnPressed -> (0, 1)
          (GT, EQ) -> (dx, dy)
          (EQ, GT) | leftBtnPressed -> (-1, 0)
          (EQ, GT) | rightBtnPressed -> (1, 0)
          (EQ, GT) -> (dx, dy)
          _ -> error "Impossible input state"
      x' = (x + dx') `mod` 60
      y' = (y + dy') `mod` 40
      tailElems' = case compare (S.length tailElems) tailLength of
        GT -> error "Impossible tail state!"
        EQ | tailLength > 0 -> S.drop 1 tailElems |> V2 x' y'
        EQ -> tailElems
        LT -> tailElems |> V2 x' y'
      
      -- fruit logic
      (fruit', rndGen', flag2') = if fruit /= V2 x y
        then (fruit, rndGen, flag2)
        else let (fx, g') = uniformR (0, 59) rndGen
                 (fy, g'') = uniformR (0, 39) g'
             in (V2 fx fy, g'', 1)
      (tailLength', score') = if fruit /= fruit' then (tailLength + 1, score + 1) else (tailLength, score)

      -- berry logic

      flag = if (tailLength -2) `mod` 5 == 0 && score > 3 then 1 else 0

      (berry', rndGen1', speed', flag', flag2'') = if flag == 1 && flag2' == 1 && (tailLength -2) `mod` 5 == 0 && score > 3
        then let (fx', g') = uniformR (0, 59) rndGen1
                 (fy', g'') = uniformR (0, 39) g'
             in (V2 fx' fy', g'', if tailLength' /= tailLength && speed >= 20000 then speed - 50000 else speed, 0, 0)
        else (berry, rndGen1, speed, 0, flag2')

      (berry'', score'',flag'' ,flag2''') = if berry' /= V2 x y
        then (berry', score', flag', flag2'')
        else (V2 200 200, score' + 5, 0, 0)
      
  -- window
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  
  -- fruit
  rendererDrawColor renderer $= V4 255 0 255 255
  fillRect renderer (Just (Rectangle (P $ (* 20) <$> fruit) (V2 20 20)))

  -- Berry
  rendererDrawColor renderer $= V4 255 0 0 255
  fillRect renderer (Just (Rectangle (P $ (* 20) <$> berry) (V2 20 20)))
  
  -- snake
  rendererDrawColor renderer $= V4 0 0 255 255
  fillRect renderer (Just (Rectangle (P $ V2 (x' * 20) (y' * 20)) (V2 20 20)))
  forM_ tailElems' $ \tailElems ->
    fillRect renderer (Just (Rectangle (P $ (* 20) <$> tailElems) (V2 20 20)))

  --putStrLn ("Berry: " ++ show berry ++ " Speed: " ++ show speed ++ " Score: " ++ show score ++ " tailLength: " ++ show (tailLength - 3) ++ " flag: " ++ show flag ++ " flag1: " ++ show flag2 )

  present renderer
  unless qPressed $  do
    threadDelay $speed
    appLoop renderer state { x = x',
                             y = y',
                             dx = dx',
                             dy = dy',
                             tailElems = tailElems',
                             fruit = fruit',
                             tailLength = tailLength',
                             rndGen = rndGen',
                             score = score'',
                             rndGen1 = rndGen1',
                             speed = speed',
                             berry = berry'',
                             flag = flag'',
                             flag2 = flag2'''
                           }
  where
    renderTailElem :: Renderer -> Int -> V2 CInt -> IO ()
    renderTailElem r ix e = do
      let blueIx = (1 + ix) * 10
          blue = if blueIx <= 255 then blueIx else 255
      rendererDrawColor r $= V4 0 0 (fromIntegral blue) 255
      fillRect renderer (Just (Rectangle (P $ (* 20) <$> e) (V2 20 20)))