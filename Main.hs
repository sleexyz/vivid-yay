{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Vivid
import Util
import qualified MyInstruments as M


-- TODO: make demand rate version of fract

main :: IO ()
main = do
  cmdPeriod
  -- replicateM_ 5 $ synth (toSD () M.coolFract) ()
  -- replicateM_ 4 $ playWith () $ M.fract1 ~* 10

  playWith () $ do
    -- gsig <- fract 0.01

    s <- dc 0
    -- s <- sinOsc (freq_ 1000)
    -- s <- mix $ replicate 5 $ M.fractShit ~/ 15

    m <- dc 0
    m <- mix $ replicate 4 M.fract2

    uOp TanH $ s ~+ (m ~* 20)

  return ()
