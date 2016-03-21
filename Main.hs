{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Main where

import Vivid
import Util
import qualified MyInstruments as M
import MyCombinators
import Data.Function


main = do
  cmdPeriod

  play $ M.birdies

  -- replicateM_ 5 $ play $ do
  --   foo <- 10 ~* layer 1 M.fract1
  --   -- lmy <- my & linexp (0, 1, 4000, 22050)
  --   lmy <- lfSaw (freq_ 1) & linexp (0, 1, 8000, 16000)
  --   -- lmx <- mx & linexp (0, 1, 100, 16000) & laag 0.01
  --   lmx <- lfSaw  (freq_ 8) & linexp (-1, 1, 100, 16000)
  --   foo
  --     & \x -> latch (in_ x, trigger_ $ impulse (freq_ lmy))
  --     & tanh'
  --     & (~*0.01)
  --     & \x -> lpf (in_ x, freq_ lmx)


  -- play $ do
  --   imp <- impulse (freq_ $ fract 0.01 & linexp (-1, 1, 1, 20) & laag 10) ? KR
  --   base <- fract 0.01 & linlin (-1, 1, 40, 50)

  --   seq <- sequence $ (midiCPS . (~+base))
  --                   <$> [0, 2, 3, 5]

  --   foo <- dseq (repeats_ inf) seq


  --   dem <- demand (trig_ imp, reset_ 0, ugen_ foo)
  --   freq <- dem
  --     & laag 0.1

  --   (mix $ [sinOsc (freq_ freq)])
  --     & (~* 0.1)
  --     & uOp TanH


  return ()
