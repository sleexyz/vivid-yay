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


-- TODO: make demand rate version of fract
dbrown_example = do
  mx <- mouseX (min_ 1, max_ 40, warp_ 1)
  imp <- impulse (freq_ mx) ? KR
  dbr <- dbrown (lo_ 0, hi_ 15, step_ 1, length_ inf)
  dem <- demand (trig_ imp, reset_ 0, ugen_ dbr)
  s <- 0.1 ~* sinOsc (freq_ $ dem ~* 30 ~+ 340)
  out 0 [s,s]

main :: IO ()
main = do
  cmdPeriod
  -- replicateM_ 5 $ synth (toSD () M.coolFract) ()

  -- replicateM_ 5 $ play $ do
  --   foo <- 10 ~* layer 1 M.fract1
  --   downsample $ return foo

  play $ do
    imp <- impulse (freq_ 1) ? KR
    -- imp <- dust  (density_ 10)

    -- foo <- drand (repeats_ inf)
    --   =<< sequence (fmap dc ([5..10] :: [Float]))

    foo <- dseq (repeats_ inf)
      =<< sequence (fmap (midiCPS . (+50)) ([0, 2, 3, 5]))


    dem <- demand (trig_ imp, reset_ 0, ugen_ foo)
    input <- lfSaw (freq_ $ dem)

    lmy  <- dc 800

    trigger <- mix $ [ lfPulse (freq_ $ dem)
                     , lfPulse (freq_ 440) ~* 1
                     -- , lfPulse (freq_ dem) ~* 1
                     ]
    uOp TanH $ latch (in_ input, trigger_ trigger)


  -- play $ do
  --   foo <- layer 10 M.fract2
  --   return foo

  --   -- lmy <- fract 0.01 & linexp (-1, 1, 4000, 24000)
  --   lmy <- dc 4000
  --   trigger <- lfPulse (freq_ lmy)
  --   uOp TanH $ latch (in_ foo, trigger_ trigger)

  return ()
