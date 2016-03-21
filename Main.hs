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
  -- replicateM_ 5 $ synth (toSD () M.coolFract) ()
  -- replicateM_ 2 $ play $ M.fractShit

  replicateM_ 5 $ play $ do
    foo <- 10 ~* layer 1 M.fract1
    foo
      -- & \x -> latch (in_ x, trigger_ $ impulse (freq_ 22050))
      & tanh'
      & (~*0.2)

  play $ do
    imp <- impulse (freq_ 10) ? KR
    foo <- dseq (repeats_ inf)
      =<< sequence ((midiCPS . (+38))
                    <$> ([0, 2, 3, 5] !!! 16)
                    ++ ((+ (-2)) <$> [0, 2, 3, 5] !!! 16)
                   )
      -- =<< sequence (fmap (midiCPS . (+38)) [-2, 2, 3, 5])


    dem <- demand (trig_ imp, reset_ 0, ugen_ foo)
    freq <- dem
      & \x -> lag2 (in_ x, secs_ 10)

    (mix $ [ sinOsc (freq_ freq)])
      & (~* 0.1)
      & uOp TanH

  -- play $ do
  --   foo <- layer 10 M.fract2
  --   return foo

  --   -- lmy <- fract 0.01 & linexp (-1, 1, 4000, 24000)
  --   lmy <- dc 4000
  --   trigger <- lfPulse (freq_ lmy)
  --   uOp TanH $ latch (in_ foo, trigger_ trigger)

  return ()
