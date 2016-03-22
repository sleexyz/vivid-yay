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

blam = do
  fork $ do
    s1 <- play $ do
      pinkNoise ~* 0.1
    wait (1/4)
    free s1


main = do
  cmdPeriod

  -- 

  -- replicateM_ 5 $ play $ do
  --   ( M.birdies =<< (
  --       my
  --       & seelect ((midiRatio . (~+36)) <$> [0, 2, 4])
  --       & laag 1
  --       )
  --     )



  -- 


  -- play $ do
  --   snd <- soundIn (Bus 0)

  --   -- ps <- pitchShift ( in_ snd
  --   --            , ratio_ $ my & seelect ((~*1) <$> [ 1, 2])
  --   --            , windowSize_ 0.01
  --   --            )


  --   shift <- mx  & linexp (0, 1, 100, 48000)
  --   freq <- my & linexp (0, 1, 4000, 16000)
  --   -- freq <- 1 ~* 8000

  --   snd
  --     & \x -> freqShift ( in_ x, freq_ $ shift)
  --     & \x -> latch (in_ x, trig_ $ impulse (freq_ freq))
  --     & \x -> lpf (in_ x, freq_ $ my & linexp (0, 1, 400, 8000))
  --     & \x -> hpf (in_ x, freq_ 800)
  --     & \x -> freqShift ( in_ x, freq_ $ (-1) ~* shift)
  --     & tanh'
  --     & (~*0.2)

  -- play $ do
  --   snd <- soundIn (Bus 0)

  --   -- shift <- mx  & linexp (0, 1, 100, 48000)
  --   let ass = do
  --         shift <- 1 ~* 40000
  --         freq <- my & linexp (-1, 1, 2000, 8000) & laag 1

  --         pulse <- impulse (freq_ freq)

  --         snd
  --           & \x -> freqShift ( in_ x, freq_ $ shift)
  --           & \x -> latch (in_ x, trig_ pulse)
  --           & \x -> freqShift ( in_ x, freq_ $ (-1) ~* shift)
  --           & \x -> latch (in_ x, trig_ $ pulse)
  --           & \x -> x
  --           & \x -> lpf (in_ x, freq_ 8000)
  --           & \x -> hpf (in_ x, freq_ 800)
  --           & (~*0.2)
  --   ass
  --     & tanh'
  --     & \x -> freeVerb (in_ x, room_ 5)

  play $ do
    shift <- my
      & laag 1
      & seelect ((~*1) <$> [60, 80])
    snd <- brownNoise
    snd
      & \x -> freqShift ( in_ x, freq_ $ midiCPS shift)


  -- 

  -- play $ do
  --   base <- my
  --       & laag 1
  --       & seelect ((~+80) <$> [0, 2, 10])

  --   wn <- whiteNoise
  --   freq <- fract 0.1
  --     & linlin (-1, 1, 0, 1)
  --     & seelect ((midiCPS . (~+base)) <$> [0, 2, 12, 14])

  --   (M.wind freq)
  --     & tanh'
  --     & (~*0.5)

  -- -- 

  -- play $ do
  --   base <- 80 ~* 1

  --   freq <- whiteNoise
  --     & linlin (-1, 1, 0, 1)
  --     & seelect ((midiCPS . (~+base)) <$> [8, 2, 12, 14])

  --   (M.wind freq) ~* sinOsc (freq_ 0.01)

  -- 




  -- play $ do

  --   base <- 72 ~* 1

  --   seq <- sequence $ (midiCPS . (~+base))
  --                   <$> [0, 7, 19, 24]

  --   foo <- dseq (repeats_ inf) seq


  --   imp <- impulse (freq_ $ fract 0.01 & linexp (-1, 1, 1, 20) & laag 10) ? KR
  --   dem <- demand (trig_ imp, reset_ 0, ugen_ foo)
  --   freq <- dem
  --     & laag 0.1

  --   (mix $ [sinOsc (freq_ freq)])
  --     & (~* 0.1)
  --     & (~* (fract 0.1 & linexp (-1, 1, 0.001, 1) & laag 1))
  --     & uOp TanH


  return ()
