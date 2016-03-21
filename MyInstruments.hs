{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module MyInstruments where

import Vivid
import Util
import Data.Function
import MyCombinators


-- dfract :: SDBody args Signal
-- dfract = do

fract1 :: SDBody args Signal
fract1 = do
  lmy <- linexp (0, 1, 100, 440) my

  sig1 <- fract 0.001
    & linexp (-1, 1, 0.1, 1)
    & laag 10

  sig2 <- fract 0.01
    & linexp (-1, 1, 0.001, 0.01)
    & laag 0

  resonz(in_ $ whiteNoise, freq_ $ 1000 ~* sig1, bwr_ sig2)

fract2 :: SDBody args Signal
fract2 = do
  -- lmy <- linexp 0 1 100 10000 my

  f1 <- fract 0.01
  sig1 <- linexp (-1, 1, 0.1, 1) $ f1

  sig2 <- linexp (-1, 1, 1/12,  1) $ fract f1

  resonz(in_ $ whiteNoise, freq_ $ 1000 ~* sig1, bwr_ 0.001)


-- birdies0 :: SDBody args Signal
-- birdies0 = do
--   let birdy = do
--         f0 <- fract 0.01
--         f1 <- fract $ f0 & linexp (-1, 1, 0.0001, 10)
--         f2 <- fract $ f1 & linexp (-1, 1, 0.01, 10)

--         let seq = (midiCPS . dc ) <$> ( (+) <$> [36, 60, 72]
--                                         <*> [0, 2, 4, 7, 9, 11]
--                                       )

--         freq <- select (f2 & linlin (-1, 1, 0, length seq)) seq

--         resonz ( in_ $ brownNoise ~* (f1  & linexp (-1, 1, 0.001, 1))
--               , freq_ $ freq & laag 0.01
--               , bwr_ $ 0.001
--               )
--   (mix =<< replicateM 5 birdy)
--     & uOp TanH
--     & \x -> freeVerb (in_ x, room_ 5)

birdy :: SDBody args Signal
birdy = do

    f0 <- fract 0.01
    f1 <- fract $ f0 & linexp (-1, 1, 0.0001, 10)
    f2 <- fract $ f1 & linexp (-1, 1, 0.01, 10)

    let seq = (midiCPS . dc ) <$> ( (+) <$> [36, 60, 72]
                                    <*> [0, 2, 4, 7, 9, 11]
                                  )

    freq <- select (f2 & linlin (-1, 1, 0, length seq)) seq

    resonz ( in_ $ brownNoise ~* (f1  & linexp (-1, 1, 0.001, 1))
          , freq_ $ freq & laag 0.01
          , bwr_ $ 0.001
          )

birdies :: SDBody args [Signal]
birdies = do
  [left,right]  <- return $ (paan birdy $ fract 0.001)
        <&> uOp TanH
        -- <&> \x -> freeVerb (in_ x, room_ 5)
  out 0 [left, right]


paan :: SDBody args Signal -> SDBody args Signal-> [SDBody args Signal]
paan input pos = [ ((1 :: Float) ~- uOp Sqrt pos)  >>= (~*) input
                 , uOp Sqrt pos  >>= (~*) input
                 ]





thingy1 :: SDBody args Signal
thingy1 = do
  imp <- impulse (freq_ $ fract 0.1 & linexp (-1, 1, 1, 20) & laag 0) ? KR
  foo <- dseq (repeats_ inf)
    =<< sequence ((midiCPS . (+38))
                  <$> ([0, 2, 3, 5] !!! 16)
                  ++ ((+12) <$> [0, 2, 3, 5] !!! 16)
                 )
    -- =<< sequence (fmap (midiCPS . (+38)) [-2, 2, 3, 5])


  dem <- demand (trig_ imp, reset_ 0, ugen_ foo)
  freq <- dem
    & laag 0.1

  (mix $ [sinOsc (freq_ freq)])
    & (~* 0.1)
    & uOp TanH

thingy2 = do
  foo <- 10 ~* layer 1 fract1
    -- lmy <- my & linexp (0, 1, 4000, 22050)
  lmy <- lfPulse (freq_ 1) & linexp (0, 1, 2000, 16000)
  -- lmy <- lfSaw (freq_ 1) & linexp (0, 1, 2000, 16000)
  -- lmx <- mx & linexp (0, 1, 100, 16000) & laag 0.01
  lmx <- lfSaw  (freq_ 2) & linexp (-1, 1, 100, 16000) & laag 0.01
  foo
    & \x -> latch (in_ x, trigger_ $ impulse (freq_ lmy))
    & tanh'
    & (~*0.2)
    & \x -> lpf (in_ x, freq_ lmx)

thingy3 = replicateM_ 5 $ play $ do
  foo <- 10 ~* layer 1 fract1
  -- lmy <- my & linexp (0, 1, 4000, 22050)
  lmy <- lfSaw (freq_ 1) & linexp (0, 1, 8000, 16000)
  -- lmx <- mx & linexp (0, 1, 100, 16000) & laag 0.01
  lmx <- lfSaw  (freq_ 8) & linexp (-1, 1, 100, 16000)
  foo
    & \x -> latch (in_ x, trigger_ $ impulse (freq_ lmy))
    & tanh'
    & (~*0.2)
    & \x -> lpf (in_ x, freq_ lmx)
