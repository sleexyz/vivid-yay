{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyInstruments where

import Vivid
import Util
import Data.Function


fract1 :: SDState args Signal
fract1 = do
  lmy <- linexp 0 1 100 440 my

  sig1 <- fract 0.01
    & linexp (-1) 1 0.1 1

  sig2 <- fract 0.01
    & linexp (-1) 1 0.001 0.01

  resonz(in_ $ whiteNoise, freq_ $ 1000 ~* sig1, bwr_ sig2)

fract2 :: SDState args Signal
fract2 = do
  lmy <- linexp 0 1 100 440 my

  f1 <- fract 0.01
  sig1 <- linexp (-1) 1 0.1 1 $ f1

  sig2 <- linexp (-1) 1 0.0833333 1 $ fract f1

  resonz(in_ $ whiteNoise, freq_ $ 1000 ~* sig1, bwr_ 0.001)

fractShit :: SDState args Signal
fractShit = do
  f1 <- fract 0.01
  f2 <- fract $ linexp (-1) 1 0.01 10 f1

  lmy <- linexp (-1) 1 25 16000 f2
  resonz (in_ whiteNoise, freq_ lmy, bwr_ 0.0001)


-- foo :: SDState args Signal
-- foo = do
--   lmy <- linexp 0 1 100 10000 my

--   sig1 <- fract 0.01
--     & linexp (-1) 1 0.001 10
--   sig2 <- fract sig1
--     & linexp (-1) 1 0.001 1

--   sig3 <- fract 0.001
--     & linexp (-1) 1 0.1 1

--   resonz (in_ $ whiteNoise ~* sig2, freq_ $ lmy ~* sig2, bwr_ sig2) ~/ 5
