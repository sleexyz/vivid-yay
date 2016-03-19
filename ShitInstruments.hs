{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ShitInstruments where

import Vivid
import Util
import Data.Function

fmthingy :: SDBody args Signal
fmthingy = do

  sig <- lfPulse (freq_ 0.5)
    & linexp (-1) 1 100 200

  pulse <- lfPulse (freq_ 1)
  pulse <- lfPulse (freq_ $ 2 ~* 8) ~* 2 ~+ pulse

  lfPulse (freq_ $ sig ~* pulse)
