
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyCombinators where

import Vivid
import Util
import Data.Function

downsample :: SDBody args Signal -> SDBody args Signal
downsample input = do
  lmy <- my & linexp (-1, 1, 20, 8000)
  trigger <- lfPulse (freq_ lmy)
  uOp TanH $ latch (in_ input, trigger_ trigger)


layer :: Int -> SDBody args Signal -> SDBody args Signal
layer n body = uOp TanH $ mix $ replicate n body


spaceify :: Signal -> SDBody args Signal
spaceify sig = do
  cont <- mx
  sig
    & (~** (cont & linlin (0, 1, 0.5, 1)))
    & (~* (cont  & linexp (0, 1, 1/16, 5/4)))
