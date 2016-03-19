{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Util where

import Vivid
import Vivid.Actions.Class (VividAction)
import GHC.TypeLits (Symbol)


mx :: SDBody args Signal
mx = mouseX (min_ 0, max_ 1)

my :: SDBody args Signal
my = mouseY (min_ 0, max_ 1)


-- infixl 8 ~**
-- (~**) :: (ToSig i0 args, ToSig i1 args)
--          => i0
--          -> i1
--          -> SDBody args Signal
-- (~**) = binaryOp Pow

linexp :: ( ToSig a args
          , ToSig b args
          , ToSig c args
          , ToSig d args
          , ToSig x args
          ) => (a, b, c, d) -> x -> SDBody args Signal
linexp (a, b, c, d) x = c ~* ((d ~/ c) ~** ((x ~- a) ~/ (b ~- a)))

linlin :: ( ToSig a args
          , ToSig b args
          , ToSig c args
          , ToSig d args
          , ToSig x args
          ) => (a, b, c, d) -> x -> SDBody args Signal
linlin (a, b, c, d) x = (d ~- c) ~* ((x ~- a) ~/ (b ~- a))

-- TODO make better arrow syntax for vivid (SignalFunctions??)

-- class ToSD s where
--   toSD :: (ToSD s, VarList argList) => argList
--           -> SDBody (InnerParams argList) s
--           -> SynthDef (InnerParams argList)

--   playWith :: (ToSD s, VarList argList, VividAction m) => argList
--            -> SDBody (InnerParams argList) s
--            -> m (Node (InnerParams argList))

--   playWith argList bit = synth (toSD argList bit) argList

-- instance ToSD Signal where
--   toSD argList bit = sd argList $ bit >>= \a -> out 0 $ [a, a]

-- instance ToSD (Signal, Signal) where
--   toSD argList bit = sd argList $ bit >>= \(a, b) -> out 0 $ [a, b]



-- fract :: (ToSig s args) => s -> Int -> SDBody args Signal
makeFract :: (ToSig input args) => Int -> input -> SDBody args Signal
makeFract numIterations baseFreq
  = mix channels ~/ ((dc 2) ~** (dc $ toF numIterations))
  where
    channels = do
      x <- [1..numIterations]
      let y = lfdClipNoise (freq_ $ baseFreq ~* (dc 2) ~** (dc $ toF $ x))
      let mul = dc 2 ~** (dc $ toF $ numIterations - (x + 1))
      return $ y ~* mul



fract :: (ToSig input args) => input -> SDBody args Signal
fract = makeFract 5
