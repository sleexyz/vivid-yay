{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Util where

import Vivid
import Vivid.Actions.Class (VividAction)
import GHC.TypeLits (Symbol)


mx :: SDState args Signal
mx = mouseX (MinVal 0) (MaxVal 1)

my :: SDState args Signal
my = mouseY (MinVal 0) (MaxVal 1)


infixl 8 ~**
(~**) :: (ToSigM i0 args, ToSigM i1 args)
         => i0
         -> i1
         -> SDState args Signal
(~**) = binaryOp Pow

linexp :: ( ToSigM a args
          , ToSigM b args
          , ToSigM c args
          , ToSigM d args
          , ToSigM x args
          ) => a -> b -> c -> d -> x -> SDState args Signal
linexp a b c d x = c ~* ((d ~/ c) ~** ((x ~- a) ~/ (b ~- a)))

linlin :: ( ToSigM a args
          , ToSigM b args
          , ToSigM c args
          , ToSigM d args
          , ToSigM x args
          ) => a -> b -> c -> d -> x -> SDState args Signal
linlin a b c d x = (d ~- c) ~* ((x ~- a) ~/ (b ~- a))

-- TODO make better arrow syntax for vivid (SignalFunctions??)

class ToSD s where
  toSD :: (ToSD s, ArgList argList) => argList
          -> SDState (InnerArgs argList) s
          -> SynthDef (InnerArgs argList)

  playWith :: (ToSD s, ArgList argList, VividAction m) => argList
           -> SDState (InnerArgs argList) s
           -> m (Node (InnerArgs argList))

  playWith argList bit = synth (toSD argList bit) argList

instance ToSD Signal where
  toSD argList bit = sd argList $ bit >>= \a -> out 0 $ [a, a]

instance ToSD (Signal, Signal) where
  toSD argList bit = sd argList $ bit >>= \(a, b) -> out 0 $ [a, b]



-- fract :: (ToSigM s args) => s -> Int -> SDState args Signal
makeFract :: (ToSigM input args) => Int -> input -> SDState args Signal
makeFract numIterations baseFreq
  = mix channels ~/ ((dc 2) ~** (dc $ toF numIterations))
  where
    channels = do
      x <- [1..numIterations]
      let y = lfdClipNoise (freq_ $ baseFreq ~* (dc 2) ~** (dc $ toF $ x))
      let mul = dc 2 ~** (dc $ toF $ numIterations - (x + 1))
      return $ y ~* mul



fract :: (ToSigM input args) => input -> SDState args Signal
fract = makeFract 5
