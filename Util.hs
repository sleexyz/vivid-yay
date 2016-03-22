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

midiRatio :: ToSig i a => i -> SDBody a Signal
midiRatio x = midiCPS x ~/ midiCPS (dc 0)


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
linlin (a, b, c, d) x = c ~+ (d ~- c) ~* ((x ~- a) ~/ (b ~- a))

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

infixl !!!
(!!!) :: [a] -> Int -> [a]
xs !!! n = join $ replicate n xs

laag :: ( ToSig input args
        , ToSig lag args
        ) => lag -> input -> SDBody args Signal
laag l input = lag2 (in_ input, secs_ l)

laaag :: ( ToSig input args
        , ToSig lag args
        ) => lag -> input -> SDBody args Signal
laaag l input = lag (in_ input, secs_ l)

infixl 5 <&>
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

raand :: SDBody args Signal
raand = rand (lo_ 0, hi_ 1)

seelect :: (ToSig i args)
           => [SDBody args Signal]
           -> i
           -> SDBody args Signal
seelect scale x = select (x ~* length scale) scale

deelay2 :: (ToSig i args)
           => i
           -> SDBody args Signal
deelay2 input = delay2 (in_ input)
