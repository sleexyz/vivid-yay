{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Util where

import Vivid


mx = mouseX (MinVal 0) (MaxVal 1)
my = mouseY (MinVal 0) (MaxVal 1)


infixl 8 ~**
(~**) :: (ToSigM i0 list, ToSigM i1 list)
         => i0
         -> i1
         -> SDState list Signal
(~**) = binaryOp Pow

linexp :: ( ToSigM a list
          , ToSigM b list
          , ToSigM c list
          , ToSigM d list
          , ToSigM x list
          ) => a -> b -> c -> d -> x -> SDState list Signal
linexp a b c d x = c ~* ((d ~/ c) ~** ((x ~- a) ~/ (b ~- a)))

linlin :: ( ToSigM a list
          , ToSigM b list
          , ToSigM c list
          , ToSigM d list
          , ToSigM x list
          ) => a -> b -> c -> d -> x -> SDState list Signal
linlin a b c d x = (d ~- c) ~* ((x ~- a) ~/ (b ~- a))
