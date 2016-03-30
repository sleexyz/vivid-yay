{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}


module Main where

import           Data.Function
import           MyCombinators
import           Util

import NotSource2016
import           Vivid

main = do
  cmdPeriod
  replicateM_ 5 $ play $ do

    do
      input <- whiteNoise
      shift <- my
        >>= laag 1
        >>= seelectKR ([0,2, 4] <&> toSig)

      let x = resonz  (in_ input, bwr_ 0.001, freq_ $ fract 0.01
                      & linlin (-1, 1, 0.0001, 0.01)
                      >>= \x -> fract x
                      & linlin (-1, 1, 0, 1)
                      >>= seelectKR ((do
                                        x <- [0, 2, 3, 7, 11]
                                        y <- [1, 1, 2, 3] <&> (*24)
                                        return $ x + y
                                    )
                                )
                      & (~+shift)
                      & (~+20)
                      & midiCPS
                      )

      x <- (mix $ replicate 1 $ x)
        & last . take 100 . iterate tanh'
        & \x -> freeVerb (in_ x, room_ 5)
        >>= (~*10)
        >>= \x -> pan2 (in_ x, pos_ $ sinOsc (freq_ 1))
      return  x
