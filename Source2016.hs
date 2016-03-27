{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}


module Main where

import           Data.Function
import           MyCombinators
import           Util

import           Vivid
import           NotSource2016


myFreq = kIn (bus_ 903) & linexp (-1, 1, 0.1, 100)

drone ::  SDBody '[] Signal -> SynthDef '[]
drone freq = sd () $ do
  pos <- sinOsc (freq_ myFreq)
  s <- sinOsc (freq_ freq)
    & \x -> x ~* sinOsc (freq_ 0.1) & linexp (-1, 1, 0.1, 1)
    & \x -> x ~* 0.01
    & \x -> pan2 ( in_ x
                 , pos_ pos
                 )
  out 2 s


main = do
  cmdPeriod
    >> fuzzwuzz
    >> masterMix_

  replicateM_ 5 $ synth (drone $ 
         do
           let seq = ((+)
                      <$> ((*12) <$> [0..7])
                      <*> [0, 2, 3, 5, 7, 8, 10]
                     )
                 <&> (~*1)
                 <&> (~+36)

           f1 <- fract 0.01 & linexp (-1, 1, 0.01, 10)
           fract f1
             & linlin (-1, 1, 0, 1)
             & seelectKR seq
             & midiCPS
         ) ()

  replicateM_ 1 $ synth (drone $ 
         do
           let seq = ((+)
                      <$> ((*12) <$> [0..3])
                      <*> [0, 2, 3, 5, 7, 8, 10]
                     )
                 <&> (~*1)
                 <&> (~+36)

           fract 0.01
             & linlin (-1, 1, 0, 1)
             & seelectKR seq
             & midiCPS
         ) ()

  play $ do
    s <- pianoize (
      \x -> sinOsc (freq_ $ midiCPS x) ~* 0.01
      )
    out 2 [s, s]


