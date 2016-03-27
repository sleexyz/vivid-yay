{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Source2016 where

import           Data.Function
import           MyCombinators
import qualified MyInstruments as M
import qualified Scales        as S
import           Util
import           Vivid

-- masterScale = [2, 2, 4, 7, 9, 11, 19]
-- masterScale = [2, 2, 4, 7, 9, 11]
masterScale = [0,  4, 7, 11, 14]
-- masterScale = [0, 2, 3, 5, 7, 8, 10]

masterShift :: SDBody args Signal -> SDBody args Signal
masterShift x = x ~+ ( my
                   & seelectKR ((~+0) <$> [0, 2, 4])
                   & laag 0.5
                 )

masterClockFreq :: SDBody args Signal
masterClockFreq = kIn (bus_ 903) & linexp (0, 1, 0.1, 10)

-- masterScale = [2, 2, 4, 7, 9, 11, 19]
-- masterScale = [2, 2, 4, 7, 9, 11]

-- masterScale = [0,  4, 7, 11, 14]
-- masterScale = [0, 2, 3, 5, 7, 8, 10]

fuzzwuzz = do
  let dur = 2

  fork $ do
    play $ do
      pinkNoise
        & (~*1)
        & \x -> x ~* adsrGen (dur * 3/20) (dur * 17/20) 0 0 Curve_Cubed none
        & \x -> pan2 ( in_ x
                     , pos_ $ sinOsc (freq_ $ lfSaw (freq_ 0.2) & linlin (-1, 1, 0, 10)) ~* 0.2
                     )
        >>= \x -> sequence $ x
                  <&> \x -> freeVerb (in_ x, room_ 0.8)
    return ()
    -- wait (dur * 5)
    -- free s1







birdies_ = sdNamed "birdies" () $ do
  let seq = ( (+)
              -- <$> [0]
              -- <$> [0, 0]
              <$> [0, 0, 24, 36]
              -- <$> [0, 24, 36, 36, 48]

              <*> ([0, 0, 0]
                   ++ masterScale
                  )
            )

  base <- masterShift (dc 60)
    -- & (~+12)

  tones <- sequence $ seq
    <&> (~+base)
    <&> midiCPS

  s <- M.birdies tones (kIn (bus_ 902))

  (sequence $  s
   <&> \x -> x ~* kIn (bus_ 901) ~* 2
    )
    >>= out 2

wind_ = sd () $ do
  mod <- masterClockFreq

  freq <- sinOsc (freq_ mod)
  -- freq <- lfSaw (freq_ mod)
  -- freq <- whiteNoise
    & linlin (-1, 1, 0, 1)
    & seelect ( (take 2 $ drop 2 masterScale)
                <&> (~+60)
                -- <&> (~+12)
                -- <&> (~+7)
              )
    & \x -> x ~+ sinOsc (freq_ 2) ~* 0.2 -- vibrato
    & masterShift
    & midiCPS

  (M.wind freq) ~* (kIn (bus_ 904) ~* 5)
    & \x -> out 2 [x, x]


masterMix_ :: IO (Node '[])
masterMix_ = do
  play $ do
    left <- soundIn (Bus 2)
    right <- soundIn (Bus 3)


    sequence ( [left, right]
               <&> \x -> x ~* 1

                         -- -- | down sampling
                         >>= \x -> (do
                                       choice <- kIn (bus_ 905)
                                       latched <- latch (in_ x, trig_ $ impulse (freq_ $ choice
                                                                                 & linexp (0, 1, 4000, 24000)
                                                                                ))
                                       latched ~* (1 ~- choice) ~+ x ~* choice
                                   )

                         -- | low pass
                         >>= \x -> lpf (in_ x, freq_ $
                                        kIn (bus_ 906) & linexp (0, 1, 100, 24000)
                                        )


                         -- | master vol
                         >>= (~*(kIn (bus_ 908) & linlin (0, 1, 0, 40)))

                         -- | reverb
                         >>= \x -> freeVerb (in_ x, room_ $
                                             kIn (bus_ 907) & linlin (0, 1, 0, 10)
                                            )

                         >>= tanh'

                         -- >>= \x -> (do
                         --               (~*0.2) x ~+ (x
                         --                             -- & \x -> freqShift ( in_ x, freq_ $ freq)
                         --                             -- & uOp Recip
                         --                             & \x -> normalizer (in_ x)
                         --                             & \x -> lpf (in_ x, freq_ $ my & linexp (0, 1, 400, 8000))
                         --                             -- -- & tanh'
                         --                             -- & uOp Recip
                         --                             & \x -> latch (in_ x, trig_ $ impulse (freq_ $ freq ~* mx ~* 1))
                         --                            )
                         --               )

                         -- >>= \x -> pitchShift ( in_ x
                         --                      , ratio_ $ my ~* 2
                         --                      , windowSize_ 1
                         --                      )

             )

piano_ :: IO (Node '[])
piano_ = do
  play $ do
    s <- (pianoize $ \x -> sinOsc (freq_ $ x
                                   & midiCPS
                                  ))
      & \x -> x ~* 0.01
      & spaceify (kIn (bus_ 902))
    out 2 [s, s]

text2speech =  do
  play $ do
    left <- soundIn (Bus 4)
    right <- soundIn (Bus 5)

    sequence ( [left, right]
               <&> \x -> x ~* 0.05
             )
      >>= out 2

drone_ :: SDBody '[] Signal ->  SynthDef '[]
drone_ f0 = sd () $ do
  phase <- raand
  let panFreq = 0.1

  s <- sinOsc (freq_ $ f0)
    & \x -> x ~* (sinOsc (freq_ masterClockFreq) & linexp (-1, 1, 0.5, 1))
    & \x -> x ~* 0.1
    & \x -> x ~* ( sinOsc (freq_ panFreq, phase_ $ phase ~+ 0.5)
                   & linexp (-1, 1, 0.5, 1)
                 )
    & \x -> pan2 ( in_ x
                 , pos_ $ sinOsc (freq_ $ panFreq , phase_ phase)
                 )
  out 2 s


playDrone = 
  replicateM_ 10 $ synth  (drone_
                           (do
                               let seq = do
                                     note <- masterScale <&> (~*1)
                                     trans <- [0, 0, 1, 2, 3, 4] <&> (~*12)
                                     return $ note ~+ trans ~+ 12

                               fract 0.01
                                 & linlin (-1, 1, 0, 1)
                                 & seelectKR seq
                                 & midiCPS
                           )
                          ) ()
{-

TODO:
- fade in/out for recompile
- adsrGen-ize piano
- figure out compander

-}
