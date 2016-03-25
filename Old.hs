
  -- play $ do

  --   base <- 72 ~* 1

  --   seq <- sequence $ (midiCPS . (~+base))
  --                   <$> [0, 7, 19, 24]

  --   foo <- dseq (repeats_ inf) seq


  --   imp <- impulse (freq_ $ fract 0.01 & linexp (-1, 1, 1, 20) & laag 10) ? KR
  --   dem <- demand (trig_ imp, reset_ 0, ugen_ foo)
  --   freq <- dem
  --     & laag 0.1

  --   (mix $ [sinOsc (freq_ freq)])
  --     & (~* 0.1)
  --     & (~* (fract 0.1 & linexp (-1, 1, 0.001, 1) & laag 1))
  --     & uOp TanH

  -- play $ do
  --   base <- my
  --       & laag 1
  --       & seelect ((~+77) <$> [0, 2, 10])

  --   wn <- whiteNoise
  --   freq <- fract 0.1
  --     & linlin (-1, 1, 0, 1)
  --     & seelect ((midiCPS . (~+base)) <$> [0, 2, 12, 14])

  --   (M.wind freq)
  --     & tanh'
  --     & (~*0.5)

  -- -- | Play  scales

  -- play $ do
  --   sinOsc (freq_ $ my
  --           & seelect (((+)
  --                       <$> ([1..4] <&> (*12))
  --                       <*> [0, 2, 4, 6, 8])

  --                      <&> (+36)
  --                      <&> midiCPS
  --                     ))
  --   & (~* 0.1)

  -- 

  -- | alsa input mixer
-- alsaIn = 
--   play $ do
--     left <- soundIn (Bus 4)
--     right <- soundIn (Bus 5)

--     lr <- sequence ( [left, right]
--                   <&> \x -> x ~* 1
--                             -- | down sampling
--                             -- >>= \x -> latch (in_ x, trig_ $ impulse (freq_ $
--                             --                                          kIn (bus_ 905) & linexp (0, 1, 4000, 24000)
--                             --                                         ))

--                             -- | low pass

--                             -- >>=  \x -> resonz (in_ x, freq_ $
--                             --                    kIn (bus_ 904) & linexp (0, 1, 100, 16000)
--                             --                   , bwr_ $ kIn (bus_ 903) & linexp (0, 1, 0.001, 1)
--                             --                   )

--                             >>= \x -> (do
--                                           s <- pianoize (\freq -> resonz (in_ x, freq_ freq
--                                                                          , bwr_ $ kIn (bus_ 903) & linexp (0, 1, 0.001, 1)
--                                                                          ))
--                                           s1 <- dc 0
--                                           s1 <- resonz (in_ x, freq_ $
--                                                         kIn (bus_ 904) & linexp (0, 1, 100, 16000)
--                                                        , bwr_ $ kIn (bus_ 903) & linexp (0, 1, 0.001, 1)
--                                                        )
--                                           s ~+ s1

--                                       )
--                             >>= \x -> x ~* 10
--                             >>= tanh'
--                             )
--     out 2 lr
