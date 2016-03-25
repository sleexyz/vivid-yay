    -- play $ do
  --   snd <- soundIn (Bus 0)

  --   snd <- pitchShift ( in_ snd
  --              , ratio_ $ (sinOsc (freq_ 0.1) ~* my) ~+ 1
  --                -- & seelect ((~*1) <$> [ 1, 2])
  --              , windowSize_ 0.01
  --              )


  --   shift <- mx  & linexp (0, 1, 100, 48000)
  --   freq <- my & linexp (0, 1, 2000, 4000)
  --   -- freq <- 1 ~* 8000

  --   snd
  --     -- & \x -> freqShift ( in_ x, freq_ $ shift)
  --     & \x -> latch (in_ x, trig_ $ impulse (freq_ freq))
  --     -- & \x -> lpf (in_ x, freq_ $ my & linexp (0, 1, 400, 8000))
  --     -- & \x -> hpf (in_ x, freq_ 800)
  --     -- & \x -> freqShift ( in_ x, freq_ $ (-1) ~* shift)
  --     -- & \x -> latch (in_ x, trig_ $ impulse (freq_ freq))
  --     & \x -> lpf (in_ x, freq_ $ my & linexp (0, 1, 400, 8000))
  --     & tanh'
  --     & (~*1.0)

  -- play $ do
  --   snd <- soundIn (Bus 0)

  --   -- shift <- mx  & linexp (0, 1, 100, 48000)
  --   let ass = do
  --         shift <- 1 ~* 40000
  --         freq <- my & linexp (-1, 1, 2000, 8000) & laag 1

  --         pulse <- impulse (freq_ freq)

  --         snd
  --           & \x -> freqShift ( in_ x, freq_ $ shift)
  --           & \x -> latch (in_ x, trig_ pulse)
  --           & \x -> freqShift ( in_ x, freq_ $ (-1) ~* shift)
  --           & \x -> latch (in_ x, trig_ $ pulse)
  --           & \x -> x
  --           & \x -> lpf (in_ x, freq_ 8000)
  --           & \x -> hpf (in_ x, freq_ 800)
  --           & (~*0.5)
  --   ass
  --     & tanh'
  --     & \x -> freeVerb (in_ x, room_ 5)
