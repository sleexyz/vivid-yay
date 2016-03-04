{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- import Reflex.Dom
import Vivid
import Util
import Data.Function



-- infix 5 &
-- (&) :: a -> (a -> b) -> b
-- a & f = f a

-- fract :: (Real s, ToSigM s args) => s -> Int -> SDState args Signal
-- fract baseFreq iterations
--   = foldl f (dc 0) [1..iterations]
--   where
--     f :: SDState args Signal -> Int -> SDState args Signal
--     f acc x = acc ~+ (lfdClipNoise (freq_ $ baseFreq ~* 2.0 ~** (dc $ toF $ x)) ~* (2.0 ~** ((dc $ toF $ iterations - x))))


-- incoherent instances?
{-|
 fract = {|iterations = 10, baserate = 100|
		  Mix.kr(Array.fill(iterations, {|i| LFDClipNoise.kr(freq: baserate*(2**i), mul: 2**(iterations - (i + 1)))}))/(2**(iterations));
	  };
-}

-- fract :: (ToSigM s args) => s -> Int -> SDState args Signal
-- fract baseFreq iterations = mix
--   [ lfdClipNoise (freq_ $ baseFreq ~* (dc 2) ~** (dc $ toF $ x))
--     ~* ((dc 2) ~** (dc $ toF $ iterations - (x + 1)))
--   | x <- [1..iterations]
--   ] ~/ ((dc 2) ~** (dc $ toF $ iterations))

fract :: (ToSigM s args) => s -> Int -> SDState args Signal
fract baseFreq iterations = (mix $ do
    x <- [1..iterations]
    return $ lfdClipNoise (freq_ $ baseFreq ~* (dc 2) ~** (dc $ toF $ x))
      ~* ((dc 2) ~** (dc $ toF $ iterations - (x + 1)))
  ) ~/ ((dc 2) ~** (dc $ toF $ iterations))

-- ilike = sd (4::I "foo") $ do
--   lmx <- linexp 0 1 1 1000 mx
--   lmy <- linexp 0 1 1 1000 my
--   m <- sinOsc (freq_ $  (A::A "foo") ~* lmy)
--   s <- sinOsc (freq_ $  m ~* lmx)
--   pulse <- pulse (freq_ 1)
--   myIn <- whiteNoise
--   s <- resonz (in_ myIn, freq_ $ 1000 ~* lmy ~* m, bwr_ $ lmx ~* 0.1)

--   out 0 [s, s]


coolFract :: SynthDef '[]
coolFract = sd () $ do
  lmy <- linexp 0 1 100 10000 my

  sig1 <- fract 0.001 10
    & linexp (-1) 1 0.1 1

  sig2 <- fract 0.01 10
    & linexp (-1) 1 0.001 0.01

  s <- resonz(in_ pinkNoise, freq_ $ lmy ~* sig1, bwr_ sig2)

  out 0 [s, s]

foo :: SynthDef '[]
foo = sd () $ do
  lmy <- linexp 0 1 100 10000 my

  sig1 <- fract 0.01 5
    & linexp (-1) 1 0.001 10
  sig2 <- fract sig1 10
    & linexp (-1) 1 0.001 1

  sig3 <- fract 0.001 10
    & linexp (-1) 1 0.1 1



  s <- resonz(in_ pinkNoise, freq_ $ lmy ~* sig2, bwr_ sig2) ~/ 5

  out 0 [s, s]

main :: IO ()
main = do
  cmdPeriod
  replicateM_ 5 $ (synth foo () :: IO (Node '[]))
  replicateM_ 5 $ (synth coolFract () :: IO (Node '[]))
  return ()
