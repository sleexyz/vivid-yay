{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}


-- import Reflex.Dom
import Vivid


mx = mouseX (MinVal 0) (MaxVal 1)
my = mouseY (MinVal 0) (MaxVal 1)

-- logify x = uOp Log2 $ x ~+ 1

-- expify :: toSigM i0 a => i0 -> SDState a Signal
-- expify = biOp Pow 2.7

lmx = uOp Log2 $ mx ~+ 1
lmy = uOp Log2 $ my ~+ 1


-- infixl 8 ~**
-- (~**) :: (ToSigM i0 a, ToSigM i1 a) => i0 -> i1 -> SDState a Signal
-- (~**) = binaryOp Pow


-- infix 5 &
-- (&) :: a -> (a -> b) -> b
-- a & f = f a

-- fract base iterations =
--   foldl (\acc x -> acc ~+ (lfdClipNoise (freq_ $ base ~* 2.0 ~** x) ~* (2.0 ~** (toF iterations ~- x)))) 0 [1..iterations]
--     ~/ (2.0 ~** toF iterations)

ilike = sd (4::I "foo") $ do
  m <- sinOsc (freq_ $  (A::A "foo") ~* lmy ~* 1000)
  s <- sinOsc (freq_ $  m ~* lmx ~* 10000)
  pulse <- pulse (freq_ 1)
  myIn <- whiteNoise
  s <- resonz (in_ myIn, freq_ $ 1000 ~* lmy ~* m, bwr_ $ lmx ~* 0.1)

  out 0 [s, s]


modSin = sd () $ do
  m <- sinOsc (freq_ $  lmy ~* 1000)
  s <- sinOsc (freq_ $  m ~* lmx ~* 10000)

  out 0 [s, s]
  -- m <- sinOsc (freq_ $  lmy ~* 100)
  -- pulse <- pulse (freq_ 1)
  -- s <- resonz (in_ whiteNoise, freq_ $ 10000 ~* lmy ~* m, bwr_ $ lmx ~* 0.1)

  -- out 0 [s, s]

main :: IO ()
main = do
  cmdPeriod
  synth modSin ()
  -- synth modSin ()
  -- synth modSin ()
  -- synth modSin ()
  -- synth modSin ()
  -- synth modSin ()
  -- synth modSin ()
  -- synth modSin ()
  -- synth modSin ()
  -- synth modSin ()
  -- synth modSin ()
  -- synth modSin ()
  return ()

  
