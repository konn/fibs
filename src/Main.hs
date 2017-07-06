{-# LANGUAGE FlexibleContexts, BangPatterns #-}
{-# LANGUAGE DataKinds, GADTs, PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where
import Data.Ratio
import GHC.TypeLits
import Control.Monad.Memo
import Control.Monad.ST
import Criterion.Main
import Data.Matrix

-- | Q(√n) for square-free n.
data Quadratic (n :: Nat) = QExt Rational Rational
                 deriving (Eq)

type QRoot5 = Quadratic 5

root5 :: Quadratic 5
root5 = QExt 0 1

instance KnownNat n => Show (Quadratic n) where
  showsPrec _ (QExt 0 0) = showString "0"
  showsPrec d (QExt a 0) = showsPrec d a
  showsPrec d q@(QExt 0 1) =
    showParen (d >= 10) $
    showChar '√' . showsPrec 10 (natVal q)
  showsPrec d q@(QExt 0 b) =
    showParen (d >= 10) $
    showsPrec 10 b . showChar '√' . showsPrec 10 (natVal q)
  showsPrec d q@(QExt a b) =
    showParen (d >= 10) $
    showsPrec 10 a . showString " + " .
    showsPrec 10 b . showChar '√' . showsPrec 10 (natVal q)

isPositive :: KnownNat n => Quadratic n -> Bool
isPositive q@(QExt a b) =
  a*a + signum b * fromInteger (natVal q) * (b * b) > 0

instance KnownNat n => Num (Quadratic n) where
  fromInteger n = QExt (fromInteger n) 0
  (QExt a b) + (QExt c d) = QExt (a + c) (b + d)
  (QExt a b) * r@(QExt c d) =
    QExt (a * c + fromInteger (natVal r) * b * d) (a*d + b*c)
  negate (QExt a b) = QExt (-a) (-b)
  QExt a b - QExt c d = QExt (a - c) (b - d)
  signum (QExt 0 0) = 0
  signum q | isPositive q = 1
           | otherwise = -1
  abs q = signum q * q

instance KnownNat n => Ord (Quadratic n) where
  q <= r = q == r || isPositive (r - q)

instance KnownNat n => Fractional (Quadratic n) where
  fromRational r = QExt r 0
  recip x@(QExt q r) =
    let base = fromInteger $ natVal x
        den = q*q - base * r*r
    in QExt (q / den) (- r / den)

rational :: Quadratic t -> Rational
rational (QExt a _) = a

fibRat :: Int -> Integer
fibRat n = numerator $ rational $ (((1+root5) / 2)^n - ((1 - root5)/2)^n)/root5

fibNaive :: Int -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)

fibZipWith :: Int -> Integer
fibZipWith = (fib0 !!)
  where fib0 = 0 : 1 : zipWith (+) fib0 (tail fib0)

fibBadZipWith :: Int -> Integer
fibBadZipWith = (fib0 !!!)
  where
    fib0 = 0 : 1 : zipWith0 (+) fib0 (tail fib0)
    _      !!! n | n < 0 =  errorWithoutStackTrace "Prelude.!!: negative index"
    []     !!! _         =  errorWithoutStackTrace "Prelude.!!: index too large"
    (x:_)  !!! 0         =  x
    (_:xs) !!! n         =  xs !!! (n-1)

    zipWith0 _f []     _bs    = []
    zipWith0 _f _as    []     = []
    zipWith0 f  (a:as) (b:bs) = f a b : zipWith0 f as bs

fibm :: (MonadMemo Int Integer m, Num Integer, Num Int, Eq Int)
     => Int -> m Integer
fibm 0 = return 0
fibm 1 = return 1
fibm n = (+) <$> memo fibm (n-1) <*> memo fibm (n-2)

fibSTArr :: Int -> Integer
fibSTArr n = runST $ evalArrayMemo (fibm n) (0,n)

fibMap :: Int -> Integer
fibMap n = startEvalMemo (fibm n)

fibDP :: Int -> Integer
fibDP = fst . loop (0, 1)
  where
    loop t 0 = t
    loop (a, !b) n = loop (b, a+b) (n-1)

fibMatPow :: Int -> Integer
fibMatPow n =
  let stepper = fromLists [[0,1],[1,1]]
  in (stepper ^ n * fromLists [[0],[1]]) ! (1,1)

fibMatDiag :: Int -> Integer
fibMatDiag n =
  let p  = fromLists [[1, 1],[(1+root5)/2, (1-root5)/2]]
      p' = fromLists [[1/2 - root5/10, root5/5]
                     ,[1/2 + root5/10, -root5/5]]
      phi = (1+root5)/2
      psi = (1-root5)/2
      ans = diagonalList 2 0 [phi^n, psi^n]
  in numerator $ rational $ (p * ans * p' * fromLists [[0],[1]]) ! (1,1)

fibLog :: Int -> Integer
fibLog = fst . loop
  where
    loop 0 = (0,1)
    loop n
      | odd n =
        let (a, b) = loop (n `div` 2)
            c      = a + b
        in (a^2 + b^2, a*b + b*c)
      | otherwise =
          let (a, b) = loop (n - 1)
          in (b, a+b)

main :: IO ()
main =
  defaultMain [ bgroup "very small (25)"
                [ bench "naive" $ nf fibNaive 25
                , bench "zipWith" $ nf fibZipWith 25
                , bench "zipWith-nofuse" $ nf fibBadZipWith 25
                , bench "Linear DP" $ nf fibDP 25
                , bench "Log DP" $ nf fibLog 25
                -- , bench "memo-map" $ nf fibMap 25
                -- , bench "memo-array" $ nf fibSTArr 25
                , bench "Q(√5)" $ nf fibRat 25
                , bench "Naive matrix power" $ nf fibMatPow 25
                , bench "Matrix power with diag" $ nf fibMatDiag 25
                ]
              , bgroup "small (100)"
                [ bench "zipWith" $ nf fibZipWith 100
                , bench "zipWith-nofuse" $ nf fibBadZipWith 100
                , bench "Linear DP" $ nf fibDP 100
                , bench "Log DP" $ nf fibLog 100
                -- , bench "memo-map" $ nf fibMap 100
                -- , bench "memo-array" $ nf fibSTArr 100
                , bench "Q(√5)" $ nf fibRat 100
                , bench "Naive matrix power" $ nf fibMatPow 100
                , bench "Matrix power with diag" $ nf fibMatDiag 100
                ]
              , bgroup "medium (10000)"
                [ bench "zipWith" $ nf fibZipWith 10000
                , bench "zipWith-nofuse" $ nf fibBadZipWith 10000
                , bench "Linear DP" $ nf fibDP 10000
                , bench "Log DP" $ nf fibLog 10000
                -- , bench "memo-map" $ nf fibMap 10000
                -- , bench "memo-array" $ nf fibSTArr 10000
                , bench "Q(√5)" $ nf fibRat 10000
                , bench "Naive matrix power" $ nf fibMatPow 10000
                , bench "Matrix power with diag" $ nf fibMatDiag 10000
                ]
              , bgroup "large (100000)"
                [ bench "zipWith" $ nf fibZipWith 100000
                , bench "zipWith-nofuse" $ nf fibBadZipWith 100000
                , bench "Linear DP" $ nf fibDP 100000
                , bench "Log DP" $ nf fibLog 100000
                -- , bench "memo-map" $ nf fibMap 100000
                -- , bench "memo-array" $ nf fibSTArr 100000
                , bench "Q(√5)" $ nf fibRat 100000
                , bench "Naive matrix power" $ nf fibMatPow 100000
                , bench "Matrix power with diag" $ nf fibMatDiag 100000
                ]
              ]
