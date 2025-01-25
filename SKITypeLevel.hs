{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, PolyKinds, RankNTypes, TypeApplications #-}
module SKITypeLevel where
import Test.Hspec

-- SKI Combinator Calculus at the Type Level
-- 
-- Identity function
-- I x = x 
-- 
-- Constant operator
-- K x y = x
--
-- Substitution operator
-- S x y z = x z (y z)
--

data SKI = S | K | I | SKI :@: SKI

type family Eval (x :: SKI) :: SKI where
  Eval (I :@: x)             = Eval x                       -- Identity function
  Eval (K :@: x :@: y)       = Eval x                       -- Constant operator
  Eval (S :@: x :@: y :@: z) = Eval (x :@: z :@: (y :@: z)) -- Substitution operator
  Eval (x :@: y)             = Apply (Eval x) (Eval y)      -- Walk the tree by applying reductions
  Eval x                     = x                            -- Base case

-- Apply is a helper that prevents unchecked recursion in the Eval type family
type family Apply (x :: SKI) (y :: SKI) :: SKI where
  Apply I arg         = arg
  Apply (K :@: x) _   = x
  Apply (S :@: x :@: y) z = Eval (x :@: z :@: (y :@: z))
  Apply x y           = x :@: y  -- Default application

type family TypeEquals (a :: k) (b :: k) :: Bool where
  TypeEquals a a = 'True
  TypeEquals a b = 'False

type family Equals (a :: SKI) (b :: SKI) :: Bool where
  Equals a b = TypeEquals (Eval a) (Eval b) 

exec :: forall a . (a ~ 'True) => Expectation
exec = return ()

-- testSpec :: Spec
testSuite = hspec $ do
  describe "Type-level SKI combinatory calculus" $ do
    it "does not change single combinators" $ do
      exec @(Equals I I)
      exec @(Equals K K)
      exec @(Equals S S)
    it "remains unchanged for expressions in normal form" $ do
      exec @(Equals (K :@: I) (K :@: I))
    it "evaluates the identity function" $ do
      exec @(Equals (I :@: K) K)
    it "evaluates the constant operator" $ do
      exec @(Equals (K :@: I :@: K) I)
    it "evaluates the substitution operator (S x y z) to (xz (yz))" $ do
      exec @(Equals (S :@: K :@: K :@: K) K)
      exec @(Equals (S :@: K :@: I :@: K) K)
      exec @(Equals (S :@: I :@: K :@: K) (K :@: (K :@: K)))
      exec @(Equals (S :@: K :@: I :@: K :@: I) (K :@: I))
    it "fully reduces nested expressions" $ do
      exec @(Equals ((S :@: K :@: I) :@: (K :@: I :@: S)) I)

main :: IO
main = testSuite

