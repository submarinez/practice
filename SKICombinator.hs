module SKICombinator where

-- SKI Combinator Calculus
--
-- Identity
-- I x = x
--
-- Constant operator
-- K x y = x
--
-- Substitution operator
-- (S x y z) = x z (y z)
--
-- (Pair Node Node) is used to make an AST representing an application

data Node = S | K | I | Pair Node Node
  deriving (Show, Eq)

-- Execute to reduce the AST using the rules defined in the step function
exec :: Node -> Node
exec x
  | x == x' = x
  | otherwise = exec x'
  where
    x' = step x

step :: Node -> Node
step (Pair I a) = a -- Identity function
step (Pair (Pair K a) _) = a -- Constant operator
step (Pair (Pair (Pair S x) y) z) = Pair (Pair x z) (Pair y z) -- Substitution operator
step (Pair a b) = Pair (step a) (step b) -- Step through the tree
step node = node -- Return the reduced node

testSuite :: IO ()
testSuite = hspec $ do
  describe "SKI Combinatory Calculus" $ do
    it "does not change single combinators" $ do
      exec K `shouldBe` K
      exec I `shouldBe` I
      exec S `shouldBe` S
    it "remains unchanged for expressions in normal form" $ do
      exec (Pair K I) `shouldBe` (Pair K I)
    it "evaluates the identity function (I x) to x" $ do
      exec (Pair I K) `shouldBe` K
    it "evaluates the constant operator (K x y) to x" $ do
      exec (Pair (Pair K I) K) `shouldBe` I
    it "evaluates the substitution operator (S x y z) to xz(yz)" $ do
      exec (Pair (Pair (Pair S K) K) K) `shouldBe` K
      exec (Pair (Pair (Pair S K) I) K) `shouldBe` K
      exec (Pair (Pair (Pair S I) K) K) `shouldBe` Pair K (Pair K K)
      exec (Pair (Pair (Pair S (Pair K I)) K) I) `shouldBe` (Pair K I)
    it "fully reduces nested expressions" $ do
      exec (Pair (Pair (Pair S K) I) (Pair (Pair K I) S)) `shouldBe` I

main :: IO ()
main = testSuite
