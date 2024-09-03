import Test.Hspec
import qualified Ex2Spec (spec)
import qualified Ex4Spec (spec)
import qualified Ex5Spec (spec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Ex2Spec" Ex2Spec.spec
  describe "Ex4Spec" Ex4Spec.spec
  describe "Ex5Spec" Ex5Spec.spec

-- to run test
-- 1) cabal test; or
-- 2) stack test

-- to run a specific spec
-- cabal test --test-options="-m Ex1Spec" 