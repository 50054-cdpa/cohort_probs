import Test.Hspec
import qualified SUTD.Compiler.MathExpLexerWithRegexSpec (spec)
import qualified SUTD.Compiler.MathExpParserSpec (spec)
import qualified SUTD.Compiler.JsonLexerWithRegexSpec (spec)
import qualified SUTD.Compiler.JsonParserSpec (spec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "SUTD.Compiler.MathExpLexerWithRegexSpec" SUTD.Compiler.MathExpLexerWithRegexSpec.spec
    describe "SUTD.Compiler.MathExpParserSpec" SUTD.Compiler.MathExpParserSpec.spec
    describe "SUTD.Compiler.JsonLexerWithRegexSpec" SUTD.Compiler.JsonLexerWithRegexSpec.spec
    describe "SUTD.Compiler.JsonParserSpec" SUTD.Compiler.JsonParserSpec.spec

-- to run test
-- 1) cabal test; or
-- 2) stack test

-- to run a specific spec
-- cabal test --test-options="-m Ex1Spec" 