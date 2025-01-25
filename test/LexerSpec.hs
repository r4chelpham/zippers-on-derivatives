module LexerSpec where
import Test.QuickCheck
import Test.Hspec
import Control.Exception (evaluate)
import qualified Lexer
import Rexp
import Val


spec :: Spec
spec = do
    describe "mkeps" $ do
        it "returns Empty for ONE" $ do
            Lexer.mkeps ONE `shouldBe` Empty
        
        it "returns Left for a nullable left side of an ALT" $ do
            property $ \r1 r2 ->
                nullable r1 ==>
                    case Lexer.mkeps (ALT r1 r2) of 
                        Val.Left _ -> True
                        _ -> False
        
        it "returns Right for a nullable right side of an ALT if the left side is not nullable" $ do
            property $ \r1 r2 ->
                not (nullable r1) && nullable r2 ==>
                    case Lexer.mkeps (ALT r1 r2) of 
                        Val.Right _ -> True
                        _ -> False
        
        it "returns Sequ for a nullable SEQ" $ do
            property $ \r1 r2 -> 
                nullable r1 && nullable r2 ==>
                    case Lexer.mkeps (SEQ r1 r2) of 
                        Val.Sequ _ _ -> nullable r1 && nullable r2
                        _ -> False
        
        it "returns a Stars for a STAR" $ do
            property $ \r -> 
                nullable r ==>
                    case Lexer.mkeps (STAR r) of 
                        Val.Stars [] -> True
                        _ -> False

    describe "lexSimp" $ do
        it "parses the empty string if nullable" $ 
            property $ \r -> 
                nullable r ==> Lexer.lexSimp r [] == Lexer.mkeps r

        it "throws an error for the empty string if not nullable" $ 
            property $ \r -> 
                not (nullable r) ==> 
                (evaluate (Lexer.lexSimp r []) `shouldThrow` anyErrorCall)