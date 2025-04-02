module LexerSpec where
import Test.QuickCheck
import Test.Hspec
import Control.Exception (evaluate)
import qualified Lexer
import Rexp
import RexpA
import Val


spec :: Spec
spec = do
    describe "mkeps" $ do
        it "returns Empty for ONE" $ do
            Lexer.mkeps ONE `shouldBe` Empty
        
        it "returns Left for a nullable left side of an ALT" $ property $ 
            \r1 r2 ->
                nullable r1 ==>
                    case Lexer.mkeps (ALT r1 r2) of 
                        Val.Left _ -> True
                        _ -> False
        
        it "returns Right for a nullable right side of an ALT if the left side is not nullable" $ property $ 
            \r1 r2 ->
                not (nullable r1) && nullable r2 ==>
                    case Lexer.mkeps (ALT r1 r2) of 
                        Val.Right _ -> True
                        _ -> False
        
        it "returns Sequ for a nullable SEQ" $ property $ 
            \r1 r2 -> 
                nullable r1 && nullable r2 ==>
                    case Lexer.mkeps (SEQ r1 r2) of 
                        Val.Sequ _ _ -> nullable r1 && nullable r2
                        _ -> False
        
        it "returns a Stars for a STAR" $ property $ 
            \r -> 
                nullable (STAR r) ==>
                    case Lexer.mkeps (STAR r) of 
                        Val.Stars [] -> True
                        _ -> False

        it "returns an Opt for an OPTIONAL" $ property $ 
            \r -> 
                nullable (OPTIONAL r) ==>
                    case Lexer.mkeps (OPTIONAL r) of 
                        Val.Opt Empty -> True
                        _ -> False

        it "returns a Pls for a PLUS" $ property $ 
            \r -> 
                nullable (Rexp.PLUS r) ==>
                    case Lexer.mkeps (PLUS r) of 
                        Val.Pls _ -> True
                        _ -> False
        
        it "returns an empty NX for a NTIMES with n == 0" $ property $ 
            \r-> 
                case Lexer.mkeps (Rexp.NTIMES r 0) of 
                    Val.NX [] -> True
                    _ -> False
        
        it "returns a NX for a NTIMES with n > 0" $  property $ 
            \r n-> 
                nullable (Rexp.NTIMES r n) ==>
                    case Lexer.mkeps (Rexp.NTIMES r n) of 
                        Val.NX _ -> True
                        _ -> False

    describe "lexSimp" $ do
        it "parses the empty string if nullable" $ property $ 
            \r -> 
                nullable r ==> Lexer.lexSimp r [] == Lexer.mkeps r

        it "throws an error for the empty string if not nullable" $ property $ 
            \r -> 
                not (nullable r) ==> 
                (evaluate (Lexer.lexSimp r []) `shouldThrow` anyErrorCall)