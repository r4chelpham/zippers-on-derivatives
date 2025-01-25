module ValSpec where
import Test.QuickCheck
import Test.Hspec
import Val

spec :: Spec
spec = do
    describe "flatten" $ do

        it "returns empty string for Empty" $ do
            flatten Empty `shouldBe` []
        
        it "returns a string consisting of a single character for Chr" $ do
            property $ \c ->
                case flatten(Chr c) of 
                    [d] -> c == d
                    _ -> False

        -- TODO: fix this...
        -- it "flattens Sequ by concatenating components" $ do
        --     property $ \v1 v2 -> flatten (Sequ v1 v2) == flatten v1 ++ flatten v2

        -- it "flattens Stars by concatenating all elements" $ do
        --     property $ \vs -> flatten (Stars vs) == concatMap flatten vs

        -- it "flattens Rec as its contained value" $ do
        --     property $ \label v -> flatten (Rec label v) == flatten v

        it "flattens a Sequ by concatenating flattened components" $ do
            flatten (Sequ (Chr 'a') (Chr 'b')) `shouldBe` "ab"

        it "flattens a nested Sequ correctly" $ do
            flatten (Sequ (Chr 'a') (Sequ (Chr 'b') (Chr 'c'))) `shouldBe` "abc"

        it "flattens a Left value correctly" $ do
            flatten (Val.Left (Chr 'a')) `shouldBe` "a"

        it "flattens a Right value correctly" $ do
            flatten (Val.Right (Chr 'b')) `shouldBe` "b"

        it "flattens a Stars value by concatenating flattened values" $ do
            flatten (Stars [Chr 'a', Chr 'b', Chr 'c']) `shouldBe` "abc"
            flatten (Stars [Empty, Chr 'x', Empty]) `shouldBe` "x"

        it "flattens a Rec value by flattening its contained value" $ do
            flatten (Rec "label" (Chr 'x')) `shouldBe` "x"
            flatten (Rec "label" (Sequ (Chr 'a') (Chr 'b'))) `shouldBe` "ab"

    describe "env" $ do
        it "returns an empty list for Empty" $ do
            env Empty `shouldBe` []

        it "returns an empty list for Chr" $ do
            env (Chr 'a') `shouldBe` []

        it "traverses Left and returns the environment of its value" $ do
            env (Val.Left (Rec "key" (Chr 'a'))) `shouldBe` [("key", "a")]

        it "traverses Right and returns the environment of its value" $ do
            env (Val.Right (Rec "key" (Chr 'b'))) `shouldBe` [("key", "b")]

        it "concatenates environments in Sequ" $ do
            env (Sequ (Rec "k1" (Chr 'a')) (Rec "k2" (Chr 'b')))
                `shouldBe` [("k1", "a"), ("k2", "b")]

        it "flattens environments in Stars" $ do
            env (Stars [Rec "k1" (Chr 'a'), Rec "k2" (Chr 'b')])
                `shouldBe` [("k1", "a"), ("k2", "b")]

        it "adds Rec bindings and continues traversal" $ do
            env (Rec "key" (Sequ (Chr 'x') (Chr 'y')))
                `shouldBe` [("key", "xy")]
