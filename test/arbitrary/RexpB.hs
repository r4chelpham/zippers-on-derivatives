module RexpB where

import RexpZipper
import Test.QuickCheck

instance Arbitrary Rexp where
    arbitrary = oneof
        [ CHAR Prelude.<$> arbitrary
        , RANGE Prelude.<$> arbitrary
        , SEQ Prelude.<$> arbitrary <*> listOf arbitrary
        , ALT Prelude.<$> listOf arbitrary
        , STAR Prelude.<$> arbitrary
        , PLUS Prelude.<$> arbitrary
        , OPTIONAL Prelude.<$> arbitrary
        , NTIMES Prelude.<$> choose (0, 100) <*> arbitrary
        , RECD Prelude.<$> arbitrary <*> arbitrary
        ]
