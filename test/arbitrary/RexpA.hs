module RexpA where

import Test.QuickCheck
import Rexp

-- We define an Arbitrary instance of Rexp for property testing purposes.
instance Arbitrary Rexp where
  arbitrary = oneof [ return ZERO
                    , return ONE
                    , CHAR Prelude.<$> arbitrary
                    , ALT Prelude.<$> arbitrary Prelude.<*> arbitrary
                    , SEQ Prelude.<$> arbitrary Prelude.<*> arbitrary
                    , STAR Prelude.<$> arbitrary
                    , OPTIONAL Prelude.<$> arbitrary
                    , PLUS Prelude.<$> arbitrary
                    , NTIMES Prelude.<$> arbitrary <*> choose (0, 100)
                    , RECD Prelude.<$> arbitrary <*> arbitrary
                    ]