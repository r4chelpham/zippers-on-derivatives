module EdelmannZipperv2Spec where

import EdelmannZipperv2
import Rexp
import Val
import Test.Hspec (Spec)

spec :: Spec
spec = do
    describe "zder" $
        it "has the same matching result as all "