module Val where
import Test.QuickCheck

data Val = Empty
        | Chr Char
        | Sequ Val Val
        | Left Val
        | Right Val
        | Stars [Val]
        | Rec String Val 
        | Pls [Val]
        | Opt Val
        | NX [Val] deriving (Show, Eq)

-- We define an Arbitrary instance of Val for property testing purposes.
instance Arbitrary Val where
  arbitrary = oneof [ return Empty
                    , Chr <$> arbitrary
                    , Sequ <$> arbitrary <*> arbitrary
                    , Val.Left <$> arbitrary
                    , Val.Right <$> arbitrary
                    , Stars <$> arbitrary
                    , Rec <$> arbitrary <*> arbitrary
                    ]


flatten :: Val -> String
flatten Empty = ""
flatten (Chr c) = [c]
flatten (Sequ v1 v2) = flatten v1 ++ flatten v2
flatten (Val.Left v) = flatten v
flatten (Val.Right v) = flatten v
flatten (Stars vs) = vs >>= \x -> flatten x
flatten (Pls vs) = vs >>= \x -> flatten x
flatten (Opt v) = flatten v
flatten (NX vs) = vs >>= \x -> flatten x 
flatten (Rec _ v) = flatten v


env :: Val -> [(String, String)]
env Empty = []
env (Chr _) = []
env (Val.Left v) = env v
env (Val.Right v) = env v
env (Sequ v1 v2) = env v1 ++ env v2
env (Stars vs) = vs >>= \x -> env x
env (Opt v) = env v
env (Pls vs) = vs >>= \x -> env x
env (NX vs) =  vs >>= \x -> env x
env (Rec s v) = (s, flatten v) : env v

-- benchmarking functions
sizeVal :: Val -> Int
sizeVal Empty = 1
sizeVal (Chr _) = 1
sizeVal (Val.Left v) = 1 + sizeVal v
sizeVal (Val.Right v) = 1 + sizeVal v
sizeVal (Sequ v1 v2) = 1 + sizeVal v1 + sizeVal v2
sizeVal (Stars []) = 1
sizeVal (Stars (v:vs)) = 1 + sizeVal v
sizeVal (Opt v) = 1 + sizeVal v
sizeVal (Pls vs) = 1 + length vs
sizeVal (Rec _ v) = sizeVal v