import TreeZipper

main :: IO ()
main = do
    let freeTree = Node 'P'
            (Node 'O'
                (Node 'L'
                    (Node 'N' Nil Nil)
                    (Node 'T' Nil Nil)
                )
                (Node 'Y'
                    (Node 'S' Nil Nil)
                    (Node 'A' Nil Nil)
                )
            )
            (Node 'L'
                (Node 'W'
                    (Node 'C' Nil Nil)
                    (Node 'R' Nil Nil)
                )
                (Node 'A'
                    (Node 'A' Nil Nil)
                    (Node 'C' Nil Nil)
                )
            )
    print $ Zipper freeTree T -: right -: left -: up

