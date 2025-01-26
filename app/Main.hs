import qualified Rexp
import qualified EdelmannZipper

main :: IO ()
main = do
    let c = Rexp.RECD "a" (Rexp.CHAR 'a') in 
        print (EdelmannZipper.matcher c "a")
    
    -- let charTree = Node ("char", CHAR 'c') Nil Nil
    -- print charTree
    -- let freeTree = Node 'P'
    --         (Node 'O'
    --             (Node 'L'
    --                 (Node 'N' Nil Nil)
    --                 (Node 'T' Nil Nil)
    --             )
    --             (Node 'Y'
    --                 (Node 'S' Nil Nil)
    --                 (Node 'A' Nil Nil)
    --             )
    --         )
    --         (Node 'L'
    --             (Node 'W'
    --                 (Node 'C' Nil Nil)
    --                 (Node 'R' Nil Nil)
    --             )
    --             (Node 'A'
    --                 (Node 'A' Nil Nil)
    --                 (Node 'C' Nil Nil)
    --             )
    --         )
    -- print $ Zipper freeTree T -: right -: left -: up




