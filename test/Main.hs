import Control.Concurrent.Supply

main = do
  supply <- newSupply
  let ids = loop supply 2048
  putStrLn $ show ids
  where
    loop s 0 = []
    loop s n = let (fId,s')  = freshId s
               in fId : (loop s' (n-1))
