--- haskell work

--- 2014. 7. 23 어금니의 염증을 항생제로 누르며 수술을 기다리는 어느 여름날
--- parseUrl, parseParam "foo=1&bar=2&baz", used for mm.hs in indra

parseParam :: String -> [(String, Maybe String)]
parseParam [] = []
parseParam s = let sep = elemIndex '&' s
               in case sep of
                    Just n -> let (h, t) = splitAt n s
                              in sub h ++ parseParam (tail t)
                    Nothing -> sub s
                   where sub s = let sep = elemIndex '=' s
                                 in case sep of 
                                      Just n -> let (h, t) = splitAt n s
                                                in [(h, Just $ tail t)]
                                      Nothing -> [(s, Nothing)]

parseUrl :: String -> (String, [(String, Maybe String)])
parseUrl s = let sep = elemIndex '?' s
             in case sep of
                  Nothing -> (s, [])
                  Just n -> let (h, t) = splitAt n s
                            in (h, parseParam (tail t))
