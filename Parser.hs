import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

eofKeepVal :: a -> Parser a
eofKeepVal x = eof >> return x

--testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "1234"
