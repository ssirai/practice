lastButOne = last . init

lastButOne' (x:_:[])  = x
lastButOne' (x:xs)    = lastButOne' xs

