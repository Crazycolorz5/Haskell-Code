type BString = [Bool]
type Schema = [Maybe Bool]

matchesSchema::Schema->BString->Bool
matchesSchema [] [] = True
matchesSchema (Nothing:schr) (str:strr) = matchesSchema schr strr
matchesSchema ((Just sch):schr) (str:strr)
    | sch /= str = False
    | otherwise  = matchesSchema schr strr

orderSchema::Schema->Int
orderSchema sch = orderSchema_rec sch 0
orderSchema_rec [] acc = acc
orderSchema_rec (Nothing:schr) acc = orderSchema_rec schr acc
orderSchema_rec (sch:schr) acc = orderSchema_rec schr (acc+1)


mySchema = [Just True,Just False,Nothing,Just True,Nothing]
myBString_1 = [True,False,False,True,True]
myBString_2 = [True,False,False,False,True]