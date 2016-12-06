
{-
newtype Voltage = V Double
newtype Current = I Double
data Node a b = Node (V a) (V b) Current --Vin, Vout

newtype CircuitPart = Part (Node -> Node)

instance Arrow CircuitPart
-}


type Signal a = Double -> a
type SignalFunction a b = Signal a -> Signal b
resistor :: Double -> CircuitPart
vSource :: Double -> CircuitPart
series :: (CircuitPart, CircuitPart) -> CircuitPart
parallel :: (CircuitPart, CircuitPart) -> CircuitPart
loop :: CircuitPart -> CircuitPart

loop (vSource 5 `series` resistor 10)
