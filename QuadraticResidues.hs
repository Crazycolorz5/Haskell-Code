import Data.Set (Set, fromList, (\\))
import qualified Data.Set as Set
import MyUsefulFunctions (primes)

residues p = fromList $ map (flip mod p . (^2)) [1..p-1]
nonresidues p = fromList [1..p-1] \\ residues p
