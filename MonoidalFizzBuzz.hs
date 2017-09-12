import Data.Monoid
import Data.Foldable (fold)

type Rule = Integer -> Maybe String

modRule :: Integer -> String -> Rule
modRule n res = \i -> case mod i n of
    0 -> Just res
    _ -> Nothing

fizz :: Rule
fizz = modRule 3 "Fizz"
buzz :: Rule
buzz = modRule 5 "Buzz"

applyRule :: Rule -> Integer -> String
applyRule rule x = case rule x of
    Nothing -> show x
    Just s -> s

applyAllRules :: [Rule] -> [Integer] -> [String]
applyAllRules rulesList = map $ applyRule (fold rulesList)

main :: IO ()
main = (sequence . map putStrLn $ applyAllRules [fizz, buzz] [1..100]) >> return ()
