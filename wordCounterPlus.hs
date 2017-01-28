import Data.Char (isAlpha)

numWords = length . filter (any isAlpha) . words
