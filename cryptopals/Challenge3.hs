import Xor
import qualified Data.Map as Map
import Data.List.Split

type Score = Int

-- decrypt :: String -> [(String, Int)]
-- decrypt encrypted =
--   let possibleChars = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
--       decryptedSolutions = map (\char -> possibleSolution encrypted char) possibleChars
--       allScores = map (\sol -> calculateScore sol) decryptedSolutions
--   in allScores
--
-- calculateScore :: String -> (String, Int)
-- calculateScore str =
--   let occurrences = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- str]
--       scores = map (\(x, y) -> matchFrequency y (letterFrequency x)) occurrences
--   in (str, foldl (+) 0 scores)
--
-- matchFrequency :: Int -> Int -> Int
-- matchFrequency strFrequency enFrequency
--     | strFrequency == enFrequency = 1
--     | strFrequency - 1 == enFrequency = 1
--     | strFrequency + 1 == enFrequency = 1
--     | otherwise = 0

possibleSolution :: String -> Char -> String
possibleSolution encrypted char =
  let possibleCodeStr = replicate (length encrypted) char
      hexSolution = xorStr encrypted possibleCodeStr
      hexChunks = splitEvery 2 hexSolution
      asciiChunks = map hexChunks $ read ""


letterFrequency :: Char -> Int
letterFrequency 'a' = 8
letterFrequency 'b' = 1
letterFrequency 'c' = 2
letterFrequency 'd' = 4
letterFrequency 'e' = 12
letterFrequency 'f' = 2
letterFrequency 'g' = 2
letterFrequency 'h' = 6
letterFrequency 'i' = 6
letterFrequency 'j' = 0
letterFrequency 'k' = 0
letterFrequency 'l' = 4
letterFrequency 'm' = 2
letterFrequency 'n' = 6
letterFrequency 'o' = 7
letterFrequency 'p' = 2
letterFrequency 'q' = 0
letterFrequency 'r' = 6
letterFrequency 's' = 6
letterFrequency 't' = 9
letterFrequency 'u' = 2
letterFrequency 'v' = 0
letterFrequency 'w' = 2
letterFrequency 'x' = 0
letterFrequency 'y' = 2
letterFrequency 'z' = 0
