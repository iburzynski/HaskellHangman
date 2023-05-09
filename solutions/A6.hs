module A6 where

import Provided
import Data.Char ( isLetter, toUpper )
import Data.List ( intersperse, sort )

-- *** A6-0: WARM-UP *** --

-- Q#01

type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

-- Q#02

data GameException = InvalidWord | InvalidMove | RepeatMove | GameOver

-- Q#03

lengthInRange :: Secret -> Bool
lengthInRange secret = len >= min && len <= max
    where len = length secret
          (min, max) = _LENGTH_

-- Q#04

invalidMove :: Move -> Bool
invalidMove move = not (isLetter move)

-- Q#05

revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters move secret guess = zipWith testChar secret guess
    where 
        testChar :: Char -> Char -> Char
        testChar schar gchar
            | schar == move = schar
            | otherwise = gchar

-- Q#06

updateChances :: Move -> Secret -> Chances -> Chances
updateChances move secret chances
    | elem move secret = chances
    | otherwise = chances - 1

-- Q#07

setSecret :: IO String
setSecret = do
    putStr "Enter a secret word:\t"
    showInput False
    secret <- getLine
    showInput True
    _SPACE_
    return secret


-- *** A6-1: Records & Instances *** --

-- Q#08
data Game = Game {
    secret :: Secret
    , currentGuess :: Guess
    , pastMoves :: [Move]
    , remainingChances :: Chances
}

-- Q#09

repeatedMove :: Move -> Game -> Bool
repeatedMove move game = elem move $ pastMoves game

-- Q#10

makeGame :: Secret -> Game
makeGame secret = Game { 
    secret = map toUpper secret 
    , currentGuess = map (const '_') secret
    , pastMoves = []
    , remainingChances = _CHANCES_
}

-- Q#11

updateGame :: Move -> Game -> Game
updateGame move game = game {
        currentGuess = revealLetters move gameSecret guess
        , pastMoves = move : moves
        , remainingChances = updateChances move gameSecret chances
    }
    where
        gameSecret = secret game
        guess = currentGuess game
        moves = pastMoves game
        chances = remainingChances game

-- Q#12

instance Show Game where
    show Game{ currentGuess=guess, pastMoves=moves, remainingChances=chances } = unlines [
         _STARS_
         , "\tCurrent Guess:\t" ++ intersperse ' ' guess ++ "\n"
         , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
         , "\tChances:\t" ++ show chances
         , _STARS_
      ]

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]


-- Q#13

instance Show GameException where
    show InvalidWord = "Invalid word!"
    show InvalidMove = "Invalid move!"
    show RepeatMove = "Duplicate move..."
    show GameOver = "Game over!"


-- *** A6-2: Exception Contexts *** --

-- Q#14

toMaybe = undefined

-- Q#15

validateSecret = undefined

-- Q#16

hasValidChars = undefined


isValidLength = undefined


isInDict = undefined

-- Q#17

validateNoDict = undefined

validateWithDict = undefined

-- Q#18

processTurn = undefined