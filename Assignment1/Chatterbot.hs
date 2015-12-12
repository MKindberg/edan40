module Chatterbot where
import Utilities
import System.Random
import Data.Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

      
--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- DONE -}
stateOfMind b = do
    r <- randomIO :: IO Float
    let a = (map.map2) (id, pick r) b
    return $ rulesApply a

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- DONE -}
rulesApply = (maybe [] id .) . (transformationsApply "*" reflect) 

reflect :: Phrase -> Phrase
{- DONE -}
reflect = map.try $ flip lookup reflections

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
{- DONE -}
rulesCompile = (map.map2) (words.map toLower, map $ words.map toLower) 


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- DONE -}
reductionsApply = fix . try . transformationsApply "*" id



-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
{- DONE -}
substitute _ [] _ = []
substitute w (x:xs) b 
    |  w == x  = b ++ substitute w xs b
    |  otherwise = x:substitute w xs b  


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
{- DONE -}
match _ [] [] = Just []
match _ _ [] = Nothing
match _ [] _ = Nothing
match w (p:ps) (x:xs) 
    | p == x = match w ps xs
    | w /= p = Nothing
    | otherwise = orElse (singleWildcardMatch (p:ps) (x:xs)) (longerWildcardMatch (p:ps) (x:xs)) 


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
{- DONE -}
singleWildcardMatch (w:ps) (x:xs) = (mmap.const) [x] $ match w ps xs

{- DONE -}
longerWildcardMatch (w:ps) (x:xs) = mmap (x:) $ match w (w:ps) xs




-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------


-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
{- DONE -}
transformationApply w f s t = mmap ((substitute w (snd t)) . f) $ match w (fst t) s


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
{- DONE -}
transformationsApply w f t s = foldr1 orElse $ map (transformationApply w f s) t
