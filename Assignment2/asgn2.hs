scoreMatch = 0
scoreMissmatch = -1
scoreSpace= -1

similarityScore string1 string2 = sim (string1, string2)

sim :: (String, String) -> Int
sim ([], _) = 0
sim (_ ,[]) = 0
sim ((x:xs), (y:ys))
    |x=='-' = scoreSpace + sim (xs, ys)
    |y=='-' = scoreSpace + sim (xs, ys)
    |x==y = scoreMatch + sim (xs, ys)
    |otherwise = maximum [sim ('-':xs, y:ys), sim (x:xs, '-':ys), scoreMissmatch+(sim (xs, ys))]

        
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy _ [] = []
maximaBy f (x:xs) 
    |length (maximaBy f xs) == 0 = [x]
    |f x < f (maximaBy f xs !! 0) = maximaBy f xs
    |f x > f (maximaBy f xs !! 0) = [x]
    |otherwise = x:maximaBy f xs
    
    
type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = maximaBy sim (concat [ attachHeads x y (optAlignments xs ys),
                     attachHeads x '-' (optAlignments xs (y:ys)),
                     attachHeads '-' y (optAlignments (x:xs) ys) ])

                     
                
outputOptAlignments :: String -> String -> IO()
outputOptAlignments s1 s2 = do printOptAlignments (optAlignments s1 s2)


printOptAlignments :: [AlignmentType] -> IO()

printOptAlignments [] = putStrLn ""
printOptAlignments (s:ss)=do
    putStrLn ""
    putStrLn (fst s)
    putStrLn (snd s)
    printOptAlignments ss