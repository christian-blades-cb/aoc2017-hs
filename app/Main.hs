{-# LANGUAGE GADTSyntax #-}
module Main where

import Lib
import Data.CircularList
import System.Environment
import Data.List
import Data.Maybe

main :: IO ()
main = do
  -- day 1
  putStrLn "day1pt1"
  putStrLn . show $ day1pt1 dayOneInput
  putStrLn "day1pt2"
  putStrLn . show $ day1pt2 dayOneInput

  -- day2
  putStrLn "day2pt2"
  args <- getArgs
  content <- readFile $ "day2-input"
  putStrLn $ show $ day2pt2 content

  -- day3
  putStrLn "day3pt1"
  putStrLn $ show $ day3pt1 day3input
  putStrLn "day3pt2"
  putStrLn $ show $ day3pt2 $ toInteger day3input

  -- day 4
  putStrLn "day4pt1"
  day4content <- readFile "day4-input"
  putStrLn $ show $ day4pt1 day4content
  putStrLn "day4pt2"
  putStrLn $ show $ day4pt2 day4content

-- day 4
noDuplicateWords :: [String] -> Bool
noDuplicateWords [] = True
noDuplicateWords [_] = True
noDuplicateWords xs = notPresentLater && (noDuplicateWords $ tail xs)
  where
    current = head xs
    later = tail xs
    notPresentLater = all ((/=) current) later

noAnagrams :: [String] -> Bool
noAnagrams [] = True
noAnagrams [_] = True
noAnagrams xs = lack && (noAnagrams $ tail xs)
  where
    current = sort $ head xs
    later = map sort $ tail xs
    lack = all ((/=) current) later

conformingToPolicy policy xs = foldr (\ a b -> (conv $ policy a) + b ) 0 xs
  where
    conv x | x == True = 1
           | x == False = 0

day4pt1 = conformingToPolicy noDuplicateWords . map words . lines
day4pt2 = conformingToPolicy noAnagrams . map words . lines

-- day 3
day3input = 289326

lvl 0 = [(0, 0)] ++ lvl 1
lvl x =
  zip (repeat x) ascender
  ++ zip descender (repeat x)
  ++ zip (repeat (-x)) descender
  ++ zip ascender (repeat (-x))
  ++ lvl (x + 1) where
  ascender = [(-x + 1) .. x]
  descender = reverse [-x .. x - 1]

data CoordVal where
  CV :: (Integer, Integer) -> Integer -> CoordVal
    deriving Show

adjacentCoords :: (Integer, Integer) -> [(Integer, Integer)]
adjacentCoords (x, y) = (x+1, y) : (x+1, y+1) : (x+1, y-1) : (x-1, y) : (x-1, y+1) : (x-1, y-1) : (x, y+1) : (x, y-1) : []

isAdjacent :: CoordVal -> CoordVal -> Bool
isAdjacent (CV origin _) (CV xcoord _) = flip any (adjacentCoords origin) ((==) xcoord)

sumAdjacent :: CoordVal -> [CoordVal] -> Integer
sumAdjacent (CV origin _) [] = 1
sumAdjacent (CV origin _) [x] = v x
  where
    v (CV _ y) = y
sumAdjacent origin xs = sum . (map cval) $ filter (isAdjacent origin) xs
  where
    cval (CV _ v) = v

buildCoordVal :: CoordVal -> [CoordVal] -> CoordVal
buildCoordVal (CV (x, y) _) xs = CV (x, y) (sumAdjacent (CV (x, y) 0) xs)

buildValSpiral :: [CoordVal] -> [(Integer, Integer)] -> [CoordVal]
buildValSpiral acc xs = thisVal : buildValSpiral (acc ++ [thisVal]) (tail xs)
  where
    thisVal = CV (head xs) (sumAdjacent (CV (head xs) 0) acc)
-- buildValSpiral = foldl (\ acc b -> (:) (buildCoordVal b acc) acc) [] $ map (\(x, y) -> CV (x, y) 0) (lvl 0)

day3pt1 cell = (abs x) + (abs y) where
  (x, y) = lvl 0 !! (cell - 1)

day3pt2 input = find (\ (CV _ x) -> x > input) $ buildValSpiral [] (lvl 0)

-- day 2

rows :: String -> [[Integer]]
rows = map (\ x -> map read $ words x ) . lines

divEvenly x y | (== 0) $ mod x y = Just (x, y)
              | (== 0) $ mod y x = Just (y, x)
              | otherwise = Nothing

evenlyDivs :: Integer -> [Integer] -> Maybe (Integer, Integer)
evenlyDivs x xs = listToMaybe $ catMaybes $ map (\y -> divEvenly x y) xs

evenDivs :: [Integer] -> [Maybe (Integer, Integer)]
evenDivs xs | length xs < 2 = []
            | otherwise = (evenlyDivs (head xs) (tail xs)) : (evenDivs $ tail xs)

divRows :: [Integer] -> Integer
divRows xs = div (fst evdiv) (snd evdiv) where
  evdiv = head (catMaybes $ evenDivs xs)

day2pt2 = sum . map divRows . rows

-- day 1

dayOneInput = 5672987533353956199629683941564528646262567117433461547747793928322958646779832484689174151918261551689221756165598898428736782194511627829355718493723961323272136452517987471351381881946883528248611611258656199812998632682668749683588515362946994415852337196718476219162124978836537348924591957188827929753417884942133844664636969742547717228255739959316351852731598292529837885992781815131876183578461135791315287135243541659853734343376618419952776165544829717676988897684141328138348382882699672957866146524759879236555935723655326743713542931693477824289283542468639522271643257212833248165391957686226311246517978319253977276663825479144321155712866946255992634876158822855382331452649953283788863248192338245943966269197421474555779135168637263279579842885347152287275679811576594376535226167894981226866222987522415785244875882556414956724976341627123557214837873872723618395529735349273241686548287549763993653379539445435319698825465289817663294436458194867278623978745981799283789237555242728291337538498616929817268211698649236646127899982839523784837752863458819965485149812959121884771849954723259365778151788719941888128618552455879369919511319735525621198185634342538848462461833332917986297445388515717463168515123732455576143447454835849565757773325367469763383757677938748319968971312267871619951657267913817242485559771582167295794259441256284168356292785568858527184122231262465193612127961685513913835274823892596923786613299747347259254823531262185328274367529265868856512185135329652635938373266759964119863494798222245536758792389789818646655287856173534479551364115976811459677123592747375296313667253413698823655218254168196162883437389718167743871216373164865426458794239496224858971694877159591215772938396827435289734165853975267521291574436567193473814247981877735223376964125359992555885137816647382139596646856417424617847981855532914872251686719394341764324395254556782277426326331441981737557262581762412544849689472281645835957667217384334435391572985228286537574388834835693416821419655967456137395465649249256572866516984318344482684936625486311718525523265165

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

pairs x = zip x (toList . rotR $ fromList x)

matchedPairs :: Eq x => [(x, x)] -> [(x, x)]
matchedPairs = filter (\ (x, y) -> x == y)

matchedDigits = map (\ (x, _) -> x)

day1pt1 :: Integral a => a -> a
day1pt1 = sum . matchedDigits . matchedPairs . pairs . digs

pairs' x = zip x (toList . rotN (div (length x) 2) $ fromList x)
day1pt2  = sum . matchedDigits . matchedPairs . pairs' . digs
