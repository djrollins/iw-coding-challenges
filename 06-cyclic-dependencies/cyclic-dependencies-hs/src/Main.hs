module Main where

import Control.Monad.State
import qualified Data.Map as M
import Data.Map (Map(..))
import qualified Data.Set as S
import Data.Set (Set(..))
import Data.List (lines, groupBy, sortBy, elem, find, intersperse)
import Data.Function (on)

input1 :: String
input1 = "a,b\nb,c\nb,d\na,e"

input2 :: String
input2 = "a,b\nb,c\na,c"

input3 :: String
input3 = "a,b\nb,c\nc,d\nc,a"

input4 :: String
input4 = "a,b\nb,c\nc,d\nd,a\nc,e\ne,f\nf,a"

type ToVisitState = Set String
type Link = (String, String)
type Graph = Map String (Set String)

mkLink :: String -> Link
mkLink str = (first, second)
  where first = takeWhile notComma str
        second = tail $ dropWhile notComma str
        notComma = (/= ',')

buildGraph :: [Link] -> Graph
buildGraph = M.fromList . map byKeys . groupBy ((==) `on` fst) . sortBy(compare `on` fst)
  where byKeys links = (fst (head links), S.fromList . map snd $ links)

findLoopsFrom :: Graph -> [String] -> String -> State [String] [[String]]
findLoopsFrom graph parents current = do
  let index = current `elemIndex` parents
  if (isJust index)
  then 
    -- loop!
  else
    let


--findLoops' :: Graph -> [String] -> [[String]]
--findLoops' _ [] = []
--findLoops' graph toVisit = (findLoopsFrom graph [] (head toVisit)) : (findLoops' graph (tail toVisit))
--
--findLoops :: Graph -> [[String]]
--findLoops graph = findLoops' graph (S.fromList . S.difference allChildren . M.keysSet $ graph)
--    where allChildren = S.fromList . concatMap S.toList . M.elems $ graph
--
--run input = findLoops . buildGraph . map mkLink . lines $ input

main :: IO ()
main = undefined
