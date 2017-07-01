{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (liftM)
import Data.Char (isAlphaNum)
import Data.List (intersperse)
import System.Random (randomRs, getStdGen)

import qualified Data.Map.Lazy    as M
import qualified Data.Text.Lazy   as T

data MarkovElement a = Start | End | Element a deriving (Eq, Ord)
type MarkovWord = MarkovElement T.Text
type Memory = M.Map MarkovWord (M.Map MarkovWord Int)
type Probability = M.Map MarkovWord (M.Map MarkovWord Float)

instance Functor MarkovElement where
  fmap _ Start       = Start
  fmap _ End         = End
  fmap f (Element e) = Element (f e)

instance (Show a) => Show (MarkovElement a) where
  show Start = "<<Start>>"
  show End   = "<<End>>"
  show (Element a) = show a

printMarkovWord :: MarkovWord -> T.Text
printMarkovWord Start = ""
printMarkovWord End   = ""
printMarkovWord (Element t) = t

buildMemory :: [T.Text] -> Memory
buildMemory = foldl addSentence emptyMemory

buildProbability :: Memory -> Probability
buildProbability = M.map buildProbability'

buildProbability' :: M.Map MarkovWord Int -> M.Map MarkovWord Float
buildProbability' m = M.map ((/ (fromIntegral sumOfElems)) . fromIntegral) m
  where
    sumOfElems = M.foldl (+) 0 m

addSentence :: Memory -> T.Text -> Memory
addSentence st = foldl addWord st
              . (\l -> zipWith (\a b -> [a,b]) l (tail l))
              . (Start:) . (++ [End]) . map Element
              . T.split (not . isAlphaNum) . T.toLower

addWord :: Memory -> [MarkovWord] -> Memory
addWord st (_:[])    = st
addWord st (w1:w2:[]) = M.insertWith (M.unionWith (+)) w1 (M.singleton w2 1) st

emptyMemory :: Memory
emptyMemory = M.empty

generateChain :: [Float] -> Probability -> MarkovWord -> [MarkovWord]
generateChain _ _ End = []
generateChain (p:ps) pr w =
  let t = select p (pr M.! w)
  in  t : generateChain ps pr t

select :: Float -> M.Map MarkovWord Float -> MarkovWord
select p pr = select' p (M.toList pr)
  where
    select' :: Float -> [(MarkovWord, Float)] -> MarkovWord
    select' p' ((t, pr'):lst) | p' > pr'    = select' (p' - pr') lst
                              | otherwise = t

showChain :: [MarkovWord] -> T.Text
showChain = T.concat . intersperse " " . map printMarkovWord

main :: IO ()
main = do
  probs <- liftM (buildProbability . buildMemory . T.splitOn "." . T.pack) $ readFile "test.txt"
  stdGen <- getStdGen
  putStrLn . T.unpack . showChain $ generateChain (randomRs (0, 1) stdGen) probs (Element "tisztelt")
