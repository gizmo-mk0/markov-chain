{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (liftM)
-- import Data.Char (isAlphaNum)
import Data.List (intersperse)
import System.Random (randomRs, getStdGen)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import qualified Data.Map.Lazy    as M
import qualified Data.Text.Lazy   as T

data MarkovElement a = Start | End | Element a deriving (Eq, Ord)
type MarkovWord = MarkovElement T.Text
type Memory = M.Map [MarkovWord] (M.Map MarkovWord Int)
type Probability = M.Map [MarkovWord] (M.Map MarkovWord Float)

instance Functor MarkovElement where
  fmap _ Start       = Start
  fmap _ End         = End
  fmap f (Element e) = Element (f e)

instance (Show a) => Show (MarkovElement a) where
  show Start = "<<Start>>"
  show End   = "<<End>>"
  show (Element a) = show a

showMarkovWord :: MarkovWord -> String
showMarkovWord Start = "<<Start>>"
showMarkovWord End   = "<<End>>"
showMarkovWord (Element t) = show t

printMarkovWord :: MarkovWord -> T.Text
printMarkovWord Start = ""
printMarkovWord End   = ""
printMarkovWord (Element t) = t

printMemory :: Memory -> String
printMemory = concat . intersperse "\n" . map snd . M.toList . M.mapWithKey (\k a -> printKey k ++ "\t->\t" ++ show a)
  where
    printKey :: [MarkovWord] -> String
    printKey = concat . intersperse " " . map showMarkovWord

buildMemory :: Int -> [T.Text] -> Memory
buildMemory n = foldl (addSentence n) emptyMemory

buildProbability :: Memory -> Probability
buildProbability = M.map buildProbability'

buildProbability' :: M.Map MarkovWord Int -> M.Map MarkovWord Float
buildProbability' m = M.map ((/ (fromIntegral sumOfElems)) . fromIntegral) m
  where
    sumOfElems = M.foldl (+) 0 m

addSentence :: Int -> Memory -> T.Text -> Memory
addSentence n st = foldl addWord st
                 . breakChain n End
                 . ((replicate n Start) ++) . (++ [End]) . map Element
                 . T.split (`elem` (" \n\"[]" :: String)) . T.toLower

breakChain :: Int -> a -> [a] -> [([a], a)]
breakChain n def xs = map (\i -> (,) (take n . drop i $ xs) (xs `getNth` (n + i))) [0..length xs - 1]
  where
    getNth xs' n' = if length xs' <= n' then def else xs' !! n'

addWord :: Memory -> ([MarkovWord], MarkovWord) -> Memory
addWord st (lst, w) = M.insertWith (M.unionWith (+)) lst (M.singleton w 1) st

emptyMemory :: Memory
emptyMemory = M.empty

generateChain :: [Float] -> Probability -> [MarkovWord] -> [MarkovWord]
generateChain _ _ (End:_) = []
generateChain (p:ps) pr w =
  let t = select p (pr M.! w)
  in  head w : generateChain ps pr (tail w ++ [t])

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
  setLocaleEncoding utf8
  let order = 3
  probs <- liftM (buildProbability . buildMemory order . T.splitOn "." . T.pack) $ readFile "test.txt"
  stdGen <- getStdGen
  putStrLn . T.unpack . showChain $ generateChain (randomRs (0, 1) stdGen) probs [Start, Start, Start] -- (Element "java")
  -- memory <- liftM (buildMemory order . T.splitOn "." . T.pack) $ readFile "test.txt"
  -- writeFile "memory.txt" $ printMemory memory
