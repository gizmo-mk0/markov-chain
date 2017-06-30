{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (liftM)
import Data.Char (isAlphaNum)
import System.Random (randomRs, mkStdGen)

import qualified Data.Map.Lazy    as M
import qualified Data.Text.Lazy   as T

type Statistic = M.Map T.Text (M.Map (Maybe T.Text) Int)

type Probability = M.Map T.Text (M.Map (Maybe T.Text) Float)

buildStatistic :: [T.Text] -> Statistic
buildStatistic = foldl addSentence emptyStatistic

buildProbability :: Statistic -> Probability
buildProbability = M.map buildProbability'

buildProbability' :: M.Map (Maybe T.Text) Int -> M.Map (Maybe T.Text) Float
buildProbability' m = M.map ((/ (fromIntegral sum)) . fromIntegral) m
  where
    sum = M.foldl (+) 0 m

addSentence :: Statistic -> T.Text -> Statistic
addSentence st = foldl addWord st . (\l -> zipWith (\a b -> [a,b]) l (tail l)) . T.split (not . isAlphaNum) . T.toLower

addWord :: Statistic -> [T.Text] -> Statistic
addWord st (w1:[])    = M.insertWith (M.unionWith (+)) w1 (M.singleton Nothing   1) st
addWord st (w1:w2:[]) = M.insertWith (M.unionWith (+)) w1 (M.singleton (Just w2) 1) st

emptyStatistic :: Statistic
emptyStatistic = M.empty

generateString :: [Float] -> Probability -> T.Text -> T.Text
generateString (p:ps) pr str =
  if str `M.member` pr
    then case select p (pr M.! str) of
          Just t -> t `T.append` T.pack " " `T.append` generateString ps pr t
          Nothing -> str
    -- else error $ "No key found: " ++ T.unpack str -- str
    else str

select :: Float -> M.Map (Maybe T.Text) Float -> Maybe T.Text
select p pr = select' p (M.toList pr)
  where
    select' :: Float -> [(Maybe T.Text, Float)] -> Maybe T.Text
    select' p ((t, pr):lst) | p > pr    = select' (p - pr) lst
                            | otherwise = t

main :: IO ()
main = do
  probs <- liftM (buildProbability . buildStatistic . T.splitOn "." . T.pack) $ readFile "test.txt"
  putStrLn . T.unpack $ generateString (randomRs (0, 1) (mkStdGen 234)) probs "highly"
  -- putStrLn $ show probs
