{-# LANGUAGE OverloadedRecordDot #-}

module SimpleSearch where

import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Ord

data IndexedDocument = IndexedDocument
  { path :: FilePath,
    totalTerms :: Int,
    termFreqs :: Map String Int
  }
  deriving (Show, Eq, Ord)

tf :: IndexedDocument -> String -> Maybe Double
tf doc str = do
  occurances <- doc.termFreqs M.!? str
  return $ fromIntegral occurances / fromIntegral doc.totalTerms

data Index = Index
  { tokenize :: String -> [String],
    termMap :: Map String (Set IndexedDocument)
  }

instance Show Index where
  show index = show index.termMap

type Tokenizer = String -> [String]

idf :: String -> Index -> Maybe Double
idf term index = do
  searchHits <- index.termMap M.!? term
  return (fromIntegral (M.size index.termMap) / fromIntegral (S.size searchHits))

tfIdf :: String -> IndexedDocument -> Index -> Maybe Double
tfIdf term doc index = do
  tfVal <- tf doc term 
  idfVal <- idf term index
  return $ tfVal * idfVal

mkIndex :: Tokenizer -> Index
mkIndex tokenizer = Index tokenizer M.empty

addDocument :: String -> FilePath -> Index -> Index
addDocument str path index = index {termMap = termMap'}
  where
    tokens = index.tokenize str
    indexedDocument =
      IndexedDocument
        path
        (length tokens)
        (M.fromListWith (+) (zip tokens (repeat 1)))
    addTerm term = M.insertWith S.union term (S.singleton indexedDocument)
    termMap' = foldr addTerm index.termMap tokens

addFile :: Index -> FilePath -> IO Index
addFile index path = do
  contents <- readFile path
  return $ addDocument contents path index

search :: String -> Index -> Maybe [FilePath]
search term index = do
  searchHits <- index.termMap M.!? term
  let tfIdfComparator = comparing $ Down . (\doc -> tfIdf term doc index)
  return $ map path $ sortBy tfIdfComparator (S.toList searchHits)

