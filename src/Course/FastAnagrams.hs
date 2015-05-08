{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams q file = (findAnagramsFast q . lines) <$> readFile file

findAnagramsFast :: Chars -> List Chars -> List Chars
findAnagramsFast q =
  let s = toSet (permutations q)
  in map ncString . filter (`S.member` s) . map NoCaseString

toSet :: List Chars -> S.Set NoCaseString
toSet = foldLeft (flip S.insert) S.empty . map NoCaseString



newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString
