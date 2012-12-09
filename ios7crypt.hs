-- Allows derivating Typeable and Data
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module IOS7Crypt where

import Language.Fay.FFI
import Language.Fay.Prelude

-- import Data.Random
-- import Data.Random.Source.IO

-- Fay
import Random

import Numeric (showInt, showHex)
import Data.List (intercalate)
import Data.Char (ord, chr)
import Bits (xor)

import Data.Maybe (fromJust)
import Control.Monad (when)

xlat' = [
	0x64, 0x73, 0x66, 0x64, 0x3b, 0x6b, 0x66, 0x6f,
	0x41, 0x2c, 0x2e, 0x69, 0x79, 0x65, 0x77, 0x72,
	0x6b, 0x6c, 0x64, 0x4a, 0x4b, 0x44, 0x48, 0x53,
	0x55, 0x42, 0x73, 0x67, 0x76, 0x63, 0x61, 0x36,
	0x39, 0x38, 0x33, 0x34, 0x6e, 0x63, 0x78, 0x76,
	0x39, 0x38, 0x37, 0x33, 0x32, 0x35, 0x34, 0x6b,
	0x3b, 0x66, 0x67, 0x38, 0x37
	]

xlat :: Int -> [Int]
xlat s = (drop s . cycle) xlat'

-- Based on Martin Grabmueller's Harpy.X86Disassembler
-- http://hackage.haskell.org/packages/archive/harpy/0.4/doc/html/src/Harpy-X86Disassembler.html
pad2 :: (Int -> String -> String) -> Int -> String
pad2 f i = take (2 - length s) "0" ++ s
	where s = f i ""

onlyPairs :: String -> [String]
onlyPairs text = take 2 text : if length text <= 3 then
		[]
	else
		(onlyPairs . drop 2) text

encrypt' :: Int -> String -> String
encrypt' seed password
	| seed < 0 || seed > 15 = encrypt' 0 password
	| otherwise = pad2 showInt seed ++ (intercalate "" . (parMap rseq) (pad2 showHex) . zipWith xor (xlat seed) . (parMap rseq) ord) password

encrypt :: String -> Fay String
encrypt password = do
	seed <- random 16
	return $ encrypt' seed password

decrypt :: String -> Maybe String
decrypt hash
	| length hash < 4 = Just ""
	| otherwise = decrypt' s p
		where
			s = (maybeRead . take 2) hash
			(p :: [Maybe Int]) = (parMap rseq) (maybeRead . ("0x" ++)) ((onlyPairs . drop 2) hash)

			decrypt' :: Maybe Int -> [Maybe Int] -> Maybe String
			decrypt' Nothing _ = Nothing
			decrypt' (Just s) p
				| Nothing `elem` p = Nothing
				| otherwise = Just $ ((parMap rseq) chr . zipWith xor (xlat s) . (parMap rseq) fromJust) p

addEventListener :: Element -> String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "%1.addEventListener(%2, %3, %4)"

getElementsByTagName :: String -> Fay [Element]
getElementsByTagName = ffi "document.getElementsByTagName(%1)"

getFirstElementByTagName :: String -> Fay Element
getFirstElementByTagName tag = do
	es <- getElementsByTagName tag
	return $ head es

-- setHTML :: Element -> String -> Fay ()
-- setHTML = ffi "%1.innerHTML = %2"
-- 
-- refresh :: Fay ()
-- refresh = do
-- 	d <- getFirstElementByTagName "div"
-- 	r <- random 100
-- 	setHTML d $ show r
-- 
-- main :: Fay ()
-- main = refresh