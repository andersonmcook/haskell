module Crypto (rotDecoder, rotEncoder) where

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Bounded, Enum, Show)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
 where
  halfAlphabet = alphabetSize `div` 2
  offset       = fromEnum c + halfAlphabet
  rotation     = offset `mod` alphabetSize

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

-- fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
-- fourLetterAlphabetEncoder vals = map rot4l vals
--  where
--   alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
--   rot4l     = rotN alphaSize
fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder =
  map (rotN (1 + fromEnum (maxBound :: FourLetterAlphabet)))

data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Bounded, Enum, Show)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
 where
  alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
  rot3l     = rotN alphaSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
 where
  halfN        = div n 2
  intermediate = fromEnum c + halfN
  offset       = if even n then intermediate else 1 + intermediate
  rotation     = mod offset n

-- threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
-- threeLetterDecoder = map rot3ldecoder
--  where
--   alphaSize    = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
--   rot3ldecoder = rotNdecoder alphaSize
threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder =
  map (rotNdecoder (1 + fromEnum (maxBound :: ThreeLetterAlphabet)))

rotEncoder :: String -> String
rotEncoder = map (rotN (1 + fromEnum (maxBound :: Char)))

rotDecoder :: String -> String
rotDecoder = map (rotNdecoder (1 + fromEnum (maxBound :: Char)))
