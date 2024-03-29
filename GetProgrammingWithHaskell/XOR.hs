import Crypto (rotDecoder, rotEncoder)

type Bits = [Bool]

xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && (not (v1 && v2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: Bits -> Bits -> Bits
xor list1 list2 = map xorPair (zip list1 list2)

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
  then False : intToBits' nextVal
  else True : intToBits' nextVal
 where
  remainder = n `mod` 2
  nextVal   = n `div` 2

maxBits :: Int
maxBits = length $ intToBits' maxBound

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
 where
  reversedBits  = reverse (intToBits' n)
  missingBits   = maxBits - (length reversedBits)
  leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
-- charToBits char = intToBits (fromEnum char)
charToBits = intToBits . fromEnum

bitsToInt :: Bits -> Int
-- bitsToInt bits = sum (map (\(_, x) -> 2 ^ x) trueLocations)
bitsToInt bits = sum (map ((2 ^) . snd) trueLocations)
 where
  size          = length bits
  indices       = [size - 1, size - 2 .. 0]
  -- trueLocations = filter (\(x, _) -> x == True) (zip bits indices)
  trueLocations = filter ((== True) . fst) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar = toEnum . bitsToInt

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText =
  map (\(x, y) -> xor x y) (zip (map charToBits pad) (map charToBits plainText))

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar (applyOTP' pad plainText)

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
  decode :: a -> String -> String
  encode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  decode Rot text = rotDecoder text
  encode Rot text = rotEncoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
  decode (OTP pad) text = applyOTP pad text
  encode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

-- linear congruential pseudo-random number generator
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber
