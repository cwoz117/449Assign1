{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |			CPSC449 Programming Paradigms Assignment 1                | --
-- |					  Chris Wozniak                               | --
-- |					    10109820                                  | --
-- |                                                                        | --
-- | 		This Module contains functions for encrypting and               | --
-- | 		decrypting Vigenere Ciphers.                                    | --
-- |                                                                        | --
-- |                            DEPENDANCIES:                               | --
-- |        QuickCheck is the only dependancy required for this             | --
-- |        module (outside of prelude). To install it, use the             | --
-- |        cabal 'package manager' service that is provided                | --
-- |        with ghci.                                                      | --
-- |        ie:    "cabal install QuickCheck"                               | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
import Test.QuickCheck

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |                           Provided Functions                           | --
-- |                                                                        | --
-- |          Provided functions to switch between ASCII code               | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
ord :: Char -> Int
ord c = fromEnum c

chr :: Int -> Char
chr i = (toEnum i)::Char

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |                      ASCII Chars to Integers (0-25)                    | --
-- |                                                                        | --
-- |          Turns an ASCII Character into a numerical value               | --
-- |	        between 0, and 25 inclusively (A = 0, B = 1 etc..)            | --
-- |          -Restrictions: uppercase chars only, otherwise the            | --
-- |                        function only returns 25                        | --
-- |          -test_aToN quickCheck Function provided                       | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
aToN :: Char -> Int
aToN n
	| (ord n) > 90 = 25
	| (ord n) < 65  = 25
	| otherwise    = ((ord n ) - 65)

test_aToN :: Char -> Bool
test_aToN a =
            0 == 0

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |                         KeyMask generation                             | --
-- |                                                                        | --
-- |          Keymask takes your key, and generates a string of             | --
-- |          equal length so that we can properly map our encoded          | --
-- |          message. either shortening, or duplicating the key            | --
-- |          as required                                                   | --
-- |                                                                        | --
-- |          -KeyHelp is a helper function to allow the key to             | --
-- |           persist through the recursive process.                       | --
-- |          -test_keyHelp quickCheck Function Provided                    | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
keyMask :: [Char] -> [Char] -> [Char]
keyMask k m = keyHelp k k m

keyHelp :: [Char] -> [Char] -> [Char] -> [Char]
keyHelp key useKey msg
	| msg == []	   = []
	| useKey == [] = keyHelp key key msg
	| otherwise    = (head useKey) : (keyHelp key (tail useKey) (tail msg))

test_keyMask :: [Char] -> [Char] -> Bool
test_keyMask a b
      | ((a == []) || (b == []))      	= True
      | length (keyMask a b) == length b 	= True
      | otherwise    				= False

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |                         Encryption Function                            | --
-- |                                                                        | --
-- |          Encode takes the key and message provided, and                | --
-- |          generates our encoded message. It uses the                    | --
-- |          functions above and thus is restricted to only                | --
-- |          capital letters. Any other ascii char will be                 | --
-- |          converted to 'A'                                              | --
-- |          - test_encode uses the message conditioner to                 | --
-- |            allow testing code with quickCheck.                         | --
-- |          - cMSG conditions all generated messages for                  | --
-- |            testing to workable ascii.                                  | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
encode :: [Char] -> [Char] -> [Char]
encode k m = encHlp (keyMask k m) m

encHlp :: [Char] -> [Char] -> [Char]
encHlp k m
	| m == []   = []
	| otherwise	= chr ((((aToN (head k)) + (aToN (head m))) `mod` 26) + 65) :
			  (encHlp (tail k) (tail m))


-- - - - - - - - - - - - - - - -| Test Encoding |- - - - - - - - - - - - - - - -
test_encode :: [Char] -> [Char] -> Bool
test_encode k m
      | ((k == []) || (m == [])) = True
	| (cMsg m) == (decode (cMsg k) (encode (cMsg k) (cMsg m))) = True

-- - - - - - - - - - - - - - | Message Conditioner | - - - - - - - - - - - - - -
cMsg :: [Char] -> [Char]
cMsg a
	| a == [] = []
	| (((ord (head a)) >= 97) && ((ord (head a)) <= 122))
			= (chr ((ord (head a))-32)) : cMsg (tail a)
	| (((ord (head a)) >= 65) && ((ord (head a)) <= 90))
			= (head a) : cMsg (tail a)
	| otherwise = 'A' : cMsg (tail a)

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |                          Decryption Function                           | --
-- |                                                                        | --
-- |          Opposite of Encrypt. Used with the same quick test            | --
-- |          as encode.	                                              | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
decode :: [Char] -> [Char] -> [Char]
decode k cryp = decHlp (keyMask k cryp) cryp

decHlp :: [Char] -> [Char] -> [Char]
decHlp k c
      | c == []   = []
      | otherwise = chr ((( aToN (head c) - aToN (head k)) `mod` 26) + 65 ):
                     (decode (tail k) (tail c))
