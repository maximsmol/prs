module Data.Parser.Presets where

import Prelude hiding (seq, repeat)
import Data.Char(isDigit, isHexDigit)

import Data.Parser.Base

noEOF :: (Char -> Bool) -> PT
noEOF f = boxFn $ \d -> case d of EOF -> False; Datum c -> f c

eof' :: Parser
eof' = Fn $ \d -> case d of EOF -> True; _ -> False

eof :: PT
eof = NP eof'

isEOF :: PT -> Bool
isEOF (NP p) = case parse p EOF of Done -> True; _ -> False
isEOF _ = False

finishWithEOF :: PT -> Parser
finishWithEOF (LAP x) = finishWithEOF $ x eof
finishWithEOF (NP x) = Seq x eof'


char :: Char -> PT
char x = noEOF (== x)

notChar :: Char -> PT
notChar x = noEOF (/= x)

str :: String -> PT
str = seq . (char <$>)

plus :: PT -> PT
plus x = doSeq x $ star x

star :: PT -> PT
star x = LAP $ \n -> doOr (plus x) n

maybe :: PT -> PT
maybe x = LAP $ \n -> doOr x n

repeat :: Int -> PT -> PT
repeat n = seq . replicate n

digit :: PT
digit = noEOF isDigit

hexDigit :: PT
hexDigit = noEOF isHexDigit
