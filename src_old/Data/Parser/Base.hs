{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Parser.Base where

data Datum = Datum Char | EOF deriving (Show, Read, Eq)
data Parser =
  Done | Fail |
  Fn (Datum -> Bool) |
  Seq Parser Parser |
  Or Parser Parser |
  And Parser Parser

instance Show Parser where
  show Done = "Done"
  show Fail = "Fail"
  show (Fn _) = "Fn"
  show (Seq _ _) = "Seq"
  show (Or _ _) = "Or"
  show (And _ _) = "And"

parse :: Parser -> Datum -> Parser
parse Fail _ = error "parse Fail _"
parse Done _ = error "parse Done _"

parse (Fn f) d = if f d then Done else Fail
parse (Seq a b) d =
  case parse a d of
    Fail -> Fail
    Done -> b
    x -> Seq x b
parse (Or a b) d =
  case parse a d of
    Done -> Done
    Fail -> parse b d
    x ->
      case parse b d of
        Done -> Done
        Fail -> x
        y -> Or x y
parse (And a b) d =
  case parse a d of
    Fail -> Fail
    Done -> parse b d
    x ->
      case parse b d of
        Fail -> Fail
        Done -> x
        y -> And x y


parseL :: Parser -> [Datum] -> (Parser, [Datum])
parseL p [] = (p, [])
parseL Done s = (Done, s)
parseL Fail s = (Fail, s)
parseL p (x:xs) = parseL (parse p x) xs

parseStr :: Parser -> String -> (Parser, String)
parseStr p str =
  let
    (p', dat) = parseL p $ (Datum <$> str)++[EOF]
    unDatum = \x -> case x of Datum c -> c
  in (p', (fmap unDatum) . filter (/= EOF) $ dat)

parseStr' :: Parser -> String -> (Bool, String)
parseStr' p str =
  let
    (p', str') = parseStr p str
  in
    (
      case p' of Done -> True; _ -> False,
      str'
    )


data PT = NP Parser | LAP (PT -> PT)

instance Show PT where
  show (NP p) = "NP("++show p++")"
  show (LAP _) = "LAP"

doSeq :: PT -> PT -> PT
doSeq (NP a) (NP b) = NP $ Seq a b
doSeq (LAP a) (LAP b) = LAP $ \n -> a . b $ n
doSeq (LAP a) b = a b
doSeq a (LAP b) = LAP $ \n -> doSeq a (b n)

doOr :: PT -> PT -> PT
doOr (NP a) (NP b) = NP $ Or a b
doOr (LAP a) (LAP b) = LAP $ \n -> doOr (a n) (b n)
doOr (LAP a) b = LAP $ \n -> doOr (a n) b
doOr a (LAP b) = LAP $ \n -> doOr a (b n)

doAnd :: PT -> PT -> PT
doAnd (NP a) (NP b) = NP $ Or a b
doAnd (LAP a) (LAP b) = LAP $ \n -> doAnd (a n) (b n)
doAnd (LAP a) b = LAP $ \n -> doAnd (a n) b
doAnd a (LAP b) = LAP $ \n -> doAnd a (b n)

boxDone :: PT
boxDone = NP Done

boxFail :: PT
boxFail = NP Fail

boxFn :: (Datum -> Bool) -> PT
boxFn = NP . Fn


combine :: (PT -> PT -> PT) -> [PT] -> PT
combine _ [] = boxDone
combine _ [x] = x
combine f (x:y:xs) = combine f $ (f x y):xs

seq :: [PT] -> PT
seq = combine doSeq

or :: [PT] -> PT
or = combine doOr

and :: [PT] -> PT
and = combine doAnd
