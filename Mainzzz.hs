import System.Environment
import Data.Char(isDigit, isHexDigit)

data Datum = Ch Char | EOF deriving (Show, Read, Eq)
type DatumStr = [Datum]

unCh :: Datum -> Char
unCh (Ch c) = c

data Parser =
  PFn (Datum -> Bool) |
  PSeq Parser Parser |
  PJnc Parser Parser |
  PDjnc Parser Parser |
  PDone |
  PFail |
  PEOF Parser

-- pcseq :: Parser -> Parser -> Parser
-- pcseq (PEOF a) b = PEOF (PSeq a b)
-- pcseq a b = PSeq a b

-- pcjnc :: Parser -> Parser -> Parser
-- pcjnc (PEOF a) (PEOF b) = PEOF (PJnc a b)
-- pcjnc (PEOF a) b = PJnc a b
-- pcjnc a (PEOF b) = PJnc a b
-- pcjnc a b = PJnc a b

-- pcdjnc :: Parser -> Parser -> Parser
-- pcdjnc (PEOF a) (PEOF b) = PEOF (PDjnc a b)
-- pcdjnc (PEOF a) b = PEOF (PDjnc a b)
-- pcdjnc a (PEOF b) = PEOF (PDjnc a b)
-- pcdjnc a b = PDjnc a b

instance Show Parser where
  show (PFn _) = "PFn"
  show (PSeq a b) = "(PSeq "++show a++" "++show b++")"
  show (PJnc a b) = "("++show a++" & "++show b++")"
  show (PDjnc a b) = "("++show a++" | "++show b++")"
  show PDone = "PDone"
  show PFail = "PFail"

pcombine :: ([Parser] -> Parser) -> (Parser -> Parser -> Parser) -> [Parser] -> Parser
pcombine _ _ [] = PDone
pcombine rec con (x:y:xs) = rec $ (con x y):xs
pcombine _ _ [x] = x

pseq :: [Parser] -> Parser
pseq = pcombine pseq PSeq

pand :: [Parser] -> Parser
pand = pcombine pand PJnc

por :: [Parser] -> Parser
por = pcombine por PDjnc

parse :: Parser -> Datum -> Parser
parse (PJnc PDone b) c = parse b c
parse (PJnc a PDone) c = parse a c

parse (PDjnc PFail b) c = parse b c
parse (PDjnc a PFail) c = parse a c

parse (PFn f) c = if f c then PDone else PFail
parse (PSeq a b) c =
  case parse a c of
    PDone -> b
    PFail -> PFail
    x -> PSeq x b
parse (PJnc a b) c =
  case parse a c of
    PDone -> parse b c
    PFail -> PFail
    x ->
      case parse b c of
        PDone -> x
        PFail -> PFail
        y -> PJnc x y
parse (PDjnc a b) c =
  case parse a c of
    PDone -> PDone
    PFail -> parse b c
    x ->
      case parse b c of
        PDone -> PDone
        PFail -> x
        y -> PDjnc x y
parse PDone _ = error "tried parsing PDone"
parse PFail _ = error "tried parsing PFail"


parseDStr :: Parser -> DatumStr -> (DatumStr, Parser)
parseDStr PDone s = (s, PDone)
parseDStr PFail s = (s, PFail)
-- parseDStr p [] = ([], p)
parseDStr p [] = ([], parse p EOF)
parseDStr p (x:xs) = parseDStr (parse p x) xs

parseDStr' :: Parser -> DatumStr -> (DatumStr, Bool)
parseDStr' p s =
  case parseDStr p s of
    (x, PDone) -> (x, True)
    (x, _) -> (x, False)

parseStr :: Parser -> String -> (String, Parser)
parseStr p xs =
  let (xs', p') = parseDStr p (Ch <$> xs)
  in (unCh <$> xs', p')

parseStr' :: Parser -> String -> (String, Bool)
parseStr' p s =
  case parseStr p s of
    (x, PDone) -> (x, True)
    (x, _) -> (x, False)


pany :: Parser
pany = PFn $ \_ -> True

pforever :: Parser -> Parser
pforever x = pseq [x, pforever x]

pall :: Parser
pall = pforever pany

pmaybe :: Parser -> Parser -> Parser
pmaybe x next = por [pseq [x, next], next]

pmaybe' :: (Parser -> Parser) -> Parser -> Parser
pmaybe' x next = por [x next, next]

pstar :: Parser -> Parser -> Parser
pstar x next = por [
    pplus x next,
    next
  ]

pplus :: Parser -> Parser -> Parser
pplus x next = pseq [x, pstar x next]

-- commented because it inverses eof-handling
-- pnot :: Parser -> Parser
-- pnot (PFn f) = PFn $ not . f

peof :: Parser
peof = PFn $ \d -> case d of Ch _ -> False; EOF -> True

pnoeof :: (Char -> Bool) -> Parser
pnoeof f = PFn $ \d -> case d of Ch c -> f c; EOF -> False

pchr :: Char -> Parser
pchr x = pnoeof (== x)

pNchr :: Char -> Parser
pNchr x = pnoeof (/= x)

pstr :: String -> Parser
pstr s = pseq $ pchr <$> s

prep :: Parser -> Int -> Parser
prep p n = pseq $ replicate n p





space = pnoeof $ \c ->
  c == ' ' ||
  c == '\t' ||
  c == '\n' ||
  c == '\f'

array' next = value $ pmaybe' (\n -> pseq [pchr ',', array' n]) next
array = pseq [pchr '[', pmaybe' array' $ pchr ']']

string' =
  por [
    pnoeof (\c -> c /= '"' && c /= '\\'),
    pseq [
      pchr '\\',
      por [
        pchr '"',
        pchr '\\',
        pchr '/',
        pchr 'b',
        pchr 'f',
        pchr 'n',
        pchr 'r',
        pchr 't',
        pseq [pchr 'u', prep (pnoeof isHexDigit) 4]
      ]
    ]
  ]
string = pseq [pchr '"', pplus string' (pchr '"')]

object' next = pseq [string, pchr ':', value $ pmaybe' (\n -> pseq [pchr ',', object' n]) next]
object = pseq [pchr '{', pmaybe' object' $ pchr '}']

pdigit = pnoeof isDigit
number_exp next =
  por [
    pseq [
      pnoeof (\c -> c == 'e' || c == 'E'),
      pmaybe (pnoeof (\c -> c == '+' || c == '-')) $
        pplus pdigit next
    ],
    next
  ]
number' next =
  por [
    pseq [pchr '.', pplus pdigit (number_exp next)],
    number_exp next
  ]
number next =
  pmaybe (pchr '-') $
    por [
      pseq [pchr '0', number' next],
      pseq [pnoeof (\c -> c /= '0' && isDigit c), pstar pdigit (number' next)]
    ]
value next = por $ (number next):((\p -> pseq [p, next]) <$> [string, object, array, pstr "true", pstr "false", pstr "null"])

json = pmaybe space (value $ pmaybe space peof)




main :: IO ()
main = do
  (path:_) <- getArgs
  str <- readFile path
  putStrLn (show $ parseStr' json str)
  return ()












