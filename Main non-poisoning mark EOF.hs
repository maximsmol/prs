import Data.Char(isDigit)

data Parser =
  PFn (Char -> Bool) |
  PSeq Parser Parser |
  PJnc Parser Parser |
  PDjnc Parser Parser |
  PDone |
  PFail |
  PEOF

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
  show PEOF = "PEOF"

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

parse :: Parser -> Char -> Parser
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
parse PEOF _ = PEOF
parse PDone _ = error "tried parsing PDone"
parse PFail _ = error "tried parsing PFail"


parseStr :: Parser -> String -> (String, Parser)
parseStr PEOF [] = ([], PDone)
parseStr p [] = ([], p)
parseStr PDone s = (s, PDone)
parseStr PFail s = (s, PFail)
parseStr p (x:xs) = parseStr (parse p x) xs

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

pstar :: Parser -> Parser -> Parser
pstar x next = por [
    pplus x next,
    next
  ]

pplus :: Parser -> Parser -> Parser
pplus x next = pseq [x, pstar x next]

pchr :: Char -> Parser
pchr x = PFn $ \c -> x == c

pstr :: String -> Parser
pstr s = pseq $ pchr <$> s

object' next = pseq [
    string,
    pchr ':',
    value next,
    por [
      pseq [pchr ',', object' next],
      next
    ]
  ]
object = pseq [
    pchr '{',
    por [
      object' $ pchr '}',
      pchr '}'
    ]
  ]

array' next = pseq [
    value next,
    por [
      pseq [pchr ',', array' next],
      next
    ]
  ]
array = pseq [
    pchr '[',
    por [
      array' $ pchr ']',
      pchr ']'
    ]
  ]

string' = por [
    pseq [PFn (/= '"'), string'],
    pchr '"'
  ]
string =
  pseq [
    pchr '"',
    string'
  ]

number = pplus $ PFn isDigit
value next = por [string, number next, object, array, pstr "true", pstr "false", pstr "null"]

json = por [object, array, por [value PFail, PEOF]]
