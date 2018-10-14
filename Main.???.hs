import Data.Char(isDigit)

data Parser =
  PFn (Char -> Bool) |
  PSeq Parser Parser |
  PJnc Parser Parser |
  PDjnc Parser Parser |
  PDone |
  PFail |
  PEOF

pliftEOF :: Parser -> Parser
pliftEOF (PJnc a b) =
  case PJnc (pliftEOF a) (pliftEOF b) of
    PJnc (PJnc PEOF x) (PJnc PEOF y) -> PJnc PEOF (PJnc x y)

    PJnc (PJnc PEOF x) PEOF -> PJnc PEOF x
    PJnc PEOF (PJnc PEOF y) -> PJnc PEOF y

    PJnc (PJnc PEOF x) y -> PJnc PEOF (PJnc x y)
    PJnc x (PJnc PEOF y) -> PJnc PEOF (PJnc x y)

    PJnc PEOF PEOF -> PEOF
    PJnc x PEOF -> pliftEOF $ PJnc PEOF x

    x -> x
pliftEOF (PDjnc a b) =
  case PDjnc (pliftEOF a) (pliftEOF b) of
    PDjnc (PDjnc PEOF x) (PDjnc PEOF y) -> PDjnc PEOF (PDjnc x y)

    PDjnc (PDjnc PEOF x) PEOF -> PDjnc PEOF x
    PDjnc PEOF (PDjnc PEOF y) -> PDjnc PEOF y

    PDjnc (PDjnc PEOF x) y -> PDjnc PEOF (PDjnc x y)
    PDjnc x (PDjnc PEOF y) -> PDjnc PEOF (PDjnc x y)

    PDjnc PEOF PEOF -> PEOF
    PDjnc x PEOF -> pliftEOF $ PDjnc PEOF x

    x -> x
pliftEOF x = x

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
-- parse (PJnc PDone b) c = parse b c
-- parse (PJnc a PDone) c = parse a c

-- parse (PDjnc PFail b) c = parse b c
-- parse (PDjnc a PFail) c = parse a c

parse (PDjnc PEOF b) c = parse b c
parse (PDjnc a PEOF) c = parse a c

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
parse PEOF _ = error "tried parsing PEOF"
parse PDone _ = error "tried parsing PDone"
parse PFail _ = error "tried parsing PFail"


parseStrH :: Parser -> String -> (String, Parser)
parseStrH (PDjnc PEOF _) [] = ([], PDone)
parseStrH (PDjnc _ PEOF) [] = ([], PDone)
parseStrH PEOF [] = ([], PDone)
parseStrH p [] = ([], p)
parseStrH PDone s = (s, PDone)
parseStrH PFail s = (s, PFail)
parseStrH p (x:xs) = parseStr (parse p x) xs

parseStr :: Parser -> String -> (String, Parser)
parseStr p s = parseStrH (pliftEOF p) s

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

json = por [object, array, value PEOF]
