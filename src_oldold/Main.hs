import Prelude hiding (seq, repeat, or, and, maybe)

import Data.Parser.Base
import Data.Parser.Presets

object' = seq [string, char ':', value, maybe $ seq [char ',', object']]
object = seq [char '{', maybe object', char '}']

array' = seq [value, maybe $ seq [char ',', array']]
array = seq [char '[', maybe array', char ']']

escape = or [
    char '"',
    char '\\',
    char '/',
    char 'b',
    char 'f',
    char 'n',
    char 'r',
    char 't',
    seq [char 'u', repeat 4 hexDigit]
  ]
string' = or [noEOF (\x -> x /= '"' && x /= '\\'), seq [char '\\', escape]]
string = seq [char '"', star string', char '"']

number'' = maybe $ seq [
    or [char 'e', char 'E'],
    maybe $ or [char '+', char '-'],
    plus digit
  ]
number' = seq [maybe $ seq [char '.', plus digit], number'']
number = seq [
    maybe $ char '-',
    or [
      seq [char '0', number'],
      seq [and [notChar '0', digit], star digit, number']
    ]
  ]

value = or [string, number, object, array, str "true", str "false", str "null"]

json = finishWithEOF value
