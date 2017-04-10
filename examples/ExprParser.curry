-- An example for the use of plural arguments:
-- An expression parser which has the possible operators as a plural argument.

import Plural

infixr 4 <*>
infixr 2 <|>

--- A parser maps a list of tokens into a list of unconsumed tokens
type Parser token = [token] -> [token]

--- A parser recognizing a particular terminal symbol.
terminal :: t -> Parser t
terminal sym (token:tokens) | sym=:=token = tokens

--- The empty parser which recognizes the empty word.
empty :: Parser _
empty sentence = sentence

--- Combines two alternative parsers.
(<|>)  :: Parser t -> Parser t -> Parser t
p <|> q = \sentence -> p sentence ? q sentence

--- Combines two parsers sequentially.
(<*>)    :: Parser t -> Parser t -> Parser t
p1 <*> p2 = seq
 where seq sentence | p1 sentence =:= sent1 = p2 sent1  where sent1 free

--- Star combinator for a parser. Since the parser argument is
--- used several time, it is passed as a plural argument.
star :: Plural (Parser t) -> Parser t
star p = empty <|> (p <*> star p)

-- A parser for numbers parameterized over the possible digits (so that
-- leading zeros are not allowed):
number :: Plural Char -> Parser Char
number d = terminal d <*> star (terminal (d?'0'))

octNzDigit = '1'?'2'?'3'?'4'?'5'?'6'?'7'
decNzDigit = octNzDigit?'8'?'9'
hexNzDigit = decNzDigit?'A'?'B'?'C'?'D'?'E'?'F'

binNum = number '1'
octNum = number octNzDigit
decNum = number decNzDigit
hexNum = number hexNzDigit

-- Expressions are parameterized over the kind of digits for numeric constants
-- and some operators (represented by a non-deterministic operation)
exp :: Plural Char -> Plural Char -> Parser Char
exp digit op =
 number digit <|>
 terminal '(' <*> exp digit op <*> terminal op <*> exp digit op <*> terminal ')'

-- A parser for standard integer expressions, e.g., "(123*(42+500))"
natOp = '+' ? '-' ? '*' ? '%'
checkNatExp s = exp decNzDigit natOp s =:= []


-- A parser for standard hexadecimal expressions, e.g., "(1A&7F)"
hexOp = '&' ? '|'
checkHexExp s = exp hexNzDigit hexOp s =:= []
