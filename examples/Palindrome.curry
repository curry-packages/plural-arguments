-- A palindrome parser parameterized over the alphabet via a plural argument

import Plural

--- A parser maps a list of tokens into a list of unconsumed tokens
type Parser token = [token] -> [token]

--- A parser recognizing a particular terminal symbol.
terminal :: Data t => t -> Parser t
terminal sym (token:tokens) | sym=:=token = tokens

--- The empty parser which recognizes the empty word.
empty :: Parser _
empty sentence = sentence

--- Combines two alternative parsers.
(<|>)  :: Parser t -> Parser t -> Parser t
p <|> q = \sentence -> p sentence ? q sentence

--- Combines two parsers sequentially.
(<*>)    :: Data t => Parser t -> Parser t -> Parser t
p1 <*> p2 = seq
 where seq sentence | p1 sentence =:= sent1 = p2 sent1  where sent1 free


-- Palindromes parameterized over some terminals (represented by
-- a non-deterministic plural argument)
pali :: Data a => Plural a -> Parser a 
pali t = empty
     <|> terminal t
     <|> let someT = terminal t
          in someT <*> pali t <*> someT


-- Check whether the argument is a palindrome over the letter 'a' and 'b':
checkPaliAB :: String -> Bool
checkPaliAB s = pali ('a' ? 'b') s =:= []

-- Check whether the argument is a palindrome over the letter 'a', 'b', 'c':
checkPaliABC :: String -> Bool
checkPaliABC s = pali ('a' ? 'b' ? 'c') s =:= []
