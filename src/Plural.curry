--- This module provides datatypes and operations
--- to implement operations with plural arguments.
---
--- @author Michael Hanus
--- @version December 2012

module Plural where

--- This trivial type synonym is used to mark plural arguments in
--- a Curry program.
type Plural a = a

--- Datatype for representing plural arguments.
--- A plural argument is a unary operation wrapped with some constructor.
--- Thus, each expression `e` to construct an actual plural argument
--- must be replaced by `(PluralArg (\_->e))`.
data PluralArg a = PluralArg (() -> a)

--- Access a plural argument by applying the wrapped operation.
--- Thus, each use of a plural argument parameter `x` in the right-hand side
--- must be replaced by `(plural x)`.
plural :: PluralArg a -> a
plural (PluralArg pfun) = pfun ()

