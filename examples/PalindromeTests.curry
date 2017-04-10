-- Some tests for Palindrome module (note that the tests are not
-- inside the module since the tests require the plural transformation)

import Palindrome
import Test.Prop

testPaliAB1 = always $ checkPaliAB "abaaba"

-- This palindrome parsing fails since it is not a palindrome over "ab"
-- (although it is a palindrome over "abc")
testPaliAB2 = failing $ checkPaliAB "abacaba"

-- This is a palindrome parsing over "abc":
testPaliABC = always $ checkPaliABC "abacaba"

-- Run the tests:
-- > cpm exec curry-plural -c Palindrome
-- > cpm exec curry-check PalindromeTests
