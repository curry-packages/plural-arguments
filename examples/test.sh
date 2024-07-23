#!/bin/sh
# Run the tests for the plural argument translation tool:

/bin/rm -rf .curry *_TRANSPLURAL.curry

cypm exec curry-plural -q -c Palindrome
cypm exec curry-check PalindromeTests

cypm exec curry-plural -q -c CoinExamples
cypm exec curry-check CoinExamplesTests

cypm exec curry-plural -q -c ExprParser
cypm exec curry-check ExprParserTests

/bin/rm -rf .curry *_TRANSPLURAL.curry
