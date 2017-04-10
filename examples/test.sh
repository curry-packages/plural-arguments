#!/bin/sh
# Run the tests for the plural argument translation tool:

/bin/rm -rf .curry *_TRANS.curry

cpm exec curry-plural -q -c Palindrome
cpm exec curry-check PalindromeTests

cpm exec curry-plural -q -c CoinExamples
cpm exec curry-check CoinExamplesTests

cpm exec curry-plural -q -c ExprParser
cpm exec curry-check ExprParserTests

/bin/rm -rf .curry *_TRANS.curry
