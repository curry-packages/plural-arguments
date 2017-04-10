import ExprParser
import Test.Prop

testIntExp = always $ checkNatExp "(123*(42+500))"

testHexExp = always $ checkHexExp "(1A&7F)"


-- Run the tests:
-- > cpm exec curry-plural -c ExprParser
-- > cpm exec curry-check ExprParserTests
