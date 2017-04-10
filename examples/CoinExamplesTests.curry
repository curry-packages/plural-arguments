import CoinExamples
import Test.Prop

testDupsCoin = dups_coin <~> ((0,0) ? (1,1))

testDuppCoin = dupp_coin <~> ((0,0) ? (0,1) ? (1,0) ? (1,1))

testPluralPat1 = fp_C0_or_C1 <~>  ((0,0) ? (0,1) ? (1,0) ? (1,1))

testPluralPat2 = fp_C_0or1 <~>  ((0,0) ? (0,1) ? (1,0) ? (1,1))

testPluralPat3 =
  fp2_0or1_0or1 <~>
  ( (0,0,0,0) ? (0,0,0,1) ? (0,0,1,0) ? (0,0,1,1) ?
    (0,1,0,0) ? (0,1,0,1) ? (0,1,1,0) ? (0,1,1,1) ?
    (1,0,0,0) ? (1,0,0,1) ? (1,0,1,0) ? (1,0,1,1) ? 
    (1,1,0,0) ? (1,1,0,1) ? (1,1,1,0) ? (1,1,1,1) )

testPluralPat4 =
  f1P_coin <~>
  ( (0,0,0,0) ? (0,0,0,1) ? (0,0,1,0) ? (0,0,1,1) ?
    (0,1,0,0) ? (0,1,0,1) ? (0,1,1,0) ? (0,1,1,1) ?
    (1,0,0,0) ? (1,0,0,1) ? (1,0,1,0) ? (1,0,1,1) ? 
    (1,1,0,0) ? (1,1,0,1) ? (1,1,1,0) ? (1,1,1,1) )


-- Run the tests:
-- > cpm exec curry-plural -c CoinExamples
-- > cpm exec curry-check CoinExamplesTests
