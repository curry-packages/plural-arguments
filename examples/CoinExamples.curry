-- Definition of operations without and with plural arguments in Curry

import Plural

coin :: Int
coin = 0 ? 1

-- The dup operation with a singular argument:
dups :: Int -> (Int,Int)
dups x = (x,x)

dups_coin = dups coin

-- The dup operation with a plural argument:
dupp :: Plural Int -> (Int,Int)
dupp x = (x,x)

dupp_coin = dupp coin

-- Pattern matching with plural arguments:
data C = C Int

fp :: Plural C -> (Int,Int)
fp (C x) = (x,x)

fp_C0_or_C1 = fp (C 0 ? C 1)

fp_C_0or1 = fp (C (0 ? 1))

fp2 :: Plural (Int,Int) -> (Int,Int,Int,Int)
fp2 (x,y) = (x,y,x,y)

fp2_0or1_0or1 = fp2 (0?1, 0?1)

-- Example for nested calls with plural arguments:

f1P :: Plural a -> (a,a,a,a)
f1P x = f3P (f2P x)

f2P :: Plural a -> (a,a)
f2P x = (x,x)

f3P :: Plural (a,b) -> (a,b,a,b)
f3P (x,y) = (x,y,x,y)

f1P_coin = f1P coin

-------------------------------------------------------

t :: Plural Int -> Int -> Plural Int -> (Int,Int,Int,Int,Int,Int)
t x y z = (x,y,z,x,y,z)

mt = t coin coin coin
