
import Control.Monad

safediv :: Maybe Float -> Maybe Float -> Maybe Float
safediv m n = m >>= \x -> n >>= \y -> if y == 0 then Nothing else Just (x / y)

senGood :: Maybe Bool
senGood = (return 2 `plusM` return 3)  `equalsM` return 5
  where plusM = liftM2 (+)
        equalsM = liftM2 (==)

-- senGood

senWhoops :: Maybe Float
senWhoops = return 8 `divM` (return 3 `minusM` return 3)
  where divM = liftM2 (/)
        minusM = liftM2 (-)

-- senWhoops

senMaybe :: Maybe Float
senMaybe = return 8 `safediv` (return 3 `minusM` return 3)
  where minusM = liftM2 (-)

-- senMaybe

the :: (Int -> Bool) -> Maybe Int
the cn = if length cns == 1 then Just (head cns) else Nothing
  where cns = filter cn domain
        domain = [1, 2, 3]

is :: a -> (a -> Bool) -> Bool
is x f = f x

big, small, mediumSized :: Maybe (Int -> Bool)
big           = return (== 3)
small         = return (== 1)
mediumSized   = return (== 2)

sen1 :: Maybe Bool
sen1 = the even `isM` mediumSized
  where isM = liftM2 is

-- sen1

sen2 :: Maybe Bool
sen2 = the even `isM` big
  where isM = liftM2 is

-- sen2

sen3 :: Maybe Bool
sen3 = the odd `isM` big
  where isM = liftM2 is

-- sen3
