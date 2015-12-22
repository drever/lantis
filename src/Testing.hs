module Testing where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck

import qualified Data.Text as T
import Data.Time

import Model


instance Arbitrary Status where
    arbitrary = toEnum `fmap` choose (0, 8)

instance Arbitrary Project where
    arbitrary = do
        n <- arbitrary
        i <- arbitrary
        is <- arbitrary
        s <- arbitrary
        return $ Project n i is s

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary

instance Arbitrary Category where
    arbitrary = toEnum `fmap` choose (0, 2)

instance Arbitrary Relationship where
    arbitrary = oneof $ [RelationshipParent `fmap` arbitrary, RelationshipRelated `fmap` arbitrary]

instance Arbitrary ViewStatus where
    arbitrary = toEnum `fmap` choose (0, 1)

instance Arbitrary Severity where
    arbitrary = toEnum `fmap` choose (0, 1)

instance Arbitrary Priority where
    arbitrary = toEnum `fmap` choose (0, 2)

instance Arbitrary Reproducibility where
    arbitrary = toEnum `fmap` choose (0, 1)

instance Arbitrary Resolution where
    arbitrary = toEnum `fmap` choose (0, 1)

-- orphan instances from Test.QuickCheck.Instances
instance Arbitrary UTCTime where
    arbitrary =
            UTCTime
            <$> arbitrary
            <*> (fromRational . toRational <$> choose (0::Double, 86400))
    shrink ut@(UTCTime day dayTime) =
            [ ut { utctDay     = d' } | d' <- shrink day     ] ++
            [ ut { utctDayTime = t' } | t' <- shrink dayTime ]

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> (2000 +) <$> arbitrary
    shrink    = (ModifiedJulianDay <$>) . shrink . toModifiedJulianDay

instance Arbitrary DiffTime where
    arbitrary = arbitrarySizedFractional
    shrink    = shrinkRealFrac
--

instance Arbitrary Issue where
    arbitrary = do
        status <- arbitrary
        summary <- arbitrary
        description <- arbitrary
        tags <- arbitrary
        relationships <- arbitrary
        iid <- arbitrary
        project <- arbitrary
        category <- arbitrary
        dateSubmitted <- arbitrary
        lastUpdate <- arbitrary
        reporter <- arbitrary
        viewStatus <- arbitrary
        assignedTo <- arbitrary
        severity <- arbitrary
        prio <- arbitrary
        reproducability <- arbitrary
        resolution <- arbitrary
        return $ Issue status summary description tags relationships iid project category dateSubmitted lastUpdate reporter viewStatus assignedTo severity prio reproducability resolution