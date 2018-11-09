module Data.Mgz.Utils where

import RIO
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.IxSet.Typed as IxSet
import  Data.IxSet.Typed (IxSet, Indexable, IsIndexOf)


ixsetGetIn :: (Indexable ixs a, IsIndexOf ix ixs) => [ix] -> IxSet ixs a  -> IxSet ixs a
ixsetGetIn = flip (IxSet.@+)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

mNonEmpty :: Maybe a -> Maybe (NonEmpty a)
mNonEmpty Nothing = Nothing
mNonEmpty (Just a) = pure $ a :| []

nonEmptyPartial :: [x] -> NonEmpty x
nonEmptyPartial [] = error "Empty list in nonEmptyPartial"
nonEmptyPartial (x:xs) = x :| xs

elemNonEmpty :: Eq a => a -> NonEmpty a -> Bool
elemNonEmpty a ne = a `elem` NE.toList ne

nonEmptySingle :: NonEmpty a -> a
nonEmptySingle (a:|_) = a

tryWhileNothing :: (Monad m) => [m (Maybe r)] -> m (Maybe r)
tryWhileNothing [] = pure $ Nothing
tryWhileNothing (x:xs) = do
  r <- x
  case r of
    Nothing -> tryWhileNothing xs
    jr -> pure jr
