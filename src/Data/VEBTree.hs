module Data.VEBTree 
    (   VEBTree
    ,   empty
    ,   member
    ,   insert
    ,   successor
    ,   delete
    ,   u
    ,   fromList
    ) where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>) )
import Control.Monad ((=<<))
import qualified Data.Vector as V (Vector, replicate, (!), (//)) 
import Test.QuickCheck

--initially assume that u is a power of 2
data VEBTree = VEBNode 
    { u :: Int
    , v_min :: Maybe Int
    , v_max :: Maybe Int
    , summary :: VEBTree
    , clusters :: V.Vector VEBTree
    }
    -- for the VEB base, u=2
    | VEBBase
    { v_min :: Maybe Int
    , v_max :: Maybe Int
    }
    deriving (Show, Eq)

instance Arbitrary VEBTree where
    arbitrary = fromList <$> (arbitrary :: Gen [Int])


isqrt :: Int -> Int
isqrt = round . sqrt . fromIntegral

upPowerOfTwo :: Int -> Int
upPowerOfTwo x | x <= 0 = 0
upPowerOfTwo x = (^) 2  . ceiling . logBase 2 . fromIntegral $ x

empty :: Int -> VEBTree
empty 2 =  VEBBase Nothing Nothing
empty a = VEBNode (upPowerOfTwo a) Nothing Nothing (empty a') (V.replicate a' (empty a'))
    where a' = isqrt (upPowerOfTwo a) 

fromList :: [Int] -> VEBTree
fromList [] = empty 4
fromList x = foldr insert (empty size) x
    where size = max 4 (1+ maximum x) 

maybeOr :: Maybe Bool -> Maybe Bool -> Bool
maybeOr x y = (fromMaybe False x) || (fromMaybe False y)

isMinOrMax :: Int -> VEBTree -> Bool
isMinOrMax a tree = ((==a) <$> (v_min tree)) `maybeOr` ((==a) <$> (v_max tree)) 

high :: VEBTree -> Int -> Int
high tree x = x `quot` isqrt (u tree)  

low :: VEBTree -> Int -> Int
low tree x = x `mod` isqrt (u tree)

index :: VEBTree -> Int -> Int -> Int
index tree x y = x * (isqrt (u tree)) + y

member :: Int -> VEBTree -> Bool
member a tree | a > maxNum tree || a < 0 = False
member a tree@(VEBBase _ _)= isMinOrMax a tree  
member a tree@(VEBNode _ _ _ _ _)= isMinOrMax a tree 
        || member (low tree a) ((clusters tree) V.! (high tree a))  

maxNum :: VEBTree -> Int
maxNum (VEBBase _ _) = 1
maxNum (VEBNode u _ _ _ _) = u-1

minEqualsMax :: VEBTree -> Bool
minEqualsMax tree = v_min tree == v_max tree

insert :: Int -> VEBTree -> VEBTree 
-- cannot insert elements outside of the key range [0,u)
insert a tree | a > maxNum tree || a < 0 = tree
-- Base case -- initialize the node
insert a (VEBNode u Nothing Nothing summary clusters) = (VEBNode u (Just a) (Just a)  summary clusters)
insert a (VEBBase Nothing Nothing) = VEBBase (Just a) (Just a)
-- The next two cases are about replacing min if a <min or max if a>max
insert a (VEBBase (Just val_min) (Just val_max)) 
    | a < val_max = VEBBase (Just a) (Just val_max)
    | a > val_max = VEBBase (Just val_min) (Just a) 
insert a (VEBNode u (Just val_min) (Just val_max) summary clusters)
    | a < val_min = insert val_min (VEBNode u (Just a) (Just val_max) summary clusters)
    | a > val_max = insert val_max (VEBNode u (Just val_min) (Just a) summary clusters)
-- insert the node deeper into the tree if minimum and maximum do not have to be replaced
insert a tree@(VEBNode u maybe_min maybe_max summary clusters) =  
    let new_node = insert (low tree a) (clusters V.! (high tree a)) 
        new_clusters = clusters V.// [(high tree a, new_node)] 
     in case v_min $ clusters V.! (high tree a) of
        Nothing -> VEBNode u maybe_min maybe_max (insert (high tree a) summary) new_clusters    
        _ -> VEBNode u maybe_min maybe_max summary new_clusters 

successor :: Int -> VEBTree -> Maybe Int
successor a (VEBBase Nothing _) = Nothing
successor a (VEBBase (Just 0) (Just 1)) = Just 1
successor a (VEBBase _ _ ) = Nothing
successor a (VEBNode  _ (Just val_min) _ _ _) | a < val_min = return val_min
successor a tree@(VEBNode _ _ _ summary clusters ) = 
    case v_max $ clusters V.! (high tree a) of
        Just max_low | low tree a < max_low ->
            let offset = successor (low tree a) (clusters V.! (high tree a))
             in index tree (high tree a) <$> offset
        _ ->
            case successor (high tree a) summary of
                Nothing -> Nothing
                Just succ_cluster ->
                    let offset = v_min $ clusters V.! succ_cluster
                    in index tree succ_cluster <$> offset

-- TODO: check if the element exists before trying to delete it. This code assumes the element already exists
delete :: Int -> VEBTree -> VEBTree
delete _ tree@(VEBNode u maybe_min maybe_max summary clusters) 
   | minEqualsMax tree = VEBNode u Nothing Nothing summary clusters
delete _ tree@(VEBBase maybe_min maybe_max) 
    | minEqualsMax tree = VEBBase Nothing Nothing
delete 0 (VEBBase _ (Just 1)) = VEBBase (Just 1) (Just 1)
delete 1 (VEBBase (Just 0) _ ) = VEBBase (Just 0) (Just 0)
delete a tree@(VEBNode u (Just val_min) maybe_max summary clusters)
    | a == val_min =
        let first_cluster = v_min summary
            next_min = (index tree) <$> first_cluster <*> ( v_min =<< (((V.!) clusters) <$> first_cluster))
            new_cluster = delete (low tree a) (clusters V.!  (high tree a))
            new_clusters = clusters V.// [(high tree a, new_cluster)]
         in delete' a (VEBNode u next_min maybe_max summary  new_clusters)
    | otherwise = 
        let new_cluster = delete (low tree a) (clusters V.!  (high tree a))
            new_clusters = clusters V.// [(high tree a, new_cluster)]
         in delete' a (VEBNode u (Just val_min) maybe_max summary new_clusters)

delete' a tree@(VEBNode u maybe_min maybe_max summary clusters)
    | (v_min $ clusters V.! (high tree a)) == Nothing = 
        let new_summary = delete (high tree a) summary
         in if (Just a) == maybe_max 
                then case v_max new_summary of
                    Nothing -> VEBNode u maybe_min maybe_min new_summary clusters
                    Just summary_max -> VEBNode u maybe_min (index tree summary_max <$> (v_min (clusters V.! summary_max))) new_summary clusters
                else VEBNode u maybe_min maybe_max new_summary clusters
    | Just a == maybe_max = VEBNode u maybe_min (index tree (high tree a)  <$> (v_max $ clusters V.! (high tree a))) summary clusters
    | otherwise = tree
