module Locus where

{-
Location tree with addresses a:
- location for Int is a single cell
- location for pair is a pair of locations for components
- location for sum is a location for tag and locations for components
-}
data LocTree a 
    = LocInt Int      -- int literal
    | LocBool Bool    -- bool literal
    | LocUnit         -- unit literal
    | LocStack a      -- stack location
    | LocPair (LocTree a) (LocTree a)  -- location for a pair
    | LocSum (LocTree a) (LocTree a) (LocTree a) -- location for a sum: tag and components
    | LocUndefined  -- FIXME: rethink, add handling
    deriving (Show)

type Location = LocTree Int

stkLoc :: Int -> String
stkLoc i = "_v" ++ show i