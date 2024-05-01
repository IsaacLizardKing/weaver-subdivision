{-# LANGUAGE GADTs #-}

module ClipSubdiv where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Graphics.GL

type TriIndices = (GLuint, GLuint, GLuint)

type Index = GLuint

getCorner :: [Index] -> Index -> ([Index], TriIndices)
getCorner list idx = (filter (/= idx) list, (prev, idx, next))
  where
    prev = getBefore idx list
    next = getAfter idx list

getBefore :: Index -> [Index] -> Index
getBefore = getWithOffset (-1)

getAfter :: Index -> [Index] -> Index
getAfter = getWithOffset 1

getWithOffset :: Int -> Index -> [Index] -> Index
getWithOffset off e list = case mIdx of
  Nothing -> error "BLAH"
  Just idx -> list !! ((idx + off) `mod` len)
  where
    mIdx = elemIndex e list
    len = length list

data TrisData where
  TrisData :: [Point] -> [TriIndices] -> TrisData
  deriving (Show, Eq)

type LOD = Point -> Word

type Point = (GLfloat, GLfloat, GLfloat)

type EdgeDivide = Point -> Point -> Point

type Level = Word

getRim :: EdgeDivide -> LOD -> Level -> TrisData -> TriIndices -> (TrisData, [GLuint])
getRim edgeDiv lod level (TrisData verts idx) tri = (TrisData verts''' idx, rim)
  where
    (i, j, k) = tri
    c0 = superEdgeDiv edgeDiv verts i j
    c1 = superEdgeDiv edgeDiv verts j k
    c2 = superEdgeDiv edgeDiv verts k i
    (m0, verts') = (toMaybe (lod c0 >= level) (toEnum (length verts)), verts ++ [c0])
    (m1, verts'') = (toMaybe (lod c1 >= level) (toEnum (length verts')), verts' ++ [c1])
    (m2, verts''') = (toMaybe (lod c2 >= level) (toEnum (length verts'')), verts'' ++ [c2])
    rim = catMaybes [Just i, m0, Just j, m1, Just k, m2]

toMaybe :: Bool -> a -> Maybe a
toMaybe cond a = if cond then Just a else Nothing

superEdgeDiv :: EdgeDivide -> [Point] -> GLuint -> GLuint -> Point
superEdgeDiv ed verts i j = ed (verts !! fromEnum i) (verts !! fromEnum j)

superDuperEdgeDiv :: EdgeDivide -> LOD -> Level -> [Point] -> Index -> Index -> Maybe Point
superDuperEdgeDiv edgeDiv lod level verts a b =
  toMaybe (lod midpoint >= level) midpoint
  where
    midpoint = superEdgeDiv edgeDiv verts a b

rot :: [a] -> [a]
rot [] = []
rot (a : as) = as ++ [a]

edgePairs :: [b] -> [(b, b)]
edgePairs edgeList = zip edgeList (rot edgeList)

getRim2 :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> (TrisData, [GLuint])
getRim2 edgeDiv lod level trisdata edgePoints =
  foldr f (trisdata, []) (edgePairs edgePoints)
  where
    f (a, b) (tdata@(TrisData verts _), r) = case superDuperEdgeDiv edgeDiv lod level verts a b of
      Nothing -> (tdata, r ++ [a])
      Just p -> let (tdata', i) = addPoint p tdata in (tdata', r ++ [a, i])

addPoint :: Point -> TrisData -> (TrisData, Index)
addPoint point (TrisData verts indices) = (TrisData (verts ++ [point]) indices, toEnum $ length verts)
