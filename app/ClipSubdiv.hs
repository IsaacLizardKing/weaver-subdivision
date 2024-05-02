{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module ClipSubdiv () where

import Data.List (elemIndex, sortBy)
import Data.Word
import qualified Data.Map as M
import Data.Maybe
import Graphics.GL

--  type GLuint = Word32
--  type GLfloat = Float

type TriIndices = (GLuint, GLuint, GLuint)

type Index = GLuint

data TrisData where
  TrisData :: [Point] -> [TriIndices] -> TrisData
  deriving (Show, Eq)

type LOD = Point -> Word

type Point = (GLfloat, GLfloat, GLfloat)

type EdgeDivide = Point -> Point -> Point

type Priority = Int

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
superDuperEdgeDiv edgeDiv lod lvl v a b = (\m -> toMaybe (lod m > lvl) m) $ superEdgeDiv edgeDiv v a b

addPoint :: Point -> TrisData -> (TrisData, Index)
addPoint point (TrisData verts indices) = (TrisData (verts ++ [point]) indices, toEnum $ length verts)

-- This function is super important and also abominably unreadable
getRim2 :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> (TrisData, [GLuint], M.Map Index Priority)
getRim2 edgeDiv lod lvl trisdata edgePoints = foldr f (trisdata, [], M.fromList (map (,0) edgePoints)) (edgePairs edgePoints)
  where
    f (a, b) (tdata@(TrisData verts _), r, m) = case superDuperEdgeDiv edgeDiv lod lvl verts a b of
      Nothing -> (tdata, r ++ [a], m)
      Just p -> 
        let (tdata', i) = addPoint p tdata in 
          (tdata', r ++ [i, a], (M.adjust (+ 1) b (M.adjust (+ 1) a m)))

-- mayhaps try decomposing into smaller functions?



--------------------------------------------------------------------------------------
-----       -    ,  *   #     ,       ^        `      *  `   #  .   ~     +      -----
-----  )       .   -       +    *  Utility Functions    '   +      ^    *    -   -----
-----      #     ,   @   ,     -  @  .      -    ~    o  -    *   @  .     .     -----
--------------------------------------------------------------------------------------


getCorner :: Index -> [Index] -> ([Index], TriIndices)
getCorner idx list = (filter (/= idx) list, (getBefore idx list, idx, getAfter idx list))

getBefore :: Index -> [Index] -> Index
getBefore = getWithOffset (-1)

getAfter :: Index -> [Index] -> Index
getAfter = getWithOffset 1

getWithOffset :: Int -> Index -> [Index] -> Index
getWithOffset off e list = case elemIndex e list of
  Nothing -> error "BLAH"
  Just idx -> list !! ((idx + off) `mod` length list)

rot :: [a] -> [a]
rot [] = []
rot (a : as) = as ++ [a]

edgePairs :: [b] -> [(b, b)]
edgePairs edgeList = zip edgeList (rot edgeList)

thd :: (a, b, c) -> c
thd (_, _, c) = c

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

fst' :: (a, b, c) -> a
fst' (a, _, _) = a



--------------------------------------------------------------------------------------
-----    <: MULCH GANG :>    -       *                 SISTERS OF       . .      -----
-----     \   4 LYFE   /  +    #   Testing Functions    THE LOAM      ^-._,-^    -----
-----  #   `-.______.-' .  *     -      -               UNITE <3    MULCH MULCH  -----
--------------------------------------------------------------------------------------

l :: (TrisData, [GLuint], M.Map Index Priority)
l = getRim2 average testLOD 0 testPoints [0,1,2]

clipInOrder :: [Index]
clipInOrder = map fst $ sortBy priority (M.toList (thd l))

clips :: ([Index], [TriIndices])
clips = foldr g (snd' l, []) clipInOrder

g :: Index -> ([Index], [TriIndices]) -> ([Index], [TriIndices])
g index (indices, tris) = (\(i, tri) -> (i, tris ++ [tri])) $ getCorner index indices

average :: Point -> Point -> Point
average (x1,y1,z1) (x2,y2,z2) = ((x1 + x2) / 2.0, (y1 + y2) / 2.0, (z1 + z2) / 2.0)

testLOD :: LOD
testLOD (x, y, z) = 2

testPoints :: TrisData
testPoints = TrisData [(1.0, 0.0, 0.0), (-1.0, 1.0, 0.0), (-1.0, -1.0, 0.0)] []

priority :: (Index, Priority) -> (Index, Priority) -> Ordering
priority (_, x) (_, y) = compare x y