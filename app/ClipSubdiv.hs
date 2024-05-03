{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module ClipSubdiv where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Graphics.GL

type TriIndices = (GLuint, GLuint, GLuint)

type Index = GLuint

getCorner :: Index -> [Index] -> ([Index], TriIndices)
getCorner idx list = (filter (/= idx) list, (prev, idx, next))
  where
    prev = getBefore idx list
    next = getAfter idx list

getBefore :: Index -> [Index] -> Index
getBefore = getWithOffset (-1)

getAfter :: Index -> [Index] -> Index
getAfter = getWithOffset 1

getWithOffset :: Int -> Index -> [Index] -> Index
getWithOffset off e list = case mIdx of
  Nothing -> error "Bad index!"
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

type Priority = Int

midpoint :: EdgeDivide -> LOD -> Level -> [Point] -> Index -> Index -> Maybe Point
midpoint edgeDiv lod level verts a b =
  if lod mid > level then Just mid else Nothing
  where
    mid = edgeDiv (verts !! fromEnum a) (verts !! fromEnum b)

rotl :: [a] -> [a]
rotl [] = []
rotl (a : as) = as ++ [a]

edgePairs :: [b] -> [(b, b)]
edgePairs edgeList = zip edgeList (rotl edgeList)

rim :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> (TrisData, [GLuint], M.Map Index Priority)
rim edgeDiv lod level trisdata corners =
  foldr f (trisdata, [], M.fromList (map (,0) corners)) (edgePairs corners)
  where
    f (a, b) (tdata@(TrisData verts _), r, m) = case midpoint edgeDiv lod level verts a b of
      Nothing -> (tdata, r ++ [a], m)
      Just p -> let (tdata', i) = addPoint p tdata in (tdata', r ++ [i, a], M.adjust (+ 1) b $ M.adjust (+ 1) a m)

gRim edgeDiv lod level trisdata corners = if length indices == length corners then Nothing else Just r
  where
    r@(tdata, indices, map) = rim edgeDiv lod level trisdata corners

addPoint :: Point -> TrisData -> (TrisData, Index)
addPoint point (TrisData verts indices) = (TrisData (verts ++ [point]) indices, toEnum $ length verts)

avg (x1, y1, z1) (x2, y2, z2) = ((x1 + x2) / 2.0, (y1 + y2) / 2.0, (z1 + z2) / 2.0)

t = (TrisData [(-0.8, 0.0, 0.0), (0.4, 0.4, 0.0), (0.5, -0.6, 0.0)] [])

t2 = (TrisData [(-0.8, 0.0, 0.0), (0.0,0.8,0.0), (0.4, 0.4, 0.0), (0.5, -0.6, 0.0), (-0.7, -0.4, 0.0)] [])

t3 = (TrisData [(-0.5, -0.5, 0.0), (-0.5,0.5,0.0), (0.5, 0.5, 0.0), (0.5, -0.5, 0.0)] [])

-- l = rim avg (\(x,y,z) -> if x < 0 then 2 else 0) 0 t [0,1,2]
-- l = rim avg (const 2) 0 t [0,1,2]
-- (d, ids, m) = l
-- clipInOrder = map fst $ sortBy (\(x,y) (a,b) -> compare y b) (M.toList m)
--
-- clips = foldr g (ids, []) clipInOrder
--
-- newdata :: TrisData
-- newdata = TrisData vs (is ++ tris)
--   where
--     (TrisData vs is) = d
--     (remaining, tris) = clips

leftOfCenter (x, _, _) = if x < 0 then 3 else 1

grad (x, _, _) = toEnum $ round ((x + 1) / 2.0 * gradCoef) + 1

gradCoef = 5.0

newdata' :: TrisData
newdata' = subdivide2 avg (grad) 0 t [0, 1, 2]

newdata'' :: TrisData
newdata'' = subdivide2 avg (grad) 0 t2 [0, 1, 2, 3, 4]

leftOfCenter2 (x, _, _) = if x < 0 then 1 else 2

newdata :: TrisData
newdata = subdivide2 avg (const 1) 0 t3 [0, 1, 2, 3]
-- newdata = subdivide2 avg (grad) 0 t2 [0, 1, 2, 3, 4]

-- subdivide :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> TrisData
-- subdivide edgeDiv lod level d start
--   | length start < 3 = d
--   | otherwise = subdivide edgeDiv lod (level + 1) (TrisData vs (is ++ clipped)) remaining
--   where
--     (TrisData vs is, rimIdxs, prio) = rim edgeDiv lod level d start
--     clipOrder = map fst $ sortBy (\(_,y) (_,b) -> compare y b) (M.toList prio)
--     (remaining, clipped) = foldr clipCorner (rimIdxs, []) clipOrder

clipCorner :: Index -> ([Index], [TriIndices]) -> ([Index], [TriIndices])
clipCorner index (remaining, tris) = (remaining', tris ++ [tri])
  where
    (remaining', tri) = getCorner index remaining

subdivide2 :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> TrisData
subdivide2 edgeDiv lod level d@(TrisData vs is) start
  | length start < 3 = d
  | otherwise = case gRim edgeDiv lod level d start of
      Nothing -> TrisData vs (is ++ triangulateIrreducible start)
      Just (d', rimIdxs, prio) -> subdivide2 edgeDiv lod (level + 1) d'' remaining
        where
          clipOrder = map fst $ sortBy (\(_, y) (_, b) -> compare y b) (M.toList prio)
          (remaining, clipped) = foldr clipCorner (rimIdxs, []) clipOrder
          d'' = foldr runSub d' clipped
          runSub (i, ii, iii) tdata = subdivide2 edgeDiv lod (level + 1) tdata [i, ii, iii]

triangulateIrreducible :: [Index]  -> [TriIndices]
triangulateIrreducible [] = []
triangulateIrreducible [_] = []
triangulateIrreducible [_, _] = []
triangulateIrreducible [i, ii, iii] = [(i,ii,iii)]
triangulateIrreducible (i:ii:iii:rest) = (i,ii,iii) : triangulateIrreducible (i:iii:rest)
