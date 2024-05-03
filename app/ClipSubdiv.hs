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
rim edgeDiv lod level trisdata edgePoints =
  foldr f (trisdata, [], M.fromList (map (,0) edgePoints)) (edgePairs edgePoints)
  where
    f (a, b) (tdata@(TrisData verts _), r, m) = case midpoint edgeDiv lod level verts a b of
      Nothing -> (tdata, r ++ [a], m)
      Just p -> let (tdata', i) = addPoint p tdata in (tdata', r ++ [i, a], M.adjust (+ 1) b $ M.adjust (+ 1) a m)

addPoint :: Point -> TrisData -> (TrisData, Index)
addPoint point (TrisData verts indices) = (TrisData (verts ++ [point]) indices, toEnum $ length verts)



avg (x1,y1,z1) (x2,y2,z2) = ((x1+x2)/2.0,(y1+y2)/2.0,(z1+z2)/2.0)
t = (TrisData [(-0.8,0.0,0.0),(0.4,0.4,0.0),(0.5,-0.6,0.0)] [])

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

newdata :: TrisData
newdata = subdivide avg (const 2) 0 t [0,1,2]

subdivide :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> TrisData
subdivide edgeDiv lod level d start
  | length start < 3 = d
  | otherwise = subdivide edgeDiv lod (level + 1) (TrisData vs (is ++ clipped)) remaining
  where
    (TrisData vs is, rimIdxs, prio) = rim edgeDiv lod level d start
    clipOrder = map fst $ sortBy (\(_,y) (_,b) -> compare y b) (M.toList prio)
    (remaining, clipped) = foldr clipCorner (rimIdxs, []) clipOrder

clipCorner :: Index -> ([Index], [TriIndices]) -> ([Index], [TriIndices])
clipCorner index (remaining, tris) = (remaining', tris ++ [tri])
  where
    (remaining', tri) = getCorner index remaining
