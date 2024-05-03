{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module ClipSubdiv (TrisData (TrisData), subdivide) where

import Data.List ( elemIndex, sortBy )
import qualified Data.Map as M
import Graphics.GL

data TrisData where
  TrisData :: [Point] -> [TriIndices] -> TrisData
  deriving (Show, Eq)

type TriIndices = (GLuint, GLuint, GLuint)

type Index = GLuint

type LOD = Point -> Word

type Point = (GLfloat, GLfloat, GLfloat)

type EdgeDivide = Point -> Point -> Point

type Level = Word

type Priority = Int

--------------------------------------------------------------------------------------
-----       -    ,  *   #     ,       ^        `      *  `   #  .   ~     +      -----
-----  )       .   -       +      Main Algo Functions   '   +      ^    *    -   -----
-----      #     ,   @   ,     -  @  .      -    ~    o  -    *   @  .     .     -----
--------------------------------------------------------------------------------------

subdivide :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> TrisData
subdivide edgeDiv lod level d@(TrisData vs is) start =
  case subdivRim edgeDiv lod level d start of
    Nothing -> TrisData vs (is ++ triangulateIrreducible start)
    Just (d', rimIdxs, prio) -> subdivide edgeDiv lod (level + 1) d'' remaining
      where
        clipOrder = map fst $ sortBy (\(_, y) (_, b) -> compare y b) (M.toList prio)
        (remaining, clipped) = foldr clipCorner (rimIdxs, []) clipOrder
        d'' = foldr runSub d' clipped
        runSub (i, ii, iii) tdata = subdivide edgeDiv lod (level + 1) tdata [i, ii, iii]

subdivRim :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> Maybe (TrisData, [GLuint], M.Map Index Priority)
subdivRim _ _ _ _ [] = Nothing
subdivRim _ _ _ _ [_] = Nothing
subdivRim _ _ _ _ [_, _] = Nothing
subdivRim edgeDiv lod level trisdata corners
  | length indices == length corners = Nothing
  | otherwise = Just r
  where
    r@(_, indices, _) = rimHelper edgeDiv lod level trisdata corners

triangulateIrreducible :: [Index] -> [TriIndices]
triangulateIrreducible [] = []
triangulateIrreducible [_] = []
triangulateIrreducible [_, _] = []
triangulateIrreducible [i, ii, iii] = [(i, ii, iii)]
triangulateIrreducible (i : ii : iii : rest) = (i, ii, iii) : triangulateIrreducible (i : iii : rest)


--------------------------------------------------------------------------------------
----- *  <: MULCH GANG :>    -       *                 SISTERS OF       . .      -----
-----   - \   4 LYFE   /  +    #   Helper Functions     THE LOAM      ^-._.-^    -----
-----  #   `-.______.-' .  *     -      -               UNITE <3    MULCH MULCH  -----
--------------------------------------------------------------------------------------

-- Corner clipping

clipCorner :: Index -> ([Index], [TriIndices]) -> ([Index], [TriIndices])
clipCorner index (remaining, tris) = (remaining', tris ++ [tri])
  where
    (remaining', tri) = getCorner index remaining

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


-- Edge list construction

rotl :: [a] -> [a]
rotl [] = []
rotl (a : as) = as ++ [a]

edgePairs :: [b] -> [(b, b)]
edgePairs edgeList = zip edgeList (rotl edgeList)


-- Main algorithm helper

rimHelper :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> (TrisData, [GLuint], M.Map Index Priority)
rimHelper edgeDiv lod level trisdata corners =
  foldr f (trisdata, [], M.fromList (map (,0) corners)) (edgePairs corners)
  where
    f (a, b) (tdata@(TrisData verts _), r, m) = case midpoint edgeDiv lod level verts a b of
      Nothing -> (tdata, r ++ [a], m)
      Just p -> let (tdata', i) = addPoint p tdata in (tdata', r ++ [i, a], M.adjust (+ 1) b $ M.adjust (+ 1) a m)

addPoint :: Point -> TrisData -> (TrisData, Index)
addPoint point (TrisData verts indices) = (TrisData (verts ++ [point]) indices, toEnum $ length verts)


-- Default Input Functions

midpoint :: EdgeDivide -> LOD -> Level -> [Point] -> Index -> Index -> Maybe Point
midpoint edgeDiv lod level verts a b =
  if lod mid > level then Just mid else Nothing
  where
    mid = edgeDiv (verts !! fromEnum a) (verts !! fromEnum b)
