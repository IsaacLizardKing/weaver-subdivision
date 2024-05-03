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

-- | Subdivides any polygon
--
-- Takes in:
-- A function that can take in an edge (2 points), and returns a third point: edgeDiv
-- A function that takes in a point, and returns a whole number level of subdivision: lod
-- The current level of subidivision: level
-- The triangle data: TrisData bundles vertices and indices which are needed for rendering
-- A ring of indices which form the frame on which to subdivide: start
--
-- Returns:
-- Another TrisData with the subdivided render data
--
-- Algorithm:
-- First, `start` gets midpoints added to its edges with edgeDiv, 
-- if the level of detail function says they should exist.
-- Then, the corners of this rim are clipped off, 
-- starting with corner next to the most subdivided edges.
-- A corner is centered on one of the original `start` vertices
-- in order of which has the most midpoints next to it.
-- Once we have this list corners triangles, we recurse on these,
-- increasing the level by 1. Sometimes, after clipping all corners,
-- there is a polygon still leftover in the middle, and we recurse on this.
--
-- The base case of all this recursion is when we find an "irreducible"
-- shape. An irreducible shape is anything we couldn't subdivide the edges of
-- This can happen either when the `level` is higher than what the LOD
-- function yeilds, or if we have less than 3 vertices.
-- Once we find an irriducable shape, we triangulate it, then return.
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

-- | Takes in the points of a polygon, and attempts to place midpoints
-- on its edges, if the the level of detail function permits this
-- If it can't place any midpoints, or generate a polygon, it returns Nothing
-- Otherwise, it returns the rim, with midpoints included
subdivRim :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> Maybe (TrisData, [GLuint], M.Map Index Priority)
subdivRim _ _ _ _ [] = Nothing
subdivRim _ _ _ _ [_] = Nothing
subdivRim _ _ _ _ [_, _] = Nothing
subdivRim edgeDiv lod level trisdata corners
  | length indices == length corners = Nothing
  | otherwise = Just r
  where
    r@(_, indices, _) = rimHelper edgeDiv lod level trisdata corners

-- | Takes in a polygon, as given by indices,
-- and returns a list of triangles that when
-- taken together, form this polygon
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

-- | Removes the central point of a corner from the rim,
-- returning a tuple with the new rim, and the triangle found around this central point
-- This can be seen as clipping a triangle including the center point off, by cutting a line through
-- the two surrounding points
clipCorner :: Index -> ([Index], [TriIndices]) -> ([Index], [TriIndices])
clipCorner index (remaining, tris) = (remaining', tris ++ [tri])
  where
    (remaining', tri) = getCorner index remaining

-- | Gives the three indices that make a corner, around a given center index
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

-- | Rotate left
rotl :: [a] -> [a]
rotl [] = []
rotl (a : as) = as ++ [a]

edgePairs :: [b] -> [(b, b)]
edgePairs edgeList = zip edgeList (rotl edgeList)


-- Main algorithm helper

-- | Loops through the edges of a polygon, subdividing when appropriate,
-- and adding the subdivided edges to TrisData
-- Returns the new TrisData, the new rim including subdivisions, 
-- and a map from the original indices of the polygon passed in,
-- to the number of subdivisons surrounding this index (this can be 0, 1, or 2)
rimHelper :: EdgeDivide -> LOD -> Level -> TrisData -> [Index] -> (TrisData, [GLuint], M.Map Index Priority)
rimHelper edgeDiv lod level trisdata corners =
  foldr f (trisdata, [], M.fromList (map (,0) corners)) (edgePairs corners)
  where
    f (a, b) (tdata@(TrisData verts _), r, m) = case midpoint edgeDiv lod level verts a b of
      Nothing -> (tdata, r ++ [a], m)
      Just p -> let (tdata', i) = addPoint p tdata in (tdata', r ++ [i, a], M.adjust (+ 1) b $ M.adjust (+ 1) a m)

-- | Adds a single vertex to the TrisData vertex list, and returns the new
-- TrisData, along with the index of this vertex in the vertex list
addPoint :: Point -> TrisData -> (TrisData, Index)
addPoint point (TrisData verts indices) = (TrisData (verts ++ [point]) indices, toEnum $ length verts)

-- | Subdivides an edge by looking up vertices and passing them to edgeDiv,
-- when appropriate
midpoint :: EdgeDivide -> LOD -> Level -> [Point] -> Index -> Index -> Maybe Point
midpoint edgeDiv lod level verts a b =
  if lod mid > level then Just mid else Nothing
  where
    mid = edgeDiv (verts !! fromEnum a) (verts !! fromEnum b)
