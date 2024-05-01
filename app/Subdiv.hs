{-# LANGUAGE GADTs #-}

module Subdiv () where

import Data.Maybe (catMaybes)
import Graphics.GL

type Tri = (GLuint, GLuint, GLuint)

data TrisData where
  TrisData :: [Point] -> [Tri] -> TrisData
  deriving (Show, Eq)

type LOD = Point -> Word

type Point = (GLfloat, GLfloat, GLfloat)

type EdgeDivide = Point -> Point -> Point

type Level = Word

-- subdiv :: EdgeDivide -> LOD -> Level -> TrisData -> Tri -> TrisData
-- subdiv edgeDiv lod level tdata start = _
--   where
--     (tdata', rim) = getRim edgeDiv lod level tdata start
--     corners = filter _ (genCorners rim) -- TODO(colin): filter out the degenerate case

getRim :: EdgeDivide -> LOD -> Level -> TrisData -> Tri -> (TrisData, [GLuint])
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

genCorners :: [a] -> [[a]]
genCorners as = take 3 $ genCornersHelper $ cycle as

genCornersHelper :: [a] -> [[a]]
genCornersHelper as = take 3 as : genCorners (drop 2 as) --TODO(colin): is this good? does this work, considering residuals
