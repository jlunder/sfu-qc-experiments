{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-unused-top-binds #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Set ((\\))
import Data.Set qualified as Set
import PebblingGame.AStar
import PebblingGame.FloydWarshall
import PebblingGame.Pebbling
import PebblingGame.TinyIrreversible
import PebblingGame.XAG

trivialPeb :: TIStates XorAndGraph
trivialPeb =
  fromComputations
    ( xagFromLists
        [ And 10 11 12,
          Xor 10 12 13
        ]
        [10, 11]
        [13]
    )

epflFig2Peb :: TIStates XorAndGraph
epflFig2Peb =
  fromComputations
    ( xagFromLists
        [ And 2 3 11,
          And 11 3 12,
          And 3 4 13,
          And 13 3 14,
          And 12 14 21,
          And 1 11 22
        ]
        [1, 2, 3, 4]
        [21, 22]
    )

epflS8Peb :: TIStates XorAndGraph
epflS8Peb =
  fromComputations
    ( xagFromLists
        [ Xor 103 105 201,
          Xor 100 106 202,
          Xor 100 103 203,
          Xor 100 105 204,
          Xor 101 102 205,
          Xor 205 107 206,
          Xor 206 103 207,
          Xor 202 201 208,
          Xor 206 100 209,
          Xor 206 106 210,
          Xor 210 204 211,
          Xor 104 208 212,
          Xor 212 105 213,
          Xor 212 101 214,
          Xor 213 107 215,
          Xor 213 205 216,
          Xor 214 203 217,
          Xor 107 217 218,
          Xor 216 217 219,
          Xor 216 204 220,
          Xor 205 217 221,
          Xor 202 221 222,
          Xor 100 221 223,
          And 208 213 224,
          And 211 215 225,
          Xor 225 224 226,
          And 207 107 227,
          Xor 227 224 228,
          And 202 221 229,
          And 210 206 230,
          Xor 230 229 231,
          And 209 218 232,
          Xor 232 229 233,
          And 203 217 234,
          And 201 219 235,
          Xor 235 234 236,
          And 204 216 237,
          Xor 237 234 238,
          Xor 226 214 239,
          Xor 228 238 240,
          Xor 231 236 241,
          Xor 233 238 242,
          Xor 239 236 243,
          Xor 240 220 244,
          Xor 241 222 245,
          Xor 242 223 246,
          Xor 243 244 247,
          And 243 245 248,
          Xor 246 248 249,
          And 247 249 250,
          Xor 250 244 251,
          Xor 245 246 252,
          Xor 244 248 253,
          And 253 252 254,
          Xor 254 246 255,
          Xor 245 255 256,
          Xor 249 255 257,
          And 246 257 258,
          Xor 258 256 259,
          Xor 249 258 260,
          And 251 260 261,
          Xor 247 261 262,
          Xor 262 259 263,
          Xor 251 255 264,
          Xor 251 262 265,
          Xor 255 259 266,
          Xor 264 263 267,
          And 266 213 268,
          And 259 215 269,
          And 255 107 270,
          And 265 221 271,
          And 262 206 272,
          And 251 218 273,
          And 264 217 274,
          And 267 219 275,
          And 263 216 276,
          And 266 208 277,
          And 259 211 278,
          And 255 207 279,
          And 265 202 280,
          And 262 210 281,
          And 251 209 282,
          And 264 203 283,
          And 267 201 284,
          And 263 204 285,
          Xor 283 284 286,
          Xor 278 286 287,
          Xor 277 287 288,
          Xor 268 270 289,
          Xor 269 268 290,
          Xor 271 272 291,
          Xor 280 289 292,
          Xor 275 291 293,
          Xor 276 292 294,
          Xor 293 294 295,
          Xor 291 290 296,
          Xor 271 273 297,
          Xor 281 286 298,
          Xor 289 297 299,
          Xor 288 296 300,
          Xor 274 293 301,
          Xor 282 295 302,
          Xor 298 299 303,
          Xor 280 303 304,
          Not 304 305,
          Xor 283 301 306,
          Xor 287 279 307,
          Xor 288 301 308,
          Xor 295 303 309,
          Not 309 310,
          Xor 299 300 311,
          Xor 300 301 312,
          Not 312 313,
          Xor 302 306 314,
          Xor 314 285 315,
          Not 315 316,
          Xor 307 302 317
        ]
        [100, 101, 102, 103, 104, 105, 106, 107]
        [300, 305, 308, 310, 311, 313, 316, 317]
    )

testPebbling :: TIStates XorAndGraph -> (TIStates XorAndGraph -> Maybe ([Vertex (TIStates XorAndGraph)], Int)) -> IO ()
testPebbling graph pathF = do
  for_ (prettyComputationGraph cmpts) putStrLn
  case pathF graph of
    Just (path, maxPebbles) -> do
      putStrLn ("Minimum pebbling found with " ++ show maxPebbles ++ " pebbles")
      putStrLn "Sequence:"
      for_
        path
        ( \v -> do
            putStrLn ("  " ++ prettyVertex graph v)
        )
    Nothing -> putStrLn "No pebbling found"
  where
    cmpts = tiComputations graph

doFloydWarshall :: (ComputationGraph d) => TIStates d -> Maybe ([Vertex (TIStates d)], Int)
doFloydWarshall graph =
  floydWarshallMinimaxShortestPath graph initV goalV (toPebbleCount graph)
  where
    initV = vertexFromResultState Set.empty
    goalV = vertexFromResultState (Set.fromList (outputs cmpts))
    cmpts = tiComputations graph

doAStarExact :: (ComputationGraph d) => Int -> TIStates d -> Maybe ([Vertex (TIStates d)], Int)
doAStarExact limit graph =
  aStarShortestPath
    (0 :: Int, 0 :: Int)
    (vertexFromResultState Set.empty)
    goalF
    edgeF
    heuristicF
    costF
    >>= (\(path, (pebbles, _)) -> Just (path, pebbles))
  where
    goalF v = goalRs == resultStateFromVertex v
    edgeF v =
      [ (edge, vertexTo graph edge)
        | edge <- edgesFrom graph v,
          toPebbleCount graph edge <= limit
      ]
    heuristicF (peb, dist) v =
      ( max peb goalRsSize,
        dist + Set.size (goalRs \\ vRs) + Set.size (vRs \\ goalRs)
      )
      where
        vRs = resultStateFromVertex v
    costF (peb, dist) _ e = (max peb (toPebbleCount graph e), dist + 1)
    goalRsSize = Set.size goalRs
    goalRs = Set.fromList (outputs cmpts)
    cmpts = tiComputations graph

doAStarSatisficing :: (ComputationGraph d) => Int -> TIStates d -> Maybe ([Vertex (TIStates d)], Int)
doAStarSatisficing limit graph =
  aStarShortestPath
    (0 :: Int, 0 :: Int)
    (vertexFromResultState Set.empty)
    goalF
    edgeF
    heuristicF
    costF
    >>= (\(path, (pebbles, _)) -> Just (path, pebbles))
  where
    goalF v = goalRs == resultStateFromVertex v
    edgeF v =
      [ (edge, vertexTo graph edge)
        | edge <- edgesFrom graph v,
          toPebbleCount graph edge <= limit
      ]
    heuristicF (peb, dist) v =
      ( max peb goalRsSize,
        dist + Set.size (goalRs \\ vRs) + Set.size (vRs \\ goalRs)
      )
      where
        vRs = resultStateFromVertex v
    costF (peb, dist) _ e = (max peb (toPebbleCount graph e), dist + 1)
    goalRsSize = Set.size goalRs
    goalRs = Set.fromList (outputs cmpts)
    cmpts = tiComputations graph

main :: IO ()
main = do
  mapM_ putStrLn (prettyStateGraph trivialPeb)
  testPebbling trivialPeb (doAStarExact 4)
  testPebbling epflFig2Peb (doAStarExact 8)
  testPebbling epflS8Peb (doAStarExact 60)
