{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XAG.MinMultSat
  ( synthesizeFromTruthTable,
  )
where

import Control.Exception (assert)
import Control.Monad
import Control.Monad.State.Strict (State, evalState, gets, modify, runState)
import Data.Bits
import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace (trace)
import SAT.MiniSat
import XAG.Graph qualified as XAG

data FormulaAlloc = FormulaAlloc {nextVar :: Int}

data XAGBuilder = XAGBuilder {xagNodesRev :: [XAG.Node], nextNodeID :: Int}

type FormulaState a = State FormulaAlloc a

type XAGState a = State XAGBuilder a

freshVars :: Int -> FormulaState [Int]
freshVars n = do
  vStart <- gets nextVar
  modify (\s -> s {nextVar = vStart + n})
  return [vStart .. vStart + n - 1]

buildConstNode :: Bool -> XAGState Int
buildConstNode val = do
  nID <- gets nextNodeID
  modify (\s -> s {xagNodesRev = XAG.Const nID val : xagNodesRev s, nextNodeID = nID + 1})
  return nID

buildXorNode :: Int -> Int -> XAGState Int
buildXorNode xID yID = do
  nID <- gets nextNodeID
  modify (\s -> s {xagNodesRev = XAG.Xor nID xID yID : xagNodesRev s, nextNodeID = nID + 1})
  return nID

-- buildAndNode :: Int -> Int -> XAGState Int
-- buildAndNode xID yID = do
--   nID <- gets nextNodeID
--   modify (\s -> s {xagNodesRev = XAG.And nID xID yID : xagNodesRev s, nextNodeID = nID + 1})
--   return nID

-- The output formulas should relate all possible assignments of input
-- variables to output values
synthesizeFromTruthTable :: Int -> Int -> [([Bool], [Bool])] -> Maybe XAG.Graph
synthesizeFromTruthTable nInputs nOutputs truthTable =
  solveMultComplexityAtLeast 0
  where
    solveMultComplexityAtLeast :: Int -> Maybe XAG.Graph
    solveMultComplexityAtLeast _ =
      case solve (trace ("Full formula: " ++ show fullFormula) fullFormula) of
        -- Found a working solution!
        Just assignments ->
          let (outputIDs :: [Int], s :: XAGBuilder) =
                runState
                  (fullXAGFunc assignments originalInputIDs)
                  -- Const 0 would be False by convention, but that's never going to be needed
                  (XAGBuilder [XAG.Const 1 True] (nInputs + 2))
           in Just $ XAG.Graph (reverse (xagNodesRev s)) originalInputIDs outputIDs
        -- Can't do, expand search?
        Nothing -> Nothing -- solveMultComplexityAtLeast (m + 1)
      where
        originalInputIDs = [2 .. nInputs + 1]

        fullFormula :: Formula Int
        fullFormula = All (concatMap (uncurry ttRowClauses) truthTable)

        ttRowClauses :: [Bool] -> [Bool] -> [Formula Int]
        ttRowClauses inputs outputs = zipWith matchExpectedOutputFml computedResultFmls outputs
          where
            matchExpectedOutputFml resultFml True = resultFml
            matchExpectedOutputFml resultFml False = Not resultFml

            computedResultFmls = fullFmlsFunc inputFmls
            inputFmls = map (\b -> if b then Yes else No) inputs

        formulasFromBools :: [Bool] -> [Formula Int]
        formulasFromBools bools = map (\b -> if b then Yes else No) bools

        (fullFmlsFunc, fullXAGFunc) =
          evalState
            ( do
                (outputFmlsFunc, outputXAGFunc) <- outputAffineFormulas nInputs nOutputs
                let formulasFunc = outputFmlsFunc
                let xagFunc = outputXAGFunc
                return (formulasFunc, xagFunc)
            )
            (FormulaAlloc 1)

-- The following formula generators produce two functions, one to construct a
-- formula that represents the parameterized output of this function, and the
-- other to construct the associated XAG node snippet.

-- When encoding a function for the SAT solve, we do not assign any variables
-- to the function output itself -- what we're solving for is the _parameters_
-- characterizing the function. For example, for an affine boolean function of
-- some set of potential inputs, the minimal parameters are a flag for each
-- potential input to say whether it's summed in, and then an extra flag for
-- whether the output is inverted (or you can omit this if one of the potential
-- inputs is constant True).

-- Since the caller doesn't really want to have to know in advance the number
-- of parameters needed by any particular formula, we use a monadic idiom to
-- handle the allocation of fresh parameter variables. That's the FormulaState.
-- Then, the returned functions implicitly carry the mapping from allocated
-- variables to the necessary parts of the structures they generate. In the
-- case of the formula function, this is logical structures generated to say
-- (to the SAT solver) how any specific set of inputs is transformed given all
-- possible parameterizations; in the case of the XAG function, the specific
-- parameters are looked up in the assignments, and that drives the generation
-- of XAG nodes to compute the function for any possible inputs. The return of
-- the formula function is the entire formula, in the case of the XAG function
-- the nodes are written into the monadic state and the return is the node IDs
-- of the output nodes. In either case it's up to the caller to keep track of
-- who else subsequently may need this information.

-- It should be clear that if you want two (potentially) _different_ affine
-- functions, you need to generate two different formula functions so that they
-- will end up with distinct parameter variables, otherwise the SAT solve will
-- be forced to give them the same parameters.

-- The expectation is that the returned formula function will be called many
-- many times, but with different literal inputs. Unfortunately there's not a
-- way to reduce this repetitiveness -- it's integral to the process. Most (but
-- not all) of the input formulas will be different combinations of "Yes" and
-- "No" literals, with each row of the truth table potentially getting a
-- distinct combination of inputs. However, some of the inputs will be the
-- outputs of prior functions, and in those cases the caller may use "Let"
-- constructs to help keep the repetitiveness down. The point is, you can
-- optimize a little by specializing the clauses output if you spot a "Yes" or
-- "No", but just don't depend on that being the only thing you encounter.

outputAffineFormulas :: Int -> Int -> FormulaState ([Formula Int] -> [Formula Int], Map Int Bool -> [Int] -> XAGState [Int])
outputAffineFormulas nInputs nOutputs = do
  affineFmlFuncXAGFuncs <- mapM affineFormula (replicate nOutputs nInputs)
  let affineFmlFuncs :: [[Formula Int] -> Formula Int]
      affineXAGFuncs :: [Map Int Bool -> [Int] -> XAGState Int]
      (affineFmlFuncs, affineXAGFuncs) = unzip affineFmlFuncXAGFuncs
  let formulasFunc :: [Formula Int] -> [Formula Int]
      formulasFunc inputFmls
        | assert (length inputFmls == nInputs) otherwise =
            map ($ inputFmls) affineFmlFuncs
  let xagFunc assignments inputIDs = do
        outputNodeIDs <- mapM (\f -> f assignments inputIDs) affineXAGFuncs
        return outputNodeIDs
  return (formulasFunc, xagFunc)

affineFormula :: Int -> FormulaState ([Formula Int] -> Formula Int, Map Int Bool -> [Int] -> XAGState Int)
affineFormula nInputs = do
  ctlVars <- freshVars nInputs
  let formulaFunc inputFmls
        | assert (length inputFmls == nInputs) otherwise =
            formulaFuncAux (filter (\(_, i) -> i /= No) (zip ctlVars inputFmls))
        where
          formulaFuncAux [] = No
          formulaFuncAux [(v, Yes)] = Var v
          formulaFuncAux [(v, fml)] = (Var v :&&: fml)
          formulaFuncAux ((v, Yes) : remain) = Var v :++: formulaFuncAux remain
          formulaFuncAux ((v, fml) : remain) = (Var v :&&: fml) :++: formulaFuncAux remain
  let xagFunc assignments inputIDs | assert (length inputIDs == nInputs) otherwise = do
        case usedInputIDs of
          [] -> buildConstNode False
          first : rest -> foldM buildXorNode first rest
        where
          usedInputIDs = ((map snd) . (filter ((assignments Map.!) . fst))) (zip ctlVars inputIDs)
  return (formulaFunc, xagFunc)

-- synthesizeFromFormulas :: [Int] -> [Formula Int] -> Int -> XAG.Graph
-- synthesizeFromFormulas inputVars outputFormulas varsStart =
-- solveMultComplexityAtLeast 0
-- where
--   solveMultComplexityAtLeast m =
--     case solve fullFormula of
--       -- Found a working solution!
--       Just assignments ->
--         let (outputIDs :: [Int], s :: XAGBuilder) =
--               runState
--                 (fullXAGFunc assignments originalInputIDs)
--                 (XAGBuilder [] (length inputVars + 1))
--          in XAG.Graph (reverse (xagNodesRev s)) originalInputIDs outputIDs
--       -- Can't do, expand search?
--       Nothing -> solveMultComplexityAtLeast (m + 1)
--     where
--       originalInputIDs = [1 .. length inputVars]
--       (fullFormula, fullXAGFunc) =
--         evalState
--           ( do
--               (equivFmls, expandedInputFmls, intermedXAGFunc) <- ofComplexityFormula m (map Var inputVars)
--               (outputEquivFmls, outputXAGFunc) <- finalAffineFormulas expandedInputFmls outputFormulas
--               let xagFunc :: Map Int Bool -> [Int] -> XAGState [Int]
--                   xagFunc assignments inputIDs = do
--                     expandedInputIDs <- intermedXAGFunc assignments inputIDs
--                     outputXAGFunc assignments expandedInputIDs
--               return (All (equivFmls ++ outputEquivFmls), xagFunc)
--           )
--           (FormulaAlloc varsStart)

--   inputs = map Var inputVars

--   finalAffineFormulas :: [Formula Int] -> [Formula Int] -> FormulaState ([Formula Int], Map Int Bool -> [Int] -> XAGState [Int])
--   finalAffineFormulas expandedInputFmls outputFmls = do
--     affineFormulaXAGFuncs <- mapM (const (affineFunctionFormula expandedInputFmls)) outputFmls
--     let (affineFmls, affineXAGFuncs) = unzip affineFormulaXAGFuncs
--     let xagFunc assignments inputIDs = do
--           outputNodeIDs <- mapM (\f -> f assignments inputIDs) affineXAGFuncs
--           return outputNodeIDs
--     return (zipWith (:<->:) affineFmls outputFmls, xagFunc)

--   -- Unlike the below (andFormula, affineFunctionFormula), this function also
--   -- returns a list of formulas for the inputs including all the intermediate
--   -- multiplicative (i.e. And) outputs, so they can be together combined by
--   -- different affine functions producing each of multiple overall outputs
--   ofComplexityFormula :: Int -> [Formula Int] -> FormulaState ([Formula Int], [Formula Int], Map Int Bool -> [Int] -> XAGState [Int])
--   ofComplexityFormula 0 inputFmls = do
--     return ([], inputFmls, (\assignments inputIDs -> return inputIDs))
--   ofComplexityFormula k inputFmls = do
--     (kLess1EquivFmls, kLess1ExpandedInputFmls, kLess1XAGFunc) <-
--       ofComplexityFormula (k - 1) inputFmls
--     (andFml, andXAGFunc) <- andFormula kLess1ExpandedInputFmls
--     andVar <- freshVar
--     let expandedInputFmls = Var andVar : inputFmls
--     let xagFunc assignments inputIDs = do
--           kLess1IDs <- kLess1XAGFunc assignments inputIDs
--           -- returned IDs and also the expanded IDs put into andXAGFunc here
--           -- should match the order in the ofComplexityFormula return and the
--           -- call to andFormula above, respectively
--           andID <- andXAGFunc assignments kLess1IDs
--           return (andID : kLess1IDs)
--     return ((andFml :<->: Var andVar) : kLess1EquivFmls, Var andVar : kLess1ExpandedInputFmls, xagFunc)

--   andFormula :: [Formula Int] -> FormulaState (Formula Int, Map Int Bool -> [Int] -> XAGState Int)
--   andFormula inputFmls = do
--     (leftFormula, leftXAGFunc) <- affineFunctionFormula inputFmls
--     (rightFormula, rightXAGFunc) <- affineFunctionFormula inputFmls
--     let xagFunc assignments inputIDs = do
--           leftNodeID <- leftXAGFunc assignments inputIDs
--           rightNodeID <- rightXAGFunc assignments inputIDs
--           andID <- buildAndNode leftNodeID rightNodeID
--           return andID
--     return (leftFormula :&&: rightFormula, xagFunc)

-- truthTableFormula :: (Ord v) => Formula v -> [Bool] -> [Formula v] -> Formula v
-- truthTableFormula outputVar truthTable inputVars =
--   All (zipWith rowClause [0 ..] truthTable)
--   where
--     rowClause i True = All (bitsToFormulas i inputVars) :->: outputVar
--     rowClause i False = All (bitsToFormulas i inputVars) :->: Not outputVar

--     bitsToFormulas :: Int -> [Formula v] -> [Formula v]
--     bitsToFormulas i vars =
--       zipWith (\t v -> if i .&. t /= 0 then v else Not v) [1 `shiftL` k | k <- [0 ..]] vars
