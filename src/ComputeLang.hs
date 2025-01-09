{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module ComputeLang where

import Data.List
import Data.List.Split
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Debug.Trace (trace)
import PointNode

type VarName = String

type ValueBody = String

newtype HaskellCode = HaskellCode String deriving (Show)

data Expression = Expression VarName ValueBody deriving (Show)

splitByFirstSpace :: String -> String -> (String, String)
splitByFirstSpace acc (a : as)
    | a == ' ' = (acc, as)
    | otherwise = splitByFirstSpace (acc ++ [a]) as
splitByFirstSpace _ _ = ("", "")

stringToExpression :: String -> Expression
stringToExpression (c : cs) = Expression varname valuebody
  where
    (varname, valuebody) = splitByFirstSpace "" (c : cs)
stringToExpression "" = Expression "" ""

sandwichInOperationStatement :: String -> String -> String
sandwichInOperationStatement opname param = "Operation " ++ opname ++ " (Collection [" ++ param ++ "])"

processBody :: String -> String
processBody = map (\x -> if x == ' ' then ',' else x)

valueBodyToNode :: ValueBody -> String
valueBodyToNode (stripPrefix "Point " -> Just body) = "Point " ++ body
valueBodyToNode (stripPrefix "Negate " -> Just body) = sandwichInOperationStatement "Negate" body
valueBodyToNode (stripPrefix "Add " -> Just body) = sandwichInOperationStatement "Add" $ processBody body
valueBodyToNode (stripPrefix "Subtract " -> Just body) = sandwichInOperationStatement "Subtract" $ processBody body
valueBodyToNode (stripPrefix "Negate " -> Just body) = sandwichInOperationStatement "Negate" $ processBody body
valueBodyToNode (stripPrefix "Zip " -> Just body) = sandwichInOperationStatement "Zip" $ processBody body
valueBodyToNode (stripPrefix "Map1 " -> Just body) = sandwichInOperationStatement "Map1" $ processBody body
valueBodyToNode (stripPrefix "Map2 " -> Just body) = sandwichInOperationStatement "Map2" $ processBody body
valueBodyToNode (stripPrefix "Collection " -> Just body) = "Collection [" ++ processBody body ++ "]"
valueBodyToNode "" = ""
valueBodyToNode theline = "error \" cannot read this line\": `" ++ theline ++ "`"

expressionToHaskellCode :: Expression -> HaskellCode
expressionToHaskellCode (Expression varname valuebody) = case bodynode of
    "" -> HaskellCode ""
    _ -> HaskellCode (varname ++ " = " ++ bodynode)
  where
    bodynode = valueBodyToNode valuebody

haskellCodeToString :: HaskellCode -> String
haskellCodeToString (HaskellCode string) = string

moduleStatement :: String
moduleStatement = "module OutputCompute where"

importStatement :: String
importStatement = "import PointNode"

beginStatement :: String
beginStatement = moduleStatement ++ "\n" ++ importStatement

type Priority = Int

type LineName = String

type FunctionName = String

type ParameterNames = [String]

data ExpandedExpression = ExpandedExpression LineName FunctionName ParameterNames deriving (Show)

expressionToExpandedExpression :: Expression -> ExpandedExpression
expressionToExpandedExpression (Expression varname valuebody) = ExpandedExpression varname functionname parameternames
  where
    (functionname : parameternames) = splitOn " " valuebody

data AnnotatedExpression = AnnotatedExpression ExpandedExpression Priority deriving (Show)

type LookUp = Map.Map String Int

thelookuptable :: LookUp
thelookuptable = Map.fromList [("Dummy", (-1 :: Int))]

determinePriority :: ExpandedExpression -> LookUp -> Priority
determinePriority (ExpandedExpression _ _ []) _ = -1
determinePriority (ExpandedExpression _ funcname parameternames) lookuptable
    | funcname == "Point" = 0
    | otherwise = foldr (max . (\x -> Map.findWithDefault 0 x lookuptable)) 0 parameternames + 1

expandedExpressionToAnnotatedExpression :: ExpandedExpression -> LookUp -> (AnnotatedExpression, LookUp)
expandedExpressionToAnnotatedExpression (ExpandedExpression varname funcname params) lookuptable = (AnnotatedExpression expr priority, newlookuptable)
  where
    expr = ExpandedExpression varname funcname params
    priority = determinePriority expr lookuptable
    newlookuptable = Map.insert varname priority lookuptable

annotatedExpressionToHaskellCode :: AnnotatedExpression -> HaskellCode
annotatedExpressionToHaskellCode (AnnotatedExpression expandedexpr _) = expressionToHaskellCode . expandedExpressionToExpression $ expandedexpr

expandedExpressionToExpression :: ExpandedExpression -> Expression
expandedExpressionToExpression (ExpandedExpression varname funcanme paramnames) = Expression varname valbody
  where
    valbody = intercalate " " (funcanme : paramnames)

sortAnnotatedExpressions :: [AnnotatedExpression] -> [AnnotatedExpression]
sortAnnotatedExpressions = sortBy sortE
  where
    sortE (AnnotatedExpression _ p1) (AnnotatedExpression _ p2) = compare p1 p2

filterAnnotatedExpressions :: [AnnotatedExpression] -> [AnnotatedExpression]
filterAnnotatedExpressions = filter (\(AnnotatedExpression _ p) -> p >= 0)

emptyAnnotatedExpression :: AnnotatedExpression
emptyAnnotatedExpression = a where (a, _) = (expandedExpressionToAnnotatedExpression . expressionToExpandedExpression . stringToExpression $ "") thelookuptable

insertEmptyLinesAfterChangeInPriority :: [AnnotatedExpression] -> [AnnotatedExpression]
insertEmptyLinesAfterChangeInPriority [] = []
insertEmptyLinesAfterChangeInPriority [a] = [a]
insertEmptyLinesAfterChangeInPriority ((AnnotatedExpression e1 p1) : (AnnotatedExpression e2 p2 : as))
    | p1 /= p2 = [AnnotatedExpression e1 p1, emptyAnnotatedExpression] ++ insertEmptyLinesAfterChangeInPriority (AnnotatedExpression e2 p2 : as)
    | otherwise = AnnotatedExpression e1 p1 : insertEmptyLinesAfterChangeInPriority (AnnotatedExpression e2 p2 : as)

data InterpretedExpression = InterpretedExpression AnnotatedExpression PointNode deriving (Show)

type LookUpOfPointNodes = Map.Map VarName PointNode

lookUpOfPointNodes :: LookUpOfPointNodes
lookUpOfPointNodes = Map.fromList [("Dummy", Dummy)]

varnameToPointNode :: LookUpOfPointNodes -> String -> PointNode
varnameToPointNode varlookup varname = case Map.lookup varname varlookup of
    Just a -> a
    Nothing -> error (varname ++ " was not found")

funcNameToPointNode :: FunctionName -> [PointNode] -> PointNode
funcNameToPointNode "Add" params = Operation Add (Collection params)
funcNameToPointNode "Subtract" params = Operation Subtract (Collection params)
funcNameToPointNode "Negate" params = Operation Negate (Collection params)
funcNameToPointNode "Map1" params = Operation Map1 (Collection params)
funcNameToPointNode "Map2" params = Operation Map2 (Collection params)
funcNameToPointNode "Zip" params = Operation Zip (Collection params)
funcNameToPointNode "Collection" params = Collection params
funcNameToPointNode "Point" [point] = point
funcNameToPointNode "Point" _ = error "compile error"
funcNameToPointNode funcname params = trace (show funcname ++ " <> " ++ show params) error "idk yet"

varnamesToPointNodes :: LookUpOfPointNodes -> [VarName] -> [PointNode]
varnamesToPointNodes varlookup = map $ varnameToPointNode varlookup

annotatedExpressionToInterpretedExpression :: AnnotatedExpression -> LookUpOfPointNodes -> InterpretedExpression
annotatedExpressionToInterpretedExpression (AnnotatedExpression (ExpandedExpression varname "Point" [a]) priority) _ =
    InterpretedExpression annotatedExpression pointNode
  where
    annotatedExpression = AnnotatedExpression (ExpandedExpression varname "Point" [a]) priority
    pointNode = Point (read a :: Float)
annotatedExpressionToInterpretedExpression (AnnotatedExpression (ExpandedExpression _ "Point" _) _) _ = error "Point should only have one parameter"
annotatedExpressionToInterpretedExpression (AnnotatedExpression (ExpandedExpression varname funcname paramnames) priority) varlookup = InterpretedExpression annotatedExpression pointNode
  where
    annotatedExpression = AnnotatedExpression (ExpandedExpression varname funcname paramnames) priority
    pointnodeParams = varnamesToPointNodes varlookup paramnames
    pointNode = funcNameToPointNode funcname pointnodeParams

addIntepretedExpressionToLookUp :: InterpretedExpression -> LookUpOfPointNodes -> LookUpOfPointNodes
addIntepretedExpressionToLookUp
    ( InterpretedExpression
            ( AnnotatedExpression
                    ( ExpandedExpression
                            varname
                            _
                            _
                        )
                    _
                )
            pointNodeRepresentation
        ) =
        Map.insert varname pointNodeRepresentation

groupStrutEqInterpretedExpressionTogether :: [InterpretedExpression] -> [[InterpretedExpression]]
groupStrutEqInterpretedExpressionTogether expressions = groupExpressions [] [] expressions []
  where
    groupExpressions :: [InterpretedExpression] -> [[InterpretedExpression]] -> [InterpretedExpression] -> [InterpretedExpression] -> [[InterpretedExpression]]
    groupExpressions [] groups [] [] = groups
    groupExpressions (representative : others) groups (candidate : candidates) rejectednodes =
        let (InterpretedExpression _ pointNodeRepresentate) = representative
            (InterpretedExpression _ pointNodeCandidate) = candidate
         in if structEq pointNodeRepresentate pointNodeCandidate
                then groupExpressions (candidate : (representative : others)) groups candidates rejectednodes
                else groupExpressions (representative : others) groups candidates (candidate : rejectednodes)
    groupExpressions curgroup groups [] rejectednodes = groupExpressions [] (curgroup : groups) rejectednodes []
    groupExpressions [] groups (candidate : candidates) rejects = groupExpressions [candidate] groups candidates rejects

regroupInterpretedExpressions :: [InterpretedExpression] -> [[[InterpretedExpression]]]
regroupInterpretedExpressions = regroup [] []
  where
    regroup :: [InterpretedExpression] -> [[[InterpretedExpression]]] -> [InterpretedExpression] -> [[[InterpretedExpression]]]
    regroup [] regrouped [] = regrouped
    regroup [] regrouped (candidate : candidates) = regroup [candidate] regrouped candidates
    regroup currentGroup regrouped [] = regrouped ++ [groupStrutEqInterpretedExpressionTogether currentGroup]
    regroup (representative : others) regrouped (candidate : candidates) =
        let InterpretedExpression (AnnotatedExpression _ prioRep) _ = representative
            InterpretedExpression (AnnotatedExpression _ prioCan) _ = candidate
         in if prioRep == prioCan
                then regroup (candidate : (representative : others)) regrouped candidates
                else regroup [] (regrouped ++ [groupStrutEqInterpretedExpressionTogether (representative : others)]) (candidate : candidates)

interpretedExpressionToHaskellCode :: InterpretedExpression -> HaskellCode
interpretedExpressionToHaskellCode (InterpretedExpression annotatedExpression _) = annotatedExpressionToHaskellCode annotatedExpression

flattenGroupedHaskellCode :: [[[HaskellCode]]] -> [HaskellCode]
flattenGroupedHaskellCode gs =
    let f1 :: [[[HaskellCode]]] -> [[HaskellCode]]
        f1 = intercalate [[HaskellCode "-- q"]]

        f2 :: [[HaskellCode]] -> [HaskellCode]
        f2 = intercalate [HaskellCode "-- p"]
     in f2 . f1 $ gs

computeToHaskell :: IO ()
computeToHaskell = do
    content <- Text.readFile "../playground/sample.compute"
    let expandedExpressions = map (expressionToExpandedExpression . stringToExpression . Text.unpack) (Text.lines content)
        (annotatedLines, _) = foldl f ([], thelookuptable) expandedExpressions
          where
            f (expressions, table) expr = (annotatedExpr : expressions, newtable)
              where
                (annotatedExpr, newtable) = expandedExpressionToAnnotatedExpression expr table

        sortedAnnotatedLines = filterAnnotatedExpressions . sortAnnotatedExpressions $ annotatedLines

        (interpretedLines, _) = foldl f ([], lookUpOfPointNodes) sortedAnnotatedLines
          where
            f :: ([InterpretedExpression], LookUpOfPointNodes) -> AnnotatedExpression -> ([InterpretedExpression], LookUpOfPointNodes)
            f (exprs, varlookup) annotatedExpression = (exprs ++ [interpretedExpression], addIntepretedExpressionToLookUp interpretedExpression varlookup)
              where
                interpretedExpression = annotatedExpressionToInterpretedExpression annotatedExpression varlookup

        -- the problem is with the grouping
        groupedInterpretedLines = regroupInterpretedExpressions interpretedLines
        groupedHaskellCode =
            let f1 :: [InterpretedExpression] -> [HaskellCode]
                f1 = map interpretedExpressionToHaskellCode
                f2 :: [[InterpretedExpression]] -> [[HaskellCode]]
                f2 = map f1
                f3 :: [[[InterpretedExpression]]] -> [[[HaskellCode]]]
                f3 = map f2
             in f3 groupedInterpretedLines

        processedLines = flattenGroupedHaskellCode groupedHaskellCode

        processedContent =
            Text.unlines . map Text.pack $
                beginStatement
                    : map haskellCodeToString processedLines

    Text.writeFile "../playground/output.hs" processedContent
    putStrLn "Success"
