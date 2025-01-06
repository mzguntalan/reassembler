{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module ComputeLang where

import Data.List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import PointNode (PointNode)

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

valueBodyToNode :: ValueBody -> String
valueBodyToNode (stripPrefix "Point " -> Just body) = "Point " ++ body
valueBodyToNode (stripPrefix "Negate " -> Just body) = "Operation Negate (Collection [" ++ body ++ "]"
valueBodyToNode (stripPrefix "Add " -> Just body) = "Operation Add (Collection [" ++ paramname1 ++ ", " ++ paramname2 ++ "])"
  where
    (paramname1, paramname2) = splitByFirstSpace "" body
valueBodyToNode (stripPrefix "Subtract " -> Just body) = "Operation Subtract (Collection [" ++ paramname1 ++ ", " ++ paramname2 ++ "])" where (paramname1, paramname2) = splitByFirstSpace "" body
valueBodyToNode _ = "error \" cannot read this line\""

expressionToNode :: Expression -> HaskellCode
expressionToNode (Expression varname valuebody) = HaskellCode (varname ++ " = " ++ valueBodyToNode valuebody)

haskellCodeToString :: HaskellCode -> String
haskellCodeToString (HaskellCode string) = string

importStatement :: String
importStatement = "import PointNode (BinaryOperator (Subtract), PointNode (Collection, Operation, Point))"

computeToHaskell :: IO ()
computeToHaskell =
  do
    let content <- Text.readFile "../playground/sample.compute"
      processedContent = content
    >>= (map Text.unpack . Text.lines)
    >>= map stringToExpression
    >>= map expressionToNode
    >>= map haskellCodeToString
    >>= ([importStatement] ++)
    >>= map Text.pack
    >>= Text.unlines
    Text.writeFile "../playground/output.hs"

type Priority = Int

data AnnotatedPointNode = AnnotatedPointNode PointNode Priority