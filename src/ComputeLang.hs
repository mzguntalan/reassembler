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

expressionToNode :: Expression -> HaskellCode
expressionToNode (Expression varname valuebody) = case bodynode of
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

computeToHaskell :: IO ()
computeToHaskell = do
  -- Read the file content
  content <- Text.readFile "../playground/sample.compute"

  -- Process the content through each step
  let processedContent =
        Text.unlines . map Text.pack $
          beginStatement
            : map (haskellCodeToString . expressionToNode . stringToExpression . Text.unpack) (Text.lines content)

  -- Write the processed content to the file
  Text.writeFile "../playground/output.hs" processedContent

  -- Print success message
  putStrLn "Success"

type Priority = Int

data AnnotatedPointNode = AnnotatedPointNode PointNode Priority
