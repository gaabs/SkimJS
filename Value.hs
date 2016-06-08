module Value (Value (..)) where

import Language.ECMAScript3.Syntax
import Data.Map as Map (Map, insert, lookup, union, toList, empty)

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
	| Nil
	| Function Id {-name-} [Id] {-args-} [Statement] {-body-}
	| Break
	| Continue
	| Double Double
	| Array [Value]
	| Return Value
	| Global (Map String Value)

--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show Nil = "undefined"
  show (Function (Id name) args sts) = "funcao"
  show (Double double) = show double
  show (Array arr) = show arr
  show (Global m) = show m
  --show (Return value) = show value
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
