module India.Interpreter.AST

import India.Language.Core

import India.Union
import India.Wrapper

import Data.String

%default total

public export
record AST a where
  constructor MkAST
  content : String

export
Wrapper f => IndianCore f AST where
  content x    = MkAST $ "Content: \{show $ unwrap x}"
  paragraph xs = MkAST $ "Paragraph:" ++ (unlines $ map (("  " ++) . content) xs)
