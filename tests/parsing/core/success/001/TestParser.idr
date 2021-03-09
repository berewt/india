module TestParser

import India.Interpreter.AST
import India.Language.Core

import India
import India.Wrapper
import India.Union
import Text.Lexer
import Text.Parser

myTest : IO ()
myTest = do
  Right res <- parseFileWith myParser "Content.in"
    | Left err => putStrLn "Test failure: \{err}"
  putStrLn res.content
  where
    myParser : String -> Either String (AST Paragraph)
    myParser = simpleParser
