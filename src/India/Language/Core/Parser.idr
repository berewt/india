module India.Language.Core.Parser

import Data.Bool

import Text.Lexer
import Text.Parser

import India.Language.Core.Lexer
import India.Language.Core.Types
import India.Union

export
isToken : Eq t => t -> Member t ts => IndianToken ts -> Bool
isToken x (MkToken line col (Tok kind text))
  = maybe False (x ==) $ prj kind

export
matchWithData : Eq t =>
                TokenKind t =>
                Member t ts =>
                (k : t) -> Grammar (IndianToken ts) True (TokenData (TokType k))
matchWithData k = terminal "Unexpected token" \(MkToken l c (Tok  uk v)) => do
  k' <- prj uk
  guard (k == k')
  pure $ MkToken l c $ tokValue k v

export
indianParser : TokenMap $ Token $ Union Prelude.id ts ->
               Grammar (IndianToken ts) True a ->
               String -> Either String a
indianParser lexer parser str =
  let (tokens, (_,_,"")) = lex lexer str
        | (_, (_,_,remains)) => Left $ "Cannot lex content: " ++ remains
      (Right (res, [])) = parse parser tokens
        | Right (_,remains)  => Left $ "Can't parse content, left: " ++ show (map (text . tok) remains)
        | Left (Error err _) => Left $ "Can't parse content: " ++ err
        in Right res

export
wordParser : IndianCore TokenData indian =>
             Member Lexer.Word ts =>
             Grammar (IndianToken ts) True (indian Content)
wordParser = map content $ matchWithData W

export
spaceParser : IndianCore TokenData indian =>
              Member Lexer.Space ts =>
              Grammar (IndianToken ts) True (indian Content)
spaceParser =  map (content . record {tok = " "}) $ matchWithData SP <* many (matchWithData SP)

export
newlineParser : IndianCore TokenData indian =>
                Member Newline ts =>
                Grammar (IndianToken ts) True (indian Content)
newlineParser = map (content . record {tok = "\n"}) $ matchWithData NL

export
asParagraph : {cond : Bool} ->
              (builder : b -> d) ->
              (start : Grammar (IndianToken ts) cond a) ->
              (main : Grammar (IndianToken ts) True b) ->
              (end: List  (Grammar (IndianToken ts) True c)) ->
              Grammar (IndianToken ts) True d
asParagraph builder start main end = rewrite sym (orTrueTrue cond)
  in map builder $ start *> (main <* many (choice end))


export
paragraphParser : IndianCore TokenData indian =>
                  (en : Member Space ts) =>
                  (parts : List (Grammar (IndianToken ts) True (indian Content))) ->
                  (end: List  (Grammar (IndianToken ts) True (indian Content))) ->
                  Grammar (IndianToken ts) True (indian Paragraph)
paragraphParser = asParagraph paragraph (optional $ spaceParser {indian}) . some . choice

export
simpleParser : IndianCore TokenData indian =>
               String -> Either String (indian Paragraph)
simpleParser = indianParser coreLexer $
  paragraphParser [wordParser, spaceParser] [newlineParser]

