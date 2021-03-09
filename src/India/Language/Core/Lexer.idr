module India.Language.Core.Lexer

import Text.Lexer

import India.Union


%default total

public export
IndianToken : List Type -> Type
IndianToken = TokenData . Token . Union Prelude.id

public export
data Space = SP

export
TokenKind Space where
  TokType = const String
  tokValue = flip const

export
Eq Space where
  (==) = const $ const True

export
Show Space where
  show = const "Space"


public export
data Newline = NL

export
TokenKind Newline where
  TokType = const Unit
  tokValue = const $ const ()

export
Eq Newline where
  (==) = const $ const True

export
Show Newline where
  show p = "Newline"

public export
data Escaped = ESC Char

export
Eq Escaped where
  (==) = (==) `on` (\(ESC c) => c)

export
Show Escaped where
  show (ESC x) = "Escaped \{pack [x]}"

export
TokenKind Escaped where
  TokType = const Char
  tokValue (ESC x) = const x

public export
data Word = W

export
Eq Word where
  (==) = const $ const $ True

export
Show Word where
  show =  const "Word"

export
TokenKind Word where
  TokType = const String
  tokValue = flip const

export
word : List Lexer -> Lexer
word stops = (reject stop <+> any) <+> manyUntil stop any
  where
    stop : Lexer
    stop = some (choice stops)

export
escaped : Lexer
escaped = is '\\' <+> any

export
record LexerEntry (ts : List Type) where
  constructor Entry
  lexer : Lexer
  tok : t
  {auto member : Member t ts}

export
record IndianLexer (ts : List Type) where
  constructor MkIndianLexer
  stop   : List $ LexerEntry ts
  exact  : List $ LexerEntry ts

export
indianLexer : (Member Word ts) => IndianLexer ts -> TokenMap $ Token $ Union Prelude.id ts
indianLexer (MkIndianLexer  stop exact)
  = let stops = map lexer stop
  in toTokenMap $ (map (\x => (x.lexer, inj @{x.member} x.tok)) $ exact ++ stop)
                 ++ [(word stops, inj W)]

export
CoreLexer : Member Space ts =>
            Member Newline ts =>
            IndianLexer ts
CoreLexer = MkIndianLexer
  [Entry newlines NL, Entry spaces SP]
  []

export
coreLexer : TokenMap $ Token $ Union Prelude.id [Word, Space, Newline]
coreLexer = indianLexer CoreLexer
