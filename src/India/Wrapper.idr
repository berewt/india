module India.Wrapper

import Text.Lexer

%default total

public export
interface Wrapper (0 f : Type -> Type) where
  unwrap : f a -> a

export
Wrapper TokenData where
  unwrap = tok
