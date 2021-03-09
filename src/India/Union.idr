module India.Union

import public Data.List.AtIndex
import public Data.OpenUnion
import Text.Lexer

export
(t : _) =>
Show (elt t) =>
Show (Union elt [t]) where
  show = show . decomp0

export
(t : _) =>
Show (elt t) => Show (Union elt ts) =>
Show (Union elt (t::ts)) where
  show x = case decomp x of
                Left y  => show y
                Right y => show y

export
(t : _) =>
Eq (elt t) =>
Eq (Union elt [t]) where
  (==) = (==) `on` decomp0
  (/=) = (/=) `on` decomp0

export
(t : _) =>
Eq (elt t) => Eq (Union elt ts) =>
Eq (Union elt (t::ts)) where
  (==) = (==) `on` decomp
  (/=) = (/=) `on` decomp

export
(t : a) =>
TokenKind (elt t) =>
TokenKind (Union elt [t]) where
    TokType x = TokType $ decomp0 x
    tokValue x = tokValue $ decomp0 x

export
(t : a) =>
TokenKind (elt t) => TokenKind (Union elt ts) =>
TokenKind (Union elt (t::ts)) where
  TokType x with (decomp x)
    TokType x | (Left y) = TokType y
    TokType x | (Right y) = TokType y
  tokValue x str with (decomp x)
    tokValue x str | (Left y) = tokValue y str
    tokValue x str | (Right y) = tokValue y str
