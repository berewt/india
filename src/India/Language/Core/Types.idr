module India.Language.Core.Types

%default total

export
data Paragraph : Type where

export
data Content : Type where

public export
interface IndianCore (0 decorator : Type -> Type)
                     (0 indian : Type -> Type) | indian where

    content : decorator String -> indian Content
    paragraph : List (indian Content) -> indian Paragraph
