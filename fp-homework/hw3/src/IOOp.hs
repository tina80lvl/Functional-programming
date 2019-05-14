module IoOp where

import Lang

data IOOp = Lang
          | Read String
          | Wrte Expr
          deriving (Show)
