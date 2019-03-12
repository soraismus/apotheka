module HaskPapers.Data.Present
  (class Present
  , present
  ) where

class Present a where
  present :: a -> String
