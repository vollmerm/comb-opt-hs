-- |
-- Module  : Control.Optimization.Evaluation
-- License : BSD3
-- Copyright: [2015..2015] Michael Vollmer, Bo Joel Svensson
-- Maintainer : Michael Vollmer <vollmerm@indiana.edu>
--
-- A typeclass for describing evaluations, which are the result of
-- calling an evaluation (scoring/fitness) function.
--
-- You can do three things with an evaluation: determine if it's
-- the best evaluation, determine if it's better than another
-- evaluation, and determine if it's equivalent to another
-- evaluation.
--
module Control.Optimization.Evaluation where

{-| An evaluation is something returned from an evaluation function
    during a search. The search strategies need to know a few things
    about these results.
    Evaluations need not have a complete ordering. These functions can
    be "fuzzy" if they need to be. -}
class Evaluation a where
  {-| An evaluation being ideal means the search can stop with that result. -}
  ideal :: a -> Bool
  {-| One evaluation is an improvement on the other if it is *better* according
      to some problem-specific metric. -}
  improves :: a -> a -> Bool
  {-| One evaluation being equivalent to another means the search can treat them
      interchangably.-}
  equivalent :: a -> a -> Bool

{-| An example instance of Evaluation for Ints, where lower numbers are better
    and 0 is the best. -}
instance Evaluation Int where
  ideal 0 = True
  ideal _ = False
  improves = (<)
  equivalent = (==)

{-| Return the best evaluation in a list of evaluations. If the list contains
    the ideal evaluation, we shortcut and just return that. -}
best :: Evaluation r => [r] -> r
best [] = error "Empty list"
best rs = findBest rs id -- Using continuation passing style
  where findBest [r]    k = k r
        findBest (r:rs) k =
          if ideal r then r -- Don't call k, just return
          else findBest rs $ \r' ->
               if r' `improves` r then k r' else k r
