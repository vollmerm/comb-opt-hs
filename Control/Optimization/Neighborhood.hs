-- |
-- Module  : Control.Optimization.Neighborhood
-- License : BSD3
-- Copyright: [2015..2015] Michael Vollmer, Bo Joel Svensson
-- Maintainer : Michael Vollmer <vollmerm@indiana.edu>
--
-- When optimizing a function, we will want to generate a set of 
-- neighbors from a particular parameter assignment. This set
-- contains adjacent parameter assignments.
--
module Control.Optimization.Neighborhood where

import Control.Optimization.Evaluation

import Data.Set (Set)
import qualified Data.Set as Set

class Neighborhood a where
  neighbors :: a -> Set a
