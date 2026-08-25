-- |
-- Module      : Harmonic.Traversal.Probabilistic
-- Description : Gamma distribution sampling for weighted selection
--
-- This module implements probabilistic selection for the generative engine.
-- The "entropy" knob targets a rank in the sorted candidate list:
-- @entropy * 10@ is roughly the median index drawn, so @entropy 0.5@
-- wanders around the 5th-ranked candidate and @entropy 2@ around the
-- 20th. The gamma distribution's right skew keeps mass near the top of
-- the ranking while allowing occasional deep reaches.
--
-- == Academic Lineage
--
-- /Data Science In The Creative Process/ (South, 2018): the probabilistic
-- traversal strategy (T component of Wiggins' Creative Systems Framework).
-- The original implementation used R's @rgamma@ via inline-r; this module
-- replaces it with a pure Haskell gamma distribution from @mwc-random@.

module Harmonic.Traversal.Probabilistic
  ( gammaIndexScaledWith
  ) where

import System.Random.MWC (GenIO)
import qualified System.Random.MWC.Distributions as Dist

-- |Draw an index into a ranked candidate pool (best first, index 0).
--
-- Entropy maps to gamma shape (@shape = entropy * 10 + 0.5@), making the
-- mean drawn index approximately @entropy * 10@: entropy 0 usually (but
-- not always) picks the top candidate, entropy 1 targets around rank 10,
-- and values above 1 reach proportionally deeper. Negative entropy is
-- clamped to 0; there is no upper bound.
--
-- The shape is capped at @poolSize - 1@ so a small pool is sampled across
-- its whole range instead of having the overflow mass collapse onto its
-- worst-ranked element, and draws past the end are re-drawn (truncation
-- by rejection) rather than clamped, for the same reason. The cap only
-- ever lowers the target — a large pool never has its reach extended.
gammaIndexScaledWith :: GenIO    -- ^ Shared random generator
                     -> Double   -- ^ Entropy (>= 0; 10x the target rank)
                     -> Int      -- ^ Pool size (e.g., 30)
                     -> IO Int
gammaIndexScaledWith gen entropy poolSize = go (8 :: Int)
  where
    shape = max 0.01 (min (10 * max 0 entropy + 0.5)
                          (fromIntegral poolSize - 1))
    go tries = do
      x <- Dist.gamma shape 1.0 gen
      let idx = floor x
      if idx < poolSize then pure (max 0 idx)
        else if tries <= 1 then pure (poolSize - 1)
        else go (tries - 1)
