-- | Interface to the @udis86@ disassembler.
--
-- Exports the simplest, most high-level interface.
module Hdis86
  ( disassemble
  , disassembleMetadata
  , module Hdis86.Types
  ) where

import Hdis86.Types
import Hdis86.Pure
