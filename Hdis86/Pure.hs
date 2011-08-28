-- | Interface to the @udis86@ disassembler.
--
-- This is the simplest, most high-level interface.
--
-- See "Hdis86.IO" if you need more control or performance.
module Hdis86.Pure
  ( -- * Simple disassembly
    disassemble

    -- * Disassembly with full metadata
  , disassembleMetadata
  , Metadata(..)
  ) where

import Hdis86.Types
import Hdis86.IO ( UD )
import qualified Hdis86.IO as I

import System.IO.Unsafe ( unsafePerformIO )

import qualified Data.ByteString as BS

-- internal only; this isn't referentially transparent in general
disWith :: (UD -> IO a) -> Config -> BS.ByteString -> [a]
disWith f cfg bs = unsafePerformIO $ do
  ud <- I.newUD
  I.setInputBuffer ud bs
  I.setConfig      ud cfg
  I.unsafeRunLazy  ud (f ud)

-- | Disassemble machine code.
--
-- Common values for @'Config'@ such as @'intel32'@ or @'amd64'@
-- are provided in "Hdis86.Types".
--
-- The output is produced lazily.
disassemble :: Config -> BS.ByteString -> [Instruction]
disassemble = disWith I.getInstruction

-- | Disassemble machine code, with full metadata.
--
-- The output is produced lazily.
disassembleMetadata :: Config -> BS.ByteString -> [Metadata]
disassembleMetadata = disWith I.getMetadata
