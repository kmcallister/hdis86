{-# LANGUAGE
    DeriveDataTypeable #-}

-- | Types provided by the disassembler.
module Udis86.Types
  (  -- * Instructions
    Instruction(..)
  , Prefix(..)
  , Operand(..), Memory(..), Pointer(..), Immediate(..)

    -- * Registers
  , Register(..), GPR(..), Half(..)
  , Segment(..), ControlRegister(..), DebugRegister(..)
  , MMXRegister(..), X87Register(..), XMMRegister(..)

    -- * Word sizes
  , WordSize(..), wordSize, bitsInWord

    -- * Configuration
  , Config(..)
  , Vendor(..), Syntax(..)

    -- * Common configurations
  , intel32, intel64, amd32, amd64

    -- * Opcodes
  , Opcode(..)

    -- * For internal use
  , UDTM, makeUDTM, lookupUDTM
  , register, opcode
  ) where

import Udis86.C
import Udis86.Opcode

import Data.Typeable ( Typeable )
import Data.Data     ( Data )
import Data.Maybe
import Data.Word
import Data.Int

import Foreign.C.Types ( CUInt )

import qualified Data.IntMap as IM

-- | Machine word sizes.
--
-- Some fields, such as immediate operands, come in different
-- widths.  We store the equivalent integer value in a @'Word64'@,
-- along with a @'WordSize'@ to indicate the original width.
data WordSize
  = Bits0   -- ^ Field not present, value will be 0
  | Bits8
  | Bits16
  | Bits32
  | Bits48
  | Bits64
  | Bits80
  deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

-- | Convert a number of bits to a @'WordSize'@.
wordSize :: Word8 -> Maybe WordSize
wordSize 0  = Just Bits0
wordSize 8  = Just Bits8
wordSize 16 = Just Bits16
wordSize 32 = Just Bits32
wordSize 48 = Just Bits48
wordSize 64 = Just Bits64
wordSize 80 = Just Bits80
wordSize _  = Nothing

-- | Number of bits in a word of a given size.
bitsInWord :: WordSize -> Word8
bitsInWord Bits0  = 0
bitsInWord Bits8  = 8
bitsInWord Bits16 = 16
bitsInWord Bits32 = 32
bitsInWord Bits48 = 48
bitsInWord Bits64 = 64
bitsInWord Bits80 = 80

-- | An @x86@ \/ @amd64@ register.
data Register
  = RegNone                 -- ^ No register specified.
  | Reg8   GPR Half         -- ^ Either 8-bit half of the low 16 bits
                            --   of a general-purpose register
  | Reg16  GPR              -- ^ Low 16 bits of a general-purpose register
                            --   (full register in 16-bit mode)
  | Reg32  GPR              -- ^ Low 32 bits of a general-purpose register
                            --   (full register in 32-bit mode)
  | Reg64  GPR              -- ^ Full 64-bit general-purpose register
  | RegSeg Segment          -- ^ Segment register
  | RegCtl ControlRegister  -- ^ Control register
  | RegDbg DebugRegister    -- ^ Debug register
  | RegMMX MMXRegister      -- ^ MMX register
  | RegX87 X87Register      -- ^ @x87@ floating point unit register
  | RegXMM XMMRegister      -- ^ XMM register
  | RegIP                   -- ^ Instruction pointer
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | A general-purpose register.
--
-- The names are taken from the 64-bit architecture, but they map onto
-- other modes in the obvious way.
data GPR
  = RAX | RCX | RDX | RBX
  | RSP | RBP | RSI | RDI
  | R8  | R9  | R10 | R11
  | R12 | R13 | R14 | R15
  deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

-- | Indicates which half of a 16-bit register is used as an 8-bit register.
data Half
  = L  -- ^ Low or least significant 8 bits
  | H  -- ^ High or most significant 8 bits
  deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

-- | A segment register.
data Segment
  = ES | CS | SS | DS | FS | GS
  deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

-- | A control register.
data ControlRegister
  = CR0  | CR1  | CR2  | CR3
  | CR4  | CR5  | CR6  | CR7
  | CR8  | CR9  | CR10 | CR11
  | CR12 | CR13 | CR14 | CR15
  deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

-- | A debug register.
data DebugRegister
  = DR0  | DR1  | DR2  | DR3
  | DR4  | DR5  | DR6  | DR7
  | DR8  | DR9  | DR10 | DR11
  | DR12 | DR13 | DR14 | DR15
  deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

-- | An MMX register.
data MMXRegister
  = MM0  | MM1  | MM2  | MM3
  | MM4  | MM5  | MM6  | MM7
  deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

-- | An @x87@ floating-point unit register.
data X87Register
  = ST0  | ST1  | ST2  | ST3
  | ST4  | ST5  | ST6  | ST7
  deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

-- | An XMM register.
data XMMRegister
  = XMM0  | XMM1  | XMM2  | XMM3
  | XMM4  | XMM5  | XMM6  | XMM7
  | XMM8  | XMM9  | XMM10 | XMM11
  | XMM12 | XMM13 | XMM14 | XMM15
  deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

-- | An @x86@ \/ @amd64@ CPU instruction.
data Instruction
  = Inst [Prefix] Opcode [Operand]
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | Prefixes, used to modify an instruction.
data Prefix
  = Seg Segment  -- ^ Segment override
  | Rex          -- ^ `REX` prefix; enables certain 64-bit features
  | OperSize     -- ^ Operand size override
  | AddrSize     -- ^ Address size override
  | Lock         -- ^ Perform memory operations atomically
  | Rep          -- ^ Repeat
  | RepE         -- ^ Repeat while equal
  | RepNE        -- ^ Repeat while not equal
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | Instruction operands.
data Operand
  = Mem   Memory              -- ^ Memory access
  | Reg   Register            -- ^ Register
  | Ptr   Pointer             -- ^ Segmented pointer
  | Imm   (Immediate Word64)  -- ^ Immediate value
  | Jump  (Immediate Int64 )  -- ^ Immediate value, for a relative jump
  | Const (Immediate Word64)  -- ^ Constant value
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | A memory-access operand.
data Memory = Memory
  { mSize         :: WordSize  -- ^ Size of the word in memory
  , mBase         :: Register  -- ^ Base register
  , mIndex        :: Register  -- ^ Index register
  , mScale        :: Word8     -- ^ Scale of index
  , mOffsetSize   :: WordSize  -- ^ Size of displacement / offset field
  , mOffset       :: Int64     -- ^ Displacement / offset value
  } deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | A segmented pointer operand.
data Pointer = Pointer
  { pSegment    :: Word16    -- ^ Segment
  , pOffsetSize :: WordSize  -- ^ Size of the offset, 16 or 32 bits
  , pOffset     :: Word32    -- ^ Offset
  } deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | An immediate operand.
data Immediate t = Immediate
  { iSize  :: WordSize  -- ^ Size of the field
  , iValue :: t         -- ^ Immediate value, e.g @'Int64'@ or @'Word64'@
  } deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | CPU vendors, supporting slightly different instruction sets.
data Vendor
  = Intel
  | AMD
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

-- | Selection of assembly output syntax.
data Syntax
  = SyntaxNone   -- ^ Don't generate assembly syntax
  | SyntaxIntel  -- ^ Intel- / NASM-like syntax
  | SyntaxATT    -- ^ AT&T- / @gas@-like syntax
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

-- | Overall configuration of the disassembler.
data Config = Config
  { cfgVendor   :: Vendor    -- ^ CPU vendor; determines the instruction set used
  , cfgWordSize :: WordSize  -- ^ Disassemble 16-, 32-, or 64-bit code
  , cfgSyntax   :: Syntax    -- ^ Syntax to use when generating assembly
  , cfgOrigin   :: Word64    -- ^ Address where the first instruction would live in memory
  } deriving (Eq, Ord, Show, Read, Typeable, Data)

intel32, intel64, amd32, amd64 :: Config
intel32 = Config Intel Bits32 SyntaxNone 0
intel64 = Config Intel Bits64 SyntaxNone 0
amd32   = Config AMD   Bits32 SyntaxNone 0
amd64   = Config AMD   Bits64 SyntaxNone 0

newtype UDTM v = UDTM (IM.IntMap v)

makeUDTM :: [(UD_type, v)] -> UDTM v
makeUDTM = UDTM . IM.fromList . map (\(k,v) -> (fromIntegral k, v))

lookupUDTM :: UD_type -> UDTM v -> Maybe v
lookupUDTM t (UDTM m) = IM.lookup (fromIntegral t) m

regMap :: UDTM Register
regMap = makeUDTM
  [ (udNone, RegNone)

  , (udRAl,   Reg8 RAX L)
  , (udRCl,   Reg8 RCX L)
  , (udRDl,   Reg8 RDX L)
  , (udRBl,   Reg8 RBX L)
  , (udRAh,   Reg8 RAX H)
  , (udRCh,   Reg8 RCX H)
  , (udRDh,   Reg8 RDX H)
  , (udRBh,   Reg8 RBX H)
  , (udRSpl,  Reg8 RSI L)
  , (udRBpl,  Reg8 RBP L)
  , (udRSil,  Reg8 RSI L)
  , (udRDil,  Reg8 RDI L)
  , (udRR8b,  Reg8 R8  L)
  , (udRR9b,  Reg8 R9  L)
  , (udRR10b, Reg8 R10 L)
  , (udRR11b, Reg8 R11 L)
  , (udRR12b, Reg8 R12 L)
  , (udRR13b, Reg8 R13 L)
  , (udRR14b, Reg8 R14 L)
  , (udRR15b, Reg8 R15 L)

  , (udRAx,   Reg16 RAX)
  , (udRCx,   Reg16 RCX)
  , (udRDx,   Reg16 RDX)
  , (udRBx,   Reg16 RBX)
  , (udRSp,   Reg16 RSP)
  , (udRBp,   Reg16 RBP)
  , (udRSi,   Reg16 RSI)
  , (udRDi,   Reg16 RDI)
  , (udRR8w,  Reg16 R8 )
  , (udRR9w,  Reg16 R9 )
  , (udRR10w, Reg16 R10)
  , (udRR11w, Reg16 R11)
  , (udRR12w, Reg16 R12)
  , (udRR13w, Reg16 R13)
  , (udRR14w, Reg16 R14)
  , (udRR15w, Reg16 R15)

  , (udREax,  Reg32 RAX)
  , (udREcx,  Reg32 RCX)
  , (udREdx,  Reg32 RDX)
  , (udREbx,  Reg32 RBX)
  , (udREsp,  Reg32 RSP)
  , (udREbp,  Reg32 RBP)
  , (udREsi,  Reg32 RSI)
  , (udREdi,  Reg32 RDI)
  , (udRR8d,  Reg32 R8 )
  , (udRR9d,  Reg32 R9 )
  , (udRR10d, Reg32 R10)
  , (udRR11d, Reg32 R11)
  , (udRR12d, Reg32 R12)
  , (udRR13d, Reg32 R13)
  , (udRR14d, Reg32 R14)
  , (udRR15d, Reg32 R15)

  , (udRRax, Reg64 RAX)
  , (udRRcx, Reg64 RCX)
  , (udRRdx, Reg64 RDX)
  , (udRRbx, Reg64 RBX)
  , (udRRsp, Reg64 RSP)
  , (udRRbp, Reg64 RBP)
  , (udRRsi, Reg64 RSI)
  , (udRRdi, Reg64 RDI)
  , (udRR8,  Reg64 R8 )
  , (udRR9,  Reg64 R9 )
  , (udRR10, Reg64 R10)
  , (udRR11, Reg64 R11)
  , (udRR12, Reg64 R12)
  , (udRR13, Reg64 R13)
  , (udRR14, Reg64 R14)
  , (udRR15, Reg64 R15)

  , (udREs, RegSeg ES)
  , (udRCs, RegSeg CS)
  , (udRSs, RegSeg SS)
  , (udRDs, RegSeg DS)
  , (udRFs, RegSeg FS)
  , (udRGs, RegSeg GS)

  , (udRCr0,  RegCtl CR0 )
  , (udRCr1,  RegCtl CR1 )
  , (udRCr2,  RegCtl CR2 )
  , (udRCr3,  RegCtl CR3 )
  , (udRCr4,  RegCtl CR4 )
  , (udRCr5,  RegCtl CR5 )
  , (udRCr6,  RegCtl CR6 )
  , (udRCr7,  RegCtl CR7 )
  , (udRCr8,  RegCtl CR8 )
  , (udRCr9,  RegCtl CR9 )
  , (udRCr10, RegCtl CR10)
  , (udRCr11, RegCtl CR11)
  , (udRCr12, RegCtl CR12)
  , (udRCr13, RegCtl CR13)
  , (udRCr14, RegCtl CR14)
  , (udRCr15, RegCtl CR15)

  , (udRDr0,  RegDbg DR0 )
  , (udRDr1,  RegDbg DR1 )
  , (udRDr2,  RegDbg DR2 )
  , (udRDr3,  RegDbg DR3 )
  , (udRDr4,  RegDbg DR4 )
  , (udRDr5,  RegDbg DR5 )
  , (udRDr6,  RegDbg DR6 )
  , (udRDr7,  RegDbg DR7 )
  , (udRDr8,  RegDbg DR8 )
  , (udRDr9,  RegDbg DR9 )
  , (udRDr10, RegDbg DR10)
  , (udRDr11, RegDbg DR11)
  , (udRDr12, RegDbg DR12)
  , (udRDr13, RegDbg DR13)
  , (udRDr14, RegDbg DR14)
  , (udRDr15, RegDbg DR15)

  , (udRMm0,  RegMMX MM0)
  , (udRMm1,  RegMMX MM1)
  , (udRMm2,  RegMMX MM2)
  , (udRMm3,  RegMMX MM3)
  , (udRMm4,  RegMMX MM4)
  , (udRMm5,  RegMMX MM5)
  , (udRMm6,  RegMMX MM6)
  , (udRMm7,  RegMMX MM7)

  , (udRSt0,  RegX87 ST0)
  , (udRSt1,  RegX87 ST1)
  , (udRSt2,  RegX87 ST2)
  , (udRSt3,  RegX87 ST3)
  , (udRSt4,  RegX87 ST4)
  , (udRSt5,  RegX87 ST5)
  , (udRSt6,  RegX87 ST6)
  , (udRSt7,  RegX87 ST7)

  , (udRXmm0,  RegXMM XMM0 )
  , (udRXmm1,  RegXMM XMM1 )
  , (udRXmm2,  RegXMM XMM2 )
  , (udRXmm3,  RegXMM XMM3 )
  , (udRXmm4,  RegXMM XMM4 )
  , (udRXmm5,  RegXMM XMM5 )
  , (udRXmm6,  RegXMM XMM6 )
  , (udRXmm7,  RegXMM XMM7 )
  , (udRXmm8,  RegXMM XMM8 )
  , (udRXmm9,  RegXMM XMM9 )
  , (udRXmm10, RegXMM XMM10)
  , (udRXmm11, RegXMM XMM11)
  , (udRXmm12, RegXMM XMM12)
  , (udRXmm13, RegXMM XMM13)
  , (udRXmm14, RegXMM XMM14)
  , (udRXmm15, RegXMM XMM15)

  , (udRRip, RegIP)]

-- | Translate `udis86` internal register numbers.
register :: CUInt -> Register
register n = fromMaybe RegNone $ lookupUDTM n regMap

-- | Translate `udis86` internal opcode numbers.
opcode :: CUInt -> Opcode
opcode = toEnum . fromIntegral
