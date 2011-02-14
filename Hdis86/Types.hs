{-# LANGUAGE
    DeriveDataTypeable #-}

-- | Types provided by the disassembler.
module Hdis86.Types
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
  , Vendor(..), CPUMode(..), Syntax(..)

    -- * Common configurations
  , intel32, intel64, amd32, amd64

    -- * Opcodes
  , Opcode(..)
  ) where

import Hdis86.Internal.Opcode

import Data.Typeable ( Typeable )
import Data.Data     ( Data )
import Data.Word
import Data.Int

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

-- | An x86 \/ AMD64 register.
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

-- | An x86 \/ AMD64 CPU instruction.
--
-- The destination @'Operand'@ (if any) will precede the source
-- @'Operand'@.
data Instruction
  = Inst [Prefix] Opcode [Operand]
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | Prefixes, used to modify an instruction.
data Prefix
  = Seg Segment  -- ^ Segment override
  | Rex          -- ^ @REX@ prefix; enables certain 64-bit features
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
  { mSize   :: WordSize         -- ^ Size of the word in memory
  , mBase   :: Register         -- ^ Base register
  , mIndex  :: Register         -- ^ Index register
  , mScale  :: Word8            -- ^ Scale of index
  , mOffset :: Immediate Int64  -- ^ Displacement / offset value
  } deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | A segmented pointer operand.
data Pointer = Pointer
  { pSegment :: Word16            -- ^ Segment
  , pOffset  :: Immediate Word32  -- ^ Offset, 16 or 32 bits
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
  { cfgVendor  :: Vendor   -- ^ CPU vendor; determines the instruction set used
  , cfgCPUMode :: CPUMode  -- ^ Disassemble 16-, 32-, or 64-bit code
  , cfgSyntax  :: Syntax   -- ^ Syntax to use when generating assembly
  , cfgOrigin  :: Word64   -- ^ Address where the first instruction would live in memory
  } deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | CPU execution mode.
data CPUMode
  = Mode16  -- ^ 16-bit mode
  | Mode32  -- ^ 32-bit mode
  | Mode64  -- ^ 64-bit mode
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

intel32, intel64, amd32, amd64 :: Config
intel32 = Config Intel Mode32 SyntaxNone 0
intel64 = Config Intel Mode64 SyntaxNone 0
amd32   = Config AMD   Mode32 SyntaxNone 0
amd64   = Config AMD   Mode64 SyntaxNone 0
