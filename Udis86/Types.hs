{-# LANGUAGE
    DeriveDataTypeable #-}
module Udis86.Types
  ( UInt, WordSize(..), bitsInWord
  , Operand(..), Memory(..), Pointer(..), Immediate(..)
  , Prefix(..), Instruction(..)
  , Register(..), GPR(..), Half(..)
  , Segment(..), ControlRegister(..), DebugRegister(..)
  , MMXRegister(..), X87Register(..), XMMRegister(..)
  , Opcode(..)
  , register, opcode
  ) where

import Udis86.C
import Udis86.Opcode

import Data.Typeable ( Typeable )
import Data.Data     ( Data )
import Data.Maybe
import Data.Word
import Foreign.C.Types ( CUInt )

import qualified Data.IntMap as IM

type UInt = CUInt

data WordSize
  = Bits8
  | Bits16
  | Bits32
  | Bits64
  deriving (Eq, Ord, Show, Typeable, Data, Enum, Bounded)

bitsInWord :: WordSize -> UInt
bitsInWord Bits8  = 8
bitsInWord Bits16 = 16
bitsInWord Bits32 = 32
bitsInWord Bits64 = 64

data Register
  = RegNone
  | Reg8   GPR Half
  | Reg16  GPR
  | Reg32  GPR
  | Reg64  GPR
  | RegSeg Segment
  | RegCtl ControlRegister
  | RegDbg DebugRegister
  | RegMMX MMXRegister
  | RegX87 X87Register
  | RegXMM XMMRegister
  | RegIP
  deriving (Eq, Ord, Show, Typeable, Data)

data GPR
  = RAX | RCX | RDX | RBX
  | RSP | RBP | RSI | RDI
  | R8  | R9  | R10 | R11
  | R12 | R13 | R14 | R15
  deriving (Eq, Ord, Show, Typeable, Data, Enum, Bounded)

data Half = L | H
  deriving (Eq, Ord, Show, Typeable, Data, Enum, Bounded)

data Segment
  = ES | CS | SS | DS | FS | GS
  deriving (Eq, Ord, Show, Typeable, Data, Enum, Bounded)

data ControlRegister
  = CR0  | CR1  | CR2  | CR3
  | CR4  | CR5  | CR6  | CR7
  | CR8  | CR9  | CR10 | CR11
  | CR12 | CR13 | CR14 | CR15
  deriving (Eq, Ord, Show, Typeable, Data, Enum, Bounded)

data DebugRegister
  = DR0  | DR1  | DR2  | DR3
  | DR4  | DR5  | DR6  | DR7
  | DR8  | DR9  | DR10 | DR11
  | DR12 | DR13 | DR14 | DR15
  deriving (Eq, Ord, Show, Typeable, Data, Enum, Bounded)

data MMXRegister
  = MM0  | MM1  | MM2  | MM3
  | MM4  | MM5  | MM6  | MM7
  deriving (Eq, Ord, Show, Typeable, Data, Enum, Bounded)

data X87Register
  = ST0  | ST1  | ST2  | ST3
  | ST4  | ST5  | ST6  | ST7
  deriving (Eq, Ord, Show, Typeable, Data, Enum, Bounded)

data XMMRegister
  = XMM0  | XMM1  | XMM2  | XMM3
  | XMM4  | XMM5  | XMM6  | XMM7
  | XMM8  | XMM9  | XMM10 | XMM11
  | XMM12 | XMM13 | XMM14 | XMM15
  deriving (Eq, Ord, Show, Typeable, Data, Enum, Bounded)

data Instruction
  = Inst [Prefix] Opcode [Operand]
  deriving (Eq, Ord, Show, Typeable, Data)

data Prefix
  = Seg Segment
  | Rex
  | OperSize
  | AddrSize
  | Lock
  | Rep
  | RepE
  | RepNE
  deriving (Eq, Ord, Show, Typeable, Data)

data Operand
  = OpMem   Memory
  | OpReg   Register
  | OpPtr   Pointer
  | OpImm   Immediate
  | OpJump  Immediate
  | OpConst Immediate
  deriving (Eq, Ord, Show, Typeable, Data)

data Memory = Memory
  { mBase, mIndex :: Register
  , mScale        :: Word8
  , mOffsetSize   :: WordSize
  , mOffset       :: Word64
  } deriving (Eq, Ord, Show, Typeable, Data)

data Pointer = Pointer
  { pSegment    :: Word16
  , pOffsetSize :: WordSize
  , pOffset     :: Word32
  } deriving (Eq, Ord, Show, Typeable, Data)

data Immediate = Immediate
  { iSize  :: WordSize
  , iValue :: Word64
  } deriving (Eq, Ord, Show, Typeable, Data)

regMap :: IM.IntMap Register
regMap = IM.fromList [
    (udNone, RegNone)

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

register :: Int -> Register
register n = fromMaybe RegNone $ IM.lookup n regMap

opcode :: Int -> Opcode
opcode = toEnum
