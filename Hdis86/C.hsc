{-# LANGUAGE
    ForeignFunctionInterface
  , EmptyDataDecls #-}

-- | Bare import of the @udis86@ C library.
--
-- This module is not recommended for most users. What you see is what you get.
-- You may instead be looking for @'Hdis86.IO'@ or @'Hdis86.Pure'@.
--
-- If you want to use this module, see the @udis86@ documentation: <http://udis86.sourceforge.net>
--
-- Instruction opcodes are not enumerated here. You can convert between the
-- C-level integer codes and the type @'Hdis86.Types.Opcode'@ using the latter's
-- @'Enum'@ instance.

module Hdis86.C where

import Foreign
import Foreign.C
import Control.Monad ( liftM2 )

#include <udis86.h>

-- * The type @ud_t@

-- | Just a pointer tag, with no Haskell representation.
data UD_t

sizeof_ud_t :: Int
sizeof_ud_t = (#size ud_t)

-- * Callbacks

type CInputHook = IO CInt
type CTranslator = Ptr UD_t -> IO ()

foreign import ccall "wrapper"
  c_mkInputHook :: CInputHook -> IO (FunPtr CInputHook)

foreign import ccall "wrapper"
  c_mkTranslator :: CTranslator -> IO (FunPtr CTranslator)

ud_eoi :: CInt
ud_eoi = (#const UD_EOI)

-- * Imported functions

foreign import ccall "ud_init"
  ud_init :: Ptr UD_t -> IO ()

foreign import ccall "ud_set_input_hook"
  ud_set_input_hook :: Ptr UD_t -> FunPtr CInputHook -> IO ()

foreign import ccall "ud_set_input_buffer"
  ud_set_input_buffer :: Ptr UD_t -> Ptr CChar -> CSize -> IO ()

foreign import ccall "ud_set_input_file"
  ud_set_input_file :: Ptr UD_t -> Ptr CFile -> IO ()

foreign import ccall "ud_set_mode"
  ud_set_mode :: Ptr UD_t -> (#type uint8_t) -> IO ()

foreign import ccall "ud_set_pc"
  ud_set_pc :: Ptr UD_t -> (#type uint64_t) -> IO ()

foreign import ccall "ud_set_syntax"
  ud_set_syntax :: Ptr UD_t -> FunPtr CTranslator -> IO ()

foreign import ccall "ud_set_vendor"
  ud_set_vendor :: Ptr UD_t -> CUInt -> IO ()

foreign import ccall "ud_disassemble"
  ud_disassemble :: Ptr UD_t -> IO CUInt

foreign import ccall "ud_insn_len"
  ud_insn_len :: Ptr UD_t -> IO CUInt

foreign import ccall "ud_insn_off"
  ud_insn_off :: Ptr UD_t -> IO (#type uint64_t)

foreign import ccall "ud_insn_hex"
  ud_insn_hex :: Ptr UD_t -> IO CString

foreign import ccall "ud_insn_ptr"
  ud_insn_ptr :: Ptr UD_t -> IO (Ptr (#type uint8_t))

foreign import ccall "ud_insn_asm"
  ud_insn_asm :: Ptr UD_t -> IO CString

foreign import ccall "ud_input_skip"
  ud_input_skip :: Ptr UD_t -> CSize -> IO ()

foreign import ccall "&ud_translate_intel"
  ud_translate_intel :: FunPtr CTranslator

foreign import ccall "&ud_translate_att"
  ud_translate_att :: FunPtr CTranslator

-- * Struct accessors

get_mnemonic :: Ptr UD_t -> IO CUInt
get_mnemonic p = (#peek struct ud, mnemonic) p

-- | Another pointer tag.
data UD_operand

get_operand1, get_operand2, get_operand3 :: Ptr UD_t -> Ptr UD_operand
get_operand1 = (#ptr struct ud, operand[0])
get_operand2 = (#ptr struct ud, operand[1])
get_operand3 = (#ptr struct ud, operand[2])

get_type, get_base, get_index :: Ptr UD_operand -> IO UD_type
get_type  = (#peek struct ud_operand, type)
get_base  = (#peek struct ud_operand, base)
get_index = (#peek struct ud_operand, index)

get_size, get_offset, get_scale :: Ptr UD_operand -> IO (#type uint8_t)
get_size   = (#peek struct ud_operand, size)
get_offset = (#peek struct ud_operand, offset)
get_scale  = (#peek struct ud_operand, scale)

get_lval_u8  :: Ptr UD_operand -> IO Word8
get_lval_u8  = (#peek struct ud_operand, lval.ubyte)
get_lval_u16 :: Ptr UD_operand -> IO Word16
get_lval_u16 = (#peek struct ud_operand, lval.uword)
get_lval_u32 :: Ptr UD_operand -> IO Word32
get_lval_u32 = (#peek struct ud_operand, lval.udword)
get_lval_u64 :: Ptr UD_operand -> IO Word64
get_lval_u64 = (#peek struct ud_operand, lval.uqword)

get_lval_s8  :: Ptr UD_operand -> IO Int8
get_lval_s8  = (#peek struct ud_operand, lval.sbyte)
get_lval_s16 :: Ptr UD_operand -> IO Int16
get_lval_s16 = (#peek struct ud_operand, lval.sword)
get_lval_s32 :: Ptr UD_operand -> IO Int32
get_lval_s32 = (#peek struct ud_operand, lval.sdword)
get_lval_s64 :: Ptr UD_operand -> IO Int64
get_lval_s64 = (#peek struct ud_operand, lval.sqword)

get_lval_ptr :: Ptr UD_operand -> IO (Word16, Word32)
get_lval_ptr p = liftM2 (,) ((#peek struct ud_operand, lval.ptr.seg) p)
                            ((#peek struct ud_operand, lval.ptr.off) p)

get_pfx_rex, get_pfx_seg, get_pfx_opr, get_pfx_adr,
  get_pfx_lock, get_pfx_rep, get_pfx_repe, get_pfx_repne :: Ptr UD_t -> IO (#type uint8_t)
get_pfx_rex   = (#peek ud_t, pfx_rex)
get_pfx_seg   = (#peek ud_t, pfx_seg)
get_pfx_opr   = (#peek ud_t, pfx_opr)
get_pfx_adr   = (#peek ud_t, pfx_adr)
get_pfx_lock  = (#peek ud_t, pfx_lock)
get_pfx_rep   = (#peek ud_t, pfx_rep)
get_pfx_repe  = (#peek ud_t, pfx_repe)
get_pfx_repne = (#peek ud_t, pfx_repne)

get_pc :: Ptr UD_t -> IO (#type uint64_t)
get_pc = (#peek ud_t, pc)

-- * Enumerations

type UD_vendor = CUInt
#enum UD_vendor,, UD_VENDOR_INTEL, UD_VENDOR_AMD

type UD_type = CUInt
#{enum UD_type,,
  UD_NONE,
  UD_R_AL,      UD_R_CL,        UD_R_DL,        UD_R_BL,
  UD_R_AH,      UD_R_CH,        UD_R_DH,        UD_R_BH,
  UD_R_SPL,     UD_R_BPL,       UD_R_SIL,       UD_R_DIL,
  UD_R_R8B,     UD_R_R9B,       UD_R_R10B,      UD_R_R11B,
  UD_R_R12B,    UD_R_R13B,      UD_R_R14B,      UD_R_R15B,
  UD_R_AX,      UD_R_CX,        UD_R_DX,        UD_R_BX,
  UD_R_SP,      UD_R_BP,        UD_R_SI,        UD_R_DI,
  UD_R_R8W,     UD_R_R9W,       UD_R_R10W,      UD_R_R11W,
  UD_R_R12W,    UD_R_R13W,      UD_R_R14W,      UD_R_R15W,
  UD_R_EAX,     UD_R_ECX,       UD_R_EDX,       UD_R_EBX,
  UD_R_ESP,     UD_R_EBP,       UD_R_ESI,       UD_R_EDI,
  UD_R_R8D,     UD_R_R9D,       UD_R_R10D,      UD_R_R11D,
  UD_R_R12D,    UD_R_R13D,      UD_R_R14D,      UD_R_R15D,
  UD_R_RAX,     UD_R_RCX,       UD_R_RDX,       UD_R_RBX,
  UD_R_RSP,     UD_R_RBP,       UD_R_RSI,       UD_R_RDI,
  UD_R_R8,      UD_R_R9,        UD_R_R10,       UD_R_R11,
  UD_R_R12,     UD_R_R13,       UD_R_R14,       UD_R_R15,
  UD_R_ES,      UD_R_CS,        UD_R_SS,        UD_R_DS,
  UD_R_FS,      UD_R_GS,        
  UD_R_CR0,     UD_R_CR1,       UD_R_CR2,       UD_R_CR3,
  UD_R_CR4,     UD_R_CR5,       UD_R_CR6,       UD_R_CR7,
  UD_R_CR8,     UD_R_CR9,       UD_R_CR10,      UD_R_CR11,
  UD_R_CR12,    UD_R_CR13,      UD_R_CR14,      UD_R_CR15,
  UD_R_DR0,     UD_R_DR1,       UD_R_DR2,       UD_R_DR3,
  UD_R_DR4,     UD_R_DR5,       UD_R_DR6,       UD_R_DR7,
  UD_R_DR8,     UD_R_DR9,       UD_R_DR10,      UD_R_DR11,
  UD_R_DR12,    UD_R_DR13,      UD_R_DR14,      UD_R_DR15,
  UD_R_MM0,     UD_R_MM1,       UD_R_MM2,       UD_R_MM3,
  UD_R_MM4,     UD_R_MM5,       UD_R_MM6,       UD_R_MM7,
  UD_R_ST0,     UD_R_ST1,       UD_R_ST2,       UD_R_ST3,
  UD_R_ST4,     UD_R_ST5,       UD_R_ST6,       UD_R_ST7, 
  UD_R_XMM0,    UD_R_XMM1,      UD_R_XMM2,      UD_R_XMM3,
  UD_R_XMM4,    UD_R_XMM5,      UD_R_XMM6,      UD_R_XMM7,
  UD_R_XMM8,    UD_R_XMM9,      UD_R_XMM10,     UD_R_XMM11,
  UD_R_XMM12,   UD_R_XMM13,     UD_R_XMM14,     UD_R_XMM15,
  UD_R_RIP,
  UD_OP_REG,    UD_OP_MEM,      UD_OP_PTR,      UD_OP_IMM,
  UD_OP_JIMM,   UD_OP_CONST }
