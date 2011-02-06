{-# LANGUAGE
    DeriveDataTypeable
  , RecordWildCards
  , NamedFieldPuns
  , ViewPatterns #-}

-- | Interface to the `udis86` disassembler.
--
-- The goal at this level of wrapping is to provide the
-- maximum feature-set from the underlying C library,
-- with the minimum of C-related headaches. Therefore,
-- this module's API is thoroughly imperative, but uses
-- Haskellish types and automatic resource management.
--
-- For a higher-level, @IO@-free API, see @'Udis86.Pure'@.

module Udis86.IO
  ( -- * Instances
    UD
  , newUD

    -- * Input sources
  , setInputBuffer
  , InputHook, endOfInput, setInputHook

    -- * Disassembly
  , disassemble, skip, setIP

    -- * Inspecting the output
  , getInstruction
  , getLength, getOffset
  , getHex, getBytes, getAssembly

    -- * Configuration
  , setWordSize
  , setSyntax
  , setVendor
  , setCallback
  ) where

import Udis86.C
import Udis86.Types

import Data.Typeable ( Typeable )
import Control.Concurrent.MVar
import Foreign
import Foreign.C.String
import Control.Applicative hiding ( Const )
import Control.Monad
import Data.Maybe

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS

-- We keep track of the current input source and
-- the current translator so that these resources
-- can be freed automatically.

data Input
  = InNone
  | InHook (FunPtr CInputHook)
  | InBuf  (ForeignPtr Word8)

data XlatType
  = XlBuiltin
  | XlCustom

data State = State
  { udPtr      :: Ptr UD_t
  , udInput    :: Input
  , udXlatType :: XlatType
  , udXlat     :: FunPtr CTranslator }

-- | Abstract type representing an instance of the disassembler.
newtype UD = UD (MVar State)
  deriving (Typeable)

setInput :: Input -> State -> IO State
setInput inpt st@State{udInput} = do
  case udInput of
    InHook fp -> freeHaskellFunPtr fp
    -- We make sure any ByteString's contents stay valid to this point
    InBuf ptr -> touchForeignPtr ptr
    _ -> return ()
  return $ st { udInput = inpt }

setXlat :: XlatType -> FunPtr CTranslator -> State -> IO State
setXlat ty fp st@State{..} = do
  case udXlatType of
    XlCustom -> freeHaskellFunPtr udXlat
    _ -> return ()
  ud_set_syntax udPtr fp
  return $ st { udXlatType = ty, udXlat = fp }

finalizeState :: MVar State -> IO ()
finalizeState s = withMVar s $ \st@State{..} -> do
  _ <- setInput InNone st
  _ <- setXlat  XlBuiltin nullFunPtr st
  free udPtr

-- | Create a new disassembler instance.
--
-- There is no @deleteUD@.  Associated resources will be freed automatically
-- when this @'UD'@ value becomes unreachable.
newUD :: IO UD
newUD = do
  p <- mallocBytes sizeof_ud_t
  ud_init p
  s <- newMVar $ State p InNone XlBuiltin nullFunPtr
  addMVarFinalizer s (finalizeState s)
  return $ UD s

withUDPtr :: UD -> (Ptr UD_t -> IO a) -> IO a
withUDPtr (UD s) f = withMVar s $ \State{udPtr} -> f udPtr

-- | A custom input source.
--
-- Each time this action is executed, it should return a single byte of
-- input, or return @'endOfInput'@ if there are no more bytes to read.
type InputHook = IO Int

-- | Special value representing the end of input.
endOfInput :: Int
endOfInput = fromIntegral ud_eoi

-- | Register an @'InputHook'@ to provide machine code to disassemble.
setInputHook :: UD -> InputHook -> IO ()
setInputHook (UD s) f = modifyMVar_ s $ \st@State{..} -> do
  fp <- c_mkInputHook (fromIntegral <$> f)
  ud_set_input_hook udPtr fp
  setInput (InHook fp) st

-- FIXME: setInputFile

-- | Disassemble machine code from a @'ByteString'@.
--
-- This does not involve copying the contents.
setInputBuffer :: UD -> BS.ByteString -> IO ()
setInputBuffer (UD s) bs = modifyMVar_ s $ \st@State{..} -> do
  let (ptr, off, len) = BS.toForeignPtr bs
  ud_set_input_buffer udPtr
    (unsafeForeignPtrToPtr ptr `plusPtr` off)
    (fromIntegral len)
  setInput (InBuf ptr) st

-- TODO: error checking

-- | Set the word size, i.e. whether to disassemble
--   16-bit, 32-bit, or 64-bit code.
setWordSize :: UD -> WordSize -> IO ()
setWordSize _ Bits8 = error "no 8-bit disassembly mode"
setWordSize s w = withUDPtr s $ flip ud_set_mode (fromIntegral $ bitsInWord w)

-- | Set the instruction pointer, i.e. the disassembler's idea of
-- where the current instruction would live in memory.
setIP :: UD -> Word64 -> IO ()
setIP s w = withUDPtr s $ flip ud_set_pc w

-- | Set the assembly syntax to be used by @'getAssembly'@.
--
-- This takes effect after the next call to @'disassemble'@.
setSyntax :: UD -> Syntax -> IO ()
setSyntax (UD s) = modifyMVar_ s . setXlat XlBuiltin . f where
  f SyntaxNone  = nullFunPtr
  f SyntaxIntel = ud_translate_intel
  f SyntaxATT   = ud_translate_att

-- no point passing the UD since we can close over it easily
-- | Register an action to be performed after each instruction is disassembled.
--
-- This disables updating of the string returned by `getAssembly`.
setCallback :: UD -> IO () -> IO ()
setCallback (UD s) act = do
  fp <- c_mkTranslator (const act)
  modifyMVar_ s $ setXlat XlCustom fp

-- | Choose an instruction set variation.
setVendor :: UD -> Vendor -> IO ()
setVendor ud = withUDPtr ud . flip ud_set_vendor . f where
  f Intel = udVendorIntel
  f AMD   = udVendorAmd

-- | Disassemble the next instruction and return its length in bytes.
--
-- Returns zero if there are no more instructions.
disassemble :: UD -> IO Word
disassemble = (fromIntegral <$>) . flip withUDPtr ud_disassemble

-- | Get the length of the current instruction in bytes.
getLength :: UD -> IO Word
getLength = (fromIntegral <$>) . flip withUDPtr ud_insn_len

-- | Get the offset of the current instruction from FIXME.
getOffset :: UD -> IO Word64
getOffset = flip withUDPtr ud_insn_off

-- | Get the current instruction's machine code as a hexadecimal string.
getHex :: UD -> IO String
getHex s = withUDPtr s $ \p ->
  ud_insn_hex p >>= peekCString

-- | Get the current instruction's machine code as a @'ByteString'@.
--
-- The bytes are copied out of internal state.
getBytes :: UD -> IO BS.ByteString
getBytes s = withUDPtr s $ \p -> do
  len <- ud_insn_len p
  ptr <- ud_insn_ptr p
  -- Int vs. CUInt overflow problems?
  BS.packCStringLen (castPtr ptr, fromIntegral len)

-- | Get the assembly syntax for the current instruction.
--
-- See also @'setSyntax'@.
getAssembly :: UD -> IO String
getAssembly s = withUDPtr s $ \p ->
  ud_insn_asm p >>= peekCString

-- | Skip the next /n/ bytes of the input.
skip :: UD -> Word -> IO ()
skip s n = withUDPtr s $ flip ud_input_skip (fromIntegral n)

getPfx :: Ptr UD_t -> IO [Prefix]
getPfx udt = catMaybes <$> mapM get allPfx where
  allPfx =
    [ (get_pfx_seg,   getSeg)
    , (get_pfx_rex,   k Rex)
    , (get_pfx_opr,   k OperSize)
    , (get_pfx_adr,   k AddrSize)
    , (get_pfx_lock,  k Lock)
    , (get_pfx_rep,   k Rep)
    , (get_pfx_repe,  k RepE)
    , (get_pfx_repne, k RepNE)
    ]
  get (retr, conv) = do
    n <- fromIntegral <$> retr udt
    return (guard (n /= udNone) >> conv n)
  getSeg (register -> RegSeg seg) = Just $ Seg seg
  getSeg _ = Nothing
  k v _ = Just v

getLval :: WordSize -> Ptr UD_operand -> IO Word64
getLval Bits8  uop = fromIntegral <$> get_lval8  uop
getLval Bits16 uop = fromIntegral <$> get_lval16 uop
getLval Bits32 uop = fromIntegral <$> get_lval32 uop
getLval Bits64 uop = get_lval64 uop

opDecode :: UDTM (Ptr UD_operand -> IO Operand)
opDecode = makeUDTM
  [ (udOpMem,   (Mem   <$>) . getMem)
  , (udOpReg,   (Reg   <$>) . getReg get_base)
  , (udOpPtr,   (Ptr   <$>) . getPtr)
  , (udOpImm,   (Imm   <$>) . getImm)
  , (udOpJimm,  (Jump  <$>) . getImm)
  , (udOpConst, (Const <$>) . getImm) ] where

    wordSize :: Word8 -> WordSize
    wordSize 8  = Bits8
    wordSize 16 = Bits16
    wordSize 32 = Bits32
    wordSize 64 = Bits64
    wordSize n  = error ("bad word size " ++ show n)

    getReg f uop = register <$> f uop

    getMem uop = do
      sz <- wordSize <$> get_size uop
      Memory
        <$> getReg get_base  uop
        <*> getReg get_index uop
        <*> get_scale uop
        <*> return sz
        <*> getLval sz uop

    getPtr uop = do
      sz <- get_size uop
      (seg, off) <- get_lval_ptr uop
      return $ case sz of
        32 -> Pointer seg Bits16 off
        48 -> Pointer seg Bits32 off
        _  -> error ("invaild pointer size " ++ show sz)

    getImm uop = do
      sz  <- wordSize <$> get_size uop
      val <- getLval sz uop
      return $ Immediate sz val

getOperands :: Ptr UD_t -> IO [Operand]
getOperands udt = catMaybes <$> mapM decode getters where
  getters = [get_operand1, get_operand2, get_operand3]
  decode f = do
    let uop = f udt
    ty  <- get_type uop
    case lookupUDTM ty opDecode of
      Just g  -> Just <$> g uop
      Nothing -> return Nothing

-- | Get the current instruction.
getInstruction :: UD -> IO Instruction
getInstruction = flip withUDPtr $ \udt ->
  Inst <$> getPfx udt <*> (opcode <$> get_mnemonic udt) <*> getOperands udt
