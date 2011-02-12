{-# LANGUAGE
    DeriveDataTypeable
  , RecordWildCards
  , NamedFieldPuns
  , ViewPatterns #-}

-- | Interface to the @udis86@ disassembler.
--
-- The goal at this level of wrapping is to provide the
-- maximum feature-set from the underlying C library,
-- with the minimum of C-related headaches. Therefore,
-- this module's API is thoroughly imperative, but uses
-- Haskellish types and automatic resource management.
--
-- For a higher-level, @IO@-free API, see @'Hdis86.Pure'@.
--
-- This module is fully thread-safe: any number of threads
-- may manipulate one or several @'UD'@ objects at the same
-- time.

module Hdis86.IO
  ( -- * Instances
    UD
  , newUD
  , disassemble

    -- * Input sources
  , setInputBuffer
  , InputHook, setInputHook

    -- * Disassembly
  , advance, skip, setIP

    -- * Inspecting the output
  , getInstruction
  , getLength, getOffset
  , getHex, getBytes, getAssembly

    -- * Configuration
  , setConfig
  , setVendor, setCPUMode, setSyntax

    -- * Callbacks
  , setCallback
  ) where

import qualified Hdis86.C as C
import Hdis86.Types
import Hdis86.Internal.Map

import Data.Typeable ( Typeable )
import Control.Concurrent.MVar
  ( MVar, newMVar, withMVar, modifyMVar_, addMVarFinalizer )
import Foreign
import Foreign.C.String
import Control.Applicative hiding ( Const )
import Control.Monad
import Data.Maybe
import Data.Function

import qualified Data.ByteString          as BS
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Internal as BS

-- We keep track of the current input source and
-- the current translator so that these resources
-- can be freed automatically.

data Input
  = InNone
  | InHook (FunPtr C.InputHook)
  | InBuf  (ForeignPtr Word8)

data XlatType
  = XlBuiltin
  | XlCustom

data State = State
  { udPtr      :: Ptr C.UD_t
  , udInput    :: Input
  , udXlatType :: XlatType
  , udXlat     :: FunPtr C.Translator }

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

setXlat :: XlatType -> FunPtr C.Translator -> State -> IO State
setXlat ty fp st@State{..} = do
  case udXlatType of
    XlCustom -> freeHaskellFunPtr udXlat
    _ -> return ()
  C.set_syntax udPtr fp
  return $ st { udXlatType = ty, udXlat = fp }

finalizeState :: MVar State -> IO ()
finalizeState = flip withMVar $ \st@State{..} -> do
  _ <- setInput InNone st
  _ <- setXlat  XlBuiltin nullFunPtr st
  free udPtr

-- | Create a new disassembler instance.
--
-- There is no @deleteUD@.  Associated resources will be freed automatically
-- when this @'UD'@ value becomes unreachable.
newUD :: IO UD
newUD = do
  p <- mallocBytes C.sizeof_ud_t
  C.init p
  s <- newMVar $ State p InNone XlBuiltin nullFunPtr
  addMVarFinalizer s (finalizeState s)
  return $ UD s

withUDPtr :: UD -> (Ptr C.UD_t -> IO a) -> IO a
withUDPtr (UD s) f = withMVar s $ \State{udPtr} -> f udPtr

-- | A custom input source.
--
-- Each time this action is executed, it should return a single byte of
-- input, or @'Nothing'@ if there are no more bytes to read.
type InputHook = IO (Maybe Word8)

-- | Register an @'InputHook'@ to provide machine code to disassemble.
setInputHook :: UD -> InputHook -> IO ()
setInputHook (UD s) f = modifyMVar_ s $ \st@State{..} -> do
  fp <- C.wrap_InputHook (maybe C.eoi fromIntegral <$> f)
  C.set_input_hook udPtr fp
  setInput (InHook fp) st

-- FIXME: setInputFile

-- | Set up the @'UD'@ instance to read machine code from a @'ByteString'@.
--
-- This library does not copy the contents of the @'ByteString'@.
-- It will hold onto the value until another input source is selected,
-- or until the @'UD'@ value becomes unreachable.
setInputBuffer :: UD -> ByteString -> IO ()
setInputBuffer (UD s) bs = modifyMVar_ s $ \st@State{..} -> do
  let (ptr, off, len) = BS.toForeignPtr bs
  C.set_input_buffer udPtr
    (unsafeForeignPtrToPtr ptr `plusPtr` off)
    (fromIntegral len)
  setInput (InBuf ptr) st

-- TODO: error checking

-- | Set the CPU mode, i.e. 16-bit, 32-bit, or 64-bit.
setCPUMode :: UD -> CPUMode -> IO ()
setCPUMode s = withUDPtr s . flip C.set_mode . f where
  f Mode16 = 16
  f Mode32 = 32
  f Mode64 = 64

-- | Set the instruction pointer, i.e. the disassembler's idea of
-- where the current instruction would live in memory.
setIP :: UD -> Word64 -> IO ()
setIP s w = withUDPtr s $ flip C.set_pc w

-- | Set the assembly syntax to be used by @'getAssembly'@.
--
-- This takes effect after the next call to @'disassemble'@.
setSyntax :: UD -> Syntax -> IO ()
setSyntax (UD s) = modifyMVar_ s . setXlat XlBuiltin . f where
  f SyntaxNone  = nullFunPtr
  f SyntaxIntel = C.translate_intel
  f SyntaxATT   = C.translate_att

-- no point passing the UD since we can close over it easily
-- | Register an action to be performed after each instruction is disassembled.
--
-- This disables updating of the string returned by @'getAssembly'@.
setCallback :: UD -> IO () -> IO ()
setCallback (UD s) act = do
  fp <- C.wrap_Translator (const act)
  modifyMVar_ s $ setXlat XlCustom fp

-- | Choose an instruction set variation.
setVendor :: UD -> Vendor -> IO ()
setVendor ud = withUDPtr ud . flip C.set_vendor . f where
  f Intel = C.udVendorIntel
  f AMD   = C.udVendorAmd

-- | Set an overall configuration.
--
-- Calls each of @'setVendor'@, @'setCPUMode'@, @'setSyntax'@, @'setIP'@.
setConfig :: UD -> Config -> IO ()
setConfig ud Config{..} = do
  setVendor  ud cfgVendor
  setCPUMode ud cfgCPUMode
  setSyntax  ud cfgSyntax
  setIP      ud cfgOrigin

-- | Disassemble the next instruction and return its length in bytes.
--
-- Returns zero if there are no more instructions.
advance :: UD -> IO Word
advance = (fromIntegral <$>) . flip withUDPtr C.disassemble

-- | A convenience function which disassembles an entire
-- @'ByteString'@ using a fresh @'UD'@ instance.
--
-- At each instruction the user-specified action is performed,
-- and the results are collected.
disassemble :: (UD -> IO a) -> Config -> ByteString -> IO [a]
disassemble get cfg bs = do
  ud <- newUD
  setInputBuffer ud bs
  setConfig      ud cfg
  fix $ \loop -> do
    n <- advance ud
    if n > 0
      then liftA2 (:) (get ud) loop
      else return []

-- | Get the length of the current instruction in bytes.
getLength :: UD -> IO Word
getLength = (fromIntegral <$>) . flip withUDPtr C.insn_len

-- | Get the offset of the current instruction from FIXME.
getOffset :: UD -> IO Word64
getOffset = flip withUDPtr C.insn_off

-- | Get the current instruction's machine code as a hexadecimal string.
getHex :: UD -> IO String
getHex = flip withUDPtr $ \p ->
  C.insn_hex p >>= peekCString

-- | Get the current instruction's machine code as a @'ByteString'@.
--
-- The bytes are copied out of internal state.
getBytes :: UD -> IO ByteString
getBytes = flip withUDPtr $ \p -> do
  len <- C.insn_len p
  ptr <- C.insn_ptr p
  -- Int vs. CUInt overflow problems?
  BS.packCStringLen (castPtr ptr, fromIntegral len)

-- | Get the assembly syntax for the current instruction.
--
-- See also @'setSyntax'@.
getAssembly :: UD -> IO String
getAssembly = flip withUDPtr $ \p ->
  C.insn_asm p >>= peekCString

-- | Skip the next /n/ bytes of the input.
skip :: UD -> Word -> IO ()
skip s n = withUDPtr s $ flip C.input_skip (fromIntegral n)

getPfx :: Ptr C.UD_t -> IO [Prefix]
getPfx udt = catMaybes <$> mapM get allPfx where
  allPfx =
    [ (C.get_pfx_seg,   getSeg)
    , (C.get_pfx_rex,   k Rex)
    , (C.get_pfx_opr,   k OperSize)
    , (C.get_pfx_adr,   k AddrSize)
    , (C.get_pfx_lock,  k Lock)
    , (C.get_pfx_rep,   k Rep)
    , (C.get_pfx_repe,  k RepE)
    , (C.get_pfx_repne, k RepNE)
    ]
  get (retr, conv) = do
    n <- fromIntegral <$> retr udt
    return (guard (n /= C.udNone) >> conv n)
  getSeg (register -> RegSeg seg) = Just $ Seg seg
  getSeg _ = Nothing
  k v _ = Just v

getLvalU :: WordSize -> Ptr C.Operand -> IO Word64
getLvalU Bits0  _   = return 0
getLvalU Bits8  uop = fromIntegral <$> C.get_lval_u8  uop
getLvalU Bits16 uop = fromIntegral <$> C.get_lval_u16 uop
getLvalU Bits32 uop = fromIntegral <$> C.get_lval_u32 uop
getLvalU _      uop = C.get_lval_u64 uop

getLvalS :: WordSize -> Ptr C.Operand -> IO Int64
getLvalS Bits0  _   = return 0
getLvalS Bits8  uop = fromIntegral <$> C.get_lval_s8  uop
getLvalS Bits16 uop = fromIntegral <$> C.get_lval_s16 uop
getLvalS Bits32 uop = fromIntegral <$> C.get_lval_s32 uop
getLvalS _      uop = C.get_lval_s64 uop

opDecode :: UDTM (Ptr C.Operand -> IO Operand)
opDecode = makeUDTM
  [ (C.udOpMem,   (Mem   <$>) . getMem)
  , (C.udOpReg,   (Reg   <$>) . getReg C.get_base)
  , (C.udOpPtr,   (Ptr   <$>) . getPtr)
  , (C.udOpImm,   (Imm   <$>) . getImm getLvalU)
  , (C.udOpJimm,  (Jump  <$>) . getImm getLvalS)
  , (C.udOpConst, (Const <$>) . getImm getLvalU) ] where

    getReg f uop = register <$> f uop

    getMem uop = do
      -- These uses of fromJust are safe unless the
      -- C library is giving us bad data.
      Just sz <- wordSize <$> C.get_offset uop
      off <- Immediate sz <$> getLvalS sz uop
      Memory
        <$> ((fromJust . wordSize) <$> C.get_size uop)
        <*> getReg C.get_base  uop
        <*> getReg C.get_index uop
        <*> C.get_scale uop
        <*> pure off

    getPtr uop = do
      sz <- C.get_size uop
      (seg, off) <- C.get_lval_ptr uop
      let szw = case sz of
            32 -> Bits16
            48 -> Bits32
            _  -> error ("invaild pointer size " ++ show sz)
      return . Pointer seg $ Immediate szw off

    getImm f uop = do
      Just sz <- wordSize <$> C.get_size uop
      val <- f sz uop
      return $ Immediate sz val

getOperands :: Ptr C.UD_t -> IO [Operand]
getOperands udt = catMaybes <$> mapM decode getters where
  getters = [C.get_operand1, C.get_operand2, C.get_operand3]
  decode f = do
    let uop = f udt
    ty  <- C.get_type uop
    case lookupUDTM ty opDecode of
      Just g  -> Just <$> g uop
      Nothing -> return Nothing

-- | Get the current instruction.
getInstruction :: UD -> IO Instruction
getInstruction = flip withUDPtr $ \udt ->
  Inst <$> getPfx udt <*> (opcode <$> C.get_mnemonic udt) <*> getOperands udt
