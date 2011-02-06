{-# LANGUAGE
    DeriveDataTypeable
  , RecordWildCards
  , NamedFieldPuns
  , ViewPatterns #-}
module Udis86.IO
  ( UD
  , newUD
  , setInputBuffer
  , InputHook, endOfInput, setInputHook
  , setWordSize
  , setIP
  , setSyntaxNone, setSyntaxIntel, setSyntaxATT
  , setCallback
  , setVendorIntel, setVendorAMD
  , disassemble, skip
  , getLength, getOffset
  , getHex, getBytes, getAssembly
  , getInstruction
  ) where

import Udis86.C
import Udis86.Types

import Data.Typeable ( Typeable )
import Control.Concurrent.MVar
import Foreign
import Foreign.C.String
import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.IntMap as IM

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

newtype UD = UD (MVar State)
  deriving (Typeable)

setInput :: Input -> State -> IO State
setInput inpt st@State{udInput} = do
  case udInput of
    InHook fp -> freeHaskellFunPtr fp
    InBuf ptr -> touchForeignPtr ptr -- just in case
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

newUD :: IO UD
newUD = do
  p <- mallocBytes sizeof_ud_t
  ud_init p
  s <- newMVar $ State p InNone XlBuiltin nullFunPtr
  addMVarFinalizer s (finalizeState s)
  return $ UD s

withUDPtr :: UD -> (Ptr UD_t -> IO a) -> IO a
withUDPtr (UD s) f = withMVar s $ \State{udPtr} -> f udPtr

type InputHook = IO Int

endOfInput :: Int
endOfInput = ud_eoi

setInputHook :: UD -> InputHook -> IO ()
setInputHook (UD s) f = modifyMVar_ s $ \st@State{..} -> do
  fp <- c_mkInputHook f
  ud_set_input_hook udPtr fp
  setInput (InHook fp) st

-- FIXME: setInputFile

setInputBuffer :: UD -> BS.ByteString -> IO ()
setInputBuffer (UD s) bs = modifyMVar_ s $ \st@State{..} -> do
  let (ptr, off, len) = BS.toForeignPtr bs
  ud_set_input_buffer udPtr
    (unsafeForeignPtrToPtr ptr `plusPtr` off)
    (fromIntegral len)
  setInput (InBuf ptr) st

-- TODO: error checking
setWordSize :: UD -> WordSize -> IO ()
setWordSize s w = withUDPtr s $ flip ud_set_mode (fromIntegral $ bitsInWord w)

setIP :: UD -> Word64 -> IO ()
setIP s w = withUDPtr s $ flip ud_set_pc w

setSyntax :: XlatType -> FunPtr CTranslator -> UD -> IO ()
setSyntax ty fp (UD s) = modifyMVar_ s $ setXlat ty fp

setSyntaxNone, setSyntaxIntel, setSyntaxATT :: UD -> IO ()
setSyntaxNone  = setSyntax XlBuiltin nullFunPtr
setSyntaxIntel = setSyntax XlBuiltin ud_translate_intel
setSyntaxATT   = setSyntax XlBuiltin ud_translate_att

-- no point passing the UD since we can close over it easily
setCallback :: UD -> IO () -> IO ()
setCallback s act = do
  fp <- c_mkTranslator (const act)
  setSyntax XlCustom fp s

setVendor :: UD_vendor -> UD -> IO ()
setVendor v s = withUDPtr s $ flip ud_set_vendor v

setVendorIntel, setVendorAMD :: UD -> IO ()
setVendorIntel = setVendor udVendorIntel
setVendorAMD   = setVendor udVendorAmd

disassemble :: UD -> IO UInt
disassemble = flip withUDPtr ud_disassemble

getLength :: UD -> IO UInt
getLength = flip withUDPtr ud_insn_len

getOffset :: UD -> IO Word64
getOffset = flip withUDPtr ud_insn_off

getHex :: UD -> IO String
getHex s = withUDPtr s $ \p ->
  ud_insn_hex p >>= peekCString

getBytes :: UD -> IO BS.ByteString
getBytes s = withUDPtr s $ \p -> do
  len <- ud_insn_len p
  ptr <- ud_insn_ptr p
  -- Int vs. UInt overflow problems?
  BS.packCStringLen (castPtr ptr, fromIntegral len)

getAssembly :: UD -> IO String
getAssembly s = withUDPtr s $ \p ->
  ud_insn_asm p >>= peekCString

skip :: UD -> UInt -> IO ()
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

opDecode :: IM.IntMap (Ptr UD_operand -> IO Operand)
opDecode = IM.fromList
  [ (udOpMem,   (OpMem   <$>) . getMem)
  , (udOpReg,   (OpReg   <$>) . getReg get_base)
  , (udOpPtr,   (OpPtr   <$>) . getPtr)
  , (udOpImm,   (OpImm   <$>) . getImm)
  , (udOpJimm,  (OpJump  <$>) . getImm)
  , (udOpConst, (OpConst <$>) . getImm) ] where

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
    case IM.lookup ty opDecode of
      Just g  -> Just <$> g uop
      Nothing -> return Nothing

getInstruction :: UD -> IO Instruction
getInstruction = flip withUDPtr $ \udt ->
  Inst <$> getPfx udt <*> (opcode <$> get_mnemonic udt) <*> getOperands udt
