{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module ByteArray
    ( MutableByteArray
    , newByteArray
    , readByteArray
    , writeByteArray
    ) where

import GHC.Prim
import GHC.Types
import GHC.Word ( Word64(..) )
import GHC.IO

-- Very small subset of the primitive package specalized to IO.

data MutableByteArray = MutableByteArray (MutableByteArray# RealWorld)

newByteArray :: Int -> IO MutableByteArray
{-# INLINE newByteArray #-}
newByteArray (I# n#)
  = IO (\s# -> case newByteArray# n# s# of
                   (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

readByteArray :: MutableByteArray -> Int -> IO Word64
{-# INLINE readByteArray #-}
readByteArray (MutableByteArray arr#) (I# i#)
  = IO (readByteArray# arr# i#)

-- | Write a primitive value to the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
writeByteArray :: MutableByteArray -> Int -> Word64 -> IO ()
{-# INLINE writeByteArray #-}
writeByteArray (MutableByteArray arr#) (I# i#) x
  = IO (\s# -> case writeByteArray# arr# i# x s# of
                 s'# -> (# s'#, () #))

{-# INLINE readByteArray# #-}
readByteArray#  arr# i# s# =
    case readWord64Array# arr# i# s# of
        (# s1#, x# #) -> (# s1#, W64# x# #)

{-# INLINE writeByteArray# #-}
writeByteArray# arr# i# (W64# x#) s# = writeWord64Array# arr# i# x# s#
