--  Copyright (C) 2012 Michael Zuser mikezuser@gmail.com
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Qualified exports
module KitchenSink.Qualified
  ( -- * Data.Array.IArray (a)
    Array
  , a_bounds
  , a_array
  , a_listArray
  , a_accumArray
  , a_get
  , a_indices
  , a_elems
  , a_assocs
  , a_upd
  , a_accum
  , a_amap
  , a_ixmap

    -- * Data.Array.IO (a)
  , IOArray
  , IOUArray
  , a_hGetArray
  , a_hPutArray

    -- * Data.Array.MArray (a)
  , a_getBounds
  , a_newArray
  , a_newArray_
  , a_newListArray
  , a_readArray
  , a_writeArray
  , a_mapArray
  , a_mapIndices
  , a_getElems
  , a_getAssocs
  , a_freeze
  , a_thaw

    -- * Data.Array.MArray.Safe (as)
  , as_getBounds
  , as_newArray
  , as_newArray_
  , as_newListArray
  , as_readArray
  , as_writeArray
  , as_mapArray
  , as_mapIndices
  , as_getElems
  , as_getAssocs
  , as_freeze
  , as_thaw

    -- * Data.Array.ST (a)
  , STArray
  , STUArray
  , a_runSTArray
  , a_runSTUArray

    -- * Data.Array.Storable (a)
  , StorableArray
  , a_withStorableArray
  , a_touchStorableArray

    -- * Data.Array.Unboxed (u)
  , UArray

    -- * Data.Array.Unsafe (a)
  , a_castSTUArray
  , a_castIOUArray
  , a_unsafeFreeze
  , a_unsafeThaw
  , a_unsafeForeignPtrToStorableArray

    -- * Data.Binary (bin)
  , BinGet
  , BinPut
  , bin_put
  , bin_get
  , bin_encode
  , bin_decode
  , bin_encodeFile
  , bin_decodeFile

    -- * Data.Binary.Builder (binb)
  , BinBuilder
  , binb_toLazyByteString
  , binb_empty
  , binb_singleton
  , binb_append
  , binb_fromByteString
  , binb_fromLazyByteString
  , binb_flush
  , binb_putWord16be
  , binb_putWord32be
  , binb_putWord64be
  , binb_putWord16le
  , binb_putWord32le
  , binb_putWord64le
  , binb_putWordhost
  , binb_putWord16host
  , binb_putWord32host
  , binb_putWord64host
  , binb_putCharUtf8

    -- * Data.Binary.Get (bin)
  , bin_runGet
  , bin_runGetState
  , bin_skip
  -- , bin_uncheckedSkip
  , bin_lookAhead
  , bin_lookAheadM
  , bin_lookAheadE
  -- , bin_uncheckedLookAhead
  , bin_bytesRead
  , bin_getBytes
  , bin_remaining
  , bin_isEmpty
  , bin_getWord8
  , bin_getByteString
  , bin_getLazyByteString
  , bin_getLazyByteStringNul
  , bin_getRemainingLazyByteString
  , bin_getWord16be
  , bin_getWord32be
  , bin_getWord64be
  , bin_getWord16le
  , bin_getWord32le
  , bin_getWord64le
  , bin_getWordhost
  , bin_getWord16host
  , bin_getWord32host
  , bin_getWord64host

    -- * Data.Binary.Put (bin)
  , BinPutM
  , bin_unPut
  , bin_runPut
  , bin_runPutM
  , bin_putBuilder
  , bin_execPut
  , bin_flush
  , bin_putWord8
  , bin_putByteString
  , bin_putLazyByteString
  , bin_putWord16be
  , bin_putWord32be
  , bin_putWord64be
  , bin_putWord16le
  , bin_putWord32le
  , bin_putWord64le
  , bin_putWordhost
  , bin_putWord16host
  , bin_putWord32host
  , bin_putWord64host

    -- * Data.ByteString (b)
  , ByteString
  , b_empty
  , b_singleton
  , b_pack
  , b_unpack
  , b_cons
  , b_snoc
  , b_append
  , b_head
  , b_uncons
  , b_last
  , b_tail
  , b_init
  , b_null
  , b_length
  , b_map
  , b_reverse
  , b_intersperse
  , b_intercalate
  , b_transpose
  , b_foldl
  , b_foldl'
  , b_foldl1
  , b_foldl1'
  , b_foldr
  , b_foldr'
  , b_foldr1
  , b_foldr1'
  , b_concat
  , b_concatMap
  , b_any
  , b_all
  , b_maximum
  , b_minimum
  , b_scanl
  , b_scanl1
  , b_scanr
  , b_scanr1
  , b_mapAccumL
  , b_mapAccumR
  , b_replicate
  , b_unfoldr
  , b_unfoldrN
  , b_take
  , b_drop
  , b_splitAt
  , b_takeWhile
  , b_dropWhile
  , b_span
  , b_spanEnd
  , b_break
  , b_breakEnd
  , b_group
  , b_groupBy
  , b_inits
  , b_tails
  , b_split
  , b_splitWith
  , b_isPrefixOf
  , b_isSuffixOf
  , b_isInfixOf
  , b_breakSubstring
  , b_elem
  , b_notElem
  , b_find
  , b_filter
  , b_partition
  , b_index
  , b_elemIndex
  , b_elemIndices
  , b_elemIndexEnd
  , b_findIndex
  , b_findIndices
  , b_count
  , b_zip
  , b_zipWith
  , b_unzip
  , b_sort
  , b_copy
  , b_packCString
  , b_packCStringLen
  , b_useAsCString
  , b_useAsCStringLen
  , b_getLine
  , b_getContents
  , b_putStr
  , b_interact
  , b_readFile
  , b_writeFile
  , b_appendFile
  , b_hGetLine
  , b_hGetContents
  , b_hGet
  , b_hGetSome
  , b_hGetNonBlocking
  , b_hPut
  , b_hPutNonBlocking
  , b_hPutStr
  , b_breakByte

    -- * Data.ByteString.Char8 (bc)
  , ByteStringChar8
  , bc_empty
  , bc_singleton
  , bc_pack
  , bc_unpack
  , bc_cons
  , bc_snoc
  , bc_append
  , bc_head
  , bc_uncons
  , bc_last
  , bc_tail
  , bc_init
  , bc_null
  , bc_length
  , bc_map
  , bc_reverse
  , bc_intersperse
  , bc_intercalate
  , bc_transpose
  , bc_foldl
  , bc_foldl'
  , bc_foldl1
  , bc_foldl1'
  , bc_foldr
  , bc_foldr'
  , bc_foldr1
  , bc_foldr1'
  , bc_concat
  , bc_concatMap
  , bc_any
  , bc_all
  , bc_maximum
  , bc_minimum
  , bc_scanl
  , bc_scanl1
  , bc_scanr
  , bc_scanr1
  , bc_mapAccumL
  , bc_mapAccumR
  , bc_replicate
  , bc_unfoldr
  , bc_unfoldrN
  , bc_take
  , bc_drop
  , bc_splitAt
  , bc_takeWhile
  , bc_dropWhile
  , bc_span
  , bc_spanEnd
  , bc_break
  , bc_breakEnd
  , bc_group
  , bc_groupBy
  , bc_inits
  , bc_tails
  , bc_split
  , bc_splitWith
  , bc_lines
  , bc_words
  , bc_unlines
  , bc_unwords
  , bc_isPrefixOf
  , bc_isSuffixOf
  , bc_isInfixOf
  , bc_breakSubstring
  , bc_elem
  , bc_notElem
  , bc_find
  , bc_filter
  , bc_index
  , bc_elemIndex
  , bc_elemIndices
  , bc_elemIndexEnd
  , bc_findIndex
  , bc_findIndices
  , bc_count
  , bc_zip
  , bc_zipWith
  , bc_unzip
  , bc_sort
  , bc_readInt
  , bc_readInteger
  , bc_copy
  , bc_packCString
  , bc_packCStringLen
  , bc_useAsCString
  , bc_useAsCStringLen
  , bc_getLine
  , bc_getContents
  , bc_putStr
  , bc_putStrLn
  , bc_interact
  , bc_readFile
  , bc_writeFile
  , bc_appendFile
  , bc_hGetLine
  , bc_hGetContents
  , bc_hGet
  , bc_hGetNonBlocking
  , bc_hPut
  , bc_hPutNonBlocking
  , bc_hPutStr
  , bc_hPutStrLn

    -- * Data.ByteString.Lazy (bl)
  , ByteStringL
  , bl_empty
  , bl_singleton
  , bl_pack
  , bl_unpack
  , bl_fromStrict
  , bl_toStrict
  , bl_fromChunks
  , bl_toChunks
  , bl_foldrChunks
  , bl_foldlChunks
  , bl_cons
  , bl_cons'
  , bl_snoc
  , bl_append
  , bl_head
  , bl_uncons
  , bl_last
  , bl_tail
  , bl_init
  , bl_null
  , bl_length
  , bl_map
  , bl_reverse
  , bl_intersperse
  , bl_intercalate
  , bl_transpose
  , bl_foldl
  , bl_foldl'
  , bl_foldl1
  , bl_foldl1'
  , bl_foldr
  , bl_foldr1
  , bl_concat
  , bl_concatMap
  , bl_any
  , bl_all
  , bl_maximum
  , bl_minimum
  , bl_scanl
  , bl_mapAccumL
  , bl_mapAccumR
  , bl_repeat
  , bl_replicate
  , bl_cycle
  , bl_iterate
  , bl_unfoldr
  , bl_take
  , bl_drop
  , bl_splitAt
  , bl_takeWhile
  , bl_dropWhile
  , bl_span
  , bl_break
  , bl_group
  , bl_groupBy
  , bl_inits
  , bl_tails
  , bl_split
  , bl_splitWith
  , bl_isPrefixOf
  , bl_isSuffixOf
  , bl_elem
  , bl_notElem
  , bl_find
  , bl_filter
  , bl_partition
  , bl_index
  , bl_elemIndex
  , bl_elemIndices
  , bl_findIndex
  , bl_findIndices
  , bl_count
  , bl_zip
  , bl_zipWith
  , bl_unzip
  , bl_copy
  , bl_getContents
  , bl_putStr
  , bl_interact
  , bl_readFile
  , bl_writeFile
  , bl_appendFile
  , bl_hGetContents
  , bl_hGet
  , bl_hGetNonBlocking
  , bl_hPut
  , bl_hPutNonBlocking
  , bl_hPutStr

    -- * Data.ByteString.Lazy.Builder (blb)
  , BLBuilder
  , blb_toLazyByteString
  , blb_hPutBuilder
  , blb_byteString
  , blb_lazyByteString
  , blb_int8
  , blb_word8
  , blb_int16BE
  , blb_int32BE
  , blb_int64BE
  , blb_word16BE
  , blb_word32BE
  , blb_word64BE
  , blb_floatBE
  , blb_doubleBE
  , blb_int16LE
  , blb_int32LE
  , blb_int64LE
  , blb_word16LE
  , blb_word32LE
  , blb_word64LE
  , blb_floatLE
  , blb_doubleLE
  , blb_char7
  , blb_string7
  , blb_char8
  , blb_string8
  , blb_charUtf8
  , blb_stringUtf8

    -- * Data.ByteString.Lazy.Builder.ASCII (blb)
  , blb_int8Dec
  , blb_int16Dec
  , blb_int32Dec
  , blb_int64Dec
  , blb_intDec
  , blb_integerDec
  , blb_word8Dec
  , blb_word16Dec
  , blb_word32Dec
  , blb_word64Dec
  , blb_wordDec
  , blb_floatDec
  , blb_doubleDec
  , blb_word8Hex
  , blb_word16Hex
  , blb_word32Hex
  , blb_word64Hex
  , blb_wordHex
  , blb_int8HexFixed
  , blb_int16HexFixed
  , blb_int32HexFixed
  , blb_int64HexFixed
  , blb_word8HexFixed
  , blb_word16HexFixed
  , blb_word32HexFixed
  , blb_word64HexFixed
  , blb_floatHexFixed
  , blb_doubleHexFixed
  , blb_byteStringHexFixed
  , blb_lazyByteStringHexFixed

    -- * Data.ByteString.Lazy.Builder.Extras (blb)
  , AllocationStrategy
  , blb_toLazyByteStringWith
  , blb_safeStrategy
  , blb_untrimmedStrategy
  , blb_smallChunkSize
  , blb_defaultChunkSize
  , blb_byteStringCopy
  , blb_byteStringInsert
  , blb_byteStringThreshold
  , blb_lazyByteStringCopy
  , blb_lazyByteStringInsert
  , blb_lazyByteStringThreshold
  , blb_flush
  , blb_intHost
  , blb_int16Host
  , blb_int32Host
  , blb_int64Host
  , blb_wordHost
  , blb_word16Host
  , blb_word32Host
  , blb_word64Host
  , blb_floatHost
  , blb_doubleHost

    -- * Data.ByteString.Lazy.Char8 (blc)
  , ByteStringLChar8
  , blc_empty
  , blc_singleton
  , blc_pack
  , blc_unpack
  , blc_fromChunks
  , blc_toChunks
  , blc_fromStrict
  , blc_toStrict
  , blc_cons
  , blc_cons'
  , blc_snoc
  , blc_append
  , blc_head
  , blc_uncons
  , blc_last
  , blc_tail
  , blc_init
  , blc_null
  , blc_length
  , blc_map
  , blc_reverse
  , blc_intersperse
  , blc_intercalate
  , blc_transpose
  , blc_foldl
  , blc_foldl'
  , blc_foldl1
  , blc_foldl1'
  , blc_foldr
  , blc_foldr1
  , blc_concat
  , blc_concatMap
  , blc_any
  , blc_all
  , blc_maximum
  , blc_minimum
  , blc_scanl
  , blc_mapAccumL
  , blc_mapAccumR
  , blc_repeat
  , blc_replicate
  , blc_cycle
  , blc_iterate
  , blc_unfoldr
  , blc_take
  , blc_drop
  , blc_splitAt
  , blc_takeWhile
  , blc_dropWhile
  , blc_span
  , blc_break
  , blc_group
  , blc_groupBy
  , blc_inits
  , blc_tails
  , blc_split
  , blc_splitWith
  , blc_lines
  , blc_words
  , blc_unlines
  , blc_unwords
  , blc_isPrefixOf
  , blc_elem
  , blc_notElem
  , blc_find
  , blc_filter
  , blc_index
  , blc_elemIndex
  , blc_elemIndices
  , blc_findIndex
  , blc_findIndices
  , blc_count
  , blc_zip
  , blc_zipWith
  , blc_copy
  , blc_readInt
  , blc_readInteger
  , blc_getContents
  , blc_putStr
  , blc_putStrLn
  , blc_interact
  , blc_readFile
  , blc_writeFile
  , blc_appendFile
  , blc_hGetContents
  , blc_hGet
  , blc_hGetNonBlocking
  , blc_hPut
  , blc_hPutNonBlocking
  , blc_hPutStr
  , blc_hPutStrLn

    -- * Data.ByteString.Unsafe (b)
  , b_unsafeHead
  , b_unsafeTail
  , b_unsafeIndex
  , b_unsafeTake
  , b_unsafeDrop
  , b_unsafeUseAsCString
  , b_unsafeUseAsCStringLen
  , b_unsafePackCString
  , b_unsafePackCStringLen
  , b_unsafePackMallocCString
  , b_unsafePackAddress
  , b_unsafePackAddressLen
  , b_unsafePackCStringFinalizer
  , b_unsafeFinalize

    -- * Data.CaseInsensitive (ci)
  , CI
  , ci_mk
  , ci_original
  , ci_foldedCase
  , ci_map
  , ci_foldCase

    -- * Data.HashMap.Lazy (hml)
  , HashMapL
  , hml_get
  , hml_empty
  , hml_singleton
  , hml_null
  , hml_size
  , hml_member
  , hml_lookup
  , hml_lookupDefault
  , hml_insert
  , hml_insertWith
  , hml_delete
  , hml_adjust
  , hml_union
  , hml_unionWith
  , hml_unions
  , hml_map
  , hml_traverseWithKey
  , hml_difference
  , hml_intersection
  , hml_intersectionWith
  , hml_foldl'
  , hml_foldlWithKey'
  , hml_foldr
  , hml_foldrWithKey
  , hml_filter
  , hml_filterWithKey
  , hml_keys
  , hml_elems
  , hml_toList
  , hml_fromList
  , hml_fromListWith

    -- * Data.HashMap.Strict (hms)
  , HashMapS
  , hms_get
  , hms_empty
  , hms_singleton
  , hms_null
  , hms_size
  , hms_member
  , hms_lookup
  , hms_lookupDefault
  , hms_insert
  , hms_insertWith
  , hms_delete
  , hms_adjust
  , hms_union
  , hms_unionWith
  , hms_unions
  , hms_map
  , hms_traverseWithKey
  , hms_difference
  , hms_intersection
  , hms_intersectionWith
  , hms_foldl'
  , hms_foldlWithKey'
  , hms_foldr
  , hms_foldrWithKey
  , hms_filter
  , hms_filterWithKey
  , hms_keys
  , hms_elems
  , hms_toList
  , hms_fromList
  , hms_fromListWith

    -- * Data.HashSet (hs)
  , HashSet
  , hs_empty
  , hs_singleton
  , hs_union
  , hs_unions
  , hs_null
  , hs_size
  , hs_member
  , hs_insert
  , hs_delete
  , hs_map
  , hs_difference
  , hs_intersection
  , hs_foldl'
  , hs_foldr
  , hs_filter
  , hs_toList
  , hs_fromList

    -- * Data.IntMap.Lazy (iml)
  , IntMapL
  , iml_get
  , iml_null
  , iml_size
  , iml_member
  , iml_notMember
  , iml_lookup
  , iml_findWithDefault
  , iml_lookupLT
  , iml_lookupGT
  , iml_lookupLE
  , iml_lookupGE
  , iml_empty
  , iml_singleton
  , iml_insert
  , iml_insertWith
  , iml_insertWithKey
  , iml_insertLookupWithKey
  , iml_delete
  , iml_adjust
  , iml_adjustWithKey
  , iml_update
  , iml_updateWithKey
  , iml_updateLookupWithKey
  , iml_alter
  , iml_union
  , iml_unionWith
  , iml_unionWithKey
  , iml_unions
  , iml_unionsWith
  , iml_difference
  , iml_differenceWith
  , iml_differenceWithKey
  , iml_intersection
  , iml_intersectionWith
  , iml_intersectionWithKey
  , iml_mergeWithKey
  , iml_map
  , iml_mapWithKey
  , iml_traverseWithKey
  , iml_mapAccum
  , iml_mapAccumWithKey
  , iml_mapAccumRWithKey
  , iml_mapKeys
  , iml_mapKeysWith
  , iml_mapKeysMonotonic
  , iml_foldr
  , iml_foldl
  , iml_foldrWithKey
  , iml_foldlWithKey
  , iml_foldr'
  , iml_foldl'
  , iml_foldrWithKey'
  , iml_foldlWithKey'
  , iml_elems
  , iml_keys
  , iml_assocs
  , iml_keysSet
  , iml_fromSet
  , iml_toList
  , iml_fromList
  , iml_fromListWith
  , iml_fromListWithKey
  , iml_toAscList
  , iml_toDescList
  , iml_fromAscList
  , iml_fromAscListWith
  , iml_fromAscListWithKey
  , iml_fromDistinctAscList
  , iml_filter
  , iml_filterWithKey
  , iml_partition
  , iml_partitionWithKey
  , iml_mapMaybe
  , iml_mapMaybeWithKey
  , iml_mapEither
  , iml_mapEitherWithKey
  , iml_split
  , iml_splitLookup
  , iml_isSubmapOf
  , iml_isSubmapOfBy
  , iml_isProperSubmapOf
  , iml_isProperSubmapOfBy
  , iml_findMin
  , iml_findMax
  , iml_deleteMin
  , iml_deleteMax
  , iml_deleteFindMin
  , iml_deleteFindMax
  , iml_updateMin
  , iml_updateMax
  , iml_updateMinWithKey
  , iml_updateMaxWithKey
  , iml_minView
  , iml_maxView
  , iml_minViewWithKey
  , iml_maxViewWithKey
  , iml_showTree
  , iml_showTreeWith

    -- * Data.IntMap.Strict (ims)
  , IntMapS
  , ims_get
  , ims_null
  , ims_size
  , ims_member
  , ims_notMember
  , ims_lookup
  , ims_findWithDefault
  , ims_lookupLT
  , ims_lookupGT
  , ims_lookupLE
  , ims_lookupGE
  , ims_empty
  , ims_singleton
  , ims_insert
  , ims_insertWith
  , ims_insertWithKey
  , ims_insertLookupWithKey
  , ims_delete
  , ims_adjust
  , ims_adjustWithKey
  , ims_update
  , ims_updateWithKey
  , ims_updateLookupWithKey
  , ims_alter
  , ims_union
  , ims_unionWith
  , ims_unionWithKey
  , ims_unions
  , ims_unionsWith
  , ims_difference
  , ims_differenceWith
  , ims_differenceWithKey
  , ims_intersection
  , ims_intersectionWith
  , ims_intersectionWithKey
  , ims_mergeWithKey
  , ims_map
  , ims_mapWithKey
  , ims_traverseWithKey
  , ims_mapAccum
  , ims_mapAccumWithKey
  , ims_mapAccumRWithKey
  , ims_mapKeys
  , ims_mapKeysWith
  , ims_mapKeysMonotonic
  , ims_foldr
  , ims_foldl
  , ims_foldrWithKey
  , ims_foldlWithKey
  , ims_foldr'
  , ims_foldl'
  , ims_foldrWithKey'
  , ims_foldlWithKey'
  , ims_elems
  , ims_keys
  , ims_assocs
  , ims_keysSet
  , ims_fromSet
  , ims_toList
  , ims_fromList
  , ims_fromListWith
  , ims_fromListWithKey
  , ims_toAscList
  , ims_toDescList
  , ims_fromAscList
  , ims_fromAscListWith
  , ims_fromAscListWithKey
  , ims_fromDistinctAscList
  , ims_filter
  , ims_filterWithKey
  , ims_partition
  , ims_partitionWithKey
  , ims_mapMaybe
  , ims_mapMaybeWithKey
  , ims_mapEither
  , ims_mapEitherWithKey
  , ims_split
  , ims_splitLookup
  , ims_isSubmapOf
  , ims_isSubmapOfBy
  , ims_isProperSubmapOf
  , ims_isProperSubmapOfBy
  , ims_findMin
  , ims_findMax
  , ims_deleteMin
  , ims_deleteMax
  , ims_deleteFindMin
  , ims_deleteFindMax
  , ims_updateMin
  , ims_updateMax
  , ims_updateMinWithKey
  , ims_updateMaxWithKey
  , ims_minView
  , ims_maxView
  , ims_minViewWithKey
  , ims_maxViewWithKey
  , ims_showTree
  , ims_showTreeWith

    -- * Data.IntSet (is)
  , IntSet
  , is_null
  , is_size
  , is_member
  , is_notMember
  , is_lookupLT
  , is_lookupGT
  , is_lookupLE
  , is_lookupGE
  , is_isSubsetOf
  , is_isProperSubsetOf
  , is_empty
  , is_singleton
  , is_insert
  , is_delete
  , is_union
  , is_unions
  , is_difference
  , is_intersection
  , is_filter
  , is_partition
  , is_split
  , is_splitMember
  , is_map
  , is_foldr
  , is_foldl
  , is_foldr'
  , is_foldl'
  , is_fold
  , is_findMin
  , is_findMax
  , is_deleteMin
  , is_deleteMax
  , is_deleteFindMin
  , is_deleteFindMax
  , is_maxView
  , is_minView
  , is_elems
  , is_toList
  , is_fromList
  , is_toAscList
  , is_toDescList
  , is_fromAscList
  , is_fromDistinctAscList
  , is_showTree
  , is_showTreeWith

    -- * Data.Map.Lazy (ml)
  , MapL
  , ml_get
  , ml_null
  , ml_size
  , ml_member
  , ml_notMember
  , ml_lookup
  , ml_findWithDefault
  , ml_lookupLT
  , ml_lookupGT
  , ml_lookupLE
  , ml_lookupGE
  , ml_empty
  , ml_singleton
  , ml_insert
  , ml_insertWith
  , ml_insertWithKey
  , ml_insertLookupWithKey
  , ml_delete
  , ml_adjust
  , ml_adjustWithKey
  , ml_update
  , ml_updateWithKey
  , ml_updateLookupWithKey
  , ml_alter
  , ml_union
  , ml_unionWith
  , ml_unionWithKey
  , ml_unions
  , ml_unionsWith
  , ml_difference
  , ml_differenceWith
  , ml_differenceWithKey
  , ml_intersection
  , ml_intersectionWith
  , ml_intersectionWithKey
  , ml_mergeWithKey
  , ml_map
  , ml_mapWithKey
  , ml_traverseWithKey
  , ml_mapAccum
  , ml_mapAccumWithKey
  , ml_mapAccumRWithKey
  , ml_mapKeys
  , ml_mapKeysWith
  , ml_mapKeysMonotonic
  , ml_foldr
  , ml_foldl
  , ml_foldrWithKey
  , ml_foldlWithKey
  , ml_foldr'
  , ml_foldl'
  , ml_foldrWithKey'
  , ml_foldlWithKey'
  , ml_elems
  , ml_keys
  , ml_assocs
  , ml_keysSet
  , ml_fromSet
  , ml_toList
  , ml_fromList
  , ml_fromListWith
  , ml_fromListWithKey
  , ml_toAscList
  , ml_toDescList
  , ml_fromAscList
  , ml_fromAscListWith
  , ml_fromAscListWithKey
  , ml_fromDistinctAscList
  , ml_filter
  , ml_filterWithKey
  , ml_partition
  , ml_partitionWithKey
  , ml_mapMaybe
  , ml_mapMaybeWithKey
  , ml_mapEither
  , ml_mapEitherWithKey
  , ml_split
  , ml_splitLookup
  , ml_isSubmapOf
  , ml_isSubmapOfBy
  , ml_isProperSubmapOf
  , ml_isProperSubmapOfBy
  , ml_lookupIndex
  , ml_findIndex
  , ml_elemAt
  , ml_updateAt
  , ml_deleteAt
  , ml_findMin
  , ml_findMax
  , ml_deleteMin
  , ml_deleteMax
  , ml_deleteFindMin
  , ml_deleteFindMax
  , ml_updateMin
  , ml_updateMax
  , ml_updateMinWithKey
  , ml_updateMaxWithKey
  , ml_minView
  , ml_maxView
  , ml_minViewWithKey
  , ml_maxViewWithKey
  , ml_showTree
  , ml_showTreeWith
  , ml_valid

    -- * Data.Map.Strict (ms)
  , MapS
  , ms_get
  , ms_null
  , ms_size
  , ms_member
  , ms_notMember
  , ms_lookup
  , ms_findWithDefault
  , ms_lookupLT
  , ms_lookupGT
  , ms_lookupLE
  , ms_lookupGE
  , ms_empty
  , ms_singleton
  , ms_insert
  , ms_insertWith
  , ms_insertWithKey
  , ms_insertLookupWithKey
  , ms_delete
  , ms_adjust
  , ms_adjustWithKey
  , ms_update
  , ms_updateWithKey
  , ms_updateLookupWithKey
  , ms_alter
  , ms_union
  , ms_unionWith
  , ms_unionWithKey
  , ms_unions
  , ms_unionsWith
  , ms_difference
  , ms_differenceWith
  , ms_differenceWithKey
  , ms_intersection
  , ms_intersectionWith
  , ms_intersectionWithKey
  , ms_mergeWithKey
  , ms_map
  , ms_mapWithKey
  , ms_traverseWithKey
  , ms_mapAccum
  , ms_mapAccumWithKey
  , ms_mapAccumRWithKey
  , ms_mapKeys
  , ms_mapKeysWith
  , ms_mapKeysMonotonic
  , ms_foldr
  , ms_foldl
  , ms_foldrWithKey
  , ms_foldlWithKey
  , ms_foldr'
  , ms_foldl'
  , ms_foldrWithKey'
  , ms_foldlWithKey'
  , ms_elems
  , ms_keys
  , ms_assocs
  , ms_keysSet
  , ms_fromSet
  , ms_toList
  , ms_fromList
  , ms_fromListWith
  , ms_fromListWithKey
  , ms_toAscList
  , ms_toDescList
  , ms_fromAscList
  , ms_fromAscListWith
  , ms_fromAscListWithKey
  , ms_fromDistinctAscList
  , ms_filter
  , ms_filterWithKey
  , ms_partition
  , ms_partitionWithKey
  , ms_mapMaybe
  , ms_mapMaybeWithKey
  , ms_mapEither
  , ms_mapEitherWithKey
  , ms_split
  , ms_splitLookup
  , ms_isSubmapOf
  , ms_isSubmapOfBy
  , ms_isProperSubmapOf
  , ms_isProperSubmapOfBy
  , ms_lookupIndex
  , ms_findIndex
  , ms_elemAt
  , ms_updateAt
  , ms_deleteAt
  , ms_findMin
  , ms_findMax
  , ms_deleteMin
  , ms_deleteMax
  , ms_deleteFindMin
  , ms_deleteFindMax
  , ms_updateMin
  , ms_updateMax
  , ms_updateMinWithKey
  , ms_updateMaxWithKey
  , ms_minView
  , ms_maxView
  , ms_minViewWithKey
  , ms_maxViewWithKey
  , ms_showTree
  , ms_showTreeWith
  , ms_valid

    -- * Data.Set (s)
  , Set
  , s_null
  , s_size
  , s_member
  , s_notMember
  , s_lookupLT
  , s_lookupGT
  , s_lookupLE
  , s_lookupGE
  , s_isSubsetOf
  , s_isProperSubsetOf
  , s_empty
  , s_singleton
  , s_insert
  , s_delete
  , s_union
  , s_unions
  , s_difference
  , s_intersection
  , s_filter
  , s_partition
  , s_split
  , s_splitMember
  , s_map
  , s_mapMonotonic
  , s_foldr
  , s_foldl
  , s_foldr'
  , s_foldl'
  , s_fold
  , s_findMin
  , s_findMax
  , s_deleteMin
  , s_deleteMax
  , s_deleteFindMin
  , s_deleteFindMax
  , s_maxView
  , s_minView
  , s_elems
  , s_toList
  , s_fromList
  , s_toAscList
  , s_toDescList
  , s_fromAscList
  , s_fromDistinctAscList
  , s_showTree
  , s_showTreeWith
  , s_valid

    -- * Data.Sequence (seq)
  , Seq
  , seq_empty
  , seq_singleton
  , seq_cons
  , seq_snoc
  , seq_cat
  , seq_fromList
  , seq_replicate
  , seq_replicateA
  , seq_replicateM
  , seq_iterateN
  , seq_unfoldr
  , seq_unfoldl
  , seq_null
  , seq_length
  , ViewL
  , seq_viewl
  , ViewR
  , seq_viewr
  , seq_scanl
  , seq_scanl1
  , seq_scanr
  , seq_scanr1
  , seq_tails
  , seq_inits
  , seq_takeWhileL
  , seq_takeWhileR
  , seq_dropWhileL
  , seq_dropWhileR
  , seq_spanl
  , seq_spanr
  , seq_breakl
  , seq_breakr
  , seq_partition
  , seq_filter
  , seq_sort
  , seq_sortBy
  , seq_unstableSort
  , seq_unstableSortBy
  , seq_index
  , seq_adjust
  , seq_update
  , seq_take
  , seq_drop
  , seq_splitAt
  , seq_elemIndexL
  , seq_elemIndicesL
  , seq_elemIndexR
  , seq_elemIndicesR
  , seq_findIndexL
  , seq_findIndicesL
  , seq_findIndexR
  , seq_findIndicesR
  , seq_foldlWithIndex
  , seq_foldrWithIndex
  , seq_mapWithIndex
  , seq_reverse
  , seq_zip
  , seq_zipWith
  , seq_zip3
  , seq_zipWith3
  , seq_zip4
  , seq_zipWith4

    -- * Data.Text (t)
  , Text
  , t_pack
  , t_unpack
  , t_singleton
  , t_empty
  , t_cons
  , t_snoc
  , t_append
  , t_uncons
  , t_head
  , t_last
  , t_tail
  , t_init
  , t_null
  , t_length
  , t_compareLength
  , t_map
  , t_intercalate
  , t_intersperse
  , t_transpose
  , t_reverse
  , t_replace
  , t_toCaseFold
  , t_toLower
  , t_toUpper
  , t_justifyLeft
  , t_justifyRight
  , t_center
  , t_foldl
  , t_foldl'
  , t_foldl1
  , t_foldl1'
  , t_foldr
  , t_foldr1
  , t_concat
  , t_concatMap
  , t_any
  , t_all
  , t_maximum
  , t_minimum
  , t_scanl
  , t_scanl1
  , t_scanr
  , t_scanr1
  , t_mapAccumL
  , t_mapAccumR
  , t_replicate
  , t_unfoldr
  , t_unfoldrN
  , t_take
  , t_drop
  , t_takeWhile
  , t_dropWhile
  , t_dropWhileEnd
  , t_dropAround
  , t_strip
  , t_stripStart
  , t_stripEnd
  , t_splitAt
  , t_breakOn
  , t_breakOnEnd
  , t_break
  , t_span
  , t_group
  , t_groupBy
  , t_inits
  , t_tails
  , t_splitOn
  , t_split
  , t_chunksOf
  , t_lines
  , t_words
  , t_unlines
  , t_unwords
  , t_isPrefixOf
  , t_isSuffixOf
  , t_isInfixOf
  , t_stripPrefix
  , t_stripSuffix
  , t_commonPrefixes
  , t_filter
  , t_breakOnAll
  , t_find
  , t_partition
  , t_index
  , t_findIndex
  , t_count
  , t_zip
  , t_zipWith
  , t_copy

    -- * Data.Text.Array
  , TxtArray
  , TxtMArray
  , ta_copyM
  , ta_copyI
  , ta_empty
  , ta_equal
  , ta_run
  , ta_run2
  , ta_toList
  , ta_unsafeFreeze
  , ta_unsafeIndex
  , ta_new
  , ta_unsafeWrite

    -- * Data.Text.Encoding (t)
  , t_decodeLatin1
  , t_decodeUtf8
  , t_decodeUtf16LE
  , t_decodeUtf16BE
  , t_decodeUtf32LE
  , t_decodeUtf32BE
  , t_decodeUtf8'
  , t_decodeUtf8With
  , t_decodeUtf16LEWith
  , t_decodeUtf16BEWith
  , t_decodeUtf32LEWith
  , t_decodeUtf32BEWith
  , t_encodeUtf8
  , t_encodeUtf16LE
  , t_encodeUtf16BE
  , t_encodeUtf32LE
  , t_encodeUtf32BE

    -- * Data.Text.Encoding.Error (te)
  , TUnicodeException
  , TOnError
  , TOnDecodeError
  , TOnEncodeError
  , te_lenientDecode
  , te_strictDecode
  , te_strictEncode
  , te_ignore
  , te_replace

    -- * Data.Text.Foreign (t)
  , I16
  , t_fromPtr
  , t_useAsPtr
  , t_asForeignPtr
  , t_lengthWord16
  , t_unsafeCopyToPtr
  , t_dropWord16
  , t_takeWord16

    -- * Data.Text.IO (t)
  , t_readFile
  , t_writeFile
  , t_appendFile
  , t_hGetContents
  , t_hGetChunk
  , t_hGetLine
  , t_hPutStr
  , t_hPutStrLn
  , t_interact
  , t_getContents
  , t_getLine
  , t_putStr
  , t_putStrLn

    -- * Data.Text.Lazy (tl)
  , TextL
  , tl_pack
  , tl_unpack
  , tl_singleton
  , tl_empty
  , tl_fromChunks
  , tl_toChunks
  , tl_toStrict
  , tl_fromStrict
  , tl_foldrChunks
  , tl_foldlChunks
  , tl_cons
  , tl_snoc
  , tl_append
  , tl_uncons
  , tl_head
  , tl_last
  , tl_tail
  , tl_init
  , tl_null
  , tl_length
  , tl_compareLength
  , tl_map
  , tl_intercalate
  , tl_intersperse
  , tl_transpose
  , tl_reverse
  , tl_replace
  , tl_toCaseFold
  , tl_toLower
  , tl_toUpper
  , tl_justifyLeft
  , tl_justifyRight
  , tl_center
  , tl_foldl
  , tl_foldl'
  , tl_foldl1
  , tl_foldl1'
  , tl_foldr
  , tl_foldr1
  , tl_concat
  , tl_concatMap
  , tl_any
  , tl_all
  , tl_maximum
  , tl_minimum
  , tl_scanl
  , tl_scanl1
  , tl_scanr
  , tl_scanr1
  , tl_mapAccumL
  , tl_mapAccumR
  , tl_replicate
  , tl_unfoldr
  , tl_unfoldrN
  , tl_take
  , tl_drop
  , tl_takeWhile
  , tl_dropWhile
  , tl_dropWhileEnd
  , tl_dropAround
  , tl_strip
  , tl_stripStart
  , tl_stripEnd
  , tl_splitAt
  , tl_span
  , tl_breakOn
  , tl_breakOnEnd
  , tl_break
  , tl_group
  , tl_groupBy
  , tl_inits
  , tl_tails
  , tl_splitOn
  , tl_split
  , tl_chunksOf
  , tl_lines
  , tl_words
  , tl_unlines
  , tl_unwords
  , tl_isPrefixOf
  , tl_isSuffixOf
  , tl_isInfixOf
  , tl_stripPrefix
  , tl_stripSuffix
  , tl_commonPrefixes
  , tl_filter
  , tl_find
  , tl_breakOnAll
  , tl_partition
  , tl_index
  , tl_count
  , tl_zip
  , tl_zipWith

    -- * Data.Text.Lazy.Builder (tb)
  , TBuilder
  , tb_toLazyText
  , tb_toLazyTextWith
  , tb_singleton
  , tb_fromText
  , tb_fromLazyText
  , tb_fromString
  , tb_flush

    -- * Data.Text.Lazy.Builder.Int (tb)
  , tb_decimal
  , tb_hexadecimal

    -- * Data.Text.Lazy.Builder.RealFloat (tb)
  , TBFPFormat
  , tb_exponent
  , tb_fixed
  , tb_generic
  , tb_realFloat
  , tb_formatRealFloat

    -- * Data.Text.Lazy.Encoding (tl)
  , tl_decodeLatin1
  , tl_decodeUtf8
  , tl_decodeUtf16LE
  , tl_decodeUtf16BE
  , tl_decodeUtf32LE
  , tl_decodeUtf32BE
  , tl_decodeUtf8'
  , tl_decodeUtf8With
  , tl_decodeUtf16LEWith
  , tl_decodeUtf16BEWith
  , tl_decodeUtf32LEWith
  , tl_decodeUtf32BEWith
  , tl_encodeUtf8
  , tl_encodeUtf16LE
  , tl_encodeUtf16BE
  , tl_encodeUtf32LE
  , tl_encodeUtf32BE

    -- * Data.Text.Lazy.IO (tl)
  , tl_readFile
  , tl_writeFile
  , tl_appendFile
  , tl_hGetContents
  , tl_hGetLine
  , tl_hPutStr
  , tl_hPutStrLn
  , tl_interact
  , tl_getContents
  , tl_getLine
  , tl_putStr
  , tl_putStrLn

    -- * Data.Text.Lazy.Read (tl)
  , TLReader
  , tl_decimal
  , tl_hexadecimal
  , tl_signed
  , tl_rational
  , tl_double

    -- * Data.Text.Read (t)
  , RReader
  , t_decimal
  , t_hexadecimal
  , t_signed
  , t_rational
  , t_double

    -- * Data.Text.Unsafe (tu)
  , TIter
  , tu_iter
  , tu_iter_
  , tu_reverseIter
  , tu_unsafeHead
  , tu_unsafeTail
  , tu_lengthWord16
  , tu_takeWord16
  , tu_dropWord16

    -- * Data.Tree (tr)
  , Tree
  , Forest
  , tr_rootLabel
  , tr_subForest
  , tr_drawTree
  , tr_drawForest
  , tr_flatten
  , tr_levels
  , tr_unfoldTree
  , tr_unfoldForest
  , tr_unfoldTreeM
  , tr_unfoldForestM
  , tr_unfoldTreeM_BF
  , tr_unfoldForestM_BF

    -- * Data.Vector (v)
  , Vector
  , MVector
  , v_length
  , v_null
  , v_get
  , v_getq
  , v_head
  , v_last
  , v_unsafeIndex
  , v_unsafeHead
  , v_unsafeLast
  , v_indexM
  , v_headM
  , v_lastM
  , v_unsafeIndexM
  , v_unsafeHeadM
  , v_unsafeLastM
  , v_slice
  , v_init
  , v_tail
  , v_take
  , v_drop
  , v_splitAt
  , v_unsafeSlice
  , v_unsafeInit
  , v_unsafeTail
  , v_unsafeTake
  , v_unsafeDrop
  , v_empty
  , v_singleton
  , v_replicate
  , v_generate
  , v_iterateN
  , v_replicateM
  , v_generateM
  , v_create
  , v_unfoldr
  , v_unfoldrN
  , v_constructN
  , v_constructrN
  , v_enumFromN
  , v_enumFromStepN
  , v_enumFromTo
  , v_enumFromThenTo
  , v_cons
  , v_snoc
  , v_cat
  , v_concat
  , v_force
  , v_upd
  , v_update
  , v_update_
  , v_unsafeUpd
  , v_unsafeUpdate
  , v_unsafeUpdate_
  , v_accum
  , v_accumulate
  , v_accumulate_
  , v_unsafeAccum
  , v_unsafeAccumulate
  , v_unsafeAccumulate_
  , v_reverse
  , v_backpermute
  , v_unsafeBackpermute
  , v_modify
  , v_indexed
  , v_map
  , v_imap
  , v_concatMap
  , v_mapM
  , v_mapM_
  , v_forM
  , v_forM_
  , v_zipWith
  , v_zipWith3
  , v_zipWith4
  , v_zipWith5
  , v_zipWith6
  , v_izipWith
  , v_izipWith3
  , v_izipWith4
  , v_izipWith5
  , v_izipWith6
  , v_zip
  , v_zip3
  , v_zip4
  , v_zip5
  , v_zip6
  , v_zipWithM
  , v_zipWithM_
  , v_unzip
  , v_unzip3
  , v_unzip4
  , v_unzip5
  , v_unzip6
  , v_filter
  , v_ifilter
  , v_filterM
  , v_takeWhile
  , v_dropWhile
  , v_partition
  , v_unstablePartition
  , v_span
  , v_break
  , v_elem
  , v_notElem
  , v_find
  , v_findIndex
  , v_findIndices
  , v_elemIndex
  , v_elemIndices
  , v_foldl
  , v_foldl1
  , v_foldl'
  , v_foldl1'
  , v_foldr
  , v_foldr1
  , v_foldr'
  , v_foldr1'
  , v_ifoldl
  , v_ifoldl'
  , v_ifoldr
  , v_ifoldr'
  , v_all
  , v_any
  , v_and
  , v_or
  , v_sum
  , v_product
  , v_maximum
  , v_maximumBy
  , v_minimum
  , v_minimumBy
  , v_minIndex
  , v_minIndexBy
  , v_maxIndex
  , v_maxIndexBy
  , v_foldM
  , v_foldM'
  , v_fold1M
  , v_fold1M'
  , v_foldM_
  , v_foldM'_
  , v_fold1M_
  , v_fold1M'_
  , v_sequence
  , v_sequence_
  , v_prescanl
  , v_prescanl'
  , v_postscanl
  , v_postscanl'
  , v_scanl
  , v_scanl'
  , v_scanl1
  , v_scanl1'
  , v_prescanr
  , v_prescanr'
  , v_postscanr
  , v_postscanr'
  , v_scanr
  , v_scanr'
  , v_scanr1
  , v_scanr1'
  , v_toList
  , v_fromList
  , v_fromListN
  , v_convert
  , v_freeze
  , v_thaw
  , v_copy
  , v_unsafeFreeze
  , v_unsafeThaw
  , v_unsafeCopy

    -- * Data.Vector.Fusion.Stream (vf)
  , VFStep
  , VFStream
  , vf_inplace
  , vf_size
  , vf_sized
  , vf_length
  , vf_null
  , vf_empty
  , vf_singleton
  , vf_cons
  , vf_snoc
  , vf_replicate
  , vf_generate
  , vf_cat
  , vf_head
  , vf_last
  , vf_get
  , vf_getq
  , vf_slice
  , vf_init
  , vf_tail
  , vf_take
  , vf_drop
  , vf_map
  , vf_concatMap
  , vf_flatten
  , vf_unbox
  , vf_indexed
  , vf_indexedR
  , vf_zipWith
  , vf_zipWith3
  , vf_zipWith4
  , vf_zipWith5
  , vf_zipWith6
  , vf_zip
  , vf_zip3
  , vf_zip4
  , vf_zip5
  , vf_zip6
  , vf_filter
  , vf_takeWhile
  , vf_dropWhile
  , vf_elem
  , vf_notElem
  , vf_find
  , vf_findIndex
  , vf_foldl
  , vf_foldl1
  , vf_foldl'
  , vf_foldl1'
  , vf_foldr
  , vf_foldr1
  , vf_and
  , vf_or
  , vf_unfoldr
  , vf_unfoldrN
  , vf_iterateN
  , vf_prescanl
  , vf_prescanl'
  , vf_postscanl
  , vf_postscanl'
  , vf_scanl
  , vf_scanl'
  , vf_scanl1
  , vf_scanl1'
  , vf_enumFromStepN
  , vf_enumFromTo
  , vf_enumFromThenTo
  , vf_toList
  , vf_fromList
  , vf_fromListN
  , vf_unsafeFromList
  , vf_liftStream
  , vf_mapM
  , vf_mapM_
  , vf_zipWithM
  , vf_zipWithM_
  , vf_filterM
  , vf_foldM
  , vf_fold1M
  , vf_foldM'
  , vf_fold1M'
  , vf_eq
  , vf_cmp

    -- * Data.Vector.Fusion.Stream.Monadic (vfm)
  , VFMStream
  , VFMStep
  , vfm_size
  , vfm_sized
  , vfm_length
  , vfm_null
  , vfm_empty
  , vfm_singleton
  , vfm_cons
  , vfm_snoc
  , vfm_replicate
  , vfm_replicateM
  , vfm_generate
  , vfm_generateM
  , vfm_cat
  , vfm_head
  , vfm_last
  , vfm_get
  , vfm_getq
  , vfm_slice
  , vfm_init
  , vfm_tail
  , vfm_take
  , vfm_drop
  , vfm_map
  , vfm_mapM
  , vfm_mapM_
  , vfm_trans
  , vfm_unbox
  , vfm_concatMap
  , vfm_flatten
  , vfm_indexed
  , vfm_indexedR
  , vfm_zipWithM_
  , vfm_zipWithM
  , vfm_zipWith3M
  , vfm_zipWith4M
  , vfm_zipWith5M
  , vfm_zipWith6M
  , vfm_zipWith
  , vfm_zipWith3
  , vfm_zipWith4
  , vfm_zipWith5
  , vfm_zipWith6
  , vfm_zip
  , vfm_zip3
  , vfm_zip4
  , vfm_zip5
  , vfm_zip6
  , vfm_filter
  , vfm_filterM
  , vfm_takeWhile
  , vfm_takeWhileM
  , vfm_dropWhile
  , vfm_dropWhileM
  , vfm_elem
  , vfm_notElem
  , vfm_find
  , vfm_findM
  , vfm_findIndex
  , vfm_findIndexM
  , vfm_foldl
  , vfm_foldlM
  , vfm_foldl1
  , vfm_foldl1M
  , vfm_foldM
  , vfm_fold1M
  , vfm_foldl'
  , vfm_foldlM'
  , vfm_foldl1'
  , vfm_foldl1M'
  , vfm_foldM'
  , vfm_fold1M'
  , vfm_foldr
  , vfm_foldrM
  , vfm_foldr1
  , vfm_foldr1M
  , vfm_and
  , vfm_or
  , vfm_concatMapM
  , vfm_unfoldr
  , vfm_unfoldrM
  , vfm_unfoldrN
  , vfm_unfoldrNM
  , vfm_iterateN
  , vfm_iterateNM
  , vfm_prescanl
  , vfm_prescanlM
  , vfm_prescanl'
  , vfm_prescanlM'
  , vfm_postscanl
  , vfm_postscanlM
  , vfm_postscanl'
  , vfm_postscanlM'
  , vfm_scanl
  , vfm_scanlM
  , vfm_scanl'
  , vfm_scanlM'
  , vfm_scanl1
  , vfm_scanl1M
  , vfm_scanl1'
  , vfm_scanl1M'
  , vfm_enumFromStepN
  , vfm_enumFromTo
  , vfm_enumFromThenTo
  , vfm_toList
  , vfm_fromList
  , vfm_fromListN
  , vfm_unsafeFromList

    -- * Data.Vector.Fusion.Stream.Size (vf)
  , VFSize
  , vf_smaller
  , vf_larger
  , vf_toMax
  , vf_upperBound

    -- * Data.Vector.Fusion.Util (vf)
  , VFId
  , vf_Id
  , vf_unId
  , VFBox
  , vf_Box
  , vf_unBox
  , vf_delay_inline
  , vf_delayed_min

    -- * Data.Vector.Generic (vg)
  , Mutable
  , vg_basicUnsafeFreeze
  , vg_basicUnsafeThaw
  , vg_basicLength
  , vg_basicUnsafeSlice
  , vg_basicUnsafeIndexM
  , vg_basicUnsafeCopy
  , vg_elemseq
  , vg_length
  , vg_null
  , vg_get
  , vg_getq
  , vg_head
  , vg_last
  , vg_unsafeIndex
  , vg_unsafeHead
  , vg_unsafeLast
  , vg_indexM
  , vg_headM
  , vg_lastM
  , vg_unsafeIndexM
  , vg_unsafeHeadM
  , vg_unsafeLastM
  , vg_slice
  , vg_init
  , vg_tail
  , vg_take
  , vg_drop
  , vg_splitAt
  , vg_unsafeSlice
  , vg_unsafeInit
  , vg_unsafeTail
  , vg_unsafeTake
  , vg_unsafeDrop
  , vg_empty
  , vg_singleton
  , vg_replicate
  , vg_generate
  , vg_iterateN
  , vg_replicateM
  , vg_generateM
  , vg_create
  , vg_unfoldr
  , vg_unfoldrN
  , vg_constructN
  , vg_constructrN
  , vg_enumFromN
  , vg_enumFromStepN
  , vg_enumFromTo
  , vg_enumFromThenTo
  , vg_cons
  , vg_snoc
  , vg_cat
  , vg_concat
  , vg_force
  , vg_upd
  , vg_update
  , vg_update_
  , vg_unsafeUpd
  , vg_unsafeUpdate
  , vg_unsafeUpdate_
  , vg_accum
  , vg_accumulate
  , vg_accumulate_
  , vg_unsafeAccum
  , vg_unsafeAccumulate
  , vg_unsafeAccumulate_
  , vg_reverse
  , vg_backpermute
  , vg_unsafeBackpermute
  , vg_modify
  , vg_indexed
  , vg_map
  , vg_imap
  , vg_concatMap
  , vg_mapM
  , vg_mapM_
  , vg_forM
  , vg_forM_
  , vg_zipWith
  , vg_zipWith3
  , vg_zipWith4
  , vg_zipWith5
  , vg_zipWith6
  , vg_izipWith
  , vg_izipWith3
  , vg_izipWith4
  , vg_izipWith5
  , vg_izipWith6
  , vg_zip
  , vg_zip3
  , vg_zip4
  , vg_zip5
  , vg_zip6
  , vg_zipWithM
  , vg_zipWithM_
  , vg_unzip
  , vg_unzip3
  , vg_unzip4
  , vg_unzip5
  , vg_unzip6
  , vg_filter
  , vg_ifilter
  , vg_filterM
  , vg_takeWhile
  , vg_dropWhile
  , vg_partition
  , vg_unstablePartition
  , vg_span
  , vg_break
  , vg_elem
  , vg_notElem
  , vg_find
  , vg_findIndex
  , vg_findIndices
  , vg_elemIndex
  , vg_elemIndices
  , vg_foldl
  , vg_foldl1
  , vg_foldl'
  , vg_foldl1'
  , vg_foldr
  , vg_foldr1
  , vg_foldr'
  , vg_foldr1'
  , vg_ifoldl
  , vg_ifoldl'
  , vg_ifoldr
  , vg_ifoldr'
  , vg_all
  , vg_any
  , vg_and
  , vg_or
  , vg_sum
  , vg_product
  , vg_maximum
  , vg_maximumBy
  , vg_minimum
  , vg_minimumBy
  , vg_minIndex
  , vg_minIndexBy
  , vg_maxIndex
  , vg_maxIndexBy
  , vg_foldM
  , vg_foldM'
  , vg_fold1M
  , vg_fold1M'
  , vg_foldM_
  , vg_foldM'_
  , vg_fold1M_
  , vg_fold1M'_
  , vg_sequence
  , vg_sequence_
  , vg_prescanl
  , vg_prescanl'
  , vg_postscanl
  , vg_postscanl'
  , vg_scanl
  , vg_scanl'
  , vg_scanl1
  , vg_scanl1'
  , vg_prescanr
  , vg_prescanr'
  , vg_postscanr
  , vg_postscanr'
  , vg_scanr
  , vg_scanr'
  , vg_scanr1
  , vg_scanr1'
  , vg_toList
  , vg_fromList
  , vg_fromListN
  , vg_convert
  , vg_freeze
  , vg_thaw
  , vg_copy
  , vg_unsafeFreeze
  , vg_unsafeThaw
  , vg_unsafeCopy
  , vg_stream
  , vg_unstream
  , vg_streamR
  , vg_unstreamR
  , vg_new
  , vg_clone
  , vg_eq
  , vg_cmp
  , vg_showsPrec
  , vg_readPrec
  , vg_gfoldl
  , vg_dataCast
  , vg_mkType

    -- * Data.Vector.Generic.Mutable (vgm)
  , vgm_basicLength
  , vgm_basicUnsafeSlice
  , vgm_basicOverlaps
  , vgm_basicUnsafeNew
  , vgm_basicUnsafeReplicate
  , vgm_basicUnsafeRead
  , vgm_basicUnsafeWrite
  , vgm_basicClear
  , vgm_basicSet
  , vgm_basicUnsafeCopy
  , vgm_basicUnsafeMove
  , vgm_basicUnsafeGrow
  , vgm_length
  , vgm_null
  , vgm_slice
  , vgm_init
  , vgm_tail
  , vgm_take
  , vgm_drop
  , vgm_splitAt
  , vgm_unsafeSlice
  , vgm_unsafeInit
  , vgm_unsafeTail
  , vgm_unsafeTake
  , vgm_unsafeDrop
  , vgm_overlaps
  , vgm_new
  , vgm_unsafeNew
  , vgm_replicate
  , vgm_replicateM
  , vgm_clone
  , vgm_grow
  , vgm_unsafeGrow
  , vgm_clear
  , vgm_read
  , vgm_write
  , vgm_swap
  , vgm_unsafeRead
  , vgm_unsafeWrite
  , vgm_unsafeSwap
  , vgm_set
  , vgm_copy
  , vgm_move
  , vgm_unsafeCopy
  , vgm_unsafeMove
  , vgm_mstream
  , vgm_mstreamR
  , vgm_unstream
  , vgm_unstreamR
  , vgm_munstream
  , vgm_munstreamR
  , vgm_transform
  , vgm_transformR
  , vgm_fill
  , vgm_fillR
  , vgm_unsafeAccum
  , vgm_accum
  , vgm_unsafeUpdate
  , vgm_update
  , vgm_reverse
  , vgm_unstablePartition
  , vgm_unstablePartitionStream
  , vgm_partitionStream

    -- * Data.Vector.Generic.New (vgn)
  , VGNew
  , vgn_create
  , vgn_run
  , vgn_runPrim
  , vgn_apply
  , vgn_modify
  , vgn_modifyWithStream
  , vgn_unstream
  , vgn_transform
  , vgn_unstreamR
  , vgn_transformR
  , vgn_slice
  , vgn_init
  , vgn_tail
  , vgn_take
  , vgn_drop
  , vgn_unsafeSlice
  , vgn_unsafeInit
  , vgn_unsafeTail

    -- * Data.Vector.Mutable (vm)
  , IOVector
  , STVector
  , vm_length
  , vm_null
  , vm_slice
  , vm_init
  , vm_tail
  , vm_take
  , vm_drop
  , vm_splitAt
  , vm_unsafeSlice
  , vm_unsafeInit
  , vm_unsafeTail
  , vm_unsafeTake
  , vm_unsafeDrop
  , vm_overlaps
  , vm_new
  , vm_unsafeNew
  , vm_replicate
  , vm_replicateM
  , vm_clone
  , vm_grow
  , vm_unsafeGrow
  , vm_clear
  , vm_read
  , vm_write
  , vm_swap
  , vm_unsafeRead
  , vm_unsafeWrite
  , vm_unsafeSwap
  , vm_set
  , vm_copy
  , vm_move
  , vm_unsafeCopy
  , vm_unsafeMove

    -- * Data.Text.Lazy.Builder (vp)
  , PVector
  , PMVector
  , vp_length
  , vp_null
  , vp_get
  , vp_getq
  , vp_head
  , vp_last
  , vp_unsafeIndex
  , vp_unsafeHead
  , vp_unsafeLast
  , vp_indexM
  , vp_headM
  , vp_lastM
  , vp_unsafeIndexM
  , vp_unsafeHeadM
  , vp_unsafeLastM
  , vp_slice
  , vp_init
  , vp_tail
  , vp_take
  , vp_drop
  , vp_splitAt
  , vp_unsafeSlice
  , vp_unsafeInit
  , vp_unsafeTail
  , vp_unsafeTake
  , vp_unsafeDrop
  , vp_empty
  , vp_singleton
  , vp_replicate
  , vp_generate
  , vp_iterateN
  , vp_replicateM
  , vp_generateM
  , vp_create
  , vp_unfoldr
  , vp_unfoldrN
  , vp_constructN
  , vp_constructrN
  , vp_enumFromN
  , vp_enumFromStepN
  , vp_enumFromTo
  , vp_enumFromThenTo
  , vp_cons
  , vp_snoc
  , vp_cat
  , vp_concat
  , vp_force
  , vp_upd
  , vp_update_
  , vp_unsafeUpd
  , vp_unsafeUpdate_
  , vp_accum
  , vp_accumulate_
  , vp_unsafeAccum
  , vp_unsafeAccumulate_
  , vp_reverse
  , vp_backpermute
  , vp_unsafeBackpermute
  , vp_modify
  , vp_map
  , vp_imap
  , vp_concatMap
  , vp_mapM
  , vp_mapM_
  , vp_forM
  , vp_forM_
  , vp_zipWith
  , vp_zipWith3
  , vp_zipWith4
  , vp_zipWith5
  , vp_zipWith6
  , vp_izipWith
  , vp_izipWith3
  , vp_izipWith4
  , vp_izipWith5
  , vp_izipWith6
  , vp_zipWithM
  , vp_zipWithM_
  , vp_filter
  , vp_ifilter
  , vp_filterM
  , vp_takeWhile
  , vp_dropWhile
  , vp_partition
  , vp_unstablePartition
  , vp_span
  , vp_break
  , vp_elem
  , vp_notElem
  , vp_find
  , vp_findIndex
  , vp_findIndices
  , vp_elemIndex
  , vp_elemIndices
  , vp_foldl
  , vp_foldl1
  , vp_foldl'
  , vp_foldl1'
  , vp_foldr
  , vp_foldr1
  , vp_foldr'
  , vp_foldr1'
  , vp_ifoldl
  , vp_ifoldl'
  , vp_ifoldr
  , vp_ifoldr'
  , vp_all
  , vp_any
  , vp_sum
  , vp_product
  , vp_maximum
  , vp_maximumBy
  , vp_minimum
  , vp_minimumBy
  , vp_minIndex
  , vp_minIndexBy
  , vp_maxIndex
  , vp_maxIndexBy
  , vp_foldM
  , vp_foldM'
  , vp_fold1M
  , vp_fold1M'
  , vp_foldM_
  , vp_foldM'_
  , vp_fold1M_
  , vp_fold1M'_
  , vp_prescanl
  , vp_prescanl'
  , vp_postscanl
  , vp_postscanl'
  , vp_scanl
  , vp_scanl'
  , vp_scanl1
  , vp_scanl1'
  , vp_prescanr
  , vp_prescanr'
  , vp_postscanr
  , vp_postscanr'
  , vp_scanr
  , vp_scanr'
  , vp_scanr1
  , vp_scanr1'
  , vp_toList
  , vp_fromList
  , vp_fromListN
  , vp_convert
  , vp_freeze
  , vp_thaw
  , vp_copy
  , vp_unsafeFreeze
  , vp_unsafeThaw
  , vp_unsafeCopy

    -- * Data.Vector.Primitive.Mutable (vpm)
  , PIOVector
  , PSTVector
  , vpm_length
  , vpm_null
  , vpm_slice
  , vpm_init
  , vpm_tail
  , vpm_take
  , vpm_drop
  , vpm_splitAt
  , vpm_unsafeSlice
  , vpm_unsafeInit
  , vpm_unsafeTail
  , vpm_unsafeTake
  , vpm_unsafeDrop
  , vpm_overlaps
  , vpm_new
  , vpm_unsafeNew
  , vpm_replicate
  , vpm_replicateM
  , vpm_clone
  , vpm_grow
  , vpm_unsafeGrow
  , vpm_clear
  , vpm_read
  , vpm_write
  , vpm_swap
  , vpm_unsafeRead
  , vpm_unsafeWrite
  , vpm_unsafeSwap
  , vpm_set
  , vpm_copy
  , vpm_move
  , vpm_unsafeCopy
  , vpm_unsafeMove

    -- * Data.Vector.Storable (vs)
  , SVector
  , SMVector
  , vs_length
  , vs_null
  , vs_get
  , vs_getq
  , vs_head
  , vs_last
  , vs_unsafeIndex
  , vs_unsafeHead
  , vs_unsafeLast
  , vs_indexM
  , vs_headM
  , vs_lastM
  , vs_unsafeIndexM
  , vs_unsafeHeadM
  , vs_unsafeLastM
  , vs_slice
  , vs_init
  , vs_tail
  , vs_take
  , vs_drop
  , vs_splitAt
  , vs_unsafeSlice
  , vs_unsafeInit
  , vs_unsafeTail
  , vs_unsafeTake
  , vs_unsafeDrop
  , vs_empty
  , vs_singleton
  , vs_replicate
  , vs_generate
  , vs_iterateN
  , vs_replicateM
  , vs_generateM
  , vs_create
  , vs_unfoldr
  , vs_unfoldrN
  , vs_constructN
  , vs_constructrN
  , vs_enumFromN
  , vs_enumFromStepN
  , vs_enumFromTo
  , vs_enumFromThenTo
  , vs_cons
  , vs_snoc
  , vs_cat
  , vs_concat
  , vs_force
  , vs_upd
  , vs_update_
  , vs_unsafeUpd
  , vs_unsafeUpdate_
  , vs_accum
  , vs_accumulate_
  , vs_unsafeAccum
  , vs_unsafeAccumulate_
  , vs_reverse
  , vs_backpermute
  , vs_unsafeBackpermute
  , vs_modify
  , vs_map
  , vs_imap
  , vs_concatMap
  , vs_mapM
  , vs_mapM_
  , vs_forM
  , vs_forM_
  , vs_zipWith
  , vs_zipWith3
  , vs_zipWith4
  , vs_zipWith5
  , vs_zipWith6
  , vs_izipWith
  , vs_izipWith3
  , vs_izipWith4
  , vs_izipWith5
  , vs_izipWith6
  , vs_zipWithM
  , vs_zipWithM_
  , vs_filter
  , vs_ifilter
  , vs_filterM
  , vs_takeWhile
  , vs_dropWhile
  , vs_partition
  , vs_unstablePartition
  , vs_span
  , vs_break
  , vs_elem
  , vs_notElem
  , vs_find
  , vs_findIndex
  , vs_findIndices
  , vs_elemIndex
  , vs_elemIndices
  , vs_foldl
  , vs_foldl1
  , vs_foldl'
  , vs_foldl1'
  , vs_foldr
  , vs_foldr1
  , vs_foldr'
  , vs_foldr1'
  , vs_ifoldl
  , vs_ifoldl'
  , vs_ifoldr
  , vs_ifoldr'
  , vs_all
  , vs_any
  , vs_and
  , vs_or
  , vs_sum
  , vs_product
  , vs_maximum
  , vs_maximumBy
  , vs_minimum
  , vs_minimumBy
  , vs_minIndex
  , vs_minIndexBy
  , vs_maxIndex
  , vs_maxIndexBy
  , vs_foldM
  , vs_foldM'
  , vs_fold1M
  , vs_fold1M'
  , vs_foldM_
  , vs_foldM'_
  , vs_fold1M_
  , vs_fold1M'_
  , vs_prescanl
  , vs_prescanl'
  , vs_postscanl
  , vs_postscanl'
  , vs_scanl
  , vs_scanl'
  , vs_scanl1
  , vs_scanl1'
  , vs_prescanr
  , vs_prescanr'
  , vs_postscanr
  , vs_postscanr'
  , vs_scanr
  , vs_scanr'
  , vs_scanr1
  , vs_scanr1'
  , vs_toList
  , vs_fromList
  , vs_fromListN
  , vs_convert
  , vs_unsafeCast
  , vs_freeze
  , vs_thaw
  , vs_copy
  , vs_unsafeFreeze
  , vs_unsafeThaw
  , vs_unsafeCopy
  , vs_unsafeFromForeignPtr
  , vs_unsafeFromForeignPtr0
  , vs_unsafeToForeignPtr
  , vs_unsafeToForeignPtr0
  , vs_unsafeWith

    -- * Data.Vector.Storable.Mutable (vsm)
  , SIOVector
  , SSTVector
  , vsm_length
  , vsm_null
  , vsm_slice
  , vsm_init
  , vsm_tail
  , vsm_take
  , vsm_drop
  , vsm_splitAt
  , vsm_unsafeSlice
  , vsm_unsafeInit
  , vsm_unsafeTail
  , vsm_unsafeTake
  , vsm_unsafeDrop
  , vsm_overlaps
  , vsm_new
  , vsm_unsafeNew
  , vsm_replicate
  , vsm_replicateM
  , vsm_clone
  , vsm_grow
  , vsm_unsafeGrow
  , vsm_clear
  , vsm_read
  , vsm_write
  , vsm_swap
  , vsm_unsafeRead
  , vsm_unsafeWrite
  , vsm_unsafeSwap
  , vsm_set
  , vsm_copy
  , vsm_move
  , vsm_unsafeCopy
  , vsm_unsafeMove
  , vsm_unsafeCast
  , vsm_unsafeFromForeignPtr
  , vsm_unsafeFromForeignPtr0
  , vsm_unsafeToForeignPtr
  , vsm_unsafeToForeignPtr0
  , vsm_unsafeWith

    -- * Data.Vector.Unboxed (vu)
  , UVector
  , UMVector
  , vu_length
  , vu_null
  , vu_get
  , vu_getq
  , vu_head
  , vu_last
  , vu_unsafeIndex
  , vu_unsafeHead
  , vu_unsafeLast
  , vu_indexM
  , vu_headM
  , vu_lastM
  , vu_unsafeIndexM
  , vu_unsafeHeadM
  , vu_unsafeLastM
  , vu_slice
  , vu_init
  , vu_tail
  , vu_take
  , vu_drop
  , vu_splitAt
  , vu_unsafeSlice
  , vu_unsafeInit
  , vu_unsafeTail
  , vu_unsafeTake
  , vu_unsafeDrop
  , vu_empty
  , vu_singleton
  , vu_replicate
  , vu_generate
  , vu_iterateN
  , vu_replicateM
  , vu_generateM
  , vu_create
  , vu_unfoldr
  , vu_unfoldrN
  , vu_constructN
  , vu_constructrN
  , vu_enumFromN
  , vu_enumFromStepN
  , vu_enumFromTo
  , vu_enumFromThenTo
  , vu_cons
  , vu_snoc
  , vu_cat
  , vu_concat
  , vu_force
  , vu_upd
  , vu_update
  , vu_update_
  , vu_unsafeUpd
  , vu_unsafeUpdate
  , vu_unsafeUpdate_
  , vu_accum
  , vu_accumulate
  , vu_accumulate_
  , vu_unsafeAccum
  , vu_unsafeAccumulate
  , vu_unsafeAccumulate_
  , vu_reverse
  , vu_backpermute
  , vu_unsafeBackpermute
  , vu_modify
  , vu_indexed
  , vu_map
  , vu_imap
  , vu_concatMap
  , vu_mapM
  , vu_mapM_
  , vu_forM
  , vu_forM_
  , vu_zipWith
  , vu_zipWith3
  , vu_zipWith4
  , vu_zipWith5
  , vu_zipWith6
  , vu_izipWith
  , vu_izipWith3
  , vu_izipWith4
  , vu_izipWith5
  , vu_izipWith6
  , vu_zip
  , vu_zip3
  , vu_zip4
  , vu_zip5
  , vu_zip6
  , vu_zipWithM
  , vu_zipWithM_
  , vu_unzip
  , vu_unzip3
  , vu_unzip4
  , vu_unzip5
  , vu_unzip6
  , vu_filter
  , vu_ifilter
  , vu_filterM
  , vu_takeWhile
  , vu_dropWhile
  , vu_partition
  , vu_unstablePartition
  , vu_span
  , vu_break
  , vu_elem
  , vu_notElem
  , vu_find
  , vu_findIndex
  , vu_findIndices
  , vu_elemIndex
  , vu_elemIndices
  , vu_foldl
  , vu_foldl1
  , vu_foldl'
  , vu_foldl1'
  , vu_foldr
  , vu_foldr1
  , vu_foldr'
  , vu_foldr1'
  , vu_ifoldl
  , vu_ifoldl'
  , vu_ifoldr
  , vu_ifoldr'
  , vu_all
  , vu_any
  , vu_and
  , vu_or
  , vu_sum
  , vu_product
  , vu_maximum
  , vu_maximumBy
  , vu_minimum
  , vu_minimumBy
  , vu_minIndex
  , vu_minIndexBy
  , vu_maxIndex
  , vu_maxIndexBy
  , vu_foldM
  , vu_foldM'
  , vu_fold1M
  , vu_fold1M'
  , vu_foldM_
  , vu_foldM'_
  , vu_fold1M_
  , vu_fold1M'_
  , vu_prescanl
  , vu_prescanl'
  , vu_postscanl
  , vu_postscanl'
  , vu_scanl
  , vu_scanl'
  , vu_scanl1
  , vu_scanl1'
  , vu_prescanr
  , vu_prescanr'
  , vu_postscanr
  , vu_postscanr'
  , vu_scanr
  , vu_scanr'
  , vu_scanr1
  , vu_scanr1'
  , vu_toList
  , vu_fromList
  , vu_fromListN
  , vu_convert
  , vu_freeze
  , vu_thaw
  , vu_copy
  , vu_unsafeFreeze
  , vu_unsafeThaw
  , vu_unsafeCopy

    -- * Data.Vector.Unboxed.Mutable (vum)
  , UIOVector
  , USTVector
  , vum_length
  , vum_null
  , vum_slice
  , vum_init
  , vum_tail
  , vum_take
  , vum_drop
  , vum_splitAt
  , vum_unsafeSlice
  , vum_unsafeInit
  , vum_unsafeTail
  , vum_unsafeTake
  , vum_unsafeDrop
  , vum_overlaps
  , vum_new
  , vum_unsafeNew
  , vum_replicate
  , vum_replicateM
  , vum_clone
  , vum_grow
  , vum_unsafeGrow
  , vum_clear
  , vum_zip
  , vum_zip3
  , vum_zip4
  , vum_zip5
  , vum_zip6
  , vum_unzip
  , vum_unzip3
  , vum_unzip4
  , vum_unzip5
  , vum_unzip6
  , vum_read
  , vum_write
  , vum_swap
  , vum_unsafeRead
  , vum_unsafeWrite
  , vum_unsafeSwap
  , vum_set
  , vum_copy
  , vum_move
  , vum_unsafeCopy
  , vum_unsafeMove
  ) where

import Prelude (Char, Either, Maybe, String)
import Data.Word (Word8)
import Control.Monad.ST.Safe (RealWorld)

import qualified Data.Array.IArray                   as ArrayI
import qualified Data.Array.IO                       as ArrayIO
import qualified Data.Array.MArray                   as ArrayM
import qualified Data.Array.MArray.Safe              as ArrayMS
import qualified Data.Array.ST                       as ArrayST
import qualified Data.Array.Storable                 as ArraySR
import qualified Data.Array.Unboxed                  as ArrayUB
import qualified Data.Array.Unsafe                   as ArrayUS
import qualified Data.Binary                         as Binary
import qualified Data.Binary.Builder                 as BinaryBuilder
import qualified Data.Binary.Get                     as BinaryGet
import qualified Data.Binary.Put                     as BinaryPut
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as BC
import qualified Data.ByteString.Lazy                as BL
import qualified Data.ByteString.Lazy.Builder        as BLB
import qualified Data.ByteString.Lazy.Builder.ASCII  as BLBA
import qualified Data.ByteString.Lazy.Builder.Extras as BLBE
import qualified Data.ByteString.Lazy.Char8          as BLC
import qualified Data.ByteString.Unsafe              as BU
import qualified Data.CaseInsensitive                as CI
import qualified Data.HashMap.Lazy                   as HashMapL
import qualified Data.HashMap.Strict                 as HashMapS
import qualified Data.HashSet                        as HashSet
import qualified Data.IntMap.Lazy                    as IntMapL
import qualified Data.IntMap.Strict                  as IntMapS
import qualified Data.IntSet                         as IntSet
import qualified Data.Map.Lazy                       as MapL
import qualified Data.Map.Strict                     as MapS
import qualified Data.Set                            as Set
import qualified Data.Sequence                       as Seq
import qualified Data.Text                           as T
import qualified Data.Text.Array                     as TA
import qualified Data.Text.Encoding                  as TE
import qualified Data.Text.Encoding.Error            as TEE
import qualified Data.Text.Foreign                   as TF
import qualified Data.Text.IO                        as TIO
import qualified Data.Text.Lazy                      as TL
import qualified Data.Text.Lazy.Builder              as TLB
import qualified Data.Text.Lazy.Builder.Int          as TLBI
import qualified Data.Text.Lazy.Builder.RealFloat    as TLBR
import qualified Data.Text.Lazy.Encoding             as TLE
import qualified Data.Text.Lazy.IO                   as TLIO
import qualified Data.Text.Lazy.Read                 as TLR
import qualified Data.Text.Read                      as TR
import qualified Data.Text.Unsafe                    as TU
import qualified Data.Tree                           as Tree
import qualified Data.Vector                         as V
import qualified Data.Vector.Fusion.Stream           as VF
import qualified Data.Vector.Fusion.Stream.Monadic   as VFM
import qualified Data.Vector.Fusion.Stream.Size      as VFS
import qualified Data.Vector.Fusion.Util             as VFU
import qualified Data.Vector.Generic                 as VG
import qualified Data.Vector.Generic.Mutable         as VGM
import qualified Data.Vector.Generic.New             as VGN
import qualified Data.Vector.Mutable                 as VM
import qualified Data.Vector.Primitive               as VP
import qualified Data.Vector.Primitive.Mutable       as VPM
import qualified Data.Vector.Storable                as VS
import qualified Data.Vector.Storable.Mutable        as VSM
import qualified Data.Vector.Unboxed                 as VU
import qualified Data.Vector.Unboxed.Mutable         as VUM

-- Data.Array.IArray
-- |'ArrayI.Array'
type Array = ArrayI.Array
-- |'ArrayI.bounds'
a_bounds = ArrayI.bounds
-- |'ArrayI.array'
a_array = ArrayI.array
-- |'ArrayI.listArray'
a_listArray = ArrayI.listArray
-- |'ArrayI.accumArray'
a_accumArray = ArrayI.accumArray
-- |('ArrayI.!')
a_get = (ArrayI.!)
-- |'ArrayI.indices'
a_indices = ArrayI.indices
-- |'ArrayI.elems'
a_elems = ArrayI.elems
-- |'ArrayI.assocs'
a_assocs = ArrayI.assocs
-- |('ArrayI.//')
a_upd = (ArrayI.//)
-- |'ArrayI.accum'
a_accum = ArrayI.accum
-- |'ArrayI.amap'
a_amap = ArrayI.amap
-- |'ArrayI.ixmap'
a_ixmap = ArrayI.ixmap

-- Data.Array.IO
-- |'ArrayIO.IOArray'
type IOArray  = ArrayIO.IOArray
-- |'ArrayIO.IOUArray'
type IOUArray = ArrayIO.IOUArray
-- |'ArrayIO.hGetArray'
a_hGetArray = ArrayIO.hGetArray
-- |'ArrayIO.hPutArray'
a_hPutArray = ArrayIO.hPutArray

-- Data.Array.MArray
-- |'ArrayM.getBounds'
a_getBounds = ArrayM.getBounds
-- |'ArrayM.newArray'
a_newArray = ArrayM.newArray
-- |'ArrayM.newArray_'
a_newArray_ = ArrayM.newArray_
-- |'ArrayM.newListArray'
a_newListArray = ArrayM.newListArray
-- |'ArrayM.readArray'
a_readArray = ArrayM.readArray
-- |'ArrayM.writeArray'
a_writeArray = ArrayM.writeArray
-- |'ArrayM.mapArray'
a_mapArray = ArrayM.mapArray
-- |'ArrayM.mapIndices'
a_mapIndices = ArrayM.mapIndices
-- |'ArrayM.getElems'
a_getElems = ArrayM.getElems
-- |'ArrayM.getAssocs'
a_getAssocs = ArrayM.getAssocs
-- |'ArrayM.freeze'
a_freeze = ArrayM.freeze
-- |'ArrayM.thaw'
a_thaw = ArrayM.thaw

-- Data.Array.MArray.Safe
-- |'ArrayMS.getBounds'
as_getBounds = ArrayMS.getBounds
-- |'ArrayMS.newArray'
as_newArray = ArrayMS.newArray
-- |'ArrayMS.newArray_'
as_newArray_ = ArrayMS.newArray_
-- |'ArrayMS.newListArray'
as_newListArray = ArrayMS.newListArray
-- |'ArrayMS.readArray'
as_readArray = ArrayMS.readArray
-- |'ArrayMS.writeArray'
as_writeArray = ArrayMS.writeArray
-- |'ArrayMS.mapArray'
as_mapArray = ArrayMS.mapArray
-- |'ArrayMS.mapIndices'
as_mapIndices = ArrayMS.mapIndices
-- |'ArrayMS.getElems'
as_getElems = ArrayMS.getElems
-- |'ArrayMS.getAssocs'
as_getAssocs = ArrayMS.getAssocs
-- |'ArrayMS.freeze'
as_freeze = ArrayMS.freeze
-- |'ArrayMS.thaw'
as_thaw = ArrayMS.thaw

-- Data.Array.ST
-- |'ArrayST.STArray'
type STArray  = ArrayST.STArray
-- |'ArrayST.STUArray'
type STUArray = ArrayST.STUArray
-- |'ArrayST.runSTArray'
a_runSTArray = ArrayST.runSTArray
-- |'ArrayST.runSTUArray'
a_runSTUArray = ArrayST.runSTUArray

-- Data.Array.Storable
-- |'ArraySR.StorableArray'
type StorableArray = ArraySR.StorableArray
-- |'ArraySR.withStorableArray'
a_withStorableArray = ArraySR.withStorableArray
-- |'ArraySR.touchStorableArray'
a_touchStorableArray = ArraySR.touchStorableArray

-- Data.Array.Unboxed
-- |'ArrayUB.UArray'
type UArray = ArrayUB.UArray

-- Data.Array.Unsafe
-- |'ArrayUS.castSTUArray'
a_castSTUArray = ArrayUS.castSTUArray
-- |'ArrayUS.castIOUArray'
a_castIOUArray = ArrayUS.castIOUArray
-- |'ArrayUS.unsafeFreeze'
a_unsafeFreeze = ArrayUS.unsafeFreeze
-- |'ArrayUS.unsafeThaw'
a_unsafeThaw = ArrayUS.unsafeThaw
-- |'ArrayUS.unsafeForeignPtrToStorableArray'
a_unsafeForeignPtrToStorableArray = ArrayUS.unsafeForeignPtrToStorableArray

-- Data.Binary
-- |'Binary.Get'
type BinGet = Binary.Get
-- |'Binary.Put'
type BinPut = Binary.Put
-- |'Binary.put'
bin_put = Binary.put
-- |'Binary.get'
bin_get = Binary.get
-- |'Binary.encode'
bin_encode = Binary.encode
-- |'Binary.decode'
bin_decode = Binary.decode
-- |'Binary.encodeFile'
bin_encodeFile = Binary.encodeFile
-- |'Binary.decodeFile'
bin_decodeFile = Binary.decodeFile

-- Data.Binary.Builder
-- |'BinaryBuilder.Builder'
type BinBuilder = BinaryBuilder.Builder
-- |'BinaryBuilder.toLazyByteString'
binb_toLazyByteString = BinaryBuilder.toLazyByteString
-- |'BinaryBuilder.empty'
binb_empty = BinaryBuilder.empty
-- |'BinaryBuilder.singleton'
binb_singleton = BinaryBuilder.singleton
-- |'BinaryBuilder.append'
binb_append = BinaryBuilder.append
-- |'BinaryBuilder.fromByteString'
binb_fromByteString = BinaryBuilder.fromByteString
-- |'BinaryBuilder.fromLazyByteString'
binb_fromLazyByteString = BinaryBuilder.fromLazyByteString
-- |'BinaryBuilder.flush'
binb_flush = BinaryBuilder.flush
-- |'BinaryBuilder.putWord16be'
binb_putWord16be = BinaryBuilder.putWord16be
-- |'BinaryBuilder.putWord32be'
binb_putWord32be = BinaryBuilder.putWord32be
-- |'BinaryBuilder.putWord64be'
binb_putWord64be = BinaryBuilder.putWord64be
-- |'BinaryBuilder.putWord16le'
binb_putWord16le = BinaryBuilder.putWord16le
-- |'BinaryBuilder.putWord32le'
binb_putWord32le = BinaryBuilder.putWord32le
-- |'BinaryBuilder.putWord64le'
binb_putWord64le = BinaryBuilder.putWord64le
-- |'BinaryBuilder.putWordhost'
binb_putWordhost = BinaryBuilder.putWordhost
-- |'BinaryBuilder.putWord16host'
binb_putWord16host = BinaryBuilder.putWord16host
-- |'BinaryBuilder.putWord32host'
binb_putWord32host = BinaryBuilder.putWord32host
-- |'BinaryBuilder.putWord64host'
binb_putWord64host = BinaryBuilder.putWord64host
-- |'BinaryBuilder.putCharUtf8'
binb_putCharUtf8 = BinaryBuilder.putCharUtf8

-- Data.Binary.Get
-- |'BinaryGet.runGet'
bin_runGet = BinaryGet.runGet
-- |'BinaryGet.runGetState'
bin_runGetState = BinaryGet.runGetState
-- |'BinaryGet.skip'
bin_skip = BinaryGet.skip
-- |'BinaryGet.uncheckedSkip'
-- bin_uncheckedSkip = BinaryGet.uncheckedSkip
-- |'BinaryGet.lookAhead'
bin_lookAhead = BinaryGet.lookAhead
-- |'BinaryGet.lookAheadM'
bin_lookAheadM = BinaryGet.lookAheadM
-- |'BinaryGet.lookAheadE'
bin_lookAheadE = BinaryGet.lookAheadE
-- |'BinaryGet.uncheckedLookAhead'
-- bin_uncheckedLookAhead = BinaryGet.uncheckedLookAhead
-- |'BinaryGet.bytesRead'
bin_bytesRead = BinaryGet.bytesRead
-- |'BinaryGet.getBytes'
bin_getBytes = BinaryGet.getBytes
-- |'BinaryGet.remaining'
bin_remaining = BinaryGet.remaining
-- |'BinaryGet.isEmpty'
bin_isEmpty = BinaryGet.isEmpty
-- |'BinaryGet.getWord8'
bin_getWord8 = BinaryGet.getWord8
-- |'BinaryGet.getByteString'
bin_getByteString = BinaryGet.getByteString
-- |'BinaryGet.getLazyByteString'
bin_getLazyByteString = BinaryGet.getLazyByteString
-- |'BinaryGet.getLazyByteStringNul'
bin_getLazyByteStringNul = BinaryGet.getLazyByteStringNul
-- |'BinaryGet.getRemainingLazyByteString'
bin_getRemainingLazyByteString = BinaryGet.getRemainingLazyByteString
-- |'BinaryGet.getWord16be'
bin_getWord16be = BinaryGet.getWord16be
-- |'BinaryGet.getWord32be'
bin_getWord32be = BinaryGet.getWord32be
-- |'BinaryGet.getWord64be'
bin_getWord64be = BinaryGet.getWord64be
-- |'BinaryGet.getWord16le'
bin_getWord16le = BinaryGet.getWord16le
-- |'BinaryGet.getWord32le'
bin_getWord32le = BinaryGet.getWord32le
-- |'BinaryGet.getWord64le'
bin_getWord64le = BinaryGet.getWord64le
-- |'BinaryGet.getWordhost'
bin_getWordhost = BinaryGet.getWordhost
-- |'BinaryGet.getWord16host'
bin_getWord16host = BinaryGet.getWord16host
-- |'BinaryGet.getWord32host'
bin_getWord32host = BinaryGet.getWord32host
-- |'BinaryGet.getWord64host'
bin_getWord64host = BinaryGet.getWord64host

-- Data.Binary.Put
-- |'BinaryPut.PutM'
type BinPutM = BinaryPut.PutM
-- |'BinaryPut.unPut'
bin_unPut = BinaryPut.unPut
-- |'BinaryPut.runPut'
bin_runPut = BinaryPut.runPut
-- |'BinaryPut.runPutM'
bin_runPutM = BinaryPut.runPutM
-- |'BinaryPut.putBuilder'
bin_putBuilder = BinaryPut.putBuilder
-- |'BinaryPut.execPut'
bin_execPut = BinaryPut.execPut
-- |'BinaryPut.flush'
bin_flush = BinaryPut.flush
-- |'BinaryPut.putWord8'
bin_putWord8 = BinaryPut.putWord8
-- |'BinaryPut.putByteString'
bin_putByteString = BinaryPut.putByteString
-- |'BinaryPut.putLazyByteString'
bin_putLazyByteString = BinaryPut.putLazyByteString
-- |'BinaryPut.putWord16be'
bin_putWord16be = BinaryPut.putWord16be
-- |'BinaryPut.putWord32be'
bin_putWord32be = BinaryPut.putWord32be
-- |'BinaryPut.putWord64be'
bin_putWord64be = BinaryPut.putWord64be
-- |'BinaryPut.putWord16le'
bin_putWord16le = BinaryPut.putWord16le
-- |'BinaryPut.putWord32le'
bin_putWord32le = BinaryPut.putWord32le
-- |'BinaryPut.putWord64le'
bin_putWord64le = BinaryPut.putWord64le
-- |'BinaryPut.putWordhost'
bin_putWordhost = BinaryPut.putWordhost
-- |'BinaryPut.putWord16host'
bin_putWord16host = BinaryPut.putWord16host
-- |'BinaryPut.putWord32host'
bin_putWord32host = BinaryPut.putWord32host
-- |'BinaryPut.putWord64host'
bin_putWord64host = BinaryPut.putWord64host

-- Data.ByteString
-- |'B.ByteString'
type ByteString = B.ByteString
-- |'B.empty'
b_empty = B.empty
-- |'B.singleton'
b_singleton = B.singleton
-- |'B.pack'
b_pack = B.pack
-- |'B.unpack'
b_unpack = B.unpack
-- |'B.cons'
b_cons = B.cons
-- |'B.snoc'
b_snoc = B.snoc
-- |'B.append'
b_append = B.append
-- |'B.head'
b_head = B.head
-- |'B.uncons'
b_uncons = B.uncons
-- |'B.last'
b_last = B.last
-- |'B.tail'
b_tail = B.tail
-- |'B.init'
b_init = B.init
-- |'B.null'
b_null = B.null
-- |'B.length'
b_length = B.length
-- |'B.map'
b_map = B.map
-- |'B.reverse'
b_reverse = B.reverse
-- |'B.intersperse'
b_intersperse = B.intersperse
-- |'B.intercalate'
b_intercalate = B.intercalate
-- |'B.transpose'
b_transpose = B.transpose
-- |'B.foldl'
b_foldl = B.foldl
-- |'B.foldl''
b_foldl' = B.foldl'
-- |'B.foldl1'
b_foldl1 = B.foldl1
-- |'B.foldl1''
b_foldl1' = B.foldl1'
-- |'B.foldr'
b_foldr = B.foldr
-- |'B.foldr''
b_foldr' = B.foldr'
-- |'B.foldr1'
b_foldr1 = B.foldr1
-- |'B.foldr1''
b_foldr1' = B.foldr1'
-- |'B.concat'
b_concat = B.concat
-- |'B.concatMap'
b_concatMap = B.concatMap
-- |'B.any'
b_any = B.any
-- |'B.all'
b_all = B.all
-- |'B.maximum'
b_maximum = B.maximum
-- |'B.minimum'
b_minimum = B.minimum
-- |'B.scanl'
b_scanl = B.scanl
-- |'B.scanl1'
b_scanl1 = B.scanl1
-- |'B.scanr'
b_scanr = B.scanr
-- |'B.scanr1'
b_scanr1 = B.scanr1
-- |'B.mapAccumL'
b_mapAccumL = B.mapAccumL
-- |'B.mapAccumR'
b_mapAccumR = B.mapAccumR
-- |'B.replicate'
b_replicate = B.replicate
-- |'B.unfoldr'
b_unfoldr = B.unfoldr
-- |'B.unfoldrN'
b_unfoldrN = B.unfoldrN
-- |'B.take'
b_take = B.take
-- |'B.drop'
b_drop = B.drop
-- |'B.splitAt'
b_splitAt = B.splitAt
-- |'B.takeWhile'
b_takeWhile = B.takeWhile
-- |'B.dropWhile'
b_dropWhile = B.dropWhile
-- |'B.span'
b_span = B.span
-- |'B.spanEnd'
b_spanEnd = B.spanEnd
-- |'B.break'
b_break = B.break
-- |'B.breakEnd'
b_breakEnd = B.breakEnd
-- |'B.group'
b_group = B.group
-- |'B.groupBy'
b_groupBy = B.groupBy
-- |'B.inits'
b_inits = B.inits
-- |'B.tails'
b_tails = B.tails
-- |'B.split'
b_split = B.split
-- |'B.splitWith'
b_splitWith = B.splitWith
-- |'B.isPrefixOf'
b_isPrefixOf = B.isPrefixOf
-- |'B.isSuffixOf'
b_isSuffixOf = B.isSuffixOf
-- |'B.isInfixOf'
b_isInfixOf = B.isInfixOf
-- |'B.breakSubstring'
b_breakSubstring = B.breakSubstring
-- |'B.elem'
b_elem = B.elem
-- |'B.notElem'
b_notElem = B.notElem
-- |'B.find'
b_find = B.find
-- |'B.filter'
b_filter = B.filter
-- |'B.partition'
b_partition = B.partition
-- |'B.index'
b_index = B.index
-- |'B.elemIndex'
b_elemIndex = B.elemIndex
-- |'B.elemIndices'
b_elemIndices = B.elemIndices
-- |'B.elemIndexEnd'
b_elemIndexEnd = B.elemIndexEnd
-- |'B.findIndex'
b_findIndex = B.findIndex
-- |'B.findIndices'
b_findIndices = B.findIndices
-- |'B.count'
b_count = B.count
-- |'B.zip'
b_zip = B.zip
-- |'B.zipWith'
b_zipWith = B.zipWith
-- |'B.unzip'
b_unzip = B.unzip
-- |'B.sort'
b_sort = B.sort
-- |'B.copy'
b_copy = B.copy
-- |'B.packCString'
b_packCString = B.packCString
-- |'B.packCStringLen'
b_packCStringLen = B.packCStringLen
-- |'B.useAsCString'
b_useAsCString = B.useAsCString
-- |'B.useAsCStringLen'
b_useAsCStringLen = B.useAsCStringLen
-- |'B.getLine'
b_getLine = B.getLine
-- |'B.getContents'
b_getContents = B.getContents
-- |'B.putStr'
b_putStr = B.putStr
-- |'B.interact'
b_interact = B.interact
-- |'B.readFile'
b_readFile = B.readFile
-- |'B.writeFile'
b_writeFile = B.writeFile
-- |'B.appendFile'
b_appendFile = B.appendFile
-- |'B.hGetLine'
b_hGetLine = B.hGetLine
-- |'B.hGetContents'
b_hGetContents = B.hGetContents
-- |'B.hGet'
b_hGet = B.hGet
-- |'B.hGetSome'
b_hGetSome = B.hGetSome
-- |'B.hGetNonBlocking'
b_hGetNonBlocking = B.hGetNonBlocking
-- |'B.hPut'
b_hPut = B.hPut
-- |'B.hPutNonBlocking'
b_hPutNonBlocking = B.hPutNonBlocking
-- |'B.hPutStr'
b_hPutStr = B.hPutStr
-- |'B.breakByte'
b_breakByte = B.breakByte

-- Data.ByteString.Char8
-- |'BC.ByteString'
type ByteStringChar8 = BC.ByteString
-- |'BC.empty'
bc_empty = BC.empty
-- |'BC.singleton'
bc_singleton = BC.singleton
-- |'BC.pack'
bc_pack = BC.pack
-- |'BC.unpack'
bc_unpack = BC.unpack
-- |'BC.cons'
bc_cons = BC.cons
-- |'BC.snoc'
bc_snoc = BC.snoc
-- |'BC.append'
bc_append = BC.append
-- |'BC.head'
bc_head = BC.head
-- |'BC.uncons'
bc_uncons = BC.uncons
-- |'BC.last'
bc_last = BC.last
-- |'BC.tail'
bc_tail = BC.tail
-- |'BC.init'
bc_init = BC.init
-- |'BC.null'
bc_null = BC.null
-- |'BC.length'
bc_length = BC.length
-- |'BC.map'
bc_map = BC.map
-- |'BC.reverse'
bc_reverse = BC.reverse
-- |'BC.intersperse'
bc_intersperse = BC.intersperse
-- |'BC.intercalate'
bc_intercalate = BC.intercalate
-- |'BC.transpose'
bc_transpose = BC.transpose
-- |'BC.foldl'
bc_foldl = BC.foldl
-- |'BC.foldl''
bc_foldl' = BC.foldl'
-- |'BC.foldl1'
bc_foldl1 = BC.foldl1
-- |'BC.foldl1''
bc_foldl1' = BC.foldl1'
-- |'BC.foldr'
bc_foldr = BC.foldr
-- |'BC.foldr''
bc_foldr' = BC.foldr'
-- |'BC.foldr1'
bc_foldr1 = BC.foldr1
-- |'BC.foldr1''
bc_foldr1' = BC.foldr1'
-- |'BC.concat'
bc_concat = BC.concat
-- |'BC.concatMap'
bc_concatMap = BC.concatMap
-- |'BC.any'
bc_any = BC.any
-- |'BC.all'
bc_all = BC.all
-- |'BC.maximum'
bc_maximum = BC.maximum
-- |'BC.minimum'
bc_minimum = BC.minimum
-- |'BC.scanl'
bc_scanl = BC.scanl
-- |'BC.scanl1'
bc_scanl1 = BC.scanl1
-- |'BC.scanr'
bc_scanr = BC.scanr
-- |'BC.scanr1'
bc_scanr1 = BC.scanr1
-- |'BC.mapAccumL'
bc_mapAccumL = BC.mapAccumL
-- |'BC.mapAccumR'
bc_mapAccumR = BC.mapAccumR
-- |'BC.replicate'
bc_replicate = BC.replicate
-- |'BC.unfoldr'
bc_unfoldr = BC.unfoldr
-- |'BC.unfoldrN'
bc_unfoldrN = BC.unfoldrN
-- |'BC.take'
bc_take = BC.take
-- |'BC.drop'
bc_drop = BC.drop
-- |'BC.splitAt'
bc_splitAt = BC.splitAt
-- |'BC.takeWhile'
bc_takeWhile = BC.takeWhile
-- |'BC.dropWhile'
bc_dropWhile = BC.dropWhile
-- |'BC.span'
bc_span = BC.span
-- |'BC.spanEnd'
bc_spanEnd = BC.spanEnd
-- |'BC.break'
bc_break = BC.break
-- |'BC.breakEnd'
bc_breakEnd = BC.breakEnd
-- |'BC.group'
bc_group = BC.group
-- |'BC.groupBy'
bc_groupBy = BC.groupBy
-- |'BC.inits'
bc_inits = BC.inits
-- |'BC.tails'
bc_tails = BC.tails
-- |'BC.split'
bc_split = BC.split
-- |'BC.splitWith'
bc_splitWith = BC.splitWith
-- |'BC.lines'
bc_lines = BC.lines
-- |'BC.words'
bc_words = BC.words
-- |'BC.unlines'
bc_unlines = BC.unlines
-- |'BC.unwords'
bc_unwords = BC.unwords
-- |'BC.isPrefixOf'
bc_isPrefixOf = BC.isPrefixOf
-- |'BC.isSuffixOf'
bc_isSuffixOf = BC.isSuffixOf
-- |'BC.isInfixOf'
bc_isInfixOf = BC.isInfixOf
-- |'BC.breakSubstring'
bc_breakSubstring = BC.breakSubstring
-- |'BC.elem'
bc_elem = BC.elem
-- |'BC.notElem'
bc_notElem = BC.notElem
-- |'BC.find'
bc_find = BC.find
-- |'BC.filter'
bc_filter = BC.filter
-- |'BC.index'
bc_index = BC.index
-- |'BC.elemIndex'
bc_elemIndex = BC.elemIndex
-- |'BC.elemIndices'
bc_elemIndices = BC.elemIndices
-- |'BC.elemIndexEnd'
bc_elemIndexEnd = BC.elemIndexEnd
-- |'BC.findIndex'
bc_findIndex = BC.findIndex
-- |'BC.findIndices'
bc_findIndices = BC.findIndices
-- |'BC.count'
bc_count = BC.count
-- |'BC.zip'
bc_zip = BC.zip
-- |'BC.zipWith'
bc_zipWith = BC.zipWith
-- |'BC.unzip'
bc_unzip = BC.unzip
-- |'BC.sort'
bc_sort = BC.sort
-- |'BC.readInt'
bc_readInt = BC.readInt
-- |'BC.readInteger'
bc_readInteger = BC.readInteger
-- |'BC.copy'
bc_copy = BC.copy
-- |'BC.packCString'
bc_packCString = BC.packCString
-- |'BC.packCStringLen'
bc_packCStringLen = BC.packCStringLen
-- |'BC.useAsCString'
bc_useAsCString = BC.useAsCString
-- |'BC.useAsCStringLen'
bc_useAsCStringLen = BC.useAsCStringLen
-- |'BC.getLine'
bc_getLine = BC.getLine
-- |'BC.getContents'
bc_getContents = BC.getContents
-- |'BC.putStr'
bc_putStr = BC.putStr
-- |'BC.putStrLn'
bc_putStrLn = BC.putStrLn
-- |'BC.interact'
bc_interact = BC.interact
-- |'BC.readFile'
bc_readFile = BC.readFile
-- |'BC.writeFile'
bc_writeFile = BC.writeFile
-- |'BC.appendFile'
bc_appendFile = BC.appendFile
-- |'BC.hGetLine'
bc_hGetLine = BC.hGetLine
-- |'BC.hGetContents'
bc_hGetContents = BC.hGetContents
-- |'BC.hGet'
bc_hGet = BC.hGet
-- |'BC.hGetNonBlocking'
bc_hGetNonBlocking = BC.hGetNonBlocking
-- |'BC.hPut'
bc_hPut = BC.hPut
-- |'BC.hPutNonBlocking'
bc_hPutNonBlocking = BC.hPutNonBlocking
-- |'BC.hPutStr'
bc_hPutStr = BC.hPutStr
-- |'BC.hPutStrLn'
bc_hPutStrLn = BC.hPutStrLn

-- |'BL.ByteString'
type ByteStringL = BL.ByteString
-- |'BL.empty'
bl_empty = BL.empty
-- |'BL.singleton'
bl_singleton = BL.singleton
-- |'BL.pack'
bl_pack = BL.pack
-- |'BL.unpack'
bl_unpack = BL.unpack
-- |'BL.fromStrict'
bl_fromStrict = BL.fromStrict
-- |'BL.toStrict'
bl_toStrict = BL.toStrict
-- |'BL.fromChunks'
bl_fromChunks = BL.fromChunks
-- |'BL.toChunks'
bl_toChunks = BL.toChunks
-- |'BL.foldrChunks'
bl_foldrChunks = BL.foldrChunks
-- |'BL.foldlChunks'
bl_foldlChunks = BL.foldlChunks
-- |'BL.cons'
bl_cons = BL.cons
-- |'BL.cons''
bl_cons' = BL.cons'
-- |'BL.snoc'
bl_snoc = BL.snoc
-- |'BL.append'
bl_append = BL.append
-- |'BL.head'
bl_head = BL.head
-- |'BL.uncons'
bl_uncons = BL.uncons
-- |'BL.last'
bl_last = BL.last
-- |'BL.tail'
bl_tail = BL.tail
-- |'BL.init'
bl_init = BL.init
-- |'BL.null'
bl_null = BL.null
-- |'BL.length'
bl_length = BL.length
-- |'BL.map'
bl_map = BL.map
-- |'BL.reverse'
bl_reverse = BL.reverse
-- |'BL.intersperse'
bl_intersperse = BL.intersperse
-- |'BL.intercalate'
bl_intercalate = BL.intercalate
-- |'BL.transpose'
bl_transpose = BL.transpose
-- |'BL.foldl'
bl_foldl = BL.foldl
-- |'BL.foldl''
bl_foldl' = BL.foldl'
-- |'BL.foldl1'
bl_foldl1 = BL.foldl1
-- |'BL.foldl1''
bl_foldl1' = BL.foldl1'
-- |'BL.foldr'
bl_foldr = BL.foldr
-- |'BL.foldr1'
bl_foldr1 = BL.foldr1
-- |'BL.concat'
bl_concat = BL.concat
-- |'BL.concatMap'
bl_concatMap = BL.concatMap
-- |'BL.any'
bl_any = BL.any
-- |'BL.all'
bl_all = BL.all
-- |'BL.maximum'
bl_maximum = BL.maximum
-- |'BL.minimum'
bl_minimum = BL.minimum
-- |'BL.scanl'
bl_scanl = BL.scanl
-- |'BL.mapAccumL'
bl_mapAccumL = BL.mapAccumL
-- |'BL.mapAccumR'
bl_mapAccumR = BL.mapAccumR
-- |'BL.repeat'
bl_repeat = BL.repeat
-- |'BL.replicate'
bl_replicate = BL.replicate
-- |'BL.cycle'
bl_cycle = BL.cycle
-- |'BL.iterate'
bl_iterate = BL.iterate
-- |'BL.unfoldr'
bl_unfoldr = BL.unfoldr
-- |'BL.take'
bl_take = BL.take
-- |'BL.drop'
bl_drop = BL.drop
-- |'BL.splitAt'
bl_splitAt = BL.splitAt
-- |'BL.takeWhile'
bl_takeWhile = BL.takeWhile
-- |'BL.dropWhile'
bl_dropWhile = BL.dropWhile
-- |'BL.span'
bl_span = BL.span
-- |'BL.break'
bl_break = BL.break
-- |'BL.group'
bl_group = BL.group
-- |'BL.groupBy'
bl_groupBy = BL.groupBy
-- |'BL.inits'
bl_inits = BL.inits
-- |'BL.tails'
bl_tails = BL.tails
-- |'BL.split'
bl_split = BL.split
-- |'BL.splitWith'
bl_splitWith = BL.splitWith
-- |'BL.isPrefixOf'
bl_isPrefixOf = BL.isPrefixOf
-- |'BL.isSuffixOf'
bl_isSuffixOf = BL.isSuffixOf
-- |'BL.elem'
bl_elem = BL.elem
-- |'BL.notElem'
bl_notElem = BL.notElem
-- |'BL.find'
bl_find = BL.find
-- |'BL.filter'
bl_filter = BL.filter
-- |'BL.partition'
bl_partition = BL.partition
-- |'BL.index'
bl_index = BL.index
-- |'BL.elemIndex'
bl_elemIndex = BL.elemIndex
-- |'BL.elemIndices'
bl_elemIndices = BL.elemIndices
-- |'BL.findIndex'
bl_findIndex = BL.findIndex
-- |'BL.findIndices'
bl_findIndices = BL.findIndices
-- |'BL.count'
bl_count = BL.count
-- |'BL.zip'
bl_zip = BL.zip
-- |'BL.zipWith'
bl_zipWith = BL.zipWith
-- |'BL.unzip'
bl_unzip = BL.unzip
-- |'BL.copy'
bl_copy = BL.copy
-- |'BL.getContents'
bl_getContents = BL.getContents
-- |'BL.putStr'
bl_putStr = BL.putStr
-- |'BL.interact'
bl_interact = BL.interact
-- |'BL.readFile'
bl_readFile = BL.readFile
-- |'BL.writeFile'
bl_writeFile = BL.writeFile
-- |'BL.appendFile'
bl_appendFile = BL.appendFile
-- |'BL.hGetContents'
bl_hGetContents = BL.hGetContents
-- |'BL.hGet'
bl_hGet = BL.hGet
-- |'BL.hGetNonBlocking'
bl_hGetNonBlocking = BL.hGetNonBlocking
-- |'BL.hPut'
bl_hPut = BL.hPut
-- |'BL.hPutNonBlocking'
bl_hPutNonBlocking = BL.hPutNonBlocking
-- |'BL.hPutStr'
bl_hPutStr = BL.hPutStr

-- Data.ByteString.Lazy.Builder
-- |'BLB.Builder'
type BLBuilder = BLB.Builder
-- |'BLB.toLazyByteString'
blb_toLazyByteString = BLB.toLazyByteString
-- |'BLB.hPutBuilder'
blb_hPutBuilder = BLB.hPutBuilder
-- |'BLB.byteString'
blb_byteString = BLB.byteString
-- |'BLB.lazyByteString'
blb_lazyByteString = BLB.lazyByteString
-- |'BLB.int8'
blb_int8 = BLB.int8
-- |'BLB.word8'
blb_word8 = BLB.word8
-- |'BLB.int16BE'
blb_int16BE = BLB.int16BE
-- |'BLB.int32BE'
blb_int32BE = BLB.int32BE
-- |'BLB.int64BE'
blb_int64BE = BLB.int64BE
-- |'BLB.word16BE'
blb_word16BE = BLB.word16BE
-- |'BLB.word32BE'
blb_word32BE = BLB.word32BE
-- |'BLB.word64BE'
blb_word64BE = BLB.word64BE
-- |'BLB.floatBE'
blb_floatBE = BLB.floatBE
-- |'BLB.doubleBE'
blb_doubleBE = BLB.doubleBE
-- |'BLB.int16LE'
blb_int16LE = BLB.int16LE
-- |'BLB.int32LE'
blb_int32LE = BLB.int32LE
-- |'BLB.int64LE'
blb_int64LE = BLB.int64LE
-- |'BLB.word16LE'
blb_word16LE = BLB.word16LE
-- |'BLB.word32LE'
blb_word32LE = BLB.word32LE
-- |'BLB.word64LE'
blb_word64LE = BLB.word64LE
-- |'BLB.floatLE'
blb_floatLE = BLB.floatLE
-- |'BLB.doubleLE'
blb_doubleLE = BLB.doubleLE
-- |'BLB.char7'
blb_char7 = BLB.char7
-- |'BLB.string7'
blb_string7 = BLB.string7
-- |'BLB.char8'
blb_char8 = BLB.char8
-- |'BLB.string8'
blb_string8 = BLB.string8
-- |'BLB.charUtf8'
blb_charUtf8 = BLB.charUtf8
-- |'BLB.stringUtf8'
blb_stringUtf8 = BLB.stringUtf8

-- Data.ByteString.Lazy.Builder.ASCII
-- |'BLBA.int8Dec'
blb_int8Dec = BLBA.int8Dec
-- |'BLBA.int16Dec'
blb_int16Dec = BLBA.int16Dec
-- |'BLBA.int32Dec'
blb_int32Dec = BLBA.int32Dec
-- |'BLBA.int64Dec'
blb_int64Dec = BLBA.int64Dec
-- |'BLBA.intDec'
blb_intDec = BLBA.intDec
-- |'BLBA.integerDec'
blb_integerDec = BLBA.integerDec
-- |'BLBA.word8Dec'
blb_word8Dec = BLBA.word8Dec
-- |'BLBA.word16Dec'
blb_word16Dec = BLBA.word16Dec
-- |'BLBA.word32Dec'
blb_word32Dec = BLBA.word32Dec
-- |'BLBA.word64Dec'
blb_word64Dec = BLBA.word64Dec
-- |'BLBA.wordDec'
blb_wordDec = BLBA.wordDec
-- |'BLBA.floatDec'
blb_floatDec = BLBA.floatDec
-- |'BLBA.doubleDec'
blb_doubleDec = BLBA.doubleDec
-- |'BLBA.word8Hex'
blb_word8Hex = BLBA.word8Hex
-- |'BLBA.word16Hex'
blb_word16Hex = BLBA.word16Hex
-- |'BLBA.word32Hex'
blb_word32Hex = BLBA.word32Hex
-- |'BLBA.word64Hex'
blb_word64Hex = BLBA.word64Hex
-- |'BLBA.wordHex'
blb_wordHex = BLBA.wordHex
-- |'BLBA.int8HexFixed'
blb_int8HexFixed = BLBA.int8HexFixed
-- |'BLBA.int16HexFixed'
blb_int16HexFixed = BLBA.int16HexFixed
-- |'BLBA.int32HexFixed'
blb_int32HexFixed = BLBA.int32HexFixed
-- |'BLBA.int64HexFixed'
blb_int64HexFixed = BLBA.int64HexFixed
-- |'BLBA.word8HexFixed'
blb_word8HexFixed = BLBA.word8HexFixed
-- |'BLBA.word16HexFixed'
blb_word16HexFixed = BLBA.word16HexFixed
-- |'BLBA.word32HexFixed'
blb_word32HexFixed = BLBA.word32HexFixed
-- |'BLBA.word64HexFixed'
blb_word64HexFixed = BLBA.word64HexFixed
-- |'BLBA.floatHexFixed'
blb_floatHexFixed = BLBA.floatHexFixed
-- |'BLBA.doubleHexFixed'
blb_doubleHexFixed = BLBA.doubleHexFixed
-- |'BLBA.byteStringHexFixed'
blb_byteStringHexFixed = BLBA.byteStringHexFixed
-- |'BLBA.lazyByteStringHexFixed'
blb_lazyByteStringHexFixed = BLBA.lazyByteStringHexFixed

-- Data.ByteString.Lazy.Builder.Extras
-- |'BLBE.AllocationStrategy'
type AllocationStrategy = BLBE.AllocationStrategy
-- |'BLBE.toLazyByteStringWith'
blb_toLazyByteStringWith = BLBE.toLazyByteStringWith
-- |'BLBE.safeStrategy'
blb_safeStrategy = BLBE.safeStrategy
-- |'BLBE.untrimmedStrategy'
blb_untrimmedStrategy = BLBE.untrimmedStrategy
-- |'BLBE.smallChunkSize'
blb_smallChunkSize = BLBE.smallChunkSize
-- |'BLBE.defaultChunkSize'
blb_defaultChunkSize = BLBE.defaultChunkSize
-- |'BLBE.byteStringCopy'
blb_byteStringCopy = BLBE.byteStringCopy
-- |'BLBE.byteStringInsert'
blb_byteStringInsert = BLBE.byteStringInsert
-- |'BLBE.byteStringThreshold'
blb_byteStringThreshold = BLBE.byteStringThreshold
-- |'BLBE.lazyByteStringCopy'
blb_lazyByteStringCopy = BLBE.lazyByteStringCopy
-- |'BLBE.lazyByteStringInsert'
blb_lazyByteStringInsert = BLBE.lazyByteStringInsert
-- |'BLBE.lazyByteStringThreshold'
blb_lazyByteStringThreshold = BLBE.lazyByteStringThreshold
-- |'BLBE.flush'
blb_flush = BLBE.flush
-- |'BLBE.intHost'
blb_intHost = BLBE.intHost
-- |'BLBE.int16Host'
blb_int16Host = BLBE.int16Host
-- |'BLBE.int32Host'
blb_int32Host = BLBE.int32Host
-- |'BLBE.int64Host'
blb_int64Host = BLBE.int64Host
-- |'BLBE.wordHost'
blb_wordHost = BLBE.wordHost
-- |'BLBE.word16Host'
blb_word16Host = BLBE.word16Host
-- |'BLBE.word32Host'
blb_word32Host = BLBE.word32Host
-- |'BLBE.word64Host'
blb_word64Host = BLBE.word64Host
-- |'BLBE.floatHost'
blb_floatHost = BLBE.floatHost
-- |'BLBE.doubleHost'
blb_doubleHost = BLBE.doubleHost

-- Data.ByteString.Lazy.Char8
-- |'BLC.ByteString'
type ByteStringLChar8 = BLC.ByteString
-- |'BLC.empty'
blc_empty = BLC.empty
-- |'BLC.singleton'
blc_singleton = BLC.singleton
-- |'BLC.pack'
blc_pack = BLC.pack
-- |'BLC.unpack'
blc_unpack = BLC.unpack
-- |'BLC.fromChunks'
blc_fromChunks = BLC.fromChunks
-- |'BLC.toChunks'
blc_toChunks = BLC.toChunks
-- |'BLC.fromStrict'
blc_fromStrict = BLC.fromStrict
-- |'BLC.toStrict'
blc_toStrict = BLC.toStrict
-- |'BLC.cons'
blc_cons = BLC.cons
-- |'BLC.cons''
blc_cons' = BLC.cons'
-- |'BLC.snoc'
blc_snoc = BLC.snoc
-- |'BLC.append'
blc_append = BLC.append
-- |'BLC.head'
blc_head = BLC.head
-- |'BLC.uncons'
blc_uncons = BLC.uncons
-- |'BLC.last'
blc_last = BLC.last
-- |'BLC.tail'
blc_tail = BLC.tail
-- |'BLC.init'
blc_init = BLC.init
-- |'BLC.null'
blc_null = BLC.null
-- |'BLC.length'
blc_length = BLC.length
-- |'BLC.map'
blc_map = BLC.map
-- |'BLC.reverse'
blc_reverse = BLC.reverse
-- |'BLC.intersperse'
blc_intersperse = BLC.intersperse
-- |'BLC.intercalate'
blc_intercalate = BLC.intercalate
-- |'BLC.transpose'
blc_transpose = BLC.transpose
-- |'BLC.foldl'
blc_foldl = BLC.foldl
-- |'BLC.foldl''
blc_foldl' = BLC.foldl'
-- |'BLC.foldl1'
blc_foldl1 = BLC.foldl1
-- |'BLC.foldl1''
blc_foldl1' = BLC.foldl1'
-- |'BLC.foldr'
blc_foldr = BLC.foldr
-- |'BLC.foldr1'
blc_foldr1 = BLC.foldr1
-- |'BLC.concat'
blc_concat = BLC.concat
-- |'BLC.concatMap'
blc_concatMap = BLC.concatMap
-- |'BLC.any'
blc_any = BLC.any
-- |'BLC.all'
blc_all = BLC.all
-- |'BLC.maximum'
blc_maximum = BLC.maximum
-- |'BLC.minimum'
blc_minimum = BLC.minimum
-- |'BLC.scanl'
blc_scanl = BLC.scanl
-- |'BLC.mapAccumL'
blc_mapAccumL = BLC.mapAccumL
-- |'BLC.mapAccumR'
blc_mapAccumR = BLC.mapAccumR
-- |'BLC.repeat'
blc_repeat = BLC.repeat
-- |'BLC.replicate'
blc_replicate = BLC.replicate
-- |'BLC.cycle'
blc_cycle = BLC.cycle
-- |'BLC.iterate'
blc_iterate = BLC.iterate
-- |'BLC.unfoldr'
blc_unfoldr = BLC.unfoldr
-- |'BLC.take'
blc_take = BLC.take
-- |'BLC.drop'
blc_drop = BLC.drop
-- |'BLC.splitAt'
blc_splitAt = BLC.splitAt
-- |'BLC.takeWhile'
blc_takeWhile = BLC.takeWhile
-- |'BLC.dropWhile'
blc_dropWhile = BLC.dropWhile
-- |'BLC.span'
blc_span = BLC.span
-- |'BLC.break'
blc_break = BLC.break
-- |'BLC.group'
blc_group = BLC.group
-- |'BLC.groupBy'
blc_groupBy = BLC.groupBy
-- |'BLC.inits'
blc_inits = BLC.inits
-- |'BLC.tails'
blc_tails = BLC.tails
-- |'BLC.split'
blc_split = BLC.split
-- |'BLC.splitWith'
blc_splitWith = BLC.splitWith
-- |'BLC.lines'
blc_lines = BLC.lines
-- |'BLC.words'
blc_words = BLC.words
-- |'BLC.unlines'
blc_unlines = BLC.unlines
-- |'BLC.unwords'
blc_unwords = BLC.unwords
-- |'BLC.isPrefixOf'
blc_isPrefixOf = BLC.isPrefixOf
-- |'BLC.elem'
blc_elem = BLC.elem
-- |'BLC.notElem'
blc_notElem = BLC.notElem
-- |'BLC.find'
blc_find = BLC.find
-- |'BLC.filter'
blc_filter = BLC.filter
-- |'BLC.index'
blc_index = BLC.index
-- |'BLC.elemIndex'
blc_elemIndex = BLC.elemIndex
-- |'BLC.elemIndices'
blc_elemIndices = BLC.elemIndices
-- |'BLC.findIndex'
blc_findIndex = BLC.findIndex
-- |'BLC.findIndices'
blc_findIndices = BLC.findIndices
-- |'BLC.count'
blc_count = BLC.count
-- |'BLC.zip'
blc_zip = BLC.zip
-- |'BLC.zipWith'
blc_zipWith = BLC.zipWith
-- |'BLC.copy'
blc_copy = BLC.copy
-- |'BLC.readInt'
blc_readInt = BLC.readInt
-- |'BLC.readInteger'
blc_readInteger = BLC.readInteger
-- |'BLC.getContents'
blc_getContents = BLC.getContents
-- |'BLC.putStr'
blc_putStr = BLC.putStr
-- |'BLC.putStrLn'
blc_putStrLn = BLC.putStrLn
-- |'BLC.interact'
blc_interact = BLC.interact
-- |'BLC.readFile'
blc_readFile = BLC.readFile
-- |'BLC.writeFile'
blc_writeFile = BLC.writeFile
-- |'BLC.appendFile'
blc_appendFile = BLC.appendFile
-- |'BLC.hGetContents'
blc_hGetContents = BLC.hGetContents
-- |'BLC.hGet'
blc_hGet = BLC.hGet
-- |'BLC.hGetNonBlocking'
blc_hGetNonBlocking = BLC.hGetNonBlocking
-- |'BLC.hPut'
blc_hPut = BLC.hPut
-- |'BLC.hPutNonBlocking'
blc_hPutNonBlocking = BLC.hPutNonBlocking
-- |'BLC.hPutStr'
blc_hPutStr = BLC.hPutStr
-- |'BLC.hPutStrLn'
blc_hPutStrLn = BLC.hPutStrLn

-- Data.ByteString.Unsafe
-- |'BU.unsafeHead'
b_unsafeHead = BU.unsafeHead
-- |'BU.unsafeTail'
b_unsafeTail = BU.unsafeTail
-- |'BU.unsafeIndex'
b_unsafeIndex = BU.unsafeIndex
-- |'BU.unsafeTake'
b_unsafeTake = BU.unsafeTake
-- |'BU.unsafeDrop'
b_unsafeDrop = BU.unsafeDrop
-- |'BU.unsafeUseAsCString'
b_unsafeUseAsCString = BU.unsafeUseAsCString
-- |'BU.unsafeUseAsCStringLen'
b_unsafeUseAsCStringLen = BU.unsafeUseAsCStringLen
-- |'BU.unsafePackCString'
b_unsafePackCString = BU.unsafePackCString
-- |'BU.unsafePackCStringLen'
b_unsafePackCStringLen = BU.unsafePackCStringLen
-- |'BU.unsafePackMallocCString'
b_unsafePackMallocCString = BU.unsafePackMallocCString
-- |'BU.unsafePackAddress'
b_unsafePackAddress = BU.unsafePackAddress
-- |'BU.unsafePackAddressLen'
b_unsafePackAddressLen = BU.unsafePackAddressLen
-- |'BU.unsafePackCStringFinalizer'
b_unsafePackCStringFinalizer = BU.unsafePackCStringFinalizer
-- |'BU.unsafeFinalize'
b_unsafeFinalize = BU.unsafeFinalize

-- Data.CaseInsensitive
-- |'CI.CI'
type CI = CI.CI
-- |'CI.mk'
ci_mk = CI.mk
-- |'CI.original'
ci_original = CI.original
-- |'CI.foldedCase'
ci_foldedCase = CI.foldedCase
-- |'CI.map'
ci_map = CI.map
-- |'CI.foldCase'
ci_foldCase = CI.foldCase

-- Data.HashMap.Lazy
-- |'HashMapL.HashMap'
type HashMapL = HashMapL.HashMap
-- |('HashMapL.!')
hml_get = (HashMapL.!)
-- |'HashMapL.empty'
hml_empty = HashMapL.empty
-- |'HashMapL.singleton'
hml_singleton = HashMapL.singleton
-- |'HashMapL.null'
hml_null = HashMapL.null
-- |'HashMapL.size'
hml_size = HashMapL.size
-- |'HashMapL.member'
hml_member = HashMapL.member
-- |'HashMapL.lookup'
hml_lookup = HashMapL.lookup
-- |'HashMapL.lookupDefault'
hml_lookupDefault = HashMapL.lookupDefault
-- |'HashMapL.insert'
hml_insert = HashMapL.insert
-- |'HashMapL.insertWith'
hml_insertWith = HashMapL.insertWith
-- |'HashMapL.delete'
hml_delete = HashMapL.delete
-- |'HashMapL.adjust'
hml_adjust = HashMapL.adjust
-- |'HashMapL.union'
hml_union = HashMapL.union
-- |'HashMapL.unionWith'
hml_unionWith = HashMapL.unionWith
-- |'HashMapL.unions'
hml_unions = HashMapL.unions
-- |'HashMapL.map'
hml_map = HashMapL.map
-- |'HashMapL.traverseWithKey'
hml_traverseWithKey = HashMapL.traverseWithKey
-- |'HashMapL.difference'
hml_difference = HashMapL.difference
-- |'HashMapL.intersection'
hml_intersection = HashMapL.intersection
-- |'HashMapL.intersectionWith'
hml_intersectionWith = HashMapL.intersectionWith
-- |'HashMapL.foldl''
hml_foldl' = HashMapL.foldl'
-- |'HashMapL.foldlWithKey''
hml_foldlWithKey' = HashMapL.foldlWithKey'
-- |'HashMapL.foldr'
hml_foldr = HashMapL.foldr
-- |'HashMapL.foldrWithKey'
hml_foldrWithKey = HashMapL.foldrWithKey
-- |'HashMapL.filter'
hml_filter = HashMapL.filter
-- |'HashMapL.filterWithKey'
hml_filterWithKey = HashMapL.filterWithKey
-- |'HashMapL.keys'
hml_keys = HashMapL.keys
-- |'HashMapL.elems'
hml_elems = HashMapL.elems
-- |'HashMapL.toList'
hml_toList = HashMapL.toList
-- |'HashMapL.fromList'
hml_fromList = HashMapL.fromList
-- |'HashMapL.fromListWith'
hml_fromListWith = HashMapL.fromListWith

-- Data.HashMap.Strict
-- |'HashMapS.HashMap'
type HashMapS = HashMapS.HashMap
-- |('HashMapS.!')
hms_get = (HashMapS.!)
-- |'HashMapS.empty'
hms_empty = HashMapS.empty
-- |'HashMapS.singleton'
hms_singleton = HashMapS.singleton
-- |'HashMapS.null'
hms_null = HashMapS.null
-- |'HashMapS.size'
hms_size = HashMapS.size
-- |'HashMapS.member'
hms_member = HashMapS.member
-- |'HashMapS.lookup'
hms_lookup = HashMapS.lookup
-- |'HashMapS.lookupDefault'
hms_lookupDefault = HashMapS.lookupDefault
-- |'HashMapS.insert'
hms_insert = HashMapS.insert
-- |'HashMapS.insertWith'
hms_insertWith = HashMapS.insertWith
-- |'HashMapS.delete'
hms_delete = HashMapS.delete
-- |'HashMapS.adjust'
hms_adjust = HashMapS.adjust
-- |'HashMapS.union'
hms_union = HashMapS.union
-- |'HashMapS.unionWith'
hms_unionWith = HashMapS.unionWith
-- |'HashMapS.unions'
hms_unions = HashMapS.unions
-- |'HashMapS.map'
hms_map = HashMapS.map
-- |'HashMapS.traverseWithKey'
hms_traverseWithKey = HashMapS.traverseWithKey
-- |'HashMapS.difference'
hms_difference = HashMapS.difference
-- |'HashMapS.intersection'
hms_intersection = HashMapS.intersection
-- |'HashMapS.intersectionWith'
hms_intersectionWith = HashMapS.intersectionWith
-- |'HashMapS.foldl''
hms_foldl' = HashMapS.foldl'
-- |'HashMapS.foldlWithKey''
hms_foldlWithKey' = HashMapS.foldlWithKey'
-- |'HashMapS.foldr'
hms_foldr = HashMapS.foldr
-- |'HashMapS.foldrWithKey'
hms_foldrWithKey = HashMapS.foldrWithKey
-- |'HashMapS.filter'
hms_filter = HashMapS.filter
-- |'HashMapS.filterWithKey'
hms_filterWithKey = HashMapS.filterWithKey
-- |'HashMapS.keys'
hms_keys = HashMapS.keys
-- |'HashMapS.elems'
hms_elems = HashMapS.elems
-- |'HashMapS.toList'
hms_toList = HashMapS.toList
-- |'HashMapS.fromList'
hms_fromList = HashMapS.fromList
-- |'HashMapS.fromListWith'
hms_fromListWith = HashMapS.fromListWith

-- Data.HashSet
-- |'HashSet.HashSet'
type HashSet = HashSet.HashSet
-- |'HashSet.empty'
hs_empty = HashSet.empty
-- |'HashSet.singleton'
hs_singleton = HashSet.singleton
-- |'HashSet.union'
hs_union = HashSet.union
-- |'HashSet.unions'
hs_unions = HashSet.unions
-- |'HashSet.null'
hs_null = HashSet.null
-- |'HashSet.size'
hs_size = HashSet.size
-- |'HashSet.member'
hs_member = HashSet.member
-- |'HashSet.insert'
hs_insert = HashSet.insert
-- |'HashSet.delete'
hs_delete = HashSet.delete
-- |'HashSet.map'
hs_map = HashSet.map
-- |'HashSet.difference'
hs_difference = HashSet.difference
-- |'HashSet.intersection'
hs_intersection = HashSet.intersection
-- |'HashSet.foldl''
hs_foldl' = HashSet.foldl'
-- |'HashSet.foldr'
hs_foldr = HashSet.foldr
-- |'HashSet.filter'
hs_filter = HashSet.filter
-- |'HashSet.toList'
hs_toList = HashSet.toList
-- |'HashSet.fromList'
hs_fromList = HashSet.fromList

-- Data.IntMap.Lazy
-- |'IntMapL.IntMap'
type IntMapL = IntMapL.IntMap
-- |('IntMapL.!')
iml_get = (IntMapL.!)
-- |'IntMapL.null'
iml_null = IntMapL.null
-- |'IntMapL.size'
iml_size = IntMapL.size
-- |'IntMapL.member'
iml_member = IntMapL.member
-- |'IntMapL.notMember'
iml_notMember = IntMapL.notMember
-- |'IntMapL.lookup'
iml_lookup = IntMapL.lookup
-- |'IntMapL.findWithDefault'
iml_findWithDefault = IntMapL.findWithDefault
-- |'IntMapL.lookupLT'
iml_lookupLT = IntMapL.lookupLT
-- |'IntMapL.lookupGT'
iml_lookupGT = IntMapL.lookupGT
-- |'IntMapL.lookupLE'
iml_lookupLE = IntMapL.lookupLE
-- |'IntMapL.lookupGE'
iml_lookupGE = IntMapL.lookupGE
-- |'IntMapL.empty'
iml_empty = IntMapL.empty
-- |'IntMapL.singleton'
iml_singleton = IntMapL.singleton
-- |'IntMapL.insert'
iml_insert = IntMapL.insert
-- |'IntMapL.insertWith'
iml_insertWith = IntMapL.insertWith
-- |'IntMapL.insertWithKey'
iml_insertWithKey = IntMapL.insertWithKey
-- |'IntMapL.insertLookupWithKey'
iml_insertLookupWithKey = IntMapL.insertLookupWithKey
-- |'IntMapL.delete'
iml_delete = IntMapL.delete
-- |'IntMapL.adjust'
iml_adjust = IntMapL.adjust
-- |'IntMapL.adjustWithKey'
iml_adjustWithKey = IntMapL.adjustWithKey
-- |'IntMapL.update'
iml_update = IntMapL.update
-- |'IntMapL.updateWithKey'
iml_updateWithKey = IntMapL.updateWithKey
-- |'IntMapL.updateLookupWithKey'
iml_updateLookupWithKey = IntMapL.updateLookupWithKey
-- |'IntMapL.alter'
iml_alter = IntMapL.alter
-- |'IntMapL.union'
iml_union = IntMapL.union
-- |'IntMapL.unionWith'
iml_unionWith = IntMapL.unionWith
-- |'IntMapL.unionWithKey'
iml_unionWithKey = IntMapL.unionWithKey
-- |'IntMapL.unions'
iml_unions = IntMapL.unions
-- |'IntMapL.unionsWith'
iml_unionsWith = IntMapL.unionsWith
-- |'IntMapL.difference'
iml_difference = IntMapL.difference
-- |'IntMapL.differenceWith'
iml_differenceWith = IntMapL.differenceWith
-- |'IntMapL.differenceWithKey'
iml_differenceWithKey = IntMapL.differenceWithKey
-- |'IntMapL.intersection'
iml_intersection = IntMapL.intersection
-- |'IntMapL.intersectionWith'
iml_intersectionWith = IntMapL.intersectionWith
-- |'IntMapL.intersectionWithKey'
iml_intersectionWithKey = IntMapL.intersectionWithKey
-- |'IntMapL.mergeWithKey'
iml_mergeWithKey = IntMapL.mergeWithKey
-- |'IntMapL.map'
iml_map = IntMapL.map
-- |'IntMapL.mapWithKey'
iml_mapWithKey = IntMapL.mapWithKey
-- |'IntMapL.traverseWithKey'
iml_traverseWithKey = IntMapL.traverseWithKey
-- |'IntMapL.mapAccum'
iml_mapAccum = IntMapL.mapAccum
-- |'IntMapL.mapAccumWithKey'
iml_mapAccumWithKey = IntMapL.mapAccumWithKey
-- |'IntMapL.mapAccumRWithKey'
iml_mapAccumRWithKey = IntMapL.mapAccumRWithKey
-- |'IntMapL.mapKeys'
iml_mapKeys = IntMapL.mapKeys
-- |'IntMapL.mapKeysWith'
iml_mapKeysWith = IntMapL.mapKeysWith
-- |'IntMapL.mapKeysMonotonic'
iml_mapKeysMonotonic = IntMapL.mapKeysMonotonic
-- |'IntMapL.foldr'
iml_foldr = IntMapL.foldr
-- |'IntMapL.foldl'
iml_foldl = IntMapL.foldl
-- |'IntMapL.foldrWithKey'
iml_foldrWithKey = IntMapL.foldrWithKey
-- |'IntMapL.foldlWithKey'
iml_foldlWithKey = IntMapL.foldlWithKey
-- |'IntMapL.foldr''
iml_foldr' = IntMapL.foldr'
-- |'IntMapL.foldl''
iml_foldl' = IntMapL.foldl'
-- |'IntMapL.foldrWithKey''
iml_foldrWithKey' = IntMapL.foldrWithKey'
-- |'IntMapL.foldlWithKey''
iml_foldlWithKey' = IntMapL.foldlWithKey'
-- |'IntMapL.elems'
iml_elems = IntMapL.elems
-- |'IntMapL.keys'
iml_keys = IntMapL.keys
-- |'IntMapL.assocs'
iml_assocs = IntMapL.assocs
-- |'IntMapL.keysSet'
iml_keysSet = IntMapL.keysSet
-- |'IntMapL.fromSet'
iml_fromSet = IntMapL.fromSet
-- |'IntMapL.toList'
iml_toList = IntMapL.toList
-- |'IntMapL.fromList'
iml_fromList = IntMapL.fromList
-- |'IntMapL.fromListWith'
iml_fromListWith = IntMapL.fromListWith
-- |'IntMapL.fromListWithKey'
iml_fromListWithKey = IntMapL.fromListWithKey
-- |'IntMapL.toAscList'
iml_toAscList = IntMapL.toAscList
-- |'IntMapL.toDescList'
iml_toDescList = IntMapL.toDescList
-- |'IntMapL.fromAscList'
iml_fromAscList = IntMapL.fromAscList
-- |'IntMapL.fromAscListWith'
iml_fromAscListWith = IntMapL.fromAscListWith
-- |'IntMapL.fromAscListWithKey'
iml_fromAscListWithKey = IntMapL.fromAscListWithKey
-- |'IntMapL.fromDistinctAscList'
iml_fromDistinctAscList = IntMapL.fromDistinctAscList
-- |'IntMapL.filter'
iml_filter = IntMapL.filter
-- |'IntMapL.filterWithKey'
iml_filterWithKey = IntMapL.filterWithKey
-- |'IntMapL.partition'
iml_partition = IntMapL.partition
-- |'IntMapL.partitionWithKey'
iml_partitionWithKey = IntMapL.partitionWithKey
-- |'IntMapL.mapMaybe'
iml_mapMaybe = IntMapL.mapMaybe
-- |'IntMapL.mapMaybeWithKey'
iml_mapMaybeWithKey = IntMapL.mapMaybeWithKey
-- |'IntMapL.mapEither'
iml_mapEither = IntMapL.mapEither
-- |'IntMapL.mapEitherWithKey'
iml_mapEitherWithKey = IntMapL.mapEitherWithKey
-- |'IntMapL.split'
iml_split = IntMapL.split
-- |'IntMapL.splitLookup'
iml_splitLookup = IntMapL.splitLookup
-- |'IntMapL.isSubmapOf'
iml_isSubmapOf = IntMapL.isSubmapOf
-- |'IntMapL.isSubmapOfBy'
iml_isSubmapOfBy = IntMapL.isSubmapOfBy
-- |'IntMapL.isProperSubmapOf'
iml_isProperSubmapOf = IntMapL.isProperSubmapOf
-- |'IntMapL.isProperSubmapOfBy'
iml_isProperSubmapOfBy = IntMapL.isProperSubmapOfBy
-- |'IntMapL.findMin'
iml_findMin = IntMapL.findMin
-- |'IntMapL.findMax'
iml_findMax = IntMapL.findMax
-- |'IntMapL.deleteMin'
iml_deleteMin = IntMapL.deleteMin
-- |'IntMapL.deleteMax'
iml_deleteMax = IntMapL.deleteMax
-- |'IntMapL.deleteFindMin'
iml_deleteFindMin = IntMapL.deleteFindMin
-- |'IntMapL.deleteFindMax'
iml_deleteFindMax = IntMapL.deleteFindMax
-- |'IntMapL.updateMin'
iml_updateMin = IntMapL.updateMin
-- |'IntMapL.updateMax'
iml_updateMax = IntMapL.updateMax
-- |'IntMapL.updateMinWithKey'
iml_updateMinWithKey = IntMapL.updateMinWithKey
-- |'IntMapL.updateMaxWithKey'
iml_updateMaxWithKey = IntMapL.updateMaxWithKey
-- |'IntMapL.minView'
iml_minView = IntMapL.minView
-- |'IntMapL.maxView'
iml_maxView = IntMapL.maxView
-- |'IntMapL.minViewWithKey'
iml_minViewWithKey = IntMapL.minViewWithKey
-- |'IntMapL.maxViewWithKey'
iml_maxViewWithKey = IntMapL.maxViewWithKey
-- |'IntMapL.showTree'
iml_showTree = IntMapL.showTree
-- |'IntMapL.showTreeWith'
iml_showTreeWith = IntMapL.showTreeWith

-- Data.IntMap.Strict
-- |'IntMapS.IntMap'
type IntMapS = IntMapS.IntMap
-- |('IntMapS.!')
ims_get = (IntMapS.!)
-- |'IntMapS.null'
ims_null = IntMapS.null
-- |'IntMapS.size'
ims_size = IntMapS.size
-- |'IntMapS.member'
ims_member = IntMapS.member
-- |'IntMapS.notMember'
ims_notMember = IntMapS.notMember
-- |'IntMapS.lookup'
ims_lookup = IntMapS.lookup
-- |'IntMapS.findWithDefault'
ims_findWithDefault = IntMapS.findWithDefault
-- |'IntMapS.lookupLT'
ims_lookupLT = IntMapS.lookupLT
-- |'IntMapS.lookupGT'
ims_lookupGT = IntMapS.lookupGT
-- |'IntMapS.lookupLE'
ims_lookupLE = IntMapS.lookupLE
-- |'IntMapS.lookupGE'
ims_lookupGE = IntMapS.lookupGE
-- |'IntMapS.empty'
ims_empty = IntMapS.empty
-- |'IntMapS.singleton'
ims_singleton = IntMapS.singleton
-- |'IntMapS.insert'
ims_insert = IntMapS.insert
-- |'IntMapS.insertWith'
ims_insertWith = IntMapS.insertWith
-- |'IntMapS.insertWithKey'
ims_insertWithKey = IntMapS.insertWithKey
-- |'IntMapS.insertLookupWithKey'
ims_insertLookupWithKey = IntMapS.insertLookupWithKey
-- |'IntMapS.delete'
ims_delete = IntMapS.delete
-- |'IntMapS.adjust'
ims_adjust = IntMapS.adjust
-- |'IntMapS.adjustWithKey'
ims_adjustWithKey = IntMapS.adjustWithKey
-- |'IntMapS.update'
ims_update = IntMapS.update
-- |'IntMapS.updateWithKey'
ims_updateWithKey = IntMapS.updateWithKey
-- |'IntMapS.updateLookupWithKey'
ims_updateLookupWithKey = IntMapS.updateLookupWithKey
-- |'IntMapS.alter'
ims_alter = IntMapS.alter
-- |'IntMapS.union'
ims_union = IntMapS.union
-- |'IntMapS.unionWith'
ims_unionWith = IntMapS.unionWith
-- |'IntMapS.unionWithKey'
ims_unionWithKey = IntMapS.unionWithKey
-- |'IntMapS.unions'
ims_unions = IntMapS.unions
-- |'IntMapS.unionsWith'
ims_unionsWith = IntMapS.unionsWith
-- |'IntMapS.difference'
ims_difference = IntMapS.difference
-- |'IntMapS.differenceWith'
ims_differenceWith = IntMapS.differenceWith
-- |'IntMapS.differenceWithKey'
ims_differenceWithKey = IntMapS.differenceWithKey
-- |'IntMapS.intersection'
ims_intersection = IntMapS.intersection
-- |'IntMapS.intersectionWith'
ims_intersectionWith = IntMapS.intersectionWith
-- |'IntMapS.intersectionWithKey'
ims_intersectionWithKey = IntMapS.intersectionWithKey
-- |'IntMapS.mergeWithKey'
ims_mergeWithKey = IntMapS.mergeWithKey
-- |'IntMapS.map'
ims_map = IntMapS.map
-- |'IntMapS.mapWithKey'
ims_mapWithKey = IntMapS.mapWithKey
-- |'IntMapS.traverseWithKey'
ims_traverseWithKey = IntMapS.traverseWithKey
-- |'IntMapS.mapAccum'
ims_mapAccum = IntMapS.mapAccum
-- |'IntMapS.mapAccumWithKey'
ims_mapAccumWithKey = IntMapS.mapAccumWithKey
-- |'IntMapS.mapAccumRWithKey'
ims_mapAccumRWithKey = IntMapS.mapAccumRWithKey
-- |'IntMapS.mapKeys'
ims_mapKeys = IntMapS.mapKeys
-- |'IntMapS.mapKeysWith'
ims_mapKeysWith = IntMapS.mapKeysWith
-- |'IntMapS.mapKeysMonotonic'
ims_mapKeysMonotonic = IntMapS.mapKeysMonotonic
-- |'IntMapS.foldr'
ims_foldr = IntMapS.foldr
-- |'IntMapS.foldl'
ims_foldl = IntMapS.foldl
-- |'IntMapS.foldrWithKey'
ims_foldrWithKey = IntMapS.foldrWithKey
-- |'IntMapS.foldlWithKey'
ims_foldlWithKey = IntMapS.foldlWithKey
-- |'IntMapS.foldr''
ims_foldr' = IntMapS.foldr'
-- |'IntMapS.foldl''
ims_foldl' = IntMapS.foldl'
-- |'IntMapS.foldrWithKey''
ims_foldrWithKey' = IntMapS.foldrWithKey'
-- |'IntMapS.foldlWithKey''
ims_foldlWithKey' = IntMapS.foldlWithKey'
-- |'IntMapS.elems'
ims_elems = IntMapS.elems
-- |'IntMapS.keys'
ims_keys = IntMapS.keys
-- |'IntMapS.assocs'
ims_assocs = IntMapS.assocs
-- |'IntMapS.keysSet'
ims_keysSet = IntMapS.keysSet
-- |'IntMapS.fromSet'
ims_fromSet = IntMapS.fromSet
-- |'IntMapS.toList'
ims_toList = IntMapS.toList
-- |'IntMapS.fromList'
ims_fromList = IntMapS.fromList
-- |'IntMapS.fromListWith'
ims_fromListWith = IntMapS.fromListWith
-- |'IntMapS.fromListWithKey'
ims_fromListWithKey = IntMapS.fromListWithKey
-- |'IntMapS.toAscList'
ims_toAscList = IntMapS.toAscList
-- |'IntMapS.toDescList'
ims_toDescList = IntMapS.toDescList
-- |'IntMapS.fromAscList'
ims_fromAscList = IntMapS.fromAscList
-- |'IntMapS.fromAscListWith'
ims_fromAscListWith = IntMapS.fromAscListWith
-- |'IntMapS.fromAscListWithKey'
ims_fromAscListWithKey = IntMapS.fromAscListWithKey
-- |'IntMapS.fromDistinctAscList'
ims_fromDistinctAscList = IntMapS.fromDistinctAscList
-- |'IntMapS.filter'
ims_filter = IntMapS.filter
-- |'IntMapS.filterWithKey'
ims_filterWithKey = IntMapS.filterWithKey
-- |'IntMapS.partition'
ims_partition = IntMapS.partition
-- |'IntMapS.partitionWithKey'
ims_partitionWithKey = IntMapS.partitionWithKey
-- |'IntMapS.mapMaybe'
ims_mapMaybe = IntMapS.mapMaybe
-- |'IntMapS.mapMaybeWithKey'
ims_mapMaybeWithKey = IntMapS.mapMaybeWithKey
-- |'IntMapS.mapEither'
ims_mapEither = IntMapS.mapEither
-- |'IntMapS.mapEitherWithKey'
ims_mapEitherWithKey = IntMapS.mapEitherWithKey
-- |'IntMapS.split'
ims_split = IntMapS.split
-- |'IntMapS.splitLookup'
ims_splitLookup = IntMapS.splitLookup
-- |'IntMapS.isSubmapOf'
ims_isSubmapOf = IntMapS.isSubmapOf
-- |'IntMapS.isSubmapOfBy'
ims_isSubmapOfBy = IntMapS.isSubmapOfBy
-- |'IntMapS.isProperSubmapOf'
ims_isProperSubmapOf = IntMapS.isProperSubmapOf
-- |'IntMapS.isProperSubmapOfBy'
ims_isProperSubmapOfBy = IntMapS.isProperSubmapOfBy
-- |'IntMapS.findMin'
ims_findMin = IntMapS.findMin
-- |'IntMapS.findMax'
ims_findMax = IntMapS.findMax
-- |'IntMapS.deleteMin'
ims_deleteMin = IntMapS.deleteMin
-- |'IntMapS.deleteMax'
ims_deleteMax = IntMapS.deleteMax
-- |'IntMapS.deleteFindMin'
ims_deleteFindMin = IntMapS.deleteFindMin
-- |'IntMapS.deleteFindMax'
ims_deleteFindMax = IntMapS.deleteFindMax
-- |'IntMapS.updateMin'
ims_updateMin = IntMapS.updateMin
-- |'IntMapS.updateMax'
ims_updateMax = IntMapS.updateMax
-- |'IntMapS.updateMinWithKey'
ims_updateMinWithKey = IntMapS.updateMinWithKey
-- |'IntMapS.updateMaxWithKey'
ims_updateMaxWithKey = IntMapS.updateMaxWithKey
-- |'IntMapS.minView'
ims_minView = IntMapS.minView
-- |'IntMapS.maxView'
ims_maxView = IntMapS.maxView
-- |'IntMapS.minViewWithKey'
ims_minViewWithKey = IntMapS.minViewWithKey
-- |'IntMapS.maxViewWithKey'
ims_maxViewWithKey = IntMapS.maxViewWithKey
-- |'IntMapS.showTree'
ims_showTree = IntMapS.showTree
-- |'IntMapS.showTreeWith'
ims_showTreeWith = IntMapS.showTreeWith

-- Data.IntSet
-- |'IntSet.IntSet'
type IntSet = IntSet.IntSet
-- |'IntSet.null'
is_null = IntSet.null
-- |'IntSet.size'
is_size = IntSet.size
-- |'IntSet.member'
is_member = IntSet.member
-- |'IntSet.notMember'
is_notMember = IntSet.notMember
-- |'IntSet.lookupLT'
is_lookupLT = IntSet.lookupLT
-- |'IntSet.lookupGT'
is_lookupGT = IntSet.lookupGT
-- |'IntSet.lookupLE'
is_lookupLE = IntSet.lookupLE
-- |'IntSet.lookupGE'
is_lookupGE = IntSet.lookupGE
-- |'IntSet.isSubsetOf'
is_isSubsetOf = IntSet.isSubsetOf
-- |'IntSet.isProperSubsetOf'
is_isProperSubsetOf = IntSet.isProperSubsetOf
-- |'IntSet.empty'
is_empty = IntSet.empty
-- |'IntSet.singleton'
is_singleton = IntSet.singleton
-- |'IntSet.insert'
is_insert = IntSet.insert
-- |'IntSet.delete'
is_delete = IntSet.delete
-- |'IntSet.union'
is_union = IntSet.union
-- |'IntSet.unions'
is_unions = IntSet.unions
-- |'IntSet.difference'
is_difference = IntSet.difference
-- |'IntSet.intersection'
is_intersection = IntSet.intersection
-- |'IntSet.filter'
is_filter = IntSet.filter
-- |'IntSet.partition'
is_partition = IntSet.partition
-- |'IntSet.split'
is_split = IntSet.split
-- |'IntSet.splitMember'
is_splitMember = IntSet.splitMember
-- |'IntSet.map'
is_map = IntSet.map
-- |'IntSet.foldr'
is_foldr = IntSet.foldr
-- |'IntSet.foldl'
is_foldl = IntSet.foldl
-- |'IntSet.foldr''
is_foldr' = IntSet.foldr'
-- |'IntSet.foldl''
is_foldl' = IntSet.foldl'
-- |'IntSet.fold'
is_fold = IntSet.fold
-- |'IntSet.findMin'
is_findMin = IntSet.findMin
-- |'IntSet.findMax'
is_findMax = IntSet.findMax
-- |'IntSet.deleteMin'
is_deleteMin = IntSet.deleteMin
-- |'IntSet.deleteMax'
is_deleteMax = IntSet.deleteMax
-- |'IntSet.deleteFindMin'
is_deleteFindMin = IntSet.deleteFindMin
-- |'IntSet.deleteFindMax'
is_deleteFindMax = IntSet.deleteFindMax
-- |'IntSet.maxView'
is_maxView = IntSet.maxView
-- |'IntSet.minView'
is_minView = IntSet.minView
-- |'IntSet.elems'
is_elems = IntSet.elems
-- |'IntSet.toList'
is_toList = IntSet.toList
-- |'IntSet.fromList'
is_fromList = IntSet.fromList
-- |'IntSet.toAscList'
is_toAscList = IntSet.toAscList
-- |'IntSet.toDescList'
is_toDescList = IntSet.toDescList
-- |'IntSet.fromAscList'
is_fromAscList = IntSet.fromAscList
-- |'IntSet.fromDistinctAscList'
is_fromDistinctAscList = IntSet.fromDistinctAscList
-- |'IntSet.showTree'
is_showTree = IntSet.showTree
-- |'IntSet.showTreeWith'
is_showTreeWith = IntSet.showTreeWith

-- Data.Map.Lazy
-- |'MapL.Map'
type MapL = MapL.Map
-- |('MapL.!')
ml_get = (MapL.!)
-- |'MapL.null'
ml_null = MapL.null
-- |'MapL.size'
ml_size = MapL.size
-- |'MapL.member'
ml_member = MapL.member
-- |'MapL.notMember'
ml_notMember = MapL.notMember
-- |'MapL.lookup'
ml_lookup = MapL.lookup
-- |'MapL.findWithDefault'
ml_findWithDefault = MapL.findWithDefault
-- |'MapL.lookupLT'
ml_lookupLT = MapL.lookupLT
-- |'MapL.lookupGT'
ml_lookupGT = MapL.lookupGT
-- |'MapL.lookupLE'
ml_lookupLE = MapL.lookupLE
-- |'MapL.lookupGE'
ml_lookupGE = MapL.lookupGE
-- |'MapL.empty'
ml_empty = MapL.empty
-- |'MapL.singleton'
ml_singleton = MapL.singleton
-- |'MapL.insert'
ml_insert = MapL.insert
-- |'MapL.insertWith'
ml_insertWith = MapL.insertWith
-- |'MapL.insertWithKey'
ml_insertWithKey = MapL.insertWithKey
-- |'MapL.insertLookupWithKey'
ml_insertLookupWithKey = MapL.insertLookupWithKey
-- |'MapL.delete'
ml_delete = MapL.delete
-- |'MapL.adjust'
ml_adjust = MapL.adjust
-- |'MapL.adjustWithKey'
ml_adjustWithKey = MapL.adjustWithKey
-- |'MapL.update'
ml_update = MapL.update
-- |'MapL.updateWithKey'
ml_updateWithKey = MapL.updateWithKey
-- |'MapL.updateLookupWithKey'
ml_updateLookupWithKey = MapL.updateLookupWithKey
-- |'MapL.alter'
ml_alter = MapL.alter
-- |'MapL.union'
ml_union = MapL.union
-- |'MapL.unionWith'
ml_unionWith = MapL.unionWith
-- |'MapL.unionWithKey'
ml_unionWithKey = MapL.unionWithKey
-- |'MapL.unions'
ml_unions = MapL.unions
-- |'MapL.unionsWith'
ml_unionsWith = MapL.unionsWith
-- |'MapL.difference'
ml_difference = MapL.difference
-- |'MapL.differenceWith'
ml_differenceWith = MapL.differenceWith
-- |'MapL.differenceWithKey'
ml_differenceWithKey = MapL.differenceWithKey
-- |'MapL.intersection'
ml_intersection = MapL.intersection
-- |'MapL.intersectionWith'
ml_intersectionWith = MapL.intersectionWith
-- |'MapL.intersectionWithKey'
ml_intersectionWithKey = MapL.intersectionWithKey
-- |'MapL.mergeWithKey'
ml_mergeWithKey = MapL.mergeWithKey
-- |'MapL.map'
ml_map = MapL.map
-- |'MapL.mapWithKey'
ml_mapWithKey = MapL.mapWithKey
-- |'MapL.traverseWithKey'
ml_traverseWithKey = MapL.traverseWithKey
-- |'MapL.mapAccum'
ml_mapAccum = MapL.mapAccum
-- |'MapL.mapAccumWithKey'
ml_mapAccumWithKey = MapL.mapAccumWithKey
-- |'MapL.mapAccumRWithKey'
ml_mapAccumRWithKey = MapL.mapAccumRWithKey
-- |'MapL.mapKeys'
ml_mapKeys = MapL.mapKeys
-- |'MapL.mapKeysWith'
ml_mapKeysWith = MapL.mapKeysWith
-- |'MapL.mapKeysMonotonic'
ml_mapKeysMonotonic = MapL.mapKeysMonotonic
-- |'MapL.foldr'
ml_foldr = MapL.foldr
-- |'MapL.foldl'
ml_foldl = MapL.foldl
-- |'MapL.foldrWithKey'
ml_foldrWithKey = MapL.foldrWithKey
-- |'MapL.foldlWithKey'
ml_foldlWithKey = MapL.foldlWithKey
-- |'MapL.foldr''
ml_foldr' = MapL.foldr'
-- |'MapL.foldl''
ml_foldl' = MapL.foldl'
-- |'MapL.foldrWithKey''
ml_foldrWithKey' = MapL.foldrWithKey'
-- |'MapL.foldlWithKey''
ml_foldlWithKey' = MapL.foldlWithKey'
-- |'MapL.elems'
ml_elems = MapL.elems
-- |'MapL.keys'
ml_keys = MapL.keys
-- |'MapL.assocs'
ml_assocs = MapL.assocs
-- |'MapL.keysSet'
ml_keysSet = MapL.keysSet
-- |'MapL.fromSet'
ml_fromSet = MapL.fromSet
-- |'MapL.toList'
ml_toList = MapL.toList
-- |'MapL.fromList'
ml_fromList = MapL.fromList
-- |'MapL.fromListWith'
ml_fromListWith = MapL.fromListWith
-- |'MapL.fromListWithKey'
ml_fromListWithKey = MapL.fromListWithKey
-- |'MapL.toAscList'
ml_toAscList = MapL.toAscList
-- |'MapL.toDescList'
ml_toDescList = MapL.toDescList
-- |'MapL.fromAscList'
ml_fromAscList = MapL.fromAscList
-- |'MapL.fromAscListWith'
ml_fromAscListWith = MapL.fromAscListWith
-- |'MapL.fromAscListWithKey'
ml_fromAscListWithKey = MapL.fromAscListWithKey
-- |'MapL.fromDistinctAscList'
ml_fromDistinctAscList = MapL.fromDistinctAscList
-- |'MapL.filter'
ml_filter = MapL.filter
-- |'MapL.filterWithKey'
ml_filterWithKey = MapL.filterWithKey
-- |'MapL.partition'
ml_partition = MapL.partition
-- |'MapL.partitionWithKey'
ml_partitionWithKey = MapL.partitionWithKey
-- |'MapL.mapMaybe'
ml_mapMaybe = MapL.mapMaybe
-- |'MapL.mapMaybeWithKey'
ml_mapMaybeWithKey = MapL.mapMaybeWithKey
-- |'MapL.mapEither'
ml_mapEither = MapL.mapEither
-- |'MapL.mapEitherWithKey'
ml_mapEitherWithKey = MapL.mapEitherWithKey
-- |'MapL.split'
ml_split = MapL.split
-- |'MapL.splitLookup'
ml_splitLookup = MapL.splitLookup
-- |'MapL.isSubmapOf'
ml_isSubmapOf = MapL.isSubmapOf
-- |'MapL.isSubmapOfBy'
ml_isSubmapOfBy = MapL.isSubmapOfBy
-- |'MapL.isProperSubmapOf'
ml_isProperSubmapOf = MapL.isProperSubmapOf
-- |'MapL.isProperSubmapOfBy'
ml_isProperSubmapOfBy = MapL.isProperSubmapOfBy
-- |'MapL.lookupIndex'
ml_lookupIndex = MapL.lookupIndex
-- |'MapL.findIndex'
ml_findIndex = MapL.findIndex
-- |'MapL.elemAt'
ml_elemAt = MapL.elemAt
-- |'MapL.updateAt'
ml_updateAt = MapL.updateAt
-- |'MapL.deleteAt'
ml_deleteAt = MapL.deleteAt
-- |'MapL.findMin'
ml_findMin = MapL.findMin
-- |'MapL.findMax'
ml_findMax = MapL.findMax
-- |'MapL.deleteMin'
ml_deleteMin = MapL.deleteMin
-- |'MapL.deleteMax'
ml_deleteMax = MapL.deleteMax
-- |'MapL.deleteFindMin'
ml_deleteFindMin = MapL.deleteFindMin
-- |'MapL.deleteFindMax'
ml_deleteFindMax = MapL.deleteFindMax
-- |'MapL.updateMin'
ml_updateMin = MapL.updateMin
-- |'MapL.updateMax'
ml_updateMax = MapL.updateMax
-- |'MapL.updateMinWithKey'
ml_updateMinWithKey = MapL.updateMinWithKey
-- |'MapL.updateMaxWithKey'
ml_updateMaxWithKey = MapL.updateMaxWithKey
-- |'MapL.minView'
ml_minView = MapL.minView
-- |'MapL.maxView'
ml_maxView = MapL.maxView
-- |'MapL.minViewWithKey'
ml_minViewWithKey = MapL.minViewWithKey
-- |'MapL.maxViewWithKey'
ml_maxViewWithKey = MapL.maxViewWithKey
-- |'MapL.showTree'
ml_showTree = MapL.showTree
-- |'MapL.showTreeWith'
ml_showTreeWith = MapL.showTreeWith
-- |'MapL.valid'
ml_valid = MapL.valid

-- Data.Map.Strict
-- |'MapS.Map'
type MapS = MapS.Map
-- |('MapS.!')
ms_get = (MapS.!)
-- |'MapS.null'
ms_null = MapS.null
-- |'MapS.size'
ms_size = MapS.size
-- |'MapS.member'
ms_member = MapS.member
-- |'MapS.notMember'
ms_notMember = MapS.notMember
-- |'MapS.lookup'
ms_lookup = MapS.lookup
-- |'MapS.findWithDefault'
ms_findWithDefault = MapS.findWithDefault
-- |'MapS.lookupLT'
ms_lookupLT = MapS.lookupLT
-- |'MapS.lookupGT'
ms_lookupGT = MapS.lookupGT
-- |'MapS.lookupLE'
ms_lookupLE = MapS.lookupLE
-- |'MapS.lookupGE'
ms_lookupGE = MapS.lookupGE
-- |'MapS.empty'
ms_empty = MapS.empty
-- |'MapS.singleton'
ms_singleton = MapS.singleton
-- |'MapS.insert'
ms_insert = MapS.insert
-- |'MapS.insertWith'
ms_insertWith = MapS.insertWith
-- |'MapS.insertWithKey'
ms_insertWithKey = MapS.insertWithKey
-- |'MapS.insertLookupWithKey'
ms_insertLookupWithKey = MapS.insertLookupWithKey
-- |'MapS.delete'
ms_delete = MapS.delete
-- |'MapS.adjust'
ms_adjust = MapS.adjust
-- |'MapS.adjustWithKey'
ms_adjustWithKey = MapS.adjustWithKey
-- |'MapS.update'
ms_update = MapS.update
-- |'MapS.updateWithKey'
ms_updateWithKey = MapS.updateWithKey
-- |'MapS.updateLookupWithKey'
ms_updateLookupWithKey = MapS.updateLookupWithKey
-- |'MapS.alter'
ms_alter = MapS.alter
-- |'MapS.union'
ms_union = MapS.union
-- |'MapS.unionWith'
ms_unionWith = MapS.unionWith
-- |'MapS.unionWithKey'
ms_unionWithKey = MapS.unionWithKey
-- |'MapS.unions'
ms_unions = MapS.unions
-- |'MapS.unionsWith'
ms_unionsWith = MapS.unionsWith
-- |'MapS.difference'
ms_difference = MapS.difference
-- |'MapS.differenceWith'
ms_differenceWith = MapS.differenceWith
-- |'MapS.differenceWithKey'
ms_differenceWithKey = MapS.differenceWithKey
-- |'MapS.intersection'
ms_intersection = MapS.intersection
-- |'MapS.intersectionWith'
ms_intersectionWith = MapS.intersectionWith
-- |'MapS.intersectionWithKey'
ms_intersectionWithKey = MapS.intersectionWithKey
-- |'MapS.mergeWithKey'
ms_mergeWithKey = MapS.mergeWithKey
-- |'MapS.map'
ms_map = MapS.map
-- |'MapS.mapWithKey'
ms_mapWithKey = MapS.mapWithKey
-- |'MapS.traverseWithKey'
ms_traverseWithKey = MapS.traverseWithKey
-- |'MapS.mapAccum'
ms_mapAccum = MapS.mapAccum
-- |'MapS.mapAccumWithKey'
ms_mapAccumWithKey = MapS.mapAccumWithKey
-- |'MapS.mapAccumRWithKey'
ms_mapAccumRWithKey = MapS.mapAccumRWithKey
-- |'MapS.mapKeys'
ms_mapKeys = MapS.mapKeys
-- |'MapS.mapKeysWith'
ms_mapKeysWith = MapS.mapKeysWith
-- |'MapS.mapKeysMonotonic'
ms_mapKeysMonotonic = MapS.mapKeysMonotonic
-- |'MapS.foldr'
ms_foldr = MapS.foldr
-- |'MapS.foldl'
ms_foldl = MapS.foldl
-- |'MapS.foldrWithKey'
ms_foldrWithKey = MapS.foldrWithKey
-- |'MapS.foldlWithKey'
ms_foldlWithKey = MapS.foldlWithKey
-- |'MapS.foldr''
ms_foldr' = MapS.foldr'
-- |'MapS.foldl''
ms_foldl' = MapS.foldl'
-- |'MapS.foldrWithKey''
ms_foldrWithKey' = MapS.foldrWithKey'
-- |'MapS.foldlWithKey''
ms_foldlWithKey' = MapS.foldlWithKey'
-- |'MapS.elems'
ms_elems = MapS.elems
-- |'MapS.keys'
ms_keys = MapS.keys
-- |'MapS.assocs'
ms_assocs = MapS.assocs
-- |'MapS.keysSet'
ms_keysSet = MapS.keysSet
-- |'MapS.fromSet'
ms_fromSet = MapS.fromSet
-- |'MapS.toList'
ms_toList = MapS.toList
-- |'MapS.fromList'
ms_fromList = MapS.fromList
-- |'MapS.fromListWith'
ms_fromListWith = MapS.fromListWith
-- |'MapS.fromListWithKey'
ms_fromListWithKey = MapS.fromListWithKey
-- |'MapS.toAscList'
ms_toAscList = MapS.toAscList
-- |'MapS.toDescList'
ms_toDescList = MapS.toDescList
-- |'MapS.fromAscList'
ms_fromAscList = MapS.fromAscList
-- |'MapS.fromAscListWith'
ms_fromAscListWith = MapS.fromAscListWith
-- |'MapS.fromAscListWithKey'
ms_fromAscListWithKey = MapS.fromAscListWithKey
-- |'MapS.fromDistinctAscList'
ms_fromDistinctAscList = MapS.fromDistinctAscList
-- |'MapS.filter'
ms_filter = MapS.filter
-- |'MapS.filterWithKey'
ms_filterWithKey = MapS.filterWithKey
-- |'MapS.partition'
ms_partition = MapS.partition
-- |'MapS.partitionWithKey'
ms_partitionWithKey = MapS.partitionWithKey
-- |'MapS.mapMaybe'
ms_mapMaybe = MapS.mapMaybe
-- |'MapS.mapMaybeWithKey'
ms_mapMaybeWithKey = MapS.mapMaybeWithKey
-- |'MapS.mapEither'
ms_mapEither = MapS.mapEither
-- |'MapS.mapEitherWithKey'
ms_mapEitherWithKey = MapS.mapEitherWithKey
-- |'MapS.split'
ms_split = MapS.split
-- |'MapS.splitLookup'
ms_splitLookup = MapS.splitLookup
-- |'MapS.isSubmapOf'
ms_isSubmapOf = MapS.isSubmapOf
-- |'MapS.isSubmapOfBy'
ms_isSubmapOfBy = MapS.isSubmapOfBy
-- |'MapS.isProperSubmapOf'
ms_isProperSubmapOf = MapS.isProperSubmapOf
-- |'MapS.isProperSubmapOfBy'
ms_isProperSubmapOfBy = MapS.isProperSubmapOfBy
-- |'MapS.lookupIndex'
ms_lookupIndex = MapS.lookupIndex
-- |'MapS.findIndex'
ms_findIndex = MapS.findIndex
-- |'MapS.elemAt'
ms_elemAt = MapS.elemAt
-- |'MapS.updateAt'
ms_updateAt = MapS.updateAt
-- |'MapS.deleteAt'
ms_deleteAt = MapS.deleteAt
-- |'MapS.findMin'
ms_findMin = MapS.findMin
-- |'MapS.findMax'
ms_findMax = MapS.findMax
-- |'MapS.deleteMin'
ms_deleteMin = MapS.deleteMin
-- |'MapS.deleteMax'
ms_deleteMax = MapS.deleteMax
-- |'MapS.deleteFindMin'
ms_deleteFindMin = MapS.deleteFindMin
-- |'MapS.deleteFindMax'
ms_deleteFindMax = MapS.deleteFindMax
-- |'MapS.updateMin'
ms_updateMin = MapS.updateMin
-- |'MapS.updateMax'
ms_updateMax = MapS.updateMax
-- |'MapS.updateMinWithKey'
ms_updateMinWithKey = MapS.updateMinWithKey
-- |'MapS.updateMaxWithKey'
ms_updateMaxWithKey = MapS.updateMaxWithKey
-- |'MapS.minView'
ms_minView = MapS.minView
-- |'MapS.maxView'
ms_maxView = MapS.maxView
-- |'MapS.minViewWithKey'
ms_minViewWithKey = MapS.minViewWithKey
-- |'MapS.maxViewWithKey'
ms_maxViewWithKey = MapS.maxViewWithKey
-- |'MapS.showTree'
ms_showTree = MapS.showTree
-- |'MapS.showTreeWith'
ms_showTreeWith = MapS.showTreeWith
-- |'MapS.valid'
ms_valid = MapS.valid

-- Data.Set
-- |'Set.Set'
type Set = Set.Set
-- |'Set.null'
s_null = Set.null
-- |'Set.size'
s_size = Set.size
-- |'Set.member'
s_member = Set.member
-- |'Set.notMember'
s_notMember = Set.notMember
-- |'Set.lookupLT'
s_lookupLT = Set.lookupLT
-- |'Set.lookupGT'
s_lookupGT = Set.lookupGT
-- |'Set.lookupLE'
s_lookupLE = Set.lookupLE
-- |'Set.lookupGE'
s_lookupGE = Set.lookupGE
-- |'Set.isSubsetOf'
s_isSubsetOf = Set.isSubsetOf
-- |'Set.isProperSubsetOf'
s_isProperSubsetOf = Set.isProperSubsetOf
-- |'Set.empty'
s_empty = Set.empty
-- |'Set.singleton'
s_singleton = Set.singleton
-- |'Set.insert'
s_insert = Set.insert
-- |'Set.delete'
s_delete = Set.delete
-- |'Set.union'
s_union = Set.union
-- |'Set.unions'
s_unions = Set.unions
-- |'Set.difference'
s_difference = Set.difference
-- |'Set.intersection'
s_intersection = Set.intersection
-- |'Set.filter'
s_filter = Set.filter
-- |'Set.partition'
s_partition = Set.partition
-- |'Set.split'
s_split = Set.split
-- |'Set.splitMember'
s_splitMember = Set.splitMember
-- |'Set.map'
s_map = Set.map
-- |'Set.mapMonotonic'
s_mapMonotonic = Set.mapMonotonic
-- |'Set.foldr'
s_foldr = Set.foldr
-- |'Set.foldl'
s_foldl = Set.foldl
-- |'Set.foldr''
s_foldr' = Set.foldr'
-- |'Set.foldl''
s_foldl' = Set.foldl'
-- |'Set.fold'
s_fold = Set.fold
-- |'Set.findMin'
s_findMin = Set.findMin
-- |'Set.findMax'
s_findMax = Set.findMax
-- |'Set.deleteMin'
s_deleteMin = Set.deleteMin
-- |'Set.deleteMax'
s_deleteMax = Set.deleteMax
-- |'Set.deleteFindMin'
s_deleteFindMin = Set.deleteFindMin
-- |'Set.deleteFindMax'
s_deleteFindMax = Set.deleteFindMax
-- |'Set.maxView'
s_maxView = Set.maxView
-- |'Set.minView'
s_minView = Set.minView
-- |'Set.elems'
s_elems = Set.elems
-- |'Set.toList'
s_toList = Set.toList
-- |'Set.fromList'
s_fromList = Set.fromList
-- |'Set.toAscList'
s_toAscList = Set.toAscList
-- |'Set.toDescList'
s_toDescList = Set.toDescList
-- |'Set.fromAscList'
s_fromAscList = Set.fromAscList
-- |'Set.fromDistinctAscList'
s_fromDistinctAscList = Set.fromDistinctAscList
-- |'Set.showTree'
s_showTree = Set.showTree
-- |'Set.showTreeWith'
s_showTreeWith = Set.showTreeWith
-- |'Set.valid'
s_valid = Set.valid

-- Data.Sequence
-- |'Seq.Seq'
type Seq = Seq.Seq
-- |'Seq.empty'
seq_empty = Seq.empty
-- |'Seq.singleton'
seq_singleton = Seq.singleton
-- |('Seq.<|')
seq_cons = (Seq.<|)
-- |('Seq.|>')
seq_snoc = (Seq.|>)
-- |('Seq.><')
seq_cat  = (Seq.><)
-- |'Seq.fromList'
seq_fromList = Seq.fromList
-- |'Seq.replicate'
seq_replicate = Seq.replicate
-- |'Seq.replicateA'
seq_replicateA = Seq.replicateA
-- |'Seq.replicateM'
seq_replicateM = Seq.replicateM
-- |'Seq.iterateN'
seq_iterateN = Seq.iterateN
-- |'Seq.unfoldr'
seq_unfoldr = Seq.unfoldr
-- |'Seq.unfoldl'
seq_unfoldl = Seq.unfoldl
-- |'Seq.null'
seq_null = Seq.null
-- |'Seq.length'
seq_length = Seq.length
-- |'Seq.ViewL'
--
-- Constructors 'Seq.EmptyL' and ('Seq.:<') not reexported.
type ViewL = Seq.ViewL
-- |'Seq.viewl'
seq_viewl = Seq.viewl
-- |'Seq.ViewR'
--
-- Constructors 'Seq.EmptyR' and ('Seq.:>') not reexported.
type ViewR a = Seq.ViewR
seq_viewr = Seq.viewr
-- |'Seq.scanl'
seq_scanl = Seq.scanl
-- |'Seq.scanl1'
seq_scanl1 = Seq.scanl1
-- |'Seq.scanr'
seq_scanr = Seq.scanr
-- |'Seq.scanr1'
seq_scanr1 = Seq.scanr1
-- |'Seq.tails'
seq_tails = Seq.tails
-- |'Seq.inits'
seq_inits = Seq.inits
-- |'Seq.takeWhileL'
seq_takeWhileL = Seq.takeWhileL
-- |'Seq.takeWhileR'
seq_takeWhileR = Seq.takeWhileR
-- |'Seq.dropWhileL'
seq_dropWhileL = Seq.dropWhileL
-- |'Seq.dropWhileR'
seq_dropWhileR = Seq.dropWhileR
-- |'Seq.spanl'
seq_spanl = Seq.spanl
-- |'Seq.spanr'
seq_spanr = Seq.spanr
-- |'Seq.breakl'
seq_breakl = Seq.breakl
-- |'Seq.breakr'
seq_breakr = Seq.breakr
-- |'Seq.partition'
seq_partition = Seq.partition
-- |'Seq.filter'
seq_filter = Seq.filter
-- |'Seq.sort'
seq_sort = Seq.sort
-- |'Seq.sortBy'
seq_sortBy = Seq.sortBy
-- |'Seq.unstableSort'
seq_unstableSort = Seq.unstableSort
-- |'Seq.unstableSortBy'
seq_unstableSortBy = Seq.unstableSortBy
-- |'Seq.index'
seq_index = Seq.index
-- |'Seq.adjust'
seq_adjust = Seq.adjust
-- |'Seq.update'
seq_update = Seq.update
-- |'Seq.take'
seq_take = Seq.take
-- |'Seq.drop'
seq_drop = Seq.drop
-- |'Seq.splitAt'
seq_splitAt = Seq.splitAt
-- |'Seq.elemIndexL'
seq_elemIndexL = Seq.elemIndexL
-- |'Seq.elemIndicesL'
seq_elemIndicesL = Seq.elemIndicesL
-- |'Seq.elemIndexR'
seq_elemIndexR = Seq.elemIndexR
-- |'Seq.elemIndicesR'
seq_elemIndicesR = Seq.elemIndicesR
-- |'Seq.findIndexL'
seq_findIndexL = Seq.findIndexL
-- |'Seq.findIndicesL'
seq_findIndicesL = Seq.findIndicesL
-- |'Seq.findIndexR'
seq_findIndexR = Seq.findIndexR
-- |'Seq.findIndicesR'
seq_findIndicesR = Seq.findIndicesR
-- |'Seq.foldlWithIndex'
seq_foldlWithIndex = Seq.foldlWithIndex
-- |'Seq.foldrWithIndex'
seq_foldrWithIndex = Seq.foldrWithIndex
-- |'Seq.mapWithIndex'
seq_mapWithIndex = Seq.mapWithIndex
-- |'Seq.reverse'
seq_reverse = Seq.reverse
-- |'Seq.zip'
seq_zip = Seq.zip
-- |'Seq.zipWith'
seq_zipWith = Seq.zipWith
-- |'Seq.zip3'
seq_zip3 = Seq.zip3
-- |'Seq.zipWith3'
seq_zipWith3 = Seq.zipWith3
-- |'Seq.zip4'
seq_zip4 = Seq.zip4
-- |'Seq.zipWith4'
seq_zipWith4 = Seq.zipWith4

-- Data.Text
-- |'T.Text'
type Text = T.Text
-- |'T.pack'
t_pack = T.pack
-- |'T.unpack'
t_unpack = T.unpack
-- |'T.singleton'
t_singleton = T.singleton
-- |'T.empty'
t_empty = T.empty
-- |'T.cons'
t_cons = T.cons
-- |'T.snoc'
t_snoc = T.snoc
-- |'T.append'
t_append = T.append
-- |'T.uncons'
t_uncons = T.uncons
-- |'T.head'
t_head = T.head
-- |'T.last'
t_last = T.last
-- |'T.tail'
t_tail = T.tail
-- |'T.init'
t_init = T.init
-- |'T.null'
t_null = T.null
-- |'T.length'
t_length = T.length
-- |'T.compareLength'
t_compareLength = T.compareLength
-- |'T.map'
t_map = T.map
-- |'T.intercalate'
t_intercalate = T.intercalate
-- |'T.intersperse'
t_intersperse = T.intersperse
-- |'T.transpose'
t_transpose = T.transpose
-- |'T.reverse'
t_reverse = T.reverse
-- |'T.replace'
t_replace = T.replace
-- |'T.toCaseFold'
t_toCaseFold = T.toCaseFold
-- |'T.toLower'
t_toLower = T.toLower
-- |'T.toUpper'
t_toUpper = T.toUpper
-- |'T.justifyLeft'
t_justifyLeft = T.justifyLeft
-- |'T.justifyRight'
t_justifyRight = T.justifyRight
-- |'T.center'
t_center = T.center
-- |'T.foldl'
t_foldl = T.foldl
-- |'T.foldl''
t_foldl' = T.foldl'
-- |'T.foldl1'
t_foldl1 = T.foldl1
-- |'T.foldl1''
t_foldl1' = T.foldl1'
-- |'T.foldr'
t_foldr = T.foldr
-- |'T.foldr1'
t_foldr1 = T.foldr1
-- |'T.concat'
t_concat = T.concat
-- |'T.concatMap'
t_concatMap = T.concatMap
-- |'T.any'
t_any = T.any
-- |'T.all'
t_all = T.all
-- |'T.maximum'
t_maximum = T.maximum
-- |'T.minimum'
t_minimum = T.minimum
-- |'T.scanl'
t_scanl = T.scanl
-- |'T.scanl1'
t_scanl1 = T.scanl1
-- |'T.scanr'
t_scanr = T.scanr
-- |'T.scanr1'
t_scanr1 = T.scanr1
-- |'T.mapAccumL'
t_mapAccumL = T.mapAccumL
-- |'T.mapAccumR'
t_mapAccumR = T.mapAccumR
-- |'T.replicate'
t_replicate = T.replicate
-- |'T.unfoldr'
t_unfoldr = T.unfoldr
-- |'T.unfoldrN'
t_unfoldrN = T.unfoldrN
-- |'T.take'
t_take = T.take
-- |'T.drop'
t_drop = T.drop
-- |'T.takeWhile'
t_takeWhile = T.takeWhile
-- |'T.dropWhile'
t_dropWhile = T.dropWhile
-- |'T.dropWhileEnd'
t_dropWhileEnd = T.dropWhileEnd
-- |'T.dropAround'
t_dropAround = T.dropAround
-- |'T.strip'
t_strip = T.strip
-- |'T.stripStart'
t_stripStart = T.stripStart
-- |'T.stripEnd'
t_stripEnd = T.stripEnd
-- |'T.splitAt'
t_splitAt = T.splitAt
-- |'T.breakOn'
t_breakOn = T.breakOn
-- |'T.breakOnEnd'
t_breakOnEnd = T.breakOnEnd
-- |'T.break'
t_break = T.break
-- |'T.span'
t_span = T.span
-- |'T.group'
t_group = T.group
-- |'T.groupBy'
t_groupBy = T.groupBy
-- |'T.inits'
t_inits = T.inits
-- |'T.tails'
t_tails = T.tails
-- |'T.splitOn'
t_splitOn = T.splitOn
-- |'T.split'
t_split = T.split
-- |'T.chunksOf'
t_chunksOf = T.chunksOf
-- |'T.lines'
t_lines = T.lines
-- |'T.words'
t_words = T.words
-- |'T.unlines'
t_unlines = T.unlines
-- |'T.unwords'
t_unwords = T.unwords
-- |'T.isPrefixOf'
t_isPrefixOf = T.isPrefixOf
-- |'T.isSuffixOf'
t_isSuffixOf = T.isSuffixOf
-- |'T.isInfixOf'
t_isInfixOf = T.isInfixOf
-- |'T.stripPrefix'
t_stripPrefix = T.stripPrefix
-- |'T.stripSuffix'
t_stripSuffix = T.stripSuffix
-- |'T.commonPrefixes'
t_commonPrefixes = T.commonPrefixes
-- |'T.filter'
t_filter = T.filter
-- |'T.breakOnAll'
t_breakOnAll = T.breakOnAll
-- |'T.find'
t_find = T.find
-- |'T.partition'
t_partition = T.partition
-- |'T.index'
t_index = T.index
-- |'T.findIndex'
t_findIndex = T.findIndex
-- |'T.count'
t_count = T.count
-- |'T.zip'
t_zip = T.zip
-- |'T.zipWith'
t_zipWith = T.zipWith
-- |'T.copy'
t_copy = T.copy

-- Data.Text.Array
-- |'TA.Array'
type TxtArray  = TA.Array
-- |'TA.MArray'
type TxtMArray = TA.MArray
-- |'TA.copyM'
ta_copyM = TA.copyM
-- |'TA.copyI'
ta_copyI = TA.copyI
-- |'TA.empty'
ta_empty = TA.empty
-- |'TA.equal'
ta_equal = TA.equal
-- |'TA.run'
ta_run = TA.run
-- |'TA.run2'
ta_run2 = TA.run2
-- |'TA.toList'
ta_toList = TA.toList
-- |'TA.unsafeFreeze'
ta_unsafeFreeze = TA.unsafeFreeze
-- |'TA.unsafeIndex'
ta_unsafeIndex = TA.unsafeIndex
-- |'TA.new'
ta_new = TA.new
-- |'TA.unsafeWrite'
ta_unsafeWrite = TA.unsafeWrite

-- Data.Text.Encoding
-- |'TE.decodeLatin1'
t_decodeLatin1 = TE.decodeLatin1
-- |'TE.decodeUtf8'
t_decodeUtf8 = TE.decodeUtf8
-- |'TE.decodeUtf16LE'
t_decodeUtf16LE = TE.decodeUtf16LE
-- |'TE.decodeUtf16BE'
t_decodeUtf16BE = TE.decodeUtf16BE
-- |'TE.decodeUtf32LE'
t_decodeUtf32LE = TE.decodeUtf32LE
-- |'TE.decodeUtf32BE'
t_decodeUtf32BE = TE.decodeUtf32BE
-- |'TE.decodeUtf8''
t_decodeUtf8' = TE.decodeUtf8'
-- |'TE.decodeUtf8With'
t_decodeUtf8With = TE.decodeUtf8With
-- |'TE.decodeUtf16LEWith'
t_decodeUtf16LEWith = TE.decodeUtf16LEWith
-- |'TE.decodeUtf16BEWith'
t_decodeUtf16BEWith = TE.decodeUtf16BEWith
-- |'TE.decodeUtf32LEWith'
t_decodeUtf32LEWith = TE.decodeUtf32LEWith
-- |'TE.decodeUtf32BEWith'
t_decodeUtf32BEWith = TE.decodeUtf32BEWith
-- |'TE.encodeUtf8'
t_encodeUtf8 = TE.encodeUtf8
-- |'TE.encodeUtf16LE'
t_encodeUtf16LE = TE.encodeUtf16LE
-- |'TE.encodeUtf16BE'
t_encodeUtf16BE = TE.encodeUtf16BE
-- |'TE.encodeUtf32LE'
t_encodeUtf32LE = TE.encodeUtf32LE
-- |'TE.encodeUtf32BE'
t_encodeUtf32BE = TE.encodeUtf32BE

-- Data.Text.Encoding.Error
-- |'TEE.UnicodeException'
--
-- Constructors 'TEE.DecodeError' and 'TEE.EncodeError' not reexported
type TUnicodeException = TEE.UnicodeException
-- |'TEE.OnError'
type TOnError a b = String -> Maybe a -> Maybe b
-- |'TEE.OnDecodeError'
type TOnDecodeError = TOnError Word8 Char
-- |'TEE.OnEncodeError'
type TOnEncodeError = TOnError Char Word8
-- |'TEE.lenientDecode'
te_lenientDecode = TEE.lenientDecode
-- |'TEE.strictDecode'
te_strictDecode = TEE.strictDecode
-- |'TEE.strictEncode'
te_strictEncode = TEE.strictEncode
-- |'TEE.ignore'
te_ignore = TEE.ignore
-- |'TEE.replace'
te_replace = TEE.replace

-- Data.Text.Foreign
-- |'TF.I16'
type I16 = TF.I16
-- |'TF.fromPtr'
t_fromPtr = TF.fromPtr
-- |'TF.useAsPtr'
t_useAsPtr = TF.useAsPtr
-- |'TF.asForeignPtr'
t_asForeignPtr = TF.asForeignPtr
-- |'TF.lengthWord16'
t_lengthWord16 = TF.lengthWord16
-- |'TF.unsafeCopyToPtr'
t_unsafeCopyToPtr = TF.unsafeCopyToPtr
-- |'TF.dropWord16'
t_dropWord16 = TF.dropWord16
-- |'TF.takeWord16'
t_takeWord16 = TF.takeWord16

-- Data.Text.IO
-- |'TIO.readFile'
t_readFile = TIO.readFile
-- |'TIO.writeFile'
t_writeFile = TIO.writeFile
-- |'TIO.appendFile'
t_appendFile = TIO.appendFile
-- |'TIO.hGetContents'
t_hGetContents = TIO.hGetContents
-- |'TIO.hGetChunk'
t_hGetChunk = TIO.hGetChunk
-- |'TIO.hGetLine'
t_hGetLine = TIO.hGetLine
-- |'TIO.hPutStr'
t_hPutStr = TIO.hPutStr
-- |'TIO.hPutStrLn'
t_hPutStrLn = TIO.hPutStrLn
-- |'TIO.interact'
t_interact = TIO.interact
-- |'TIO.getContents'
t_getContents = TIO.getContents
-- |'TIO.getLine'
t_getLine = TIO.getLine
-- |'TIO.putStr'
t_putStr = TIO.putStr
-- |'TIO.putStrLn'
t_putStrLn = TIO.putStrLn

-- Data.Text.Lazy
-- |'TL.Text'
type TextL = TL.Text
-- |'TL.pack'
tl_pack = TL.pack
-- |'TL.unpack'
tl_unpack = TL.unpack
-- |'TL.singleton'
tl_singleton = TL.singleton
-- |'TL.empty'
tl_empty = TL.empty
-- |'TL.fromChunks'
tl_fromChunks = TL.fromChunks
-- |'TL.toChunks'
tl_toChunks = TL.toChunks
-- |'TL.toStrict'
tl_toStrict = TL.toStrict
-- |'TL.fromStrict'
tl_fromStrict = TL.fromStrict
-- |'TL.foldrChunks'
tl_foldrChunks = TL.foldrChunks
-- |'TL.foldlChunks'
tl_foldlChunks = TL.foldlChunks
-- |'TL.cons'
tl_cons = TL.cons
-- |'TL.snoc'
tl_snoc = TL.snoc
-- |'TL.append'
tl_append = TL.append
-- |'TL.uncons'
tl_uncons = TL.uncons
-- |'TL.head'
tl_head = TL.head
-- |'TL.last'
tl_last = TL.last
-- |'TL.tail'
tl_tail = TL.tail
-- |'TL.init'
tl_init = TL.init
-- |'TL.null'
tl_null = TL.null
-- |'TL.length'
tl_length = TL.length
-- |'TL.compareLength'
tl_compareLength = TL.compareLength
-- |'TL.map'
tl_map = TL.map
-- |'TL.intercalate'
tl_intercalate = TL.intercalate
-- |'TL.intersperse'
tl_intersperse = TL.intersperse
-- |'TL.transpose'
tl_transpose = TL.transpose
-- |'TL.reverse'
tl_reverse = TL.reverse
-- |'TL.replace'
tl_replace = TL.replace
-- |'TL.toCaseFold'
tl_toCaseFold = TL.toCaseFold
-- |'TL.toLower'
tl_toLower = TL.toLower
-- |'TL.toUpper'
tl_toUpper = TL.toUpper
-- |'TL.justifyLeft'
tl_justifyLeft = TL.justifyLeft
-- |'TL.justifyRight'
tl_justifyRight = TL.justifyRight
-- |'TL.center'
tl_center = TL.center
-- |'TL.foldl'
tl_foldl = TL.foldl
-- |'TL.foldl''
tl_foldl' = TL.foldl'
-- |'TL.foldl1'
tl_foldl1 = TL.foldl1
-- |'TL.foldl1''
tl_foldl1' = TL.foldl1'
-- |'TL.foldr'
tl_foldr = TL.foldr
-- |'TL.foldr1'
tl_foldr1 = TL.foldr1
-- |'TL.concat'
tl_concat = TL.concat
-- |'TL.concatMap'
tl_concatMap = TL.concatMap
-- |'TL.any'
tl_any = TL.any
-- |'TL.all'
tl_all = TL.all
-- |'TL.maximum'
tl_maximum = TL.maximum
-- |'TL.minimum'
tl_minimum = TL.minimum
-- |'TL.scanl'
tl_scanl = TL.scanl
-- |'TL.scanl1'
tl_scanl1 = TL.scanl1
-- |'TL.scanr'
tl_scanr = TL.scanr
-- |'TL.scanr1'
tl_scanr1 = TL.scanr1
-- |'TL.mapAccumL'
tl_mapAccumL = TL.mapAccumL
-- |'TL.mapAccumR'
tl_mapAccumR = TL.mapAccumR
-- |'TL.replicate'
tl_replicate = TL.replicate
-- |'TL.unfoldr'
tl_unfoldr = TL.unfoldr
-- |'TL.unfoldrN'
tl_unfoldrN = TL.unfoldrN
-- |'TL.take'
tl_take = TL.take
-- |'TL.drop'
tl_drop = TL.drop
-- |'TL.takeWhile'
tl_takeWhile = TL.takeWhile
-- |'TL.dropWhile'
tl_dropWhile = TL.dropWhile
-- |'TL.dropWhileEnd'
tl_dropWhileEnd = TL.dropWhileEnd
-- |'TL.dropAround'
tl_dropAround = TL.dropAround
-- |'TL.strip'
tl_strip = TL.strip
-- |'TL.stripStart'
tl_stripStart = TL.stripStart
-- |'TL.stripEnd'
tl_stripEnd = TL.stripEnd
-- |'TL.splitAt'
tl_splitAt = TL.splitAt
-- |'TL.span'
tl_span = TL.span
-- |'TL.breakOn'
tl_breakOn = TL.breakOn
-- |'TL.breakOnEnd'
tl_breakOnEnd = TL.breakOnEnd
-- |'TL.break'
tl_break = TL.break
-- |'TL.group'
tl_group = TL.group
-- |'TL.groupBy'
tl_groupBy = TL.groupBy
-- |'TL.inits'
tl_inits = TL.inits
-- |'TL.tails'
tl_tails = TL.tails
-- |'TL.splitOn'
tl_splitOn = TL.splitOn
-- |'TL.split'
tl_split = TL.split
-- |'TL.chunksOf'
tl_chunksOf = TL.chunksOf
-- |'TL.lines'
tl_lines = TL.lines
-- |'TL.words'
tl_words = TL.words
-- |'TL.unlines'
tl_unlines = TL.unlines
-- |'TL.unwords'
tl_unwords = TL.unwords
-- |'TL.isPrefixOf'
tl_isPrefixOf = TL.isPrefixOf
-- |'TL.isSuffixOf'
tl_isSuffixOf = TL.isSuffixOf
-- |'TL.isInfixOf'
tl_isInfixOf = TL.isInfixOf
-- |'TL.stripPrefix'
tl_stripPrefix = TL.stripPrefix
-- |'TL.stripSuffix'
tl_stripSuffix = TL.stripSuffix
-- |'TL.commonPrefixes'
tl_commonPrefixes = TL.commonPrefixes
-- |'TL.filter'
tl_filter = TL.filter
-- |'TL.find'
tl_find = TL.find
-- |'TL.breakOnAll'
tl_breakOnAll = TL.breakOnAll
-- |'TL.partition'
tl_partition = TL.partition
-- |'TL.index'
tl_index = TL.index
-- |'TL.count'
tl_count = TL.count
-- |'TL.zip'
tl_zip = TL.zip
-- |'TL.zipWith'
tl_zipWith = TL.zipWith

-- Data.Text.Lazy.Builder
-- |'TLB.Builder'
type TBuilder = TLB.Builder
-- |'TLB.toLazyText'
tb_toLazyText = TLB.toLazyText
-- |'TLB.toLazyTextWith'
tb_toLazyTextWith = TLB.toLazyTextWith
-- |'TLB.singleton'
tb_singleton = TLB.singleton
-- |'TLB.fromText'
tb_fromText = TLB.fromText
-- |'TLB.fromLazyText'
tb_fromLazyText = TLB.fromLazyText
-- |'TLB.fromString'
tb_fromString = TLB.fromString
-- |'TLB.flush'
tb_flush = TLB.flush

-- Data.Text.Lazy.Builder.Int
-- |'TLBI.decimal'
tb_decimal = TLBI.decimal
-- |'TLBI.hexadecimal'
tb_hexadecimal = TLBI.hexadecimal

-- Data.Text.Lazy.Builder.RealFloat
-- |'TLBR.FPFormat'
type TBFPFormat = TLBR.FPFormat
-- |'TLBR.Exponent'
tb_exponent = TLBR.Exponent
-- |'TLBR.Fixed'
tb_fixed = TLBR.Fixed
-- |'TLBR.Generic'
tb_generic = TLBR.Generic
-- |'TLBR.realFloat'
tb_realFloat = TLBR.realFloat
-- |'TLBR.formatRealFloat'
tb_formatRealFloat = TLBR.formatRealFloat

-- Data.Text.Lazy.Encoding
-- |'TLE.decodeLatin1'
tl_decodeLatin1 = TLE.decodeLatin1
-- |'TLE.decodeUtf8'
tl_decodeUtf8 = TLE.decodeUtf8
-- |'TLE.decodeUtf16LE'
tl_decodeUtf16LE = TLE.decodeUtf16LE
-- |'TLE.decodeUtf16BE'
tl_decodeUtf16BE = TLE.decodeUtf16BE
-- |'TLE.decodeUtf32LE'
tl_decodeUtf32LE = TLE.decodeUtf32LE
-- |'TLE.decodeUtf32BE'
tl_decodeUtf32BE = TLE.decodeUtf32BE
-- |'TLE.decodeUtf8''
tl_decodeUtf8' = TLE.decodeUtf8'
-- |'TLE.decodeUtf8With'
tl_decodeUtf8With = TLE.decodeUtf8With
-- |'TLE.decodeUtf16LEWith'
tl_decodeUtf16LEWith = TLE.decodeUtf16LEWith
-- |'TLE.decodeUtf16BEWith'
tl_decodeUtf16BEWith = TLE.decodeUtf16BEWith
-- |'TLE.decodeUtf32LEWith'
tl_decodeUtf32LEWith = TLE.decodeUtf32LEWith
-- |'TLE.decodeUtf32BEWith'
tl_decodeUtf32BEWith = TLE.decodeUtf32BEWith
-- |'TLE.encodeUtf8'
tl_encodeUtf8 = TLE.encodeUtf8
-- |'TLE.encodeUtf16LE'
tl_encodeUtf16LE = TLE.encodeUtf16LE
-- |'TLE.encodeUtf16BE'
tl_encodeUtf16BE = TLE.encodeUtf16BE
-- |'TLE.encodeUtf32LE'
tl_encodeUtf32LE = TLE.encodeUtf32LE
-- |'TLE.encodeUtf32BE'
tl_encodeUtf32BE = TLE.encodeUtf32BE

-- Data.Text.Lazy.IO
-- |'TLIO.readFile'
tl_readFile = TLIO.readFile
-- |'TLIO.writeFile'
tl_writeFile = TLIO.writeFile
-- |'TLIO.appendFile'
tl_appendFile = TLIO.appendFile
-- |'TLIO.hGetContents'
tl_hGetContents = TLIO.hGetContents
-- |'TLIO.hGetLine'
tl_hGetLine = TLIO.hGetLine
-- |'TLIO.hPutStr'
tl_hPutStr = TLIO.hPutStr
-- |'TLIO.hPutStrLn'
tl_hPutStrLn = TLIO.hPutStrLn
-- |'TLIO.interact'
tl_interact = TLIO.interact
-- |'TLIO.getContents'
tl_getContents = TLIO.getContents
-- |'TLIO.getLine'
tl_getLine = TLIO.getLine
-- |'TLIO.putStr'
tl_putStr = TLIO.putStr
-- |'TLIO.putStrLn'
tl_putStrLn = TLIO.putStrLn

-- Data.Text.Lazy.Read
-- |'TLR.Reader'
type TLReader a = TextL -> Either String (a, TextL)
-- |'TLR.decimal'
tl_decimal = TLR.decimal
-- |'TLR.hexadecimal'
tl_hexadecimal = TLR.hexadecimal
-- |'TLR.signed'
tl_signed = TLR.signed
-- |'TLR.rational'
tl_rational = TLR.rational
-- |'TLR.double'
tl_double = TLR.double

-- Data.Text.Read
-- |'TR.Reader'
type RReader a = Text -> Either String (a, Text)
-- |'TR.decimal'
t_decimal = TR.decimal
-- |'TR.hexadecimal'
t_hexadecimal = TR.hexadecimal
-- |'TR.signed'
t_signed = TR.signed
-- |'TR.rational'
t_rational = TR.rational
-- |'TR.double'
t_double = TR.double

-- Data.Text.Unsafe
-- |'TU.Iter'
type TIter = TU.Iter
-- |'TU.iter'
tu_iter = TU.iter
-- |'TU.iter_'
tu_iter_ = TU.iter_
-- |'TU.reverseIter'
tu_reverseIter = TU.reverseIter
-- |'TU.unsafeHead'
tu_unsafeHead = TU.unsafeHead
-- |'TU.unsafeTail'
tu_unsafeTail = TU.unsafeTail
-- |'TU.lengthWord16'
tu_lengthWord16 = TU.lengthWord16
-- |'TU.takeWord16'
tu_takeWord16 = TU.takeWord16
-- |'TU.dropWord16'
tu_dropWord16 = TU.dropWord16

-- Data.Tree
-- |'Tree.Tree'
type Tree = Tree.Tree
-- |'Tree.Forest'
type Forest a = [Tree a]
-- |'Tree.rootLabel'
tr_rootLabel = Tree.rootLabel
-- |'Tree.subForest'
tr_subForest = Tree.subForest
-- |'Tree.drawTree'
tr_drawTree = Tree.drawTree
-- |'Tree.drawForest'
tr_drawForest = Tree.drawForest
-- |'Tree.flatten'
tr_flatten = Tree.flatten
-- |'Tree.levels'
tr_levels = Tree.levels
-- |'Tree.unfoldTree'
tr_unfoldTree = Tree.unfoldTree
-- |'Tree.unfoldForest'
tr_unfoldForest = Tree.unfoldForest
-- |'Tree.unfoldTreeM'
tr_unfoldTreeM = Tree.unfoldTreeM
-- |'Tree.unfoldForestM'
tr_unfoldForestM = Tree.unfoldForestM
-- |'Tree.unfoldTreeM_BF'
tr_unfoldTreeM_BF = Tree.unfoldTreeM_BF
-- |'Tree.unfoldForestM_BF'
tr_unfoldForestM_BF = Tree.unfoldForestM_BF

-- Data.Vector
-- |'V.Vector'
type Vector = V.Vector
-- |'V.MVector'
type MVector = V.MVector
-- |'V.length'
v_length = V.length
-- |'V.null'
v_null = V.null
-- |('V.!')
v_get = (V.!)
-- |('V.!?')
v_getq = (V.!?)
-- |'V.head'
v_head = V.head
-- |'V.last'
v_last = V.last
-- |'V.unsafeIndex'
v_unsafeIndex = V.unsafeIndex
-- |'V.unsafeHead'
v_unsafeHead = V.unsafeHead
-- |'V.unsafeLast'
v_unsafeLast = V.unsafeLast
-- |'V.indexM'
v_indexM = V.indexM
-- |'V.headM'
v_headM = V.headM
-- |'V.lastM'
v_lastM = V.lastM
-- |'V.unsafeIndexM'
v_unsafeIndexM = V.unsafeIndexM
-- |'V.unsafeHeadM'
v_unsafeHeadM = V.unsafeHeadM
-- |'V.unsafeLastM'
v_unsafeLastM = V.unsafeLastM
-- |'V.slice'
v_slice = V.slice
-- |'V.init'
v_init = V.init
-- |'V.tail'
v_tail = V.tail
-- |'V.take'
v_take = V.take
-- |'V.drop'
v_drop = V.drop
-- |'V.splitAt'
v_splitAt = V.splitAt
-- |'V.unsafeSlice'
v_unsafeSlice = V.unsafeSlice
-- |'V.unsafeInit'
v_unsafeInit = V.unsafeInit
-- |'V.unsafeTail'
v_unsafeTail = V.unsafeTail
-- |'V.unsafeTake'
v_unsafeTake = V.unsafeTake
-- |'V.unsafeDrop'
v_unsafeDrop = V.unsafeDrop
-- |'V.empty'
v_empty = V.empty
-- |'V.singleton'
v_singleton = V.singleton
-- |'V.replicate'
v_replicate = V.replicate
-- |'V.generate'
v_generate = V.generate
-- |'V.iterateN'
v_iterateN = V.iterateN
-- |'V.replicateM'
v_replicateM = V.replicateM
-- |'V.generateM'
v_generateM = V.generateM
-- |'V.create'
v_create = V.create
-- |'V.unfoldr'
v_unfoldr = V.unfoldr
-- |'V.unfoldrN'
v_unfoldrN = V.unfoldrN
-- |'V.constructN'
v_constructN = V.constructN
-- |'V.constructrN'
v_constructrN = V.constructrN
-- |'V.enumFromN'
v_enumFromN = V.enumFromN
-- |'V.enumFromStepN'
v_enumFromStepN = V.enumFromStepN
-- |'V.enumFromTo'
v_enumFromTo = V.enumFromTo
-- |'V.enumFromThenTo'
v_enumFromThenTo = V.enumFromThenTo
-- |'V.cons'
v_cons = V.cons
-- |'V.snoc'
v_snoc = V.snoc
-- |('V.++')
v_cat = (V.++)
-- |'V.concat'
v_concat = V.concat
-- |'V.force'
v_force = V.force
-- |('V.//')
v_upd = (V.//)
-- |'V.update'
v_update = V.update
-- |'V.update_'
v_update_ = V.update_
-- |'V.unsafeUpd'
v_unsafeUpd = V.unsafeUpd
-- |'V.unsafeUpdate'
v_unsafeUpdate = V.unsafeUpdate
-- |'V.unsafeUpdate_'
v_unsafeUpdate_ = V.unsafeUpdate_
-- |'V.accum'
v_accum = V.accum
-- |'V.accumulate'
v_accumulate = V.accumulate
-- |'V.accumulate_'
v_accumulate_ = V.accumulate_
-- |'V.unsafeAccum'
v_unsafeAccum = V.unsafeAccum
-- |'V.unsafeAccumulate'
v_unsafeAccumulate = V.unsafeAccumulate
-- |'V.unsafeAccumulate_'
v_unsafeAccumulate_ = V.unsafeAccumulate_
-- |'V.reverse'
v_reverse = V.reverse
-- |'V.backpermute'
v_backpermute = V.backpermute
-- |'V.unsafeBackpermute'
v_unsafeBackpermute = V.unsafeBackpermute
-- |'V.modify'
v_modify = V.modify
-- |'V.indexed'
v_indexed = V.indexed
-- |'V.map'
v_map = V.map
-- |'V.imap'
v_imap = V.imap
-- |'V.concatMap'
v_concatMap = V.concatMap
-- |'V.mapM'
v_mapM = V.mapM
-- |'V.mapM_'
v_mapM_ = V.mapM_
-- |'V.forM'
v_forM = V.forM
-- |'V.forM_'
v_forM_ = V.forM_
-- |'V.zipWith'
v_zipWith = V.zipWith
-- |'V.zipWith3'
v_zipWith3 = V.zipWith3
-- |'V.zipWith4'
v_zipWith4 = V.zipWith4
-- |'V.zipWith5'
v_zipWith5 = V.zipWith5
-- |'V.zipWith6'
v_zipWith6 = V.zipWith6
-- |'V.izipWith'
v_izipWith = V.izipWith
-- |'V.izipWith3'
v_izipWith3 = V.izipWith3
-- |'V.izipWith4'
v_izipWith4 = V.izipWith4
-- |'V.izipWith5'
v_izipWith5 = V.izipWith5
-- |'V.izipWith6'
v_izipWith6 = V.izipWith6
-- |'V.zip'
v_zip = V.zip
-- |'V.zip3'
v_zip3 = V.zip3
-- |'V.zip4'
v_zip4 = V.zip4
-- |'V.zip5'
v_zip5 = V.zip5
-- |'V.zip6'
v_zip6 = V.zip6
-- |'V.zipWithM'
v_zipWithM = V.zipWithM
-- |'V.zipWithM_'
v_zipWithM_ = V.zipWithM_
-- |'V.unzip'
v_unzip = V.unzip
-- |'V.unzip3'
v_unzip3 = V.unzip3
-- |'V.unzip4'
v_unzip4 = V.unzip4
-- |'V.unzip5'
v_unzip5 = V.unzip5
-- |'V.unzip6'
v_unzip6 = V.unzip6
-- |'V.filter'
v_filter = V.filter
-- |'V.ifilter'
v_ifilter = V.ifilter
-- |'V.filterM'
v_filterM = V.filterM
-- |'V.takeWhile'
v_takeWhile = V.takeWhile
-- |'V.dropWhile'
v_dropWhile = V.dropWhile
-- |'V.partition'
v_partition = V.partition
-- |'V.unstablePartition'
v_unstablePartition = V.unstablePartition
-- |'V.span'
v_span = V.span
-- |'V.break'
v_break = V.break
-- |'V.elem'
v_elem = V.elem
-- |'V.notElem'
v_notElem = V.notElem
-- |'V.find'
v_find = V.find
-- |'V.findIndex'
v_findIndex = V.findIndex
-- |'V.findIndices'
v_findIndices = V.findIndices
-- |'V.elemIndex'
v_elemIndex = V.elemIndex
-- |'V.elemIndices'
v_elemIndices = V.elemIndices
-- |'V.foldl'
v_foldl = V.foldl
-- |'V.foldl1'
v_foldl1 = V.foldl1
-- |'V.foldl''
v_foldl' = V.foldl'
-- |'V.foldl1''
v_foldl1' = V.foldl1'
-- |'V.foldr'
v_foldr = V.foldr
-- |'V.foldr1'
v_foldr1 = V.foldr1
-- |'V.foldr''
v_foldr' = V.foldr'
-- |'V.foldr1''
v_foldr1' = V.foldr1'
-- |'V.ifoldl'
v_ifoldl = V.ifoldl
-- |'V.ifoldl''
v_ifoldl' = V.ifoldl'
-- |'V.ifoldr'
v_ifoldr = V.ifoldr
-- |'V.ifoldr''
v_ifoldr' = V.ifoldr'
-- |'V.all'
v_all = V.all
-- |'V.any'
v_any = V.any
-- |'V.and'
v_and = V.and
-- |'V.or'
v_or = V.or
-- |'V.sum'
v_sum = V.sum
-- |'V.product'
v_product = V.product
-- |'V.maximum'
v_maximum = V.maximum
-- |'V.maximumBy'
v_maximumBy = V.maximumBy
-- |'V.minimum'
v_minimum = V.minimum
-- |'V.minimumBy'
v_minimumBy = V.minimumBy
-- |'V.minIndex'
v_minIndex = V.minIndex
-- |'V.minIndexBy'
v_minIndexBy = V.minIndexBy
-- |'V.maxIndex'
v_maxIndex = V.maxIndex
-- |'V.maxIndexBy'
v_maxIndexBy = V.maxIndexBy
-- |'V.foldM'
v_foldM = V.foldM
-- |'V.foldM''
v_foldM' = V.foldM'
-- |'V.fold1M'
v_fold1M = V.fold1M
-- |'V.fold1M''
v_fold1M' = V.fold1M'
-- |'V.foldM_'
v_foldM_ = V.foldM_
-- |'V.foldM'_'
v_foldM'_ = V.foldM'_
-- |'V.fold1M_'
v_fold1M_ = V.fold1M_
-- |'V.fold1M'_'
v_fold1M'_ = V.fold1M'_
-- |'V.sequence'
v_sequence = V.sequence
-- |'V.sequence_'
v_sequence_ = V.sequence_
-- |'V.prescanl'
v_prescanl = V.prescanl
-- |'V.prescanl''
v_prescanl' = V.prescanl'
-- |'V.postscanl'
v_postscanl = V.postscanl
-- |'V.postscanl''
v_postscanl' = V.postscanl'
-- |'V.scanl'
v_scanl = V.scanl
-- |'V.scanl''
v_scanl' = V.scanl'
-- |'V.scanl1'
v_scanl1 = V.scanl1
-- |'V.scanl1''
v_scanl1' = V.scanl1'
-- |'V.prescanr'
v_prescanr = V.prescanr
-- |'V.prescanr''
v_prescanr' = V.prescanr'
-- |'V.postscanr'
v_postscanr = V.postscanr
-- |'V.postscanr''
v_postscanr' = V.postscanr'
-- |'V.scanr'
v_scanr = V.scanr
-- |'V.scanr''
v_scanr' = V.scanr'
-- |'V.scanr1'
v_scanr1 = V.scanr1
-- |'V.scanr1''
v_scanr1' = V.scanr1'
-- |'V.toList'
v_toList = V.toList
-- |'V.fromList'
v_fromList = V.fromList
-- |'V.fromListN'
v_fromListN = V.fromListN
-- |'V.convert'
v_convert = V.convert
-- |'V.freeze'
v_freeze = V.freeze
-- |'V.thaw'
v_thaw = V.thaw
-- |'V.copy'
v_copy = V.copy
-- |'V.unsafeFreeze'
v_unsafeFreeze = V.unsafeFreeze
-- |'V.unsafeThaw'
v_unsafeThaw = V.unsafeThaw
-- |'V.unsafeCopy'
v_unsafeCopy = V.unsafeCopy

-- Data.Vector.Fusion.Stream
-- |'VF.Step'
--
-- Constructors 'VF.Yield', 'VF.Skip', and 'VF.Done' not reexported.
type VFStep = VF.Step
-- |'VF.MStream'
type VFStream = VFMStream VFId
-- |'VF.inplace'
vf_inplace = VF.inplace
-- |'VF.size'
vf_size = VF.size
-- |'VF.sized'
vf_sized = VF.sized
-- |'VF.length'
vf_length = VF.length
-- |'VF.null'
vf_null = VF.null
-- |'VF.empty'
vf_empty = VF.empty
-- |'VF.singleton'
vf_singleton = VF.singleton
-- |'VF.cons'
vf_cons = VF.cons
-- |'VF.snoc'
vf_snoc = VF.snoc
-- |'VF.replicate'
vf_replicate = VF.replicate
-- |'VF.generate'
vf_generate = VF.generate
-- |('VF.++')
vf_cat = (VF.++)
-- |'VF.head'
vf_head = VF.head
-- |'VF.last'
vf_last = VF.last
-- |('VF.!!')
vf_get = (VF.!!)
-- |('VF.!?')
vf_getq = (VF.!?)
-- |'VF.slice'
vf_slice = VF.slice
-- |'VF.init'
vf_init = VF.init
-- |'VF.tail'
vf_tail = VF.tail
-- |'VF.take'
vf_take = VF.take
-- |'VF.drop'
vf_drop = VF.drop
-- |'VF.map'
vf_map = VF.map
-- |'VF.concatMap'
vf_concatMap = VF.concatMap
-- |'VF.flatten'
vf_flatten = VF.flatten
-- |'VF.unbox'
vf_unbox = VF.unbox
-- |'VF.indexed'
vf_indexed = VF.indexed
-- |'VF.indexedR'
vf_indexedR = VF.indexedR
-- |'VF.zipWith'
vf_zipWith = VF.zipWith
-- |'VF.zipWith3'
vf_zipWith3 = VF.zipWith3
-- |'VF.zipWith4'
vf_zipWith4 = VF.zipWith4
-- |'VF.zipWith5'
vf_zipWith5 = VF.zipWith5
-- |'VF.zipWith6'
vf_zipWith6 = VF.zipWith6
-- |'VF.zip'
vf_zip = VF.zip
-- |'VF.zip3'
vf_zip3 = VF.zip3
-- |'VF.zip4'
vf_zip4 = VF.zip4
-- |'VF.zip5'
vf_zip5 = VF.zip5
-- |'VF.zip6'
vf_zip6 = VF.zip6
-- |'VF.filter'
vf_filter = VF.filter
-- |'VF.takeWhile'
vf_takeWhile = VF.takeWhile
-- |'VF.dropWhile'
vf_dropWhile = VF.dropWhile
-- |'VF.elem'
vf_elem = VF.elem
-- |'VF.notElem'
vf_notElem = VF.notElem
-- |'VF.find'
vf_find = VF.find
-- |'VF.findIndex'
vf_findIndex = VF.findIndex
-- |'VF.foldl'
vf_foldl = VF.foldl
-- |'VF.foldl1'
vf_foldl1 = VF.foldl1
-- |'VF.foldl''
vf_foldl' = VF.foldl'
-- |'VF.foldl1''
vf_foldl1' = VF.foldl1'
-- |'VF.foldr'
vf_foldr = VF.foldr
-- |'VF.foldr1'
vf_foldr1 = VF.foldr1
-- |'VF.and'
vf_and = VF.and
-- |'VF.or'
vf_or = VF.or
-- |'VF.unfoldr'
vf_unfoldr = VF.unfoldr
-- |'VF.unfoldrN'
vf_unfoldrN = VF.unfoldrN
-- |'VF.iterateN'
vf_iterateN = VF.iterateN
-- |'VF.prescanl'
vf_prescanl = VF.prescanl
-- |'VF.prescanl''
vf_prescanl' = VF.prescanl'
-- |'VF.postscanl'
vf_postscanl = VF.postscanl
-- |'VF.postscanl''
vf_postscanl' = VF.postscanl'
-- |'VF.scanl'
vf_scanl = VF.scanl
-- |'VF.scanl''
vf_scanl' = VF.scanl'
-- |'VF.scanl1'
vf_scanl1 = VF.scanl1
-- |'VF.scanl1''
vf_scanl1' = VF.scanl1'
-- |'VF.enumFromStepN'
vf_enumFromStepN = VF.enumFromStepN
-- |'VF.enumFromTo'
vf_enumFromTo = VF.enumFromTo
-- |'VF.enumFromThenTo'
vf_enumFromThenTo = VF.enumFromThenTo
-- |'VF.toList'
vf_toList = VF.toList
-- |'VF.fromList'
vf_fromList = VF.fromList
-- |'VF.fromListN'
vf_fromListN = VF.fromListN
-- |'VF.unsafeFromList'
vf_unsafeFromList = VF.unsafeFromList
-- |'VF.liftStream'
vf_liftStream = VF.liftStream
-- |'VF.mapM'
vf_mapM = VF.mapM
-- |'VF.mapM_'
vf_mapM_ = VF.mapM_
-- |'VF.zipWithM'
vf_zipWithM = VF.zipWithM
-- |'VF.zipWithM_'
vf_zipWithM_ = VF.zipWithM_
-- |'VF.filterM'
vf_filterM = VF.filterM
-- |'VF.foldM'
vf_foldM = VF.foldM
-- |'VF.fold1M'
vf_fold1M = VF.fold1M
-- |'VF.foldM''
vf_foldM' = VF.foldM'
-- |'VF.fold1M''
vf_fold1M' = VF.fold1M'
-- |'VF.eq'
vf_eq = VF.eq
-- |'VF.cmp'
vf_cmp = VF.cmp

-- Data.Vector.Fusion.Stream.Monadic
-- |'VFM.Stream'
type VFMStream = VFM.Stream
-- |'VFM.Step'
--
-- Constructors 'VFM.Yield', 'VFM.Skip', and 'VFM.Done' not reexported.
type VFMStep = VFM.Step
-- |'VFM.size'
vfm_size = VFM.size
-- |'VFM.sized'
vfm_sized = VFM.sized
-- |'VFM.length'
vfm_length = VFM.length
-- |'VFM.null'
vfm_null = VFM.null
-- |'VFM.empty'
vfm_empty = VFM.empty
-- |'VFM.singleton'
vfm_singleton = VFM.singleton
-- |'VFM.cons'
vfm_cons = VFM.cons
-- |'VFM.snoc'
vfm_snoc = VFM.snoc
-- |'VFM.replicate'
vfm_replicate = VFM.replicate
-- |'VFM.replicateM'
vfm_replicateM = VFM.replicateM
-- |'VFM.generate'
vfm_generate = VFM.generate
-- |'VFM.generateM'
vfm_generateM = VFM.generateM
-- |('VFM.++')
vfm_cat = (VFM.++)
-- |'VFM.head'
vfm_head = VFM.head
-- |'VFM.last'
vfm_last = VFM.last
-- |('VFM.!!')
vfm_get = (VFM.!!)
-- |('VFM.!?')
vfm_getq = (VFM.!?)
-- |'VFM.slice'
vfm_slice = VFM.slice
-- |'VFM.init'
vfm_init = VFM.init
-- |'VFM.tail'
vfm_tail = VFM.tail
-- |'VFM.take'
vfm_take = VFM.take
-- |'VFM.drop'
vfm_drop = VFM.drop
-- |'VFM.map'
vfm_map = VFM.map
-- |'VFM.mapM'
vfm_mapM = VFM.mapM
-- |'VFM.mapM_'
vfm_mapM_ = VFM.mapM_
-- |'VFM.trans'
vfm_trans = VFM.trans
-- |'VFM.unbox'
vfm_unbox = VFM.unbox
-- |'VFM.concatMap'
vfm_concatMap = VFM.concatMap
-- |'VFM.flatten'
vfm_flatten = VFM.flatten
-- |'VFM.indexed'
vfm_indexed = VFM.indexed
-- |'VFM.indexedR'
vfm_indexedR = VFM.indexedR
-- |'VFM.zipWithM_'
vfm_zipWithM_ = VFM.zipWithM_
-- |'VFM.zipWithM'
vfm_zipWithM = VFM.zipWithM
-- |'VFM.zipWith3M'
vfm_zipWith3M = VFM.zipWith3M
-- |'VFM.zipWith4M'
vfm_zipWith4M = VFM.zipWith4M
-- |'VFM.zipWith5M'
vfm_zipWith5M = VFM.zipWith5M
-- |'VFM.zipWith6M'
vfm_zipWith6M = VFM.zipWith6M
-- |'VFM.zipWith'
vfm_zipWith = VFM.zipWith
-- |'VFM.zipWith3'
vfm_zipWith3 = VFM.zipWith3
-- |'VFM.zipWith4'
vfm_zipWith4 = VFM.zipWith4
-- |'VFM.zipWith5'
vfm_zipWith5 = VFM.zipWith5
-- |'VFM.zipWith6'
vfm_zipWith6 = VFM.zipWith6
-- |'VFM.zip'
vfm_zip = VFM.zip
-- |'VFM.zip3'
vfm_zip3 = VFM.zip3
-- |'VFM.zip4'
vfm_zip4 = VFM.zip4
-- |'VFM.zip5'
vfm_zip5 = VFM.zip5
-- |'VFM.zip6'
vfm_zip6 = VFM.zip6
-- |'VFM.filter'
vfm_filter = VFM.filter
-- |'VFM.filterM'
vfm_filterM = VFM.filterM
-- |'VFM.takeWhile'
vfm_takeWhile = VFM.takeWhile
-- |'VFM.takeWhileM'
vfm_takeWhileM = VFM.takeWhileM
-- |'VFM.dropWhile'
vfm_dropWhile = VFM.dropWhile
-- |'VFM.dropWhileM'
vfm_dropWhileM = VFM.dropWhileM
-- |'VFM.elem'
vfm_elem = VFM.elem
-- |'VFM.notElem'
vfm_notElem = VFM.notElem
-- |'VFM.find'
vfm_find = VFM.find
-- |'VFM.findM'
vfm_findM = VFM.findM
-- |'VFM.findIndex'
vfm_findIndex = VFM.findIndex
-- |'VFM.findIndexM'
vfm_findIndexM = VFM.findIndexM
-- |'VFM.foldl'
vfm_foldl = VFM.foldl
-- |'VFM.foldlM'
vfm_foldlM = VFM.foldlM
-- |'VFM.foldl1'
vfm_foldl1 = VFM.foldl1
-- |'VFM.foldl1M'
vfm_foldl1M = VFM.foldl1M
-- |'VFM.foldM'
vfm_foldM = VFM.foldM
-- |'VFM.fold1M'
vfm_fold1M = VFM.fold1M
-- |'VFM.foldl''
vfm_foldl' = VFM.foldl'
-- |'VFM.foldlM''
vfm_foldlM' = VFM.foldlM'
-- |'VFM.foldl1''
vfm_foldl1' = VFM.foldl1'
-- |'VFM.foldl1M''
vfm_foldl1M' = VFM.foldl1M'
-- |'VFM.foldM''
vfm_foldM' = VFM.foldM'
-- |'VFM.fold1M''
vfm_fold1M' = VFM.fold1M'
-- |'VFM.foldr'
vfm_foldr = VFM.foldr
-- |'VFM.foldrM'
vfm_foldrM = VFM.foldrM
-- |'VFM.foldr1'
vfm_foldr1 = VFM.foldr1
-- |'VFM.foldr1M'
vfm_foldr1M = VFM.foldr1M
-- |'VFM.and'
vfm_and = VFM.and
-- |'VFM.or'
vfm_or = VFM.or
-- |'VFM.concatMapM'
vfm_concatMapM = VFM.concatMapM
-- |'VFM.unfoldr'
vfm_unfoldr = VFM.unfoldr
-- |'VFM.unfoldrM'
vfm_unfoldrM = VFM.unfoldrM
-- |'VFM.unfoldrN'
vfm_unfoldrN = VFM.unfoldrN
-- |'VFM.unfoldrNM'
vfm_unfoldrNM = VFM.unfoldrNM
-- |'VFM.iterateN'
vfm_iterateN = VFM.iterateN
-- |'VFM.iterateNM'
vfm_iterateNM = VFM.iterateNM
-- |'VFM.prescanl'
vfm_prescanl = VFM.prescanl
-- |'VFM.prescanlM'
vfm_prescanlM = VFM.prescanlM
-- |'VFM.prescanl''
vfm_prescanl' = VFM.prescanl'
-- |'VFM.prescanlM''
vfm_prescanlM' = VFM.prescanlM'
-- |'VFM.postscanl'
vfm_postscanl = VFM.postscanl
-- |'VFM.postscanlM'
vfm_postscanlM = VFM.postscanlM
-- |'VFM.postscanl''
vfm_postscanl' = VFM.postscanl'
-- |'VFM.postscanlM''
vfm_postscanlM' = VFM.postscanlM'
-- |'VFM.scanl'
vfm_scanl = VFM.scanl
-- |'VFM.scanlM'
vfm_scanlM = VFM.scanlM
-- |'VFM.scanl''
vfm_scanl' = VFM.scanl'
-- |'VFM.scanlM''
vfm_scanlM' = VFM.scanlM'
-- |'VFM.scanl1'
vfm_scanl1 = VFM.scanl1
-- |'VFM.scanl1M'
vfm_scanl1M = VFM.scanl1M
-- |'VFM.scanl1''
vfm_scanl1' = VFM.scanl1'
-- |'VFM.scanl1M''
vfm_scanl1M' = VFM.scanl1M'
-- |'VFM.enumFromStepN'
vfm_enumFromStepN = VFM.enumFromStepN
-- |'VFM.enumFromTo'
vfm_enumFromTo = VFM.enumFromTo
-- |'VFM.enumFromThenTo'
vfm_enumFromThenTo = VFM.enumFromThenTo
-- |'VFM.toList'
vfm_toList = VFM.toList
-- |'VFM.fromList'
vfm_fromList = VFM.fromList
-- |'VFM.fromListN'
vfm_fromListN = VFM.fromListN
-- |'VFM.unsafeFromList'
vfm_unsafeFromList = VFM.unsafeFromList

-- Data.Vector.Fusion.Stream.Size
-- |'VFS.Size'
--
-- Constructors 'VFS.Exact', 'VFS.Max', and 'VFS.Unknown' not reexported.
type VFSize = VFS.Size
-- |'VFS.smaller'
vf_smaller = VFS.smaller
-- |'VFS.larger'
vf_larger = VFS.larger
-- |'VFS.toMax'
vf_toMax = VFS.toMax
-- |'VFS.upperBound'
vf_upperBound = VFS.upperBound

-- Data.Vector.Fusion.Util
-- |'VFU.Id'
type VFId = VFU.Id
-- |'VFU.Id'
vf_Id = VFU.Id
-- |'VFU.unId'
vf_unId = VFU.unId
-- |'VFU.Box'
type VFBox = VFU.Box
-- |'VFU.Box'
vf_Box = VFU.Box
-- |'VFU.unBox'
vf_unBox = VFU.unBox
-- |'VFU.delay_inline'
vf_delay_inline = VFU.delay_inline
-- |'VFU.delayed_min'
vf_delayed_min = VFU.delayed_min

-- Data.Vector.Generic
-- |'VG.Mutable'
type Mutable a = VG.Mutable a
-- |'VG.basicUnsafeFreeze'
vg_basicUnsafeFreeze = VG.basicUnsafeFreeze
-- |'VG.basicUnsafeThaw'
vg_basicUnsafeThaw = VG.basicUnsafeThaw
-- |'VG.basicLength'
vg_basicLength = VG.basicLength
-- |'VG.basicUnsafeSlice'
vg_basicUnsafeSlice = VG.basicUnsafeSlice
-- |'VG.basicUnsafeIndexM'
vg_basicUnsafeIndexM = VG.basicUnsafeIndexM
-- |'VG.basicUnsafeCopy'
vg_basicUnsafeCopy = VG.basicUnsafeCopy
-- |'VG.elemseq'
vg_elemseq = VG.elemseq
-- |'VG.length'
vg_length = VG.length
-- |'VG.null'
vg_null = VG.null
-- |('VG.!')
vg_get = (VG.!)
-- |('VG.!?')
vg_getq = (VG.!?)
-- |'VG.head'
vg_head = VG.head
-- |'VG.last'
vg_last = VG.last
-- |'VG.unsafeIndex'
vg_unsafeIndex = VG.unsafeIndex
-- |'VG.unsafeHead'
vg_unsafeHead = VG.unsafeHead
-- |'VG.unsafeLast'
vg_unsafeLast = VG.unsafeLast
-- |'VG.indexM'
vg_indexM = VG.indexM
-- |'VG.headM'
vg_headM = VG.headM
-- |'VG.lastM'
vg_lastM = VG.lastM
-- |'VG.unsafeIndexM'
vg_unsafeIndexM = VG.unsafeIndexM
-- |'VG.unsafeHeadM'
vg_unsafeHeadM = VG.unsafeHeadM
-- |'VG.unsafeLastM'
vg_unsafeLastM = VG.unsafeLastM
-- |'VG.slice'
vg_slice = VG.slice
-- |'VG.init'
vg_init = VG.init
-- |'VG.tail'
vg_tail = VG.tail
-- |'VG.take'
vg_take = VG.take
-- |'VG.drop'
vg_drop = VG.drop
-- |'VG.splitAt'
vg_splitAt = VG.splitAt
-- |'VG.unsafeSlice'
vg_unsafeSlice = VG.unsafeSlice
-- |'VG.unsafeInit'
vg_unsafeInit = VG.unsafeInit
-- |'VG.unsafeTail'
vg_unsafeTail = VG.unsafeTail
-- |'VG.unsafeTake'
vg_unsafeTake = VG.unsafeTake
-- |'VG.unsafeDrop'
vg_unsafeDrop = VG.unsafeDrop
-- |'VG.empty'
vg_empty = VG.empty
-- |'VG.singleton'
vg_singleton = VG.singleton
-- |'VG.replicate'
vg_replicate = VG.replicate
-- |'VG.generate'
vg_generate = VG.generate
-- |'VG.iterateN'
vg_iterateN = VG.iterateN
-- |'VG.replicateM'
vg_replicateM = VG.replicateM
-- |'VG.generateM'
vg_generateM = VG.generateM
-- |'VG.create'
vg_create = VG.create
-- |'VG.unfoldr'
vg_unfoldr = VG.unfoldr
-- |'VG.unfoldrN'
vg_unfoldrN = VG.unfoldrN
-- |'VG.constructN'
vg_constructN = VG.constructN
-- |'VG.constructrN'
vg_constructrN = VG.constructrN
-- |'VG.enumFromN'
vg_enumFromN = VG.enumFromN
-- |'VG.enumFromStepN'
vg_enumFromStepN = VG.enumFromStepN
-- |'VG.enumFromTo'
vg_enumFromTo = VG.enumFromTo
-- |'VG.enumFromThenTo'
vg_enumFromThenTo = VG.enumFromThenTo
-- |'VG.cons'
vg_cons = VG.cons
-- |'VG.snoc'
vg_snoc = VG.snoc
-- |('VG.++')
vg_cat = (VG.++)
-- |'VG.concat'
vg_concat = VG.concat
-- |'VG.force'
vg_force = VG.force
-- |('VG.//')
vg_upd = (VG.//)
-- |'VG.update'
vg_update = VG.update
-- |'VG.update_'
vg_update_ = VG.update_
-- |'VG.unsafeUpd'
vg_unsafeUpd = VG.unsafeUpd
-- |'VG.unsafeUpdate'
vg_unsafeUpdate = VG.unsafeUpdate
-- |'VG.unsafeUpdate_'
vg_unsafeUpdate_ = VG.unsafeUpdate_
-- |'VG.accum'
vg_accum = VG.accum
-- |'VG.accumulate'
vg_accumulate = VG.accumulate
-- |'VG.accumulate_'
vg_accumulate_ = VG.accumulate_
-- |'VG.unsafeAccum'
vg_unsafeAccum = VG.unsafeAccum
-- |'VG.unsafeAccumulate'
vg_unsafeAccumulate = VG.unsafeAccumulate
-- |'VG.unsafeAccumulate_'
vg_unsafeAccumulate_ = VG.unsafeAccumulate_
-- |'VG.reverse'
vg_reverse = VG.reverse
-- |'VG.backpermute'
vg_backpermute = VG.backpermute
-- |'VG.unsafeBackpermute'
vg_unsafeBackpermute = VG.unsafeBackpermute
-- |'VG.modify'
vg_modify = VG.modify
-- |'VG.indexed'
vg_indexed = VG.indexed
-- |'VG.map'
vg_map = VG.map
-- |'VG.imap'
vg_imap = VG.imap
-- |'VG.concatMap'
vg_concatMap = VG.concatMap
-- |'VG.mapM'
vg_mapM = VG.mapM
-- |'VG.mapM_'
vg_mapM_ = VG.mapM_
-- |'VG.forM'
vg_forM = VG.forM
-- |'VG.forM_'
vg_forM_ = VG.forM_
-- |'VG.zipWith'
vg_zipWith = VG.zipWith
-- |'VG.zipWith3'
vg_zipWith3 = VG.zipWith3
-- |'VG.zipWith4'
vg_zipWith4 = VG.zipWith4
-- |'VG.zipWith5'
vg_zipWith5 = VG.zipWith5
-- |'VG.zipWith6'
vg_zipWith6 = VG.zipWith6
-- |'VG.izipWith'
vg_izipWith = VG.izipWith
-- |'VG.izipWith3'
vg_izipWith3 = VG.izipWith3
-- |'VG.izipWith4'
vg_izipWith4 = VG.izipWith4
-- |'VG.izipWith5'
vg_izipWith5 = VG.izipWith5
-- |'VG.izipWith6'
vg_izipWith6 = VG.izipWith6
-- |'VG.zip'
vg_zip = VG.zip
-- |'VG.zip3'
vg_zip3 = VG.zip3
-- |'VG.zip4'
vg_zip4 = VG.zip4
-- |'VG.zip5'
vg_zip5 = VG.zip5
-- |'VG.zip6'
vg_zip6 = VG.zip6
-- |'VG.zipWithM'
vg_zipWithM = VG.zipWithM
-- |'VG.zipWithM_'
vg_zipWithM_ = VG.zipWithM_
-- |'VG.unzip'
vg_unzip = VG.unzip
-- |'VG.unzip3'
vg_unzip3 = VG.unzip3
-- |'VG.unzip4'
vg_unzip4 = VG.unzip4
-- |'VG.unzip5'
vg_unzip5 = VG.unzip5
-- |'VG.unzip6'
vg_unzip6 = VG.unzip6
-- |'VG.filter'
vg_filter = VG.filter
-- |'VG.ifilter'
vg_ifilter = VG.ifilter
-- |'VG.filterM'
vg_filterM = VG.filterM
-- |'VG.takeWhile'
vg_takeWhile = VG.takeWhile
-- |'VG.dropWhile'
vg_dropWhile = VG.dropWhile
-- |'VG.partition'
vg_partition = VG.partition
-- |'VG.unstablePartition'
vg_unstablePartition = VG.unstablePartition
-- |'VG.span'
vg_span = VG.span
-- |'VG.break'
vg_break = VG.break
-- |'VG.elem'
vg_elem = VG.elem
-- |'VG.notElem'
vg_notElem = VG.notElem
-- |'VG.find'
vg_find = VG.find
-- |'VG.findIndex'
vg_findIndex = VG.findIndex
-- |'VG.findIndices'
vg_findIndices = VG.findIndices
-- |'VG.elemIndex'
vg_elemIndex = VG.elemIndex
-- |'VG.elemIndices'
vg_elemIndices = VG.elemIndices
-- |'VG.foldl'
vg_foldl = VG.foldl
-- |'VG.foldl1'
vg_foldl1 = VG.foldl1
-- |'VG.foldl''
vg_foldl' = VG.foldl'
-- |'VG.foldl1''
vg_foldl1' = VG.foldl1'
-- |'VG.foldr'
vg_foldr = VG.foldr
-- |'VG.foldr1'
vg_foldr1 = VG.foldr1
-- |'VG.foldr''
vg_foldr' = VG.foldr'
-- |'VG.foldr1''
vg_foldr1' = VG.foldr1'
-- |'VG.ifoldl'
vg_ifoldl = VG.ifoldl
-- |'VG.ifoldl''
vg_ifoldl' = VG.ifoldl'
-- |'VG.ifoldr'
vg_ifoldr = VG.ifoldr
-- |'VG.ifoldr''
vg_ifoldr' = VG.ifoldr'
-- |'VG.all'
vg_all = VG.all
-- |'VG.any'
vg_any = VG.any
-- |'VG.and'
vg_and = VG.and
-- |'VG.or'
vg_or = VG.or
-- |'VG.sum'
vg_sum = VG.sum
-- |'VG.product'
vg_product = VG.product
-- |'VG.maximum'
vg_maximum = VG.maximum
-- |'VG.maximumBy'
vg_maximumBy = VG.maximumBy
-- |'VG.minimum'
vg_minimum = VG.minimum
-- |'VG.minimumBy'
vg_minimumBy = VG.minimumBy
-- |'VG.minIndex'
vg_minIndex = VG.minIndex
-- |'VG.minIndexBy'
vg_minIndexBy = VG.minIndexBy
-- |'VG.maxIndex'
vg_maxIndex = VG.maxIndex
-- |'VG.maxIndexBy'
vg_maxIndexBy = VG.maxIndexBy
-- |'VG.foldM'
vg_foldM = VG.foldM
-- |'VG.foldM''
vg_foldM' = VG.foldM'
-- |'VG.fold1M'
vg_fold1M = VG.fold1M
-- |'VG.fold1M''
vg_fold1M' = VG.fold1M'
-- |'VG.foldM_'
vg_foldM_ = VG.foldM_
-- |'VG.foldM'_'
vg_foldM'_ = VG.foldM'_
-- |'VG.fold1M_'
vg_fold1M_ = VG.fold1M_
-- |'VG.fold1M'_'
vg_fold1M'_ = VG.fold1M'_
-- |'VG.sequence'
vg_sequence = VG.sequence
-- |'VG.sequence_'
vg_sequence_ = VG.sequence_
-- |'VG.prescanl'
vg_prescanl = VG.prescanl
-- |'VG.prescanl''
vg_prescanl' = VG.prescanl'
-- |'VG.postscanl'
vg_postscanl = VG.postscanl
-- |'VG.postscanl''
vg_postscanl' = VG.postscanl'
-- |'VG.scanl'
vg_scanl = VG.scanl
-- |'VG.scanl''
vg_scanl' = VG.scanl'
-- |'VG.scanl1'
vg_scanl1 = VG.scanl1
-- |'VG.scanl1''
vg_scanl1' = VG.scanl1'
-- |'VG.prescanr'
vg_prescanr = VG.prescanr
-- |'VG.prescanr''
vg_prescanr' = VG.prescanr'
-- |'VG.postscanr'
vg_postscanr = VG.postscanr
-- |'VG.postscanr''
vg_postscanr' = VG.postscanr'
-- |'VG.scanr'
vg_scanr = VG.scanr
-- |'VG.scanr''
vg_scanr' = VG.scanr'
-- |'VG.scanr1'
vg_scanr1 = VG.scanr1
-- |'VG.scanr1''
vg_scanr1' = VG.scanr1'
-- |'VG.toList'
vg_toList = VG.toList
-- |'VG.fromList'
vg_fromList = VG.fromList
-- |'VG.fromListN'
vg_fromListN = VG.fromListN
-- |'VG.convert'
vg_convert = VG.convert
-- |'VG.freeze'
vg_freeze = VG.freeze
-- |'VG.thaw'
vg_thaw = VG.thaw
-- |'VG.copy'
vg_copy = VG.copy
-- |'VG.unsafeFreeze'
vg_unsafeFreeze = VG.unsafeFreeze
-- |'VG.unsafeThaw'
vg_unsafeThaw = VG.unsafeThaw
-- |'VG.unsafeCopy'
vg_unsafeCopy = VG.unsafeCopy
-- |'VG.stream'
vg_stream = VG.stream
-- |'VG.unstream'
vg_unstream = VG.unstream
-- |'VG.streamR'
vg_streamR = VG.streamR
-- |'VG.unstreamR'
vg_unstreamR = VG.unstreamR
-- |'VG.new'
vg_new = VG.new
-- |'VG.clone'
vg_clone = VG.clone
-- |'VG.eq'
vg_eq = VG.eq
-- |'VG.cmp'
vg_cmp = VG.cmp
-- |'VG.showsPrec'
vg_showsPrec = VG.showsPrec
-- |'VG.readPrec'
vg_readPrec = VG.readPrec
-- |'VG.gfoldl'
vg_gfoldl = VG.gfoldl
-- |'VG.dataCast'
vg_dataCast = VG.dataCast
-- |'VG.mkType'
vg_mkType = VG.mkType

-- Data.Vector.Generic.Mutable
-- |'VGM.basicLength'
vgm_basicLength = VGM.basicLength
-- |'VGM.basicUnsafeSlice'
vgm_basicUnsafeSlice = VGM.basicUnsafeSlice
-- |'VGM.basicOverlaps'
vgm_basicOverlaps = VGM.basicOverlaps
-- |'VGM.basicUnsafeNew'
vgm_basicUnsafeNew = VGM.basicUnsafeNew
-- |'VGM.basicUnsafeReplicate'
vgm_basicUnsafeReplicate = VGM.basicUnsafeReplicate
-- |'VGM.basicUnsafeRead'
vgm_basicUnsafeRead = VGM.basicUnsafeRead
-- |'VGM.basicUnsafeWrite'
vgm_basicUnsafeWrite = VGM.basicUnsafeWrite
-- |'VGM.basicClear'
vgm_basicClear = VGM.basicClear
-- |'VGM.basicSet'
vgm_basicSet = VGM.basicSet
-- |'VGM.basicUnsafeCopy'
vgm_basicUnsafeCopy = VGM.basicUnsafeCopy
-- |'VGM.basicUnsafeMove'
vgm_basicUnsafeMove = VGM.basicUnsafeMove
-- |'VGM.basicUnsafeGrow'
vgm_basicUnsafeGrow = VGM.basicUnsafeGrow
-- |'VGM.length'
vgm_length = VGM.length
-- |'VGM.null'
vgm_null = VGM.null
-- |'VGM.slice'
vgm_slice = VGM.slice
-- |'VGM.init'
vgm_init = VGM.init
-- |'VGM.tail'
vgm_tail = VGM.tail
-- |'VGM.take'
vgm_take = VGM.take
-- |'VGM.drop'
vgm_drop = VGM.drop
-- |'VGM.splitAt'
vgm_splitAt = VGM.splitAt
-- |'VGM.unsafeSlice'
vgm_unsafeSlice = VGM.unsafeSlice
-- |'VGM.unsafeInit'
vgm_unsafeInit = VGM.unsafeInit
-- |'VGM.unsafeTail'
vgm_unsafeTail = VGM.unsafeTail
-- |'VGM.unsafeTake'
vgm_unsafeTake = VGM.unsafeTake
-- |'VGM.unsafeDrop'
vgm_unsafeDrop = VGM.unsafeDrop
-- |'VGM.overlaps'
vgm_overlaps = VGM.overlaps
-- |'VGM.new'
vgm_new = VGM.new
-- |'VGM.unsafeNew'
vgm_unsafeNew = VGM.unsafeNew
-- |'VGM.replicate'
vgm_replicate = VGM.replicate
-- |'VGM.replicateM'
vgm_replicateM = VGM.replicateM
-- |'VGM.clone'
vgm_clone = VGM.clone
-- |'VGM.grow'
vgm_grow = VGM.grow
-- |'VGM.unsafeGrow'
vgm_unsafeGrow = VGM.unsafeGrow
-- |'VGM.clear'
vgm_clear = VGM.clear
-- |'VGM.read'
vgm_read = VGM.read
-- |'VGM.write'
vgm_write = VGM.write
-- |'VGM.swap'
vgm_swap = VGM.swap
-- |'VGM.unsafeRead'
vgm_unsafeRead = VGM.unsafeRead
-- |'VGM.unsafeWrite'
vgm_unsafeWrite = VGM.unsafeWrite
-- |'VGM.unsafeSwap'
vgm_unsafeSwap = VGM.unsafeSwap
-- |'VGM.set'
vgm_set = VGM.set
-- |'VGM.copy'
vgm_copy = VGM.copy
-- |'VGM.move'
vgm_move = VGM.move
-- |'VGM.unsafeCopy'
vgm_unsafeCopy = VGM.unsafeCopy
-- |'VGM.unsafeMove'
vgm_unsafeMove = VGM.unsafeMove
-- |'VGM.mstream'
vgm_mstream = VGM.mstream
-- |'VGM.mstreamR'
vgm_mstreamR = VGM.mstreamR
-- |'VGM.unstream'
vgm_unstream = VGM.unstream
-- |'VGM.unstreamR'
vgm_unstreamR = VGM.unstreamR
-- |'VGM.munstream'
vgm_munstream = VGM.munstream
-- |'VGM.munstreamR'
vgm_munstreamR = VGM.munstreamR
-- |'VGM.transform'
vgm_transform = VGM.transform
-- |'VGM.transformR'
vgm_transformR = VGM.transformR
-- |'VGM.fill'
vgm_fill = VGM.fill
-- |'VGM.fillR'
vgm_fillR = VGM.fillR
-- |'VGM.unsafeAccum'
vgm_unsafeAccum = VGM.unsafeAccum
-- |'VGM.accum'
vgm_accum = VGM.accum
-- |'VGM.unsafeUpdate'
vgm_unsafeUpdate = VGM.unsafeUpdate
-- |'VGM.update'
vgm_update = VGM.update
-- |'VGM.reverse'
vgm_reverse = VGM.reverse
-- |'VGM.unstablePartition'
vgm_unstablePartition = VGM.unstablePartition
-- |'VGM.unstablePartitionStream'
vgm_unstablePartitionStream = VGM.unstablePartitionStream
-- |'VGM.partitionStream'
vgm_partitionStream = VGM.partitionStream

-- Data.Vector.Generic.New
-- |'VGN.New'
type VGNew = VGN.New
-- |'VGN.create'
vgn_create = VGN.create
-- |'VGN.run'
vgn_run = VGN.run
-- |'VGN.runPrim'
vgn_runPrim = VGN.runPrim
-- |'VGN.apply'
vgn_apply = VGN.apply
-- |'VGN.modify'
vgn_modify = VGN.modify
-- |'VGN.modifyWithStream'
vgn_modifyWithStream = VGN.modifyWithStream
-- |'VGN.unstream'
vgn_unstream = VGN.unstream
-- |'VGN.transform'
vgn_transform = VGN.transform
-- |'VGN.unstreamR'
vgn_unstreamR = VGN.unstreamR
-- |'VGN.transformR'
vgn_transformR = VGN.transformR
-- |'VGN.slice'
vgn_slice = VGN.slice
-- |'VGN.init'
vgn_init = VGN.init
-- |'VGN.tail'
vgn_tail = VGN.tail
-- |'VGN.take'
vgn_take = VGN.take
-- |'VGN.drop'
vgn_drop = VGN.drop
-- |'VGN.unsafeSlice'
vgn_unsafeSlice = VGN.unsafeSlice
-- |'VGN.unsafeInit'
vgn_unsafeInit = VGN.unsafeInit
-- |'VGN.unsafeTail'
vgn_unsafeTail = VGN.unsafeTail

-- Data.Vector.Mutable
-- |'VM.IOVector'
type IOVector = MVector RealWorld
-- |'VM.STVector'
type STVector s = MVector s
-- |'VM.length'
vm_length = VM.length
-- |'VM.null'
vm_null = VM.null
-- |'VM.slice'
vm_slice = VM.slice
-- |'VM.init'
vm_init = VM.init
-- |'VM.tail'
vm_tail = VM.tail
-- |'VM.take'
vm_take = VM.take
-- |'VM.drop'
vm_drop = VM.drop
-- |'VM.splitAt'
vm_splitAt = VM.splitAt
-- |'VM.unsafeSlice'
vm_unsafeSlice = VM.unsafeSlice
-- |'VM.unsafeInit'
vm_unsafeInit = VM.unsafeInit
-- |'VM.unsafeTail'
vm_unsafeTail = VM.unsafeTail
-- |'VM.unsafeTake'
vm_unsafeTake = VM.unsafeTake
-- |'VM.unsafeDrop'
vm_unsafeDrop = VM.unsafeDrop
-- |'VM.overlaps'
vm_overlaps = VM.overlaps
-- |'VM.new'
vm_new = VM.new
-- |'VM.unsafeNew'
vm_unsafeNew = VM.unsafeNew
-- |'VM.replicate'
vm_replicate = VM.replicate
-- |'VM.replicateM'
vm_replicateM = VM.replicateM
-- |'VM.clone'
vm_clone = VM.clone
-- |'VM.grow'
vm_grow = VM.grow
-- |'VM.unsafeGrow'
vm_unsafeGrow = VM.unsafeGrow
-- |'VM.clear'
vm_clear = VM.clear
-- |'VM.read'
vm_read = VM.read
-- |'VM.write'
vm_write = VM.write
-- |'VM.swap'
vm_swap = VM.swap
-- |'VM.unsafeRead'
vm_unsafeRead = VM.unsafeRead
-- |'VM.unsafeWrite'
vm_unsafeWrite = VM.unsafeWrite
-- |'VM.unsafeSwap'
vm_unsafeSwap = VM.unsafeSwap
-- |'VM.set'
vm_set = VM.set
-- |'VM.copy'
vm_copy = VM.copy
-- |'VM.move'
vm_move = VM.move
-- |'VM.unsafeCopy'
vm_unsafeCopy = VM.unsafeCopy
-- |'VM.unsafeMove'
vm_unsafeMove = VM.unsafeMove

-- Data.Vector.Primitive
-- |'VP.Vector'
type PVector = VP.Vector
-- |'VP.MVector'
type PMVector = VP.MVector
-- |'VP.length'
vp_length = VP.length
-- |'VP.null'
vp_null = VP.null
-- |('VP.!')
vp_get = (VP.!)
-- |('VP.!?')
vp_getq = (VP.!?)
-- |'VP.head'
vp_head = VP.head
-- |'VP.last'
vp_last = VP.last
-- |'VP.unsafeIndex'
vp_unsafeIndex = VP.unsafeIndex
-- |'VP.unsafeHead'
vp_unsafeHead = VP.unsafeHead
-- |'VP.unsafeLast'
vp_unsafeLast = VP.unsafeLast
-- |'VP.indexM'
vp_indexM = VP.indexM
-- |'VP.headM'
vp_headM = VP.headM
-- |'VP.lastM'
vp_lastM = VP.lastM
-- |'VP.unsafeIndexM'
vp_unsafeIndexM = VP.unsafeIndexM
-- |'VP.unsafeHeadM'
vp_unsafeHeadM = VP.unsafeHeadM
-- |'VP.unsafeLastM'
vp_unsafeLastM = VP.unsafeLastM
-- |'VP.slice'
vp_slice = VP.slice
-- |'VP.init'
vp_init = VP.init
-- |'VP.tail'
vp_tail = VP.tail
-- |'VP.take'
vp_take = VP.take
-- |'VP.drop'
vp_drop = VP.drop
-- |'VP.splitAt'
vp_splitAt = VP.splitAt
-- |'VP.unsafeSlice'
vp_unsafeSlice = VP.unsafeSlice
-- |'VP.unsafeInit'
vp_unsafeInit = VP.unsafeInit
-- |'VP.unsafeTail'
vp_unsafeTail = VP.unsafeTail
-- |'VP.unsafeTake'
vp_unsafeTake = VP.unsafeTake
-- |'VP.unsafeDrop'
vp_unsafeDrop = VP.unsafeDrop
-- |'VP.empty'
vp_empty = VP.empty
-- |'VP.singleton'
vp_singleton = VP.singleton
-- |'VP.replicate'
vp_replicate = VP.replicate
-- |'VP.generate'
vp_generate = VP.generate
-- |'VP.iterateN'
vp_iterateN = VP.iterateN
-- |'VP.replicateM'
vp_replicateM = VP.replicateM
-- |'VP.generateM'
vp_generateM = VP.generateM
-- |'VP.create'
vp_create = VP.create
-- |'VP.unfoldr'
vp_unfoldr = VP.unfoldr
-- |'VP.unfoldrN'
vp_unfoldrN = VP.unfoldrN
-- |'VP.constructN'
vp_constructN = VP.constructN
-- |'VP.constructrN'
vp_constructrN = VP.constructrN
-- |'VP.enumFromN'
vp_enumFromN = VP.enumFromN
-- |'VP.enumFromStepN'
vp_enumFromStepN = VP.enumFromStepN
-- |'VP.enumFromTo'
vp_enumFromTo = VP.enumFromTo
-- |'VP.enumFromThenTo'
vp_enumFromThenTo = VP.enumFromThenTo
-- |'VP.cons'
vp_cons = VP.cons
-- |'VP.snoc'
vp_snoc = VP.snoc
-- |('VP.++')
vp_cat = (VP.++)
-- |'VP.concat'
vp_concat = VP.concat
-- |'VP.force'
vp_force = VP.force
-- |('VP.//')
vp_upd = (VP.//)
-- |'VP.update_'
vp_update_ = VP.update_
-- |'VP.unsafeUpd'
vp_unsafeUpd = VP.unsafeUpd
-- |'VP.unsafeUpdate_'
vp_unsafeUpdate_ = VP.unsafeUpdate_
-- |'VP.accum'
vp_accum = VP.accum
-- |'VP.accumulate_'
vp_accumulate_ = VP.accumulate_
-- |'VP.unsafeAccum'
vp_unsafeAccum = VP.unsafeAccum
-- |'VP.unsafeAccumulate_'
vp_unsafeAccumulate_ = VP.unsafeAccumulate_
-- |'VP.reverse'
vp_reverse = VP.reverse
-- |'VP.backpermute'
vp_backpermute = VP.backpermute
-- |'VP.unsafeBackpermute'
vp_unsafeBackpermute = VP.unsafeBackpermute
-- |'VP.modify'
vp_modify = VP.modify
-- |'VP.map'
vp_map = VP.map
-- |'VP.imap'
vp_imap = VP.imap
-- |'VP.concatMap'
vp_concatMap = VP.concatMap
-- |'VP.mapM'
vp_mapM = VP.mapM
-- |'VP.mapM_'
vp_mapM_ = VP.mapM_
-- |'VP.forM'
vp_forM = VP.forM
-- |'VP.forM_'
vp_forM_ = VP.forM_
-- |'VP.zipWith'
vp_zipWith = VP.zipWith
-- |'VP.zipWith3'
vp_zipWith3 = VP.zipWith3
-- |'VP.zipWith4'
vp_zipWith4 = VP.zipWith4
-- |'VP.zipWith5'
vp_zipWith5 = VP.zipWith5
-- |'VP.zipWith6'
vp_zipWith6 = VP.zipWith6
-- |'VP.izipWith'
vp_izipWith = VP.izipWith
-- |'VP.izipWith3'
vp_izipWith3 = VP.izipWith3
-- |'VP.izipWith4'
vp_izipWith4 = VP.izipWith4
-- |'VP.izipWith5'
vp_izipWith5 = VP.izipWith5
-- |'VP.izipWith6'
vp_izipWith6 = VP.izipWith6
-- |'VP.zipWithM'
vp_zipWithM = VP.zipWithM
-- |'VP.zipWithM_'
vp_zipWithM_ = VP.zipWithM_
-- |'VP.filter'
vp_filter = VP.filter
-- |'VP.ifilter'
vp_ifilter = VP.ifilter
-- |'VP.filterM'
vp_filterM = VP.filterM
-- |'VP.takeWhile'
vp_takeWhile = VP.takeWhile
-- |'VP.dropWhile'
vp_dropWhile = VP.dropWhile
-- |'VP.partition'
vp_partition = VP.partition
-- |'VP.unstablePartition'
vp_unstablePartition = VP.unstablePartition
-- |'VP.span'
vp_span = VP.span
-- |'VP.break'
vp_break = VP.break
-- |'VP.elem'
vp_elem = VP.elem
-- |'VP.notElem'
vp_notElem = VP.notElem
-- |'VP.find'
vp_find = VP.find
-- |'VP.findIndex'
vp_findIndex = VP.findIndex
-- |'VP.findIndices'
vp_findIndices = VP.findIndices
-- |'VP.elemIndex'
vp_elemIndex = VP.elemIndex
-- |'VP.elemIndices'
vp_elemIndices = VP.elemIndices
-- |'VP.foldl'
vp_foldl = VP.foldl
-- |'VP.foldl1'
vp_foldl1 = VP.foldl1
-- |'VP.foldl''
vp_foldl' = VP.foldl'
-- |'VP.foldl1''
vp_foldl1' = VP.foldl1'
-- |'VP.foldr'
vp_foldr = VP.foldr
-- |'VP.foldr1'
vp_foldr1 = VP.foldr1
-- |'VP.foldr''
vp_foldr' = VP.foldr'
-- |'VP.foldr1''
vp_foldr1' = VP.foldr1'
-- |'VP.ifoldl'
vp_ifoldl = VP.ifoldl
-- |'VP.ifoldl''
vp_ifoldl' = VP.ifoldl'
-- |'VP.ifoldr'
vp_ifoldr = VP.ifoldr
-- |'VP.ifoldr''
vp_ifoldr' = VP.ifoldr'
-- |'VP.all'
vp_all = VP.all
-- |'VP.any'
vp_any = VP.any
-- |'VP.sum'
vp_sum = VP.sum
-- |'VP.product'
vp_product = VP.product
-- |'VP.maximum'
vp_maximum = VP.maximum
-- |'VP.maximumBy'
vp_maximumBy = VP.maximumBy
-- |'VP.minimum'
vp_minimum = VP.minimum
-- |'VP.minimumBy'
vp_minimumBy = VP.minimumBy
-- |'VP.minIndex'
vp_minIndex = VP.minIndex
-- |'VP.minIndexBy'
vp_minIndexBy = VP.minIndexBy
-- |'VP.maxIndex'
vp_maxIndex = VP.maxIndex
-- |'VP.maxIndexBy'
vp_maxIndexBy = VP.maxIndexBy
-- |'VP.foldM'
vp_foldM = VP.foldM
-- |'VP.foldM''
vp_foldM' = VP.foldM'
-- |'VP.fold1M'
vp_fold1M = VP.fold1M
-- |'VP.fold1M''
vp_fold1M' = VP.fold1M'
-- |'VP.foldM_'
vp_foldM_ = VP.foldM_
-- |'VP.foldM'_'
vp_foldM'_ = VP.foldM'_
-- |'VP.fold1M_'
vp_fold1M_ = VP.fold1M_
-- |'VP.fold1M'_'
vp_fold1M'_ = VP.fold1M'_
-- |'VP.prescanl'
vp_prescanl = VP.prescanl
-- |'VP.prescanl''
vp_prescanl' = VP.prescanl'
-- |'VP.postscanl'
vp_postscanl = VP.postscanl
-- |'VP.postscanl''
vp_postscanl' = VP.postscanl'
-- |'VP.scanl'
vp_scanl = VP.scanl
-- |'VP.scanl''
vp_scanl' = VP.scanl'
-- |'VP.scanl1'
vp_scanl1 = VP.scanl1
-- |'VP.scanl1''
vp_scanl1' = VP.scanl1'
-- |'VP.prescanr'
vp_prescanr = VP.prescanr
-- |'VP.prescanr''
vp_prescanr' = VP.prescanr'
-- |'VP.postscanr'
vp_postscanr = VP.postscanr
-- |'VP.postscanr''
vp_postscanr' = VP.postscanr'
-- |'VP.scanr'
vp_scanr = VP.scanr
-- |'VP.scanr''
vp_scanr' = VP.scanr'
-- |'VP.scanr1'
vp_scanr1 = VP.scanr1
-- |'VP.scanr1''
vp_scanr1' = VP.scanr1'
-- |'VP.toList'
vp_toList = VP.toList
-- |'VP.fromList'
vp_fromList = VP.fromList
-- |'VP.fromListN'
vp_fromListN = VP.fromListN
-- |'VP.convert'
vp_convert = VP.convert
-- |'VP.freeze'
vp_freeze = VP.freeze
-- |'VP.thaw'
vp_thaw = VP.thaw
-- |'VP.copy'
vp_copy = VP.copy
-- |'VP.unsafeFreeze'
vp_unsafeFreeze = VP.unsafeFreeze
-- |'VP.unsafeThaw'
vp_unsafeThaw = VP.unsafeThaw
-- |'VP.unsafeCopy'
vp_unsafeCopy = VP.unsafeCopy

-- Data.Vector.Primitive.Mutable
-- |'VPM.IOVector'
type PIOVector = PMVector RealWorld
-- |'VPM.STVector'
type PSTVector s = PMVector s
-- |'VPM.length'
vpm_length = VPM.length
-- |'VPM.null'
vpm_null = VPM.null
-- |'VPM.slice'
vpm_slice = VPM.slice
-- |'VPM.init'
vpm_init = VPM.init
-- |'VPM.tail'
vpm_tail = VPM.tail
-- |'VPM.take'
vpm_take = VPM.take
-- |'VPM.drop'
vpm_drop = VPM.drop
-- |'VPM.splitAt'
vpm_splitAt = VPM.splitAt
-- |'VPM.unsafeSlice'
vpm_unsafeSlice = VPM.unsafeSlice
-- |'VPM.unsafeInit'
vpm_unsafeInit = VPM.unsafeInit
-- |'VPM.unsafeTail'
vpm_unsafeTail = VPM.unsafeTail
-- |'VPM.unsafeTake'
vpm_unsafeTake = VPM.unsafeTake
-- |'VPM.unsafeDrop'
vpm_unsafeDrop = VPM.unsafeDrop
-- |'VPM.overlaps'
vpm_overlaps = VPM.overlaps
-- |'VPM.new'
vpm_new = VPM.new
-- |'VPM.unsafeNew'
vpm_unsafeNew = VPM.unsafeNew
-- |'VPM.replicate'
vpm_replicate = VPM.replicate
-- |'VPM.replicateM'
vpm_replicateM = VPM.replicateM
-- |'VPM.clone'
vpm_clone = VPM.clone
-- |'VPM.grow'
vpm_grow = VPM.grow
-- |'VPM.unsafeGrow'
vpm_unsafeGrow = VPM.unsafeGrow
-- |'VPM.clear'
vpm_clear = VPM.clear
-- |'VPM.read'
vpm_read = VPM.read
-- |'VPM.write'
vpm_write = VPM.write
-- |'VPM.swap'
vpm_swap = VPM.swap
-- |'VPM.unsafeRead'
vpm_unsafeRead = VPM.unsafeRead
-- |'VPM.unsafeWrite'
vpm_unsafeWrite = VPM.unsafeWrite
-- |'VPM.unsafeSwap'
vpm_unsafeSwap = VPM.unsafeSwap
-- |'VPM.set'
vpm_set = VPM.set
-- |'VPM.copy'
vpm_copy = VPM.copy
-- |'VPM.move'
vpm_move = VPM.move
-- |'VPM.unsafeCopy'
vpm_unsafeCopy = VPM.unsafeCopy
-- |'VPM.unsafeMove'
vpm_unsafeMove = VPM.unsafeMove

-- Data.Vector.Storable
-- |'VS.Vector'
type SVector = VS.Vector
-- |'VS.MVector'
type SMVector = VS.MVector
-- |'VS.length'
vs_length = VS.length
-- |'VS.null'
vs_null = VS.null
-- |('VS.!')
vs_get = (VS.!)
-- |('VS.!?')
vs_getq = (VS.!?)
-- |'VS.head'
vs_head = VS.head
-- |'VS.last'
vs_last = VS.last
-- |'VS.unsafeIndex'
vs_unsafeIndex = VS.unsafeIndex
-- |'VS.unsafeHead'
vs_unsafeHead = VS.unsafeHead
-- |'VS.unsafeLast'
vs_unsafeLast = VS.unsafeLast
-- |'VS.indexM'
vs_indexM = VS.indexM
-- |'VS.headM'
vs_headM = VS.headM
-- |'VS.lastM'
vs_lastM = VS.lastM
-- |'VS.unsafeIndexM'
vs_unsafeIndexM = VS.unsafeIndexM
-- |'VS.unsafeHeadM'
vs_unsafeHeadM = VS.unsafeHeadM
-- |'VS.unsafeLastM'
vs_unsafeLastM = VS.unsafeLastM
-- |'VS.slice'
vs_slice = VS.slice
-- |'VS.init'
vs_init = VS.init
-- |'VS.tail'
vs_tail = VS.tail
-- |'VS.take'
vs_take = VS.take
-- |'VS.drop'
vs_drop = VS.drop
-- |'VS.splitAt'
vs_splitAt = VS.splitAt
-- |'VS.unsafeSlice'
vs_unsafeSlice = VS.unsafeSlice
-- |'VS.unsafeInit'
vs_unsafeInit = VS.unsafeInit
-- |'VS.unsafeTail'
vs_unsafeTail = VS.unsafeTail
-- |'VS.unsafeTake'
vs_unsafeTake = VS.unsafeTake
-- |'VS.unsafeDrop'
vs_unsafeDrop = VS.unsafeDrop
-- |'VS.empty'
vs_empty = VS.empty
-- |'VS.singleton'
vs_singleton = VS.singleton
-- |'VS.replicate'
vs_replicate = VS.replicate
-- |'VS.generate'
vs_generate = VS.generate
-- |'VS.iterateN'
vs_iterateN = VS.iterateN
-- |'VS.replicateM'
vs_replicateM = VS.replicateM
-- |'VS.generateM'
vs_generateM = VS.generateM
-- |'VS.create'
vs_create = VS.create
-- |'VS.unfoldr'
vs_unfoldr = VS.unfoldr
-- |'VS.unfoldrN'
vs_unfoldrN = VS.unfoldrN
-- |'VS.constructN'
vs_constructN = VS.constructN
-- |'VS.constructrN'
vs_constructrN = VS.constructrN
-- |'VS.enumFromN'
vs_enumFromN = VS.enumFromN
-- |'VS.enumFromStepN'
vs_enumFromStepN = VS.enumFromStepN
-- |'VS.enumFromTo'
vs_enumFromTo = VS.enumFromTo
-- |'VS.enumFromThenTo'
vs_enumFromThenTo = VS.enumFromThenTo
-- |'VS.cons'
vs_cons = VS.cons
-- |'VS.snoc'
vs_snoc = VS.snoc
-- |('VS.++')
vs_cat = (VS.++)
-- |'VS.concat'
vs_concat = VS.concat
-- |'VS.force'
vs_force = VS.force
-- |('VS.//')
vs_upd = (VS.//)
-- |'VS.update_'
vs_update_ = VS.update_
-- |'VS.unsafeUpd'
vs_unsafeUpd = VS.unsafeUpd
-- |'VS.unsafeUpdate_'
vs_unsafeUpdate_ = VS.unsafeUpdate_
-- |'VS.accum'
vs_accum = VS.accum
-- |'VS.accumulate_'
vs_accumulate_ = VS.accumulate_
-- |'VS.unsafeAccum'
vs_unsafeAccum = VS.unsafeAccum
-- |'VS.unsafeAccumulate_'
vs_unsafeAccumulate_ = VS.unsafeAccumulate_
-- |'VS.reverse'
vs_reverse = VS.reverse
-- |'VS.backpermute'
vs_backpermute = VS.backpermute
-- |'VS.unsafeBackpermute'
vs_unsafeBackpermute = VS.unsafeBackpermute
-- |'VS.modify'
vs_modify = VS.modify
-- |'VS.map'
vs_map = VS.map
-- |'VS.imap'
vs_imap = VS.imap
-- |'VS.concatMap'
vs_concatMap = VS.concatMap
-- |'VS.mapM'
vs_mapM = VS.mapM
-- |'VS.mapM_'
vs_mapM_ = VS.mapM_
-- |'VS.forM'
vs_forM = VS.forM
-- |'VS.forM_'
vs_forM_ = VS.forM_
-- |'VS.zipWith'
vs_zipWith = VS.zipWith
-- |'VS.zipWith3'
vs_zipWith3 = VS.zipWith3
-- |'VS.zipWith4'
vs_zipWith4 = VS.zipWith4
-- |'VS.zipWith5'
vs_zipWith5 = VS.zipWith5
-- |'VS.zipWith6'
vs_zipWith6 = VS.zipWith6
-- |'VS.izipWith'
vs_izipWith = VS.izipWith
-- |'VS.izipWith3'
vs_izipWith3 = VS.izipWith3
-- |'VS.izipWith4'
vs_izipWith4 = VS.izipWith4
-- |'VS.izipWith5'
vs_izipWith5 = VS.izipWith5
-- |'VS.izipWith6'
vs_izipWith6 = VS.izipWith6
-- |'VS.zipWithM'
vs_zipWithM = VS.zipWithM
-- |'VS.zipWithM_'
vs_zipWithM_ = VS.zipWithM_
-- |'VS.filter'
vs_filter = VS.filter
-- |'VS.ifilter'
vs_ifilter = VS.ifilter
-- |'VS.filterM'
vs_filterM = VS.filterM
-- |'VS.takeWhile'
vs_takeWhile = VS.takeWhile
-- |'VS.dropWhile'
vs_dropWhile = VS.dropWhile
-- |'VS.partition'
vs_partition = VS.partition
-- |'VS.unstablePartition'
vs_unstablePartition = VS.unstablePartition
-- |'VS.span'
vs_span = VS.span
-- |'VS.break'
vs_break = VS.break
-- |'VS.elem'
vs_elem = VS.elem
-- |'VS.notElem'
vs_notElem = VS.notElem
-- |'VS.find'
vs_find = VS.find
-- |'VS.findIndex'
vs_findIndex = VS.findIndex
-- |'VS.findIndices'
vs_findIndices = VS.findIndices
-- |'VS.elemIndex'
vs_elemIndex = VS.elemIndex
-- |'VS.elemIndices'
vs_elemIndices = VS.elemIndices
-- |'VS.foldl'
vs_foldl = VS.foldl
-- |'VS.foldl1'
vs_foldl1 = VS.foldl1
-- |'VS.foldl''
vs_foldl' = VS.foldl'
-- |'VS.foldl1''
vs_foldl1' = VS.foldl1'
-- |'VS.foldr'
vs_foldr = VS.foldr
-- |'VS.foldr1'
vs_foldr1 = VS.foldr1
-- |'VS.foldr''
vs_foldr' = VS.foldr'
-- |'VS.foldr1''
vs_foldr1' = VS.foldr1'
-- |'VS.ifoldl'
vs_ifoldl = VS.ifoldl
-- |'VS.ifoldl''
vs_ifoldl' = VS.ifoldl'
-- |'VS.ifoldr'
vs_ifoldr = VS.ifoldr
-- |'VS.ifoldr''
vs_ifoldr' = VS.ifoldr'
-- |'VS.all'
vs_all = VS.all
-- |'VS.any'
vs_any = VS.any
-- |'VS.and'
vs_and = VS.and
-- |'VS.or'
vs_or = VS.or
-- |'VS.sum'
vs_sum = VS.sum
-- |'VS.product'
vs_product = VS.product
-- |'VS.maximum'
vs_maximum = VS.maximum
-- |'VS.maximumBy'
vs_maximumBy = VS.maximumBy
-- |'VS.minimum'
vs_minimum = VS.minimum
-- |'VS.minimumBy'
vs_minimumBy = VS.minimumBy
-- |'VS.minIndex'
vs_minIndex = VS.minIndex
-- |'VS.minIndexBy'
vs_minIndexBy = VS.minIndexBy
-- |'VS.maxIndex'
vs_maxIndex = VS.maxIndex
-- |'VS.maxIndexBy'
vs_maxIndexBy = VS.maxIndexBy
-- |'VS.foldM'
vs_foldM = VS.foldM
-- |'VS.foldM''
vs_foldM' = VS.foldM'
-- |'VS.fold1M'
vs_fold1M = VS.fold1M
-- |'VS.fold1M''
vs_fold1M' = VS.fold1M'
-- |'VS.foldM_'
vs_foldM_ = VS.foldM_
-- |'VS.foldM'_'
vs_foldM'_ = VS.foldM'_
-- |'VS.fold1M_'
vs_fold1M_ = VS.fold1M_
-- |'VS.fold1M'_'
vs_fold1M'_ = VS.fold1M'_
-- |'VS.prescanl'
vs_prescanl = VS.prescanl
-- |'VS.prescanl''
vs_prescanl' = VS.prescanl'
-- |'VS.postscanl'
vs_postscanl = VS.postscanl
-- |'VS.postscanl''
vs_postscanl' = VS.postscanl'
-- |'VS.scanl'
vs_scanl = VS.scanl
-- |'VS.scanl''
vs_scanl' = VS.scanl'
-- |'VS.scanl1'
vs_scanl1 = VS.scanl1
-- |'VS.scanl1''
vs_scanl1' = VS.scanl1'
-- |'VS.prescanr'
vs_prescanr = VS.prescanr
-- |'VS.prescanr''
vs_prescanr' = VS.prescanr'
-- |'VS.postscanr'
vs_postscanr = VS.postscanr
-- |'VS.postscanr''
vs_postscanr' = VS.postscanr'
-- |'VS.scanr'
vs_scanr = VS.scanr
-- |'VS.scanr''
vs_scanr' = VS.scanr'
-- |'VS.scanr1'
vs_scanr1 = VS.scanr1
-- |'VS.scanr1''
vs_scanr1' = VS.scanr1'
-- |'VS.toList'
vs_toList = VS.toList
-- |'VS.fromList'
vs_fromList = VS.fromList
-- |'VS.fromListN'
vs_fromListN = VS.fromListN
-- |'VS.convert'
vs_convert = VS.convert
-- |'VS.unsafeCast'
vs_unsafeCast = VS.unsafeCast
-- |'VS.freeze'
vs_freeze = VS.freeze
-- |'VS.thaw'
vs_thaw = VS.thaw
-- |'VS.copy'
vs_copy = VS.copy
-- |'VS.unsafeFreeze'
vs_unsafeFreeze = VS.unsafeFreeze
-- |'VS.unsafeThaw'
vs_unsafeThaw = VS.unsafeThaw
-- |'VS.unsafeCopy'
vs_unsafeCopy = VS.unsafeCopy
-- |'VS.unsafeFromForeignPtr'
vs_unsafeFromForeignPtr = VS.unsafeFromForeignPtr
-- |'VS.unsafeFromForeignPtr0'
vs_unsafeFromForeignPtr0 = VS.unsafeFromForeignPtr0
-- |'VS.unsafeToForeignPtr'
vs_unsafeToForeignPtr = VS.unsafeToForeignPtr
-- |'VS.unsafeToForeignPtr0'
vs_unsafeToForeignPtr0 = VS.unsafeToForeignPtr0
-- |'VS.unsafeWith'
vs_unsafeWith = VS.unsafeWith

-- Data.Vector.Storable.Mutable
-- |'VSM.IOVector'
type SIOVector = SMVector RealWorld
-- |'VSM.STVector'
type SSTVector s = SMVector s
-- |'VSM.length'
vsm_length = VSM.length
-- |'VSM.null'
vsm_null = VSM.null
-- |'VSM.slice'
vsm_slice = VSM.slice
-- |'VSM.init'
vsm_init = VSM.init
-- |'VSM.tail'
vsm_tail = VSM.tail
-- |'VSM.take'
vsm_take = VSM.take
-- |'VSM.drop'
vsm_drop = VSM.drop
-- |'VSM.splitAt'
vsm_splitAt = VSM.splitAt
-- |'VSM.unsafeSlice'
vsm_unsafeSlice = VSM.unsafeSlice
-- |'VSM.unsafeInit'
vsm_unsafeInit = VSM.unsafeInit
-- |'VSM.unsafeTail'
vsm_unsafeTail = VSM.unsafeTail
-- |'VSM.unsafeTake'
vsm_unsafeTake = VSM.unsafeTake
-- |'VSM.unsafeDrop'
vsm_unsafeDrop = VSM.unsafeDrop
-- |'VSM.overlaps'
vsm_overlaps = VSM.overlaps
-- |'VSM.new'
vsm_new = VSM.new
-- |'VSM.unsafeNew'
vsm_unsafeNew = VSM.unsafeNew
-- |'VSM.replicate'
vsm_replicate = VSM.replicate
-- |'VSM.replicateM'
vsm_replicateM = VSM.replicateM
-- |'VSM.clone'
vsm_clone = VSM.clone
-- |'VSM.grow'
vsm_grow = VSM.grow
-- |'VSM.unsafeGrow'
vsm_unsafeGrow = VSM.unsafeGrow
-- |'VSM.clear'
vsm_clear = VSM.clear
-- |'VSM.read'
vsm_read = VSM.read
-- |'VSM.write'
vsm_write = VSM.write
-- |'VSM.swap'
vsm_swap = VSM.swap
-- |'VSM.unsafeRead'
vsm_unsafeRead = VSM.unsafeRead
-- |'VSM.unsafeWrite'
vsm_unsafeWrite = VSM.unsafeWrite
-- |'VSM.unsafeSwap'
vsm_unsafeSwap = VSM.unsafeSwap
-- |'VSM.set'
vsm_set = VSM.set
-- |'VSM.copy'
vsm_copy = VSM.copy
-- |'VSM.move'
vsm_move = VSM.move
-- |'VSM.unsafeCopy'
vsm_unsafeCopy = VSM.unsafeCopy
-- |'VSM.unsafeMove'
vsm_unsafeMove = VSM.unsafeMove
-- |'VSM.unsafeCast'
vsm_unsafeCast = VSM.unsafeCast
-- |'VSM.unsafeFromForeignPtr'
vsm_unsafeFromForeignPtr = VSM.unsafeFromForeignPtr
-- |'VSM.unsafeFromForeignPtr0'
vsm_unsafeFromForeignPtr0 = VSM.unsafeFromForeignPtr0
-- |'VSM.unsafeToForeignPtr'
vsm_unsafeToForeignPtr = VSM.unsafeToForeignPtr
-- |'VSM.unsafeToForeignPtr0'
vsm_unsafeToForeignPtr0 = VSM.unsafeToForeignPtr0
-- |'VSM.unsafeWith'
vsm_unsafeWith = VSM.unsafeWith

-- Data.Vector.Unboxed
-- |'VU.Vector'
type UVector = VU.Vector
-- |'VU.MVector'
type UMVector = VU.MVector
-- |'VU.length'
vu_length = VU.length
-- |'VU.null'
vu_null = VU.null
-- |('VU.!')
vu_get = (VU.!)
-- |('VU.!?')
vu_getq = (VU.!?)
-- |'VU.head'
vu_head = VU.head
-- |'VU.last'
vu_last = VU.last
-- |'VU.unsafeIndex'
vu_unsafeIndex = VU.unsafeIndex
-- |'VU.unsafeHead'
vu_unsafeHead = VU.unsafeHead
-- |'VU.unsafeLast'
vu_unsafeLast = VU.unsafeLast
-- |'VU.indexM'
vu_indexM = VU.indexM
-- |'VU.headM'
vu_headM = VU.headM
-- |'VU.lastM'
vu_lastM = VU.lastM
-- |'VU.unsafeIndexM'
vu_unsafeIndexM = VU.unsafeIndexM
-- |'VU.unsafeHeadM'
vu_unsafeHeadM = VU.unsafeHeadM
-- |'VU.unsafeLastM'
vu_unsafeLastM = VU.unsafeLastM
-- |'VU.slice'
vu_slice = VU.slice
-- |'VU.init'
vu_init = VU.init
-- |'VU.tail'
vu_tail = VU.tail
-- |'VU.take'
vu_take = VU.take
-- |'VU.drop'
vu_drop = VU.drop
-- |'VU.splitAt'
vu_splitAt = VU.splitAt
-- |'VU.unsafeSlice'
vu_unsafeSlice = VU.unsafeSlice
-- |'VU.unsafeInit'
vu_unsafeInit = VU.unsafeInit
-- |'VU.unsafeTail'
vu_unsafeTail = VU.unsafeTail
-- |'VU.unsafeTake'
vu_unsafeTake = VU.unsafeTake
-- |'VU.unsafeDrop'
vu_unsafeDrop = VU.unsafeDrop
-- |'VU.empty'
vu_empty = VU.empty
-- |'VU.singleton'
vu_singleton = VU.singleton
-- |'VU.replicate'
vu_replicate = VU.replicate
-- |'VU.generate'
vu_generate = VU.generate
-- |'VU.iterateN'
vu_iterateN = VU.iterateN
-- |'VU.replicateM'
vu_replicateM = VU.replicateM
-- |'VU.generateM'
vu_generateM = VU.generateM
-- |'VU.create'
vu_create = VU.create
-- |'VU.unfoldr'
vu_unfoldr = VU.unfoldr
-- |'VU.unfoldrN'
vu_unfoldrN = VU.unfoldrN
-- |'VU.constructN'
vu_constructN = VU.constructN
-- |'VU.constructrN'
vu_constructrN = VU.constructrN
-- |'VU.enumFromN'
vu_enumFromN = VU.enumFromN
-- |'VU.enumFromStepN'
vu_enumFromStepN = VU.enumFromStepN
-- |'VU.enumFromTo'
vu_enumFromTo = VU.enumFromTo
-- |'VU.enumFromThenTo'
vu_enumFromThenTo = VU.enumFromThenTo
-- |'VU.cons'
vu_cons = VU.cons
-- |'VU.snoc'
vu_snoc = VU.snoc
-- |('VU.++')
vu_cat = (VU.++)
-- |'VU.concat'
vu_concat = VU.concat
-- |'VU.force'
vu_force = VU.force
-- |('VU.//')
vu_upd = (VU.//)
-- |'VU.update'
vu_update = VU.update
-- |'VU.update_'
vu_update_ = VU.update_
-- |'VU.unsafeUpd'
vu_unsafeUpd = VU.unsafeUpd
-- |'VU.unsafeUpdate'
vu_unsafeUpdate = VU.unsafeUpdate
-- |'VU.unsafeUpdate_'
vu_unsafeUpdate_ = VU.unsafeUpdate_
-- |'VU.accum'
vu_accum = VU.accum
-- |'VU.accumulate'
vu_accumulate = VU.accumulate
-- |'VU.accumulate_'
vu_accumulate_ = VU.accumulate_
-- |'VU.unsafeAccum'
vu_unsafeAccum = VU.unsafeAccum
-- |'VU.unsafeAccumulate'
vu_unsafeAccumulate = VU.unsafeAccumulate
-- |'VU.unsafeAccumulate_'
vu_unsafeAccumulate_ = VU.unsafeAccumulate_
-- |'VU.reverse'
vu_reverse = VU.reverse
-- |'VU.backpermute'
vu_backpermute = VU.backpermute
-- |'VU.unsafeBackpermute'
vu_unsafeBackpermute = VU.unsafeBackpermute
-- |'VU.modify'
vu_modify = VU.modify
-- |'VU.indexed'
vu_indexed = VU.indexed
-- |'VU.map'
vu_map = VU.map
-- |'VU.imap'
vu_imap = VU.imap
-- |'VU.concatMap'
vu_concatMap = VU.concatMap
-- |'VU.mapM'
vu_mapM = VU.mapM
-- |'VU.mapM_'
vu_mapM_ = VU.mapM_
-- |'VU.forM'
vu_forM = VU.forM
-- |'VU.forM_'
vu_forM_ = VU.forM_
-- |'VU.zipWith'
vu_zipWith = VU.zipWith
-- |'VU.zipWith3'
vu_zipWith3 = VU.zipWith3
-- |'VU.zipWith4'
vu_zipWith4 = VU.zipWith4
-- |'VU.zipWith5'
vu_zipWith5 = VU.zipWith5
-- |'VU.zipWith6'
vu_zipWith6 = VU.zipWith6
-- |'VU.izipWith'
vu_izipWith = VU.izipWith
-- |'VU.izipWith3'
vu_izipWith3 = VU.izipWith3
-- |'VU.izipWith4'
vu_izipWith4 = VU.izipWith4
-- |'VU.izipWith5'
vu_izipWith5 = VU.izipWith5
-- |'VU.izipWith6'
vu_izipWith6 = VU.izipWith6
-- |'VU.zip'
vu_zip = VU.zip
-- |'VU.zip3'
vu_zip3 = VU.zip3
-- |'VU.zip4'
vu_zip4 = VU.zip4
-- |'VU.zip5'
vu_zip5 = VU.zip5
-- |'VU.zip6'
vu_zip6 = VU.zip6
-- |'VU.zipWithM'
vu_zipWithM = VU.zipWithM
-- |'VU.zipWithM_'
vu_zipWithM_ = VU.zipWithM_
-- |'VU.unzip'
vu_unzip = VU.unzip
-- |'VU.unzip3'
vu_unzip3 = VU.unzip3
-- |'VU.unzip4'
vu_unzip4 = VU.unzip4
-- |'VU.unzip5'
vu_unzip5 = VU.unzip5
-- |'VU.unzip6'
vu_unzip6 = VU.unzip6
-- |'VU.filter'
vu_filter = VU.filter
-- |'VU.ifilter'
vu_ifilter = VU.ifilter
-- |'VU.filterM'
vu_filterM = VU.filterM
-- |'VU.takeWhile'
vu_takeWhile = VU.takeWhile
-- |'VU.dropWhile'
vu_dropWhile = VU.dropWhile
-- |'VU.partition'
vu_partition = VU.partition
-- |'VU.unstablePartition'
vu_unstablePartition = VU.unstablePartition
-- |'VU.span'
vu_span = VU.span
-- |'VU.break'
vu_break = VU.break
-- |'VU.elem'
vu_elem = VU.elem
-- |'VU.notElem'
vu_notElem = VU.notElem
-- |'VU.find'
vu_find = VU.find
-- |'VU.findIndex'
vu_findIndex = VU.findIndex
-- |'VU.findIndices'
vu_findIndices = VU.findIndices
-- |'VU.elemIndex'
vu_elemIndex = VU.elemIndex
-- |'VU.elemIndices'
vu_elemIndices = VU.elemIndices
-- |'VU.foldl'
vu_foldl = VU.foldl
-- |'VU.foldl1'
vu_foldl1 = VU.foldl1
-- |'VU.foldl''
vu_foldl' = VU.foldl'
-- |'VU.foldl1''
vu_foldl1' = VU.foldl1'
-- |'VU.foldr'
vu_foldr = VU.foldr
-- |'VU.foldr1'
vu_foldr1 = VU.foldr1
-- |'VU.foldr''
vu_foldr' = VU.foldr'
-- |'VU.foldr1''
vu_foldr1' = VU.foldr1'
-- |'VU.ifoldl'
vu_ifoldl = VU.ifoldl
-- |'VU.ifoldl''
vu_ifoldl' = VU.ifoldl'
-- |'VU.ifoldr'
vu_ifoldr = VU.ifoldr
-- |'VU.ifoldr''
vu_ifoldr' = VU.ifoldr'
-- |'VU.all'
vu_all = VU.all
-- |'VU.any'
vu_any = VU.any
-- |'VU.and'
vu_and = VU.and
-- |'VU.or'
vu_or = VU.or
-- |'VU.sum'
vu_sum = VU.sum
-- |'VU.product'
vu_product = VU.product
-- |'VU.maximum'
vu_maximum = VU.maximum
-- |'VU.maximumBy'
vu_maximumBy = VU.maximumBy
-- |'VU.minimum'
vu_minimum = VU.minimum
-- |'VU.minimumBy'
vu_minimumBy = VU.minimumBy
-- |'VU.minIndex'
vu_minIndex = VU.minIndex
-- |'VU.minIndexBy'
vu_minIndexBy = VU.minIndexBy
-- |'VU.maxIndex'
vu_maxIndex = VU.maxIndex
-- |'VU.maxIndexBy'
vu_maxIndexBy = VU.maxIndexBy
-- |'VU.foldM'
vu_foldM = VU.foldM
-- |'VU.foldM''
vu_foldM' = VU.foldM'
-- |'VU.fold1M'
vu_fold1M = VU.fold1M
-- |'VU.fold1M''
vu_fold1M' = VU.fold1M'
-- |'VU.foldM_'
vu_foldM_ = VU.foldM_
-- |'VU.foldM'_'
vu_foldM'_ = VU.foldM'_
-- |'VU.fold1M_'
vu_fold1M_ = VU.fold1M_
-- |'VU.fold1M'_'
vu_fold1M'_ = VU.fold1M'_
-- |'VU.prescanl'
vu_prescanl = VU.prescanl
-- |'VU.prescanl''
vu_prescanl' = VU.prescanl'
-- |'VU.postscanl'
vu_postscanl = VU.postscanl
-- |'VU.postscanl''
vu_postscanl' = VU.postscanl'
-- |'VU.scanl'
vu_scanl = VU.scanl
-- |'VU.scanl''
vu_scanl' = VU.scanl'
-- |'VU.scanl1'
vu_scanl1 = VU.scanl1
-- |'VU.scanl1''
vu_scanl1' = VU.scanl1'
-- |'VU.prescanr'
vu_prescanr = VU.prescanr
-- |'VU.prescanr''
vu_prescanr' = VU.prescanr'
-- |'VU.postscanr'
vu_postscanr = VU.postscanr
-- |'VU.postscanr''
vu_postscanr' = VU.postscanr'
-- |'VU.scanr'
vu_scanr = VU.scanr
-- |'VU.scanr''
vu_scanr' = VU.scanr'
-- |'VU.scanr1'
vu_scanr1 = VU.scanr1
-- |'VU.scanr1''
vu_scanr1' = VU.scanr1'
-- |'VU.toList'
vu_toList = VU.toList
-- |'VU.fromList'
vu_fromList = VU.fromList
-- |'VU.fromListN'
vu_fromListN = VU.fromListN
-- |'VU.convert'
vu_convert = VU.convert
-- |'VU.freeze'
vu_freeze = VU.freeze
-- |'VU.thaw'
vu_thaw = VU.thaw
-- |'VU.copy'
vu_copy = VU.copy
-- |'VU.unsafeFreeze'
vu_unsafeFreeze = VU.unsafeFreeze
-- |'VU.unsafeThaw'
vu_unsafeThaw = VU.unsafeThaw
-- |'VU.unsafeCopy'
vu_unsafeCopy = VU.unsafeCopy

-- Data.Vector.Unboxed.Mutable
-- |'VUM.IOVector'
type UIOVector = UMVector RealWorld
-- |'VUM.STVector'
type USTVector s = UMVector s
-- |'VUM.length'
vum_length = VUM.length
-- |'VUM.null'
vum_null = VUM.null
-- |'VUM.slice'
vum_slice = VUM.slice
-- |'VUM.init'
vum_init = VUM.init
-- |'VUM.tail'
vum_tail = VUM.tail
-- |'VUM.take'
vum_take = VUM.take
-- |'VUM.drop'
vum_drop = VUM.drop
-- |'VUM.splitAt'
vum_splitAt = VUM.splitAt
-- |'VUM.unsafeSlice'
vum_unsafeSlice = VUM.unsafeSlice
-- |'VUM.unsafeInit'
vum_unsafeInit = VUM.unsafeInit
-- |'VUM.unsafeTail'
vum_unsafeTail = VUM.unsafeTail
-- |'VUM.unsafeTake'
vum_unsafeTake = VUM.unsafeTake
-- |'VUM.unsafeDrop'
vum_unsafeDrop = VUM.unsafeDrop
-- |'VUM.overlaps'
vum_overlaps = VUM.overlaps
-- |'VUM.new'
vum_new = VUM.new
-- |'VUM.unsafeNew'
vum_unsafeNew = VUM.unsafeNew
-- |'VUM.replicate'
vum_replicate = VUM.replicate
-- |'VUM.replicateM'
vum_replicateM = VUM.replicateM
-- |'VUM.clone'
vum_clone = VUM.clone
-- |'VUM.grow'
vum_grow = VUM.grow
-- |'VUM.unsafeGrow'
vum_unsafeGrow = VUM.unsafeGrow
-- |'VUM.clear'
vum_clear = VUM.clear
-- |'VUM.zip'
vum_zip = VUM.zip
-- |'VUM.zip3'
vum_zip3 = VUM.zip3
-- |'VUM.zip4'
vum_zip4 = VUM.zip4
-- |'VUM.zip5'
vum_zip5 = VUM.zip5
-- |'VUM.zip6'
vum_zip6 = VUM.zip6
-- |'VUM.unzip'
vum_unzip = VUM.unzip
-- |'VUM.unzip3'
vum_unzip3 = VUM.unzip3
-- |'VUM.unzip4'
vum_unzip4 = VUM.unzip4
-- |'VUM.unzip5'
vum_unzip5 = VUM.unzip5
-- |'VUM.unzip6'
vum_unzip6 = VUM.unzip6
-- |'VUM.read'
vum_read = VUM.read
-- |'VUM.write'
vum_write = VUM.write
-- |'VUM.swap'
vum_swap = VUM.swap
-- |'VUM.unsafeRead'
vum_unsafeRead = VUM.unsafeRead
-- |'VUM.unsafeWrite'
vum_unsafeWrite = VUM.unsafeWrite
-- |'VUM.unsafeSwap'
vum_unsafeSwap = VUM.unsafeSwap
-- |'VUM.set'
vum_set = VUM.set
-- |'VUM.copy'
vum_copy = VUM.copy
-- |'VUM.move'
vum_move = VUM.move
-- |'VUM.unsafeCopy'
vum_unsafeCopy = VUM.unsafeCopy
-- |'VUM.unsafeMove'
vum_unsafeMove = VUM.unsafeMove

