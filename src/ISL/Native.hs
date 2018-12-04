{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An inline-c based low-level interface to isl.
module ISL.Native
  ( Once
  , Take
  , Give
  , Keep
  , IslCopy(copy)
  , IslFree(free)

  , ctxFree

  , basicSetCopy
  , basicSetFree

  , unsafeSetIntersect
  , setIntersect
  , unsafeSetUnion
  , setUnion
  , unsafeSetSubtract
  , setSubtract
  , setCopy
  , setEmpty
  , unsafeSetUniverse
  , setUniverse
  , setGetSpace
  , setFree
  , setNBasicSet
  , unsafeSetCoalesce
  , setCoalesce
  , unsafeSetParams
  , setParams
  , setComplement
  , setGetDimId
  , unsafeSetProjectOut
  , setProjectOut

  , basicMapCopy
  , basicMapFree

  , mapCopy
  , mapFree

  , localSpaceCopy
  , localSpaceFree

  , spaceCopy
  , spaceFree
  , spaceIsEqual
  , spaceHasEqualParams
  , spaceHasEqualTuples
  , spaceIsDomain
  , spaceIsRange
  , spaceTupleIsEqual

  , constraintCopy
  , constraintFree

  , idCopy
  , idFree

  -- * binary operations
  , unsafeLocalSpaceIntersect
  , localSpaceIntersect
  , unsafeBasicSetIntersectParams
  , basicSetIntersectParams
  , unsafeBasicSetIntersect
  , basicSetIntersect
  , unsafeBasicSetListIntersect
  , unsafeSetIntersectParams
  , setIntersectParams
  ) where

import Control.Monad (void)
import Foreign.Ptr
import Foreign.C
import qualified Language.C.Inline as C

import ISL.Native.Context (islCtx)
import ISL.Native.Types

C.context islCtx

C.include "<math.h>"
C.include "<isl/ctx.h>"
C.include "<isl/constraint.h>"
C.include "<isl/id.h>"
C.include "<isl/map.h>"
C.include "<isl/set.h>"
C.include "<isl/space.h>"

-- | A pointer that is only allowed to be used once
newtype Once a = Once (Ptr a)

-- | A pointer that isl is free to destroy
type Take a = Once a

-- | A pointer given as the result of a computation
type Give a = Ptr a

-- | A pointer used as input to a computation and not destroyed
type Keep a = Ptr a

copy' :: IslCopy a => Keep a -> Once a
copy' = Once . copy


class IslCopy a where
  copy :: Keep a -> Give a

class IslFree a where
  free :: Take a -> IO ()

-- __isl_take: can no longer be used
-- __isl_keep: only used temporarily

-- * Ctx

instance IslFree Ctx where free = ctxFree

ctxFree :: Take Ctx -> IO ()
ctxFree (Once ctx) = [C.block| void { isl_ctx_free($(isl_ctx* ctx)); } |]

-- * BasicSet

instance IslCopy BasicSet where copy = basicSetCopy
instance IslFree BasicSet where free = basicSetFree

basicSetCopy :: Keep BasicSet -> Give BasicSet
basicSetCopy bset =
  [C.pure| isl_basic_set* { isl_basic_set_copy($(isl_basic_set* bset)) } |]

basicSetFree :: Take BasicSet -> IO ()
basicSetFree (Once bset) = void
  [C.block| isl_basic_set* { isl_basic_set_free($(isl_basic_set* bset)); } |]

-- * Set

instance IslCopy Set where copy = setCopy
instance IslFree Set where free = setFree

setCopy :: Keep Set -> Give Set
setCopy set = [C.pure| isl_set* { isl_set_copy($(isl_set* set)) } |]

setFree :: Take Set -> IO ()
setFree (Once set) = void [C.block| isl_set* { isl_set_free($(isl_set* set)); } |]

unsafeSetUnion :: Take Set -> Take Set -> Give Set
unsafeSetUnion (Once set1) (Once set2) = [C.pure| isl_set* {
  isl_set_union($(isl_set* set1), $(isl_set* set2))
  } |]

setUnion :: Keep Set -> Keep Set -> Give Set
setUnion set1 set2 = unsafeSetUnion (copy' set1) (copy' set2)

unsafeSetSubtract :: Take Set -> Take Set -> Give Set
unsafeSetSubtract (Once set1) (Once set2) = [C.pure| isl_set* {
  isl_set_subtract($(isl_set* set1), $(isl_set* set2))
  } |]

setSubtract :: Keep Set -> Keep Set -> Give Set
setSubtract set1 set2 = unsafeSetSubtract (copy' set1) (copy' set2)

-- | Create an empty set
setEmpty :: Take Space -> Give Set
setEmpty (Once space) = [C.pure| isl_set* { isl_set_empty($(isl_space* space)) } |]

-- | Create a universe set
unsafeSetUniverse :: Take Space -> Give Set
unsafeSetUniverse (Once space) = [C.pure| isl_set* {
  isl_set_universe($(isl_space* space))
  } |]

setUniverse :: Keep Space -> Give Set
setUniverse = unsafeSetUniverse . copy'

-- | It is often useful to create objects that live in the same space as some
-- other object. This can be accomplished by creating the new objects based on
-- the space of the original object.
setGetSpace :: Keep Set -> Give Space
setGetSpace set = [C.pure| isl_space* {
  isl_set_get_space($(isl_set* set))
  } |]

-- | The number of basic sets in a set can be obtained
setNBasicSet :: Keep Set -> CInt
setNBasicSet set = [C.pure| int { isl_set_n_basic_set($(isl_set* set)) } |]

unsafeSetCoalesce :: Take Set -> Give Set
unsafeSetCoalesce (Once set) =
  [C.pure| isl_set* { isl_set_coalesce($(isl_set* set)) } |]

-- | Simplify the representation of a set by trying to combine pairs of basic
-- sets into a single basic set.
setCoalesce :: Keep Set -> Give Set
setCoalesce = unsafeSetCoalesce . copy'

-- | Projection
unsafeSetParams :: Take Set -> Give Set
unsafeSetParams (Once set)
  = [C.pure| isl_set* { isl_set_params($(isl_set* set)) } |]

setParams :: Keep Set -> Give Set
setParams = unsafeSetParams . copy'

unsafeSetComplement :: Take Set -> Give Set
unsafeSetComplement (Once set) =
  [C.pure| isl_set* { isl_set_complement($(isl_set* set)) } |]

-- | Projection
setComplement :: Keep Set -> Give Set
setComplement = unsafeSetComplement . copy'

setGetDimId :: Keep Set -> DimType -> CUInt -> Give Id
setGetDimId set ty pos =
  let ty' :: CInt
      ty' = fromDimType ty
  in [C.pure| isl_id* {
     isl_set_get_dim_id(
       $(isl_set* set),
       $(int ty'),
       $(unsigned int pos)
     )
     } |]

unsafeSetProjectOut :: Take Set -> DimType -> CUInt -> CUInt -> Give Set
unsafeSetProjectOut (Once set) ty first n =
  let ty' :: CInt
      ty' = fromDimType ty
  in [C.pure| isl_set* {
     isl_set_project_out(
       $(isl_set* set),
       $(int ty'),
       $(unsigned int first),
       $(unsigned int n)
     )
     } |]

setProjectOut :: Keep Set -> DimType -> CUInt -> CUInt -> Give Set
setProjectOut set ty first n = unsafeSetProjectOut (copy' set) ty first n

-- * BasicMap

instance IslCopy BasicMap where copy = basicMapCopy
instance IslFree BasicMap where free = basicMapFree

basicMapCopy :: Keep BasicMap -> Give BasicMap
basicMapCopy bmap =
  [C.pure| isl_basic_map* { isl_basic_map_copy($(isl_basic_map* bmap)) } |]

basicMapFree :: Take BasicMap -> IO ()
basicMapFree (Once bmap) = void
  [C.block| isl_basic_map* { isl_basic_map_free($(isl_basic_map* bmap)); } |]

-- * Map

instance IslCopy Map where copy = mapCopy
instance IslFree Map where free = mapFree

mapCopy :: Keep Map -> Give Map
mapCopy map = [C.pure| isl_map* { isl_map_copy($(isl_map* map)) } |]

mapFree :: Take Map -> IO ()
mapFree (Once map) = void [C.block| isl_map* { isl_map_free($(isl_map* map)); } |]

-- * LocalSpace

instance IslCopy LocalSpace where copy = localSpaceCopy
instance IslFree LocalSpace where free = localSpaceFree

localSpaceCopy :: Keep LocalSpace -> Give LocalSpace
localSpaceCopy ls =
  [C.pure| isl_local_space* { isl_local_space_copy($(isl_local_space* ls)) } |]

localSpaceFree :: Take LocalSpace -> IO ()
localSpaceFree (Once ls) = void
  [C.block| isl_local_space* { isl_local_space_free($(isl_local_space* ls)); } |]

-- * Space

instance IslCopy Space where copy = spaceCopy
instance IslFree Space where free = spaceFree

spaceCopy :: Keep Space -> Give Space
spaceCopy space =
  [C.pure| isl_space* { isl_space_copy($(isl_space* space)) } |]

spaceFree :: Take Space -> IO ()
spaceFree (Once space) = void
  [C.block| isl_space* { isl_space_free($(isl_space* space)); } |]

spaceIsEqual :: Keep Space -> Keep Space -> Bool
spaceIsEqual space1 space2 =
  [C.pure| isl_bool {
    isl_space_is_equal($(isl_space* space1), $(isl_space* space2)); }
  |]

spaceHasEqualParams :: Keep Space -> Keep Space -> Bool
spaceHasEqualParams space1 space2 =
  [C.pure| isl_bool {
    isl_space_has_equal_params($(isl_space* space1), $(isl_space* space2)); }
  |]

spaceHasEqualTuples :: Keep Space -> Keep Space -> Bool
spaceHasEqualTuples space1 space2 =
  [C.pure| isl_bool {
    isl_space_has_equal_tuples($(isl_space* space1), $(isl_space* space2)); }
  |]

spaceIsDomain :: Keep Space -> Keep Space -> Bool
spaceIsDomain space1 space2 =
  [C.pure| isl_bool {
    isl_space_is_domain($(isl_space* space1), $(isl_space* space2)); }
  |]

spaceIsRange :: Keep Space -> Keep Space -> Bool
spaceIsRange space1 space2 =
  [C.pure| isl_bool {
    isl_space_is_range($(isl_space* space1), $(isl_space* space2)); }
  |]

spaceTupleIsEqual :: Keep Space -> DimType -> Keep Space -> DimType -> Bool
spaceTupleIsEqual space1 type1 space2 type2 =
  let type1' :: CInt
      type1' = fromDimType type1
      type2' :: CInt
      type2' = fromDimType type2
  in [C.pure| isl_bool {
       isl_space_tuple_is_equal(
         $(isl_space* space1),
         $(int type1'),
         $(isl_space* space2),
         $(int type2')
       )
       } |]

-- * Constraint

instance IslCopy Constraint where copy = constraintCopy
instance IslFree Constraint where free = constraintFree

constraintCopy :: Keep Constraint -> Give Constraint
constraintCopy c =
  [C.pure| isl_constraint* { isl_constraint_copy($(isl_constraint* c)) } |]

constraintFree :: Take Constraint -> IO ()
constraintFree (Once c) = void
  [C.block| isl_constraint* { isl_constraint_free($(isl_constraint* c)); } |]

-- * Id

instance IslCopy Id where copy = idCopy
instance IslFree Id where free = idFree

idCopy :: Keep Id -> Give Id
idCopy i = [C.pure| isl_id* { isl_id_copy($(isl_id* i)) } |]

idFree :: Take Id -> IO ()
idFree (Once i) = void [C.block| isl_id* { isl_id_free($(isl_id* i)); } |]

-- binary operations

unsafeLocalSpaceIntersect
  :: Take LocalSpace -> Take LocalSpace -> Give LocalSpace
unsafeLocalSpaceIntersect (Once ls1) (Once ls2) = [C.pure| isl_local_space* {
  isl_local_space_intersect(
    $(isl_local_space* ls1),
    $(isl_local_space* ls2)
  )
  } |]

localSpaceIntersect :: Keep LocalSpace -> Keep LocalSpace -> Give LocalSpace
localSpaceIntersect ls1 ls2 = unsafeLocalSpaceIntersect (copy' ls1) (copy' ls2)

unsafeBasicSetIntersectParams
  :: Keep BasicSet -> Keep BasicSet -> Give BasicSet
unsafeBasicSetIntersectParams bset1 bset2 = [C.pure| isl_basic_set* {
  isl_basic_set_intersect_params(
    $(isl_basic_set* bset1),
    $(isl_basic_set* bset2)
  )
  } |]

basicSetIntersectParams :: Keep BasicSet -> Keep BasicSet -> Give BasicSet
basicSetIntersectParams bset1 bset2
  = unsafeBasicSetIntersectParams (basicSetCopy bset1) (basicSetCopy bset2)

unsafeBasicSetIntersect :: Keep BasicSet -> Keep BasicSet -> Give BasicSet
unsafeBasicSetIntersect bset1 bset2 = [C.pure| isl_basic_set* {
  isl_basic_set_intersect(
    $(isl_basic_set* bset1),
    $(isl_basic_set* bset2)
  )
  } |]

basicSetIntersect :: Keep BasicSet -> Keep BasicSet -> Give BasicSet
basicSetIntersect bset1 bset2
  = unsafeBasicSetIntersect (basicSetCopy bset1) (basicSetCopy bset2)

unsafeBasicSetListIntersect :: Take BasicSetList -> Give BasicSet
unsafeBasicSetListIntersect (Once list) =
  [C.pure| isl_basic_set* {
    isl_basic_set_intersect($(isl_basic_set_list* list))
  } |]

-- there's no way to copy an isl_basic_set_list
-- basicSetListIntersect :: Ptr BasicSetList -> Ptr BasicSet
-- basicSetListIntersect = undefined

unsafeSetIntersectParams :: Take Set -> Take Set -> Give Set
unsafeSetIntersectParams (Once set) (Once params) = [C.pure| isl_set* {
  isl_set_intersect_params( $(isl_set* set), $(isl_set* params))
  } |]

setIntersectParams :: Keep Set -> Keep Set -> Keep Set
setIntersectParams set params
  = unsafeSetIntersectParams (copy' set) (copy' params)

unsafeSetIntersect :: Take Set -> Take Set -> Give Set
unsafeSetIntersect (Once set1) (Once set2) = [C.pure| isl_set* {
  isl_set_intersect($(isl_set* set1), $(isl_set* set2))
  } |]

setIntersect :: Keep Set -> Keep Set -> Give Set
setIntersect set1 set2 = unsafeSetIntersect (copy' set1) (copy' set2)


-- pwAffLeSet :: Ptr

-- __isl_give isl_set *isl_pw_aff_le_set(
--                 __isl_take isl_pw_aff *pwaff1,
--                 __isl_take isl_pw_aff *pwaff2);
--         __isl_give isl_set *isl_pw_aff_lt_set(
--                 __isl_take isl_pw_aff *pwaff1,
--                 __isl_take isl_pw_aff *pwaff2);
