{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module InlineBindings
  ( test1
  , test2
  , setIntersect
  , setUnion
  , setSubtract
  , setCopy
  , setEmpty
  , setUniverse
  , setGetSpace
  , setFree
  , setNBasicSet
  , setCoalesce
  , setParams
  , setComplement
  , setGetDimId
  , setProjectOut
  ) where

import Foreign.Ptr
import Foreign.C
import qualified Language.C.Inline as C
import ISL (Map, Ctx, Set, BasicSet, LocalSpace, Space, Constraint, DimType,
  Id, ctxAlloc, mapFree)
import IslCtx (islCtx)

C.context islCtx

C.include "<math.h>"
C.include "<isl/ctx.h>"
C.include "<isl/constraint.h>"
C.include "<isl/map.h>"
C.include "<isl/set.h>"
C.include "<isl/space.h>"

example1 :: String
example1 = unlines
  [ "[n] -> { [i,j] -> [i2,j2] : i2 = i + 1 and j2 = j + 1 and "
  , "1 <= i and i < n and 1 <= j and j < n or "
  , "i2 = i + 1 and j2 = j - 1 and "
  , "1 <= i and i < n and 2 <= j and j <= n }"
  ]

test1 :: IO ()
test1 = withCString example1 $ \example1' -> do
  ctx <- ctxAlloc
  exact <- [C.block| int {
    int exact;
    isl_map *map;

    map = isl_map_read_from_str($(isl_ctx* ctx), $(char* example1'));
    map = isl_map_power(map, &exact);
    isl_map_free(map);
    return exact;
    } |]

  print exact

test2 :: IO ()
test2 = withCString example1 $ \example1' -> do
  ctx <- ctxAlloc
  m <- [C.block| isl_map* {
    isl_map *map;

    map = isl_map_read_from_str($(isl_ctx* ctx), $(char* example1'));
    // isl_map_free(map);
    return map;
    } |]
  mapFree m
  print m

setIntersect :: Ptr Set -> Ptr Set -> Ptr Set
setIntersect set1 set2 = [C.pure| isl_set* {
  isl_set_intersect($(isl_set* set1), $(isl_set* set2))
  } |]

setUnion :: Ptr Set -> Ptr Set -> Ptr Set
setUnion set1 set2 = [C.pure| isl_set* {
  isl_set_union($(isl_set* set1), $(isl_set* set2))
  } |]

setSubtract :: Ptr Set -> Ptr Set -> Ptr Set
setSubtract set1 set2 = [C.pure| isl_set* {
  isl_set_subtract($(isl_set* set1), $(isl_set* set2))
  } |]

setCopy :: Ptr Set -> Ptr Set
setCopy set = [C.pure| isl_set* { isl_set_copy($(isl_set* set)) } |]

-- | Create an empty set
setEmpty :: Ptr Space -> Ptr Set
setEmpty space = [C.pure| isl_set* { isl_set_empty($(isl_space* space)) } |]

-- | Create a universe set
setUniverse :: Ptr Space -> Ptr Set
setUniverse space = [C.pure| isl_set* {
  isl_set_universe($(isl_space* space))
  } |]

-- | It is often useful to create objects that live in the same space as some
-- other object. This can be accomplished by creating the new objects based on
-- the space of the original object.
setGetSpace :: Ptr Set -> Ptr Space
setGetSpace set = [C.pure| isl_space* {
  isl_set_get_space($(isl_set* set))
  } |]

setFree :: Ptr Set -> IO ()
setFree set = do
  _ <- [C.block| isl_set* { isl_set_free($(isl_set* set)); } |]
  pure ()

-- | The number of basic sets in a set can be obtained
setNBasicSet :: Ptr Set -> CInt
setNBasicSet set = [C.pure| int { isl_set_n_basic_set($(isl_set* set)) } |]

-- | Simplify the representation of a set by trying to combine pairs of basic
-- sets into a single basic set.
setCoalesce :: Ptr Set -> Ptr Set
setCoalesce set = [C.pure| isl_set* { isl_set_coalesce($(isl_set* set)) } |]

-- | Projection
setParams :: Ptr Set -> Ptr Set
setParams set = [C.pure| isl_set* { isl_set_params($(isl_set* set)) } |]

-- | Projection
setComplement :: Ptr Set -> Ptr Set
setComplement set = [C.pure| isl_set* { isl_set_complement($(isl_set* set)) } |]

setGetDimId :: Ptr Set -> DimType -> CUInt -> Ptr Id
setGetDimId set ty pos =
  let ty' :: CUInt
      ty' = toEnum $ fromEnum ty
  in [C.pure| isl_id* {
     isl_set_get_dim_id(
       $(isl_set* set),
       isl_dim_type($(unsigned int ty')),
       $(unsigned int pos)
     )
     } |]

setProjectOut :: Ptr Set -> DimType -> CUInt -> CUInt -> Ptr Set
setProjectOut set ty first n =
  let ty' :: CUInt
      ty' = toEnum $ fromEnum ty
  in [C.pure| isl_set* {
     isl_set_project_out(
       $(isl_set* set),
       isl_dim_type($(unsigned int ty')),
       $(unsigned int first),
       $(unsigned int n)
     )
     } |]
