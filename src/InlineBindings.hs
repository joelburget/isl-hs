{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module InlineBindings (test1, test2) where

import Foreign.C
import Foreign.C.String
import qualified Language.C.Inline as C
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import ISL (Map, Ctx, Set, BasicSet, LocalSpace, Space, Constraint, ctxAlloc, mapFree)
import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH
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
  map <- [C.block| isl_map* {
    isl_map *map;

    map = isl_map_read_from_str($(isl_ctx* ctx), $(char* example1'));
    // isl_map_free(map);
    return map;
    } |]
  mapFree map
  print map

doDouble :: IO ()
doDouble = do
  x <- [C.exp| double{ cos(1) } |]
  print x

cos1 :: CDouble
cos1 = [C.pure| double { cos(1) } |]
