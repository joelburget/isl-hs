{-# language TypeApplications #-}
{-# language DataKinds #-}
{-# language OverloadedStrings #-}
module Gen (gen) where

import Control.Lens
import Data.Foldable (for_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad

import Language.C.Clang
import Language.C.Clang.Cursor.Typed

files :: [ByteString]
files = ["isl_aff.c"]
  -- [ "aff.h"
  -- , "aff_type.h"
  -- , "arg.h"
  -- , "ast.h"
  -- , "ast_build.h"
  -- , "ast_type.h"
  -- , "constraint.h"
  -- , "ctx.h"
  -- , "fixed_box.h"
  -- , "flow.h"
  -- , "hash.h"
  -- , "hmap.h"
  -- -- , "hmap_templ.h"
  -- , "id.h"
  -- , "id_to_ast_expr.h"
  -- , "id_to_id.h"
  -- , "id_to_pw_aff.h"
  -- , "id_type.h"
  -- , "ilp.h"
  -- , "list.h"
  -- , "local_space.h"
  -- , "lp.h"
  -- , "map.h"
  -- , "map_to_basic_set.h"
  -- , "map_type.h"
  -- , "mat.h"
  -- , "maybe.h"
  -- , "maybe_ast_expr.h"
  -- , "maybe_basic_set.h"
  -- , "maybe_id.h"
  -- , "maybe_pw_aff.h"
  -- , "maybe_templ.h"
  -- , "multi.h"
  -- , "obj.h"
  -- , "options.h"
  -- , "point.h"
  -- , "polynomial.h"
  -- , "polynomial_type.h"
  -- , "printer.h"
  -- , "printer_type.h"
  -- , "schedule.h"
  -- , "schedule_node.h"
  -- , "schedule_type.h"
  -- , "set.h"
  -- , "set_type.h"
  -- , "space.h"
  -- , "space_type.h"
  -- , "stdint.h"
  -- , "stream.h"
  -- , "stride_info.h"
  -- , "union_map.h"
  -- , "union_map_type.h"
  -- , "union_set.h"
  -- , "union_set_type.h"
  -- , "val.h"
  -- , "val_gmp.h"
  -- , "val_type.h"
  -- , "vec.h"
  -- , "version.h"
  -- , "vertices.h"
  -- ]

gen :: IO ()
gen = for_ files $ \file -> do
  idx <- createIndex
  tu  <- parseTranslationUnit idx (BS.unpack $ "isl/" <> file) []

  let funDecs
        = cursorDescendantsF
        . folding (matchKind @'FunctionDecl)
        . filtered (isFromMainFile . rangeStart . cursorExtent)
        -- . to (\funDec -> (cursorSpelling funDec, typeKind (cursorType funDec)))
        . to (\funDec -> cursorSpelling funDec <> " :: " <> typeSpelling (cursorType funDec))

  BS.putStrLn $ "\n" <> file <> ":\n"
  BS.putStrLn $ BS.unlines $ translationUnitCursor tu ^.. funDecs

--   parse_result <- parseCFile (newGCC "gcc") Nothing ["-Iisl", "-Iisl/include/isl", "-Iheaders"] ("isl/" ++ file)
--   case parse_result of
--     Left parse_err -> error (show parse_err)
--     Right ast      -> return ast

-- printMyAST :: CTranslUnit -> IO ()
-- printMyAST ctu = (print . pretty) ctu
