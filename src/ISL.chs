{-# LANGUAGE ForeignFunctionInterface #-}
module ISL where

#include <isl/ctx.h>
#include <isl/constraint.h>
#include <isl/map.h>
#include <isl/set.h>
#include <isl/space.h>

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C

-- import System.IO.Unsafe
-- import Control.Monad
-- import Control.Applicative ((<$>))

-- type Ctx = {#type isl_ctx #}
data Ctx
type PtrCtx = Ptr Ctx
{#pointer *isl_ctx as PtrCtx -> Ctx nocode #}

{#fun isl_ctx_alloc as ctxAlloc {} -> `Ptr Ctx' id #}
{#fun isl_ctx_free  as ctxFree { id `Ptr Ctx' } -> `()' #}

-- type Map = {#type isl_map #}
data Map
type PtrMap = Ptr Map
{#pointer *isl_map as PtrMap -> Map nocode #}

-- type Set = {#type isl_set #}
data Set
type PtrSet = Ptr Set
{#pointer *isl_set as PtrSet -> Set nocode #}

data BasicSet
type PtrBasicSet = Ptr BasicSet
{#pointer *isl_basic_set as PtrBasicSet -> BasicSet nocode #}

data LocalSpace
type PtrLocalSpace = Ptr LocalSpace
{#pointer *isl_local_space as PtrLocalSpace -> LocalSpace nocode #}

data Space
type PtrSpace = Ptr Space
{#pointer *isl_space as PtrSpace -> Space nocode #}

data Constraint
type PtrConstraint = Ptr Constraint
{#pointer *isl_constraint as PtrConstraint -> Constraint nocode #}

{#fun isl_map_free as mapFree { id `Ptr Map' } -> `()' #}
{#fun isl_set_free as setFree { id `Ptr Set' } -> `()' #}
{#fun isl_set_read_from_str as setReadFromStr
  { id `Ptr Ctx'
  , `String'
  } -> `Ptr Set' id #}
{#fun isl_map_read_from_str as mapReadFromStr
  { id `Ptr Ctx'
  , `String'
  } -> `Ptr Map' id #}

{#fun isl_map_is_equal as mapIsEqual
  { id `Ptr Map'
  , id `Ptr Map'
  } -> `Bool' #}

{#fun isl_map_power as mapPower
  { id `Ptr Map'
  , alloca- `CInt' peek*
  } -> `Ptr Map' id #}

{#enum isl_dim_type as DimType {underscoreToCase} #}

fromDimType :: DimType -> CInt
fromDimType = fromIntegral . fromEnum

{#fun isl_map_add_dims as mapAddDims
  { id `Ptr Map'
  , fromDimType `DimType'
  , id `CUInt'
  } -> `Ptr Map' id #}

{#fun isl_map_project_out as mapProjectOut
  { id `Ptr Map'
  , fromDimType `DimType'
  , id `CUInt'
  , id `CUInt'
  } -> `Ptr Map' id #}

{#fun isl_map_transitive_closure as mapTransitiveClosure
  { id `Ptr Map'
  , alloca- `CInt' peek*
  } -> `Ptr Map' id #}

{#fun isl_map_intersect_domain as mapIntersectDomain
  { id `Ptr Map'
  , id `Ptr Set'
  } -> `Ptr Map' id #}

{#fun isl_map_intersect_range as mapIntersectRange
  { id `Ptr Map'
  , id `Ptr Set'
  } -> `Ptr Map' id #}

{#fun isl_map_union as mapUnion
  { id `Ptr Map'
  , id `Ptr Map'
  } -> `Ptr Map' id #}

{#fun isl_set_intersect as setIntersect
  { id `Ptr Set'
  , id `Ptr Set'
  } -> `Ptr Set' id #}

{#fun isl_set_union as setUnion
  { id `Ptr Set'
  , id `Ptr Set'
  } -> `Ptr Set' id #}

{#fun isl_constraint_alloc_equality as constraintAllocEquality
  { id `Ptr LocalSpace'
  } -> `Ptr Constraint' id #}

{#fun isl_constraint_set_coefficient_si as constraintSetCoefficientSi
  { id `Ptr Constraint'
  , fromDimType `DimType'
  , id `CInt'
  , id `CInt'
  } -> `Ptr Constraint' id #}

{#fun isl_constraint_set_constant_si as constraintSetConstantSi
  { id `Ptr Constraint'
  , id `CInt'
  } -> `Ptr Constraint' id #}

{#fun isl_basic_set_add_constraint as basicSetAddConstraint
  { id `Ptr BasicSet'
  , id `Ptr Constraint'
  } -> `Ptr BasicSet' id #}

{#fun isl_space_set_alloc as spaceSetAlloc
  { id `Ptr Ctx'
  , id `CUInt'
  , id `CUInt'
  } -> `Ptr Space' id #}

{#fun isl_basic_set_universe as basicSetUniverse
  { id `Ptr Space'
  } -> `Ptr BasicSet' id #}

{#fun isl_local_space_from_space as localSpaceFromSpace
  { id `Ptr Space'
  } -> `Ptr LocalSpace' id #}

{#fun isl_local_space_copy as localSpaceCopy
  { id `Ptr LocalSpace'
  } -> `Ptr LocalSpace' id #}

{#fun isl_space_copy as spaceCopy
  { id `Ptr Space'
  } -> `Ptr Space' id #}

{#fun isl_basic_set_project_out as basicSetProjectOut
  { id `Ptr BasicSet'
  , fromDimType `DimType'
  , id `CUInt'
  , id `CUInt'
  } -> `Ptr BasicSet' id #}

{#fun isl_basic_set_read_from_str as basicSetReadFromStr
  { id `Ptr Ctx'
  , `String'
  } -> `Ptr BasicSet' id #}

{#fun isl_basic_set_to_str as basicSetToStr
  { id `Ptr BasicSet'
  } -> `String' #}
