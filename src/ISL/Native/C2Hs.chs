{-# LANGUAGE ForeignFunctionInterface #-}

-- | Prototype of a c2hs-based low-level interface. We will prefer a ISL.Native
-- for now, which uses inline-c instead.
module ISL.Native.C2Hs where

#include <isl/ctx.h>
#include <isl/constraint.h>
#include <isl/map.h>
#include <isl/set.h>
#include <isl/space.h>

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C

import ISL.Native.Types

type PtrCtx = Ptr Ctx
{#pointer *isl_ctx as PtrCtx -> Ctx nocode #}

{#fun isl_ctx_alloc as ctxAlloc {} -> `Ptr Ctx' id #}
{#fun isl_ctx_free  as ctxFree { id `Ptr Ctx' } -> `()' #}

type PtrMap = Ptr Map
{#pointer *isl_map as PtrMap -> Map nocode #}

type PtrSet = Ptr Set
{#pointer *isl_set as PtrSet -> Set nocode #}

type PtrBasicSet = Ptr BasicSet
{#pointer *isl_basic_set as PtrBasicSet -> BasicSet nocode #}

type PtrLocalSpace = Ptr LocalSpace
{#pointer *isl_local_space as PtrLocalSpace -> LocalSpace nocode #}

type PtrSpace = Ptr Space
{#pointer *isl_space as PtrSpace -> Space nocode #}

type PtrConstraint = Ptr Constraint
{#pointer *isl_constraint as PtrConstraint -> Constraint nocode #}

type PtrId = Ptr Id
{#pointer *isl_id as PtrId -> Id nocode #}

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
