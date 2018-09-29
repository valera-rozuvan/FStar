module LowStar.Regional.Instances

open FStar.All
open FStar.Integers
open LowStar.Buffer
open LowStar.Regional
open LowStar.RVector

module HH = FStar.Monotonic.HyperHeap
module HS = FStar.HyperStack
module HST = FStar.HyperStack.ST
module MHS = FStar.Monotonic.HyperStack
module S = FStar.Seq
module B = LowStar.Buffer
module V = LowStar.Vector
module RV = LowStar.RVector

/// `LowStar.Buffer` is regional

val buffer_region_of:
  #a:Type -> v:B.buffer a -> GTot HH.rid
let buffer_region_of #a v =
  B.frameOf v

val buffer_dummy: a:Type -> Tot (B.buffer a)
let buffer_dummy _ = B.null

val buffer_repr: a:Type0 -> Type0
let buffer_repr a = S.seq a

val buffer_r_repr:
  #a:Type -> h:HS.mem -> v:B.buffer a -> GTot (buffer_repr a)
let buffer_r_repr #a h v = B.as_seq h v

val buffer_r_inv:
  #a:Type -> len:UInt32.t{len > 0ul} ->
  h:HS.mem -> v:B.buffer a -> GTot Type0
let buffer_r_inv #a len h v =
  B.live h v /\ B.freeable v /\ 
  B.len v = len

val buffer_r_inv_reg:
  #a:Type -> len:UInt32.t{len > 0ul} ->
  h:HS.mem -> v:B.buffer a ->
  Lemma (requires (buffer_r_inv len h v))
	(ensures (MHS.live_region h (buffer_region_of v)))
let buffer_r_inv_reg #a len h v = ()

val buffer_r_sep:
  #a:Type -> len:UInt32.t{len > 0ul} ->
  v:B.buffer a -> p:loc -> h0:HS.mem -> h1:HS.mem ->
  Lemma (requires (buffer_r_inv len h0 v /\
		  loc_disjoint 
		    (loc_all_regions_from false 
		      (buffer_region_of v)) p /\
		  modifies p h0 h1))
	(ensures (buffer_r_inv len h1 v /\
		 buffer_r_repr h0 v == buffer_r_repr h1 v))
let buffer_r_sep #a len v p h0 h1 =
  assert (loc_includes (loc_all_regions_from false (buffer_region_of v))
		       (loc_buffer v));
  B.modifies_buffer_elim v p h0 h1

val buffer_irepr: 
  #a:Type0 -> ia:a -> len:UInt32.t{len > 0ul} ->
  Ghost.erased (buffer_repr a)
let buffer_irepr #a ia len =
  Ghost.hide (S.create (UInt32.v len) ia)

val buffer_r_init_p:
  #a:Type0 -> v:B.buffer a -> GTot Type0
let buffer_r_init_p #a v =
  True

val buffer_r_init:
  #a:Type -> ia:a -> len:UInt32.t{len > 0ul} -> r:erid ->
  HST.ST (b:B.buffer a)
    (requires (fun h0 -> true))
    (ensures (fun h0 v h1 -> 
      Set.subset (Map.domain (MHS.get_hmap h0))
                 (Map.domain (MHS.get_hmap h1)) /\
      modifies loc_none h0 h1 /\
      buffer_r_init_p v /\
      buffer_r_inv len h1 v /\
      buffer_region_of v = r /\
      buffer_r_repr h1 v == Ghost.reveal (buffer_irepr ia len)))
let buffer_r_init #a ia len r =
  B.malloc r ia len

val buffer_r_free:
  #a:Type -> len:UInt32.t{len > 0ul} ->
  v:B.buffer a ->
  HST.ST unit
    (requires (fun h0 -> buffer_r_inv len h0 v))
    (ensures (fun h0 _ h1 ->
      modifies (loc_all_regions_from false (buffer_region_of v)) h0 h1))
let buffer_r_free #a len v =
  B.free v

val buffer_copy:
  #a:Type -> len:UInt32.t{len > 0ul} -> 
  src:B.buffer a -> dst:B.buffer a ->
  HST.ST unit
    (requires (fun h0 ->
      buffer_r_inv len h0 src /\ buffer_r_inv len h0 dst /\
      HH.disjoint (buffer_region_of src) (buffer_region_of dst)))
    (ensures (fun h0 _ h1 ->
      modifies (loc_all_regions_from false (buffer_region_of dst)) h0 h1 /\
      buffer_r_inv len h1 dst /\
      buffer_r_repr h1 dst == buffer_r_repr h0 src))
let buffer_copy #a len src dst =
  B.blit src 0ul dst 0ul len

val buffer_regional:
  #a:Type -> ia:a -> len:UInt32.t{len > 0ul} ->
  regional (B.buffer a)
let buffer_regional #a ia len =
  Rgl (buffer_region_of #a)
      (buffer_dummy a)
      (buffer_repr a)
      (buffer_r_repr #a)
      (buffer_r_inv #a len)
      (buffer_r_inv_reg #a len)
      (buffer_r_sep #a len)
      (buffer_irepr #a ia len)
      (buffer_r_init_p #a)
      (buffer_r_init #a ia len)
      (buffer_r_free #a len)

val buffer_copyable:
  #a:Type -> ia:a -> len:UInt32.t{len > 0ul} ->
  copyable (B.buffer a) (buffer_regional ia len)
let buffer_copyable #a ia len =
  Cpy (buffer_copy len)

/// If `a` is regional, then `vector a` is also regional

val vector_region_of: 
  #a:Type -> #rg:regional a -> v:rvector rg -> GTot HH.rid
let vector_region_of #a #rg v = V.frameOf v

val vector_dummy:
  #a:Type -> rg:regional a -> Tot (rvector rg)
let vector_dummy #a rg = V.create_empty a

val vector_repr: #a:Type0 -> rg:regional a -> Tot Type0
let vector_repr #a rg = S.seq (Rgl?.repr rg)

val vector_r_repr:
  #a:Type -> #rg:regional a -> h:HS.mem -> v:rvector rg ->
  GTot (vector_repr rg)
let vector_r_repr #a #rg h v = RV.as_seq h v

val vector_r_inv:
  #a:Type -> #rg:regional a -> 
  h:HS.mem -> v:rvector rg -> GTot Type0
let vector_r_inv #a #rg h v = rv_inv h v

val vector_r_inv_reg:
  #a:Type -> #rg:regional a -> 
  h:HS.mem -> v:rvector rg ->
  Lemma (requires (vector_r_inv h v))
	(ensures (MHS.live_region h (vector_region_of v)))
let vector_r_inv_reg #a #rg h v = ()    

val vector_r_sep:
  #a:Type -> #rg:regional a ->
  v:rvector rg -> p:loc -> h0:HS.mem -> h1:HS.mem ->
  Lemma (requires (vector_r_inv h0 v /\
		  loc_disjoint 
		    (loc_all_regions_from false (vector_region_of v))
		    p /\
		  modifies p h0 h1))
	(ensures (vector_r_inv h1 v /\
		 vector_r_repr h0 v == vector_r_repr h1 v))
let vector_r_sep #a #rg v p h0 h1 =
  RV.rv_inv_preserved v p h0 h1;
  RV.as_seq_preserved v p h0 h1

val vector_irepr:
  #a:Type -> rg:regional a -> Ghost.erased (vector_repr rg)
let vector_irepr #a rg =
  Ghost.hide S.empty

val vector_r_init_p:
  #a:Type -> #rg:regional a -> v:rvector rg -> GTot Type0
let vector_r_init_p #a #rg v =
  V.size_of v = 0ul

val vector_r_init: 
  #a:Type -> #rg:regional a -> r:erid ->
  HST.ST (v:rvector rg)
    (requires (fun h0 -> true))
    (ensures (fun h0 v h1 -> 
      Set.subset (Map.domain (MHS.get_hmap h0))
                 (Map.domain (MHS.get_hmap h1)) /\
      modifies loc_none h0 h1 /\
      vector_r_init_p v /\
      vector_r_inv h1 v /\
      vector_region_of v = r /\
      vector_r_repr h1 v == Ghost.reveal (vector_irepr rg)))
let vector_r_init #a #rg r =
  let nrid = new_region_ r in
  let r_init = Rgl?.r_init rg in
  let ia = r_init nrid in
  V.create_reserve 1ul ia r

val vector_r_free:
  #a:Type -> #rg:regional a -> v:rvector rg ->
  HST.ST unit
    (requires (fun h0 -> vector_r_inv h0 v))
    (ensures (fun h0 _ h1 ->
      modifies (loc_all_regions_from false (vector_region_of v)) h0 h1))
let vector_r_free #a #rg v =
  RV.free v

val vector_regional: 
  #a:Type -> rg:regional a -> regional (rvector rg)
let vector_regional #a rg =
  Rgl (vector_region_of #a #rg)
      (vector_dummy #a rg)
      (vector_repr #a rg)
      (vector_r_repr #a #rg)
      (vector_r_inv #a #rg)
      (vector_r_inv_reg #a #rg)
      (vector_r_sep #a #rg)
      (vector_irepr #a rg)
      (vector_r_init_p #a #rg)
      (vector_r_init #a #rg)
      (vector_r_free #a #rg)

