\ maki/tensor-value.f - the unified single-slot tensor value + its plan mode.
\
\ docs/archive/cad-plan.md section 3 prerequisite. Today's eager maki ops pass a tensor as many
\ stack cells (maki/linear.f: LINEAR ( ptr r ptr r ptr r ptr r n n n -- ); data,
\ shape and dtype travel apart), so they cannot be re-typed onto descriptors. A
\ `tensor` is instead ONE stack slot: an opaque nominal handle indexing a
\ module-owned record table that carries data pointer, 2D shape (rows cols),
\ dtype, layout, address space, the alignment class recorded from the pointer, and
\ the view descriptor (storage-ref + offset + row/col strides; docs/strided-views.md
\ SV-1 - a contiguous tensor is the DEGENERATE view: storage-ref = self, offset 0,
\ natural strides, so the layout enum survives as a DERIVED contiguity classification).
\
\ Representation hiding follows maki/report.f: a `tensor` is an opaque handle;
\ every constructor/accessor takes or returns `tensor` plus typed field
\ values, so the record layout never leaks. Unlike report's single live object this
\ table is multi-instance: a fixed-capacity table with a free counter (TV-U);
\ TV-RESET bumps the store generation, so every outstanding handle from an
\ earlier generation fails closed (Model-CAD V2 R3: stale handles reject).
\
\ Plan mode (docs/archive/cad-plan.md section 3) is the descriptor-vocabulary base: PLINEAR/PGELU
\ do not compute, they append IR records (op-kind, input tensors, output tensor)
\ to a plan the fusion planner (cad-1) will consume. TV-LINEAR is the eager
\ interop proof: it unpacks tensor values into the cells the existing eager
\ LINEAR wants, runs it, and wraps the result back. v1 is the mechanism only - no
\ fusion, no cost model.
\
\ Facts are recorded, never assumed (docs/archive/cad-plan.md section 4.1): the alignment class is
\ measured from the real pointer at construction; dtype facts come from
\ maki/tensor.f's family; a descriptor with no buffer records AL-UNKNOWN, never a
\ guess. Fail closed: stale/forged handle, missing data, shape mismatch,
\ plan-builder misuse and every capacity are named throws, never defaulted.
\ maki owns -5000..-5099; tensor-value uses -5040..-5049 (loss.f owns -5030..-5039).

require lib/prelude.f
require maki/tensor.f
require maki/linear.f
require maki/op-kind.f
require lib/ffi-abi.f

-5040 constant E-TV-FULL        \ tensor store capacity exceeded
-5041 constant E-TV-HANDLE      \ stale, forged, or out-of-range tensor handle
\ -5042 was E-TV-LAYOUT (retired: the layout family makes an out-of-range tag a
\ checker reject); the slot is re-owned by the generation guard below.
-5042 constant E-TV-GEN         \ tensor-store generation exhausted
-5043 constant E-TV-NODATA      \ data requested from a descriptor (no buffer)
-5044 constant E-TV-SHAPE       \ shape mismatch (eager linear inner dim)
-5045 constant E-TV-PLAN-FULL   \ plan op or input-pool capacity exceeded
-5046 constant E-TV-PLAN-IDX    \ plan index out of range
-5047 constant E-TV-PLAN-STATE  \ plan builder used out of order
\ -5048 was E-TV-OPKIND (retired: PLAN-OP-BEGIN takes an `opkind` family, so an
\ out-of-range op-kind is a checker reject); the reserved slot is re-owned below.
-5048 constant E-TV-VIEW        \ view used where owning/contiguous storage required (docs/strided-views.md §2/§3); remedy = COPY
-5049 constant E-TV-ALIGN       \ AL-* code out of the align domain (>ALIGN parse boundary)

package MAKI
public

\ ---- layout tags (v1: contiguity order only; strides arrive with cad-1) ----
\ The `layout` ENUM is the semantic type carried through construction, storage,
\ and every consumer (dot habu-cad-adt-swap, corrected plan). LAY-* codes remain
\ ONLY as the wire/hash vocabulary at the named boundaries below. DERIVE eq
\ generates MAKI-LAYOUT:EQ ( layout layout -- bool ) (derive S1) so layout can be
\ an enum FIELD of the DERIVE-eq SKEY product (dot habu-cad-adt-swap); zero
\ behavior change. The layout role deliberately stays an ENUM rather than a
\ CAD-KIND nominal (merge policy habu-merge-policy-master-961bb2b7).
0 constant LAY-ROW              \ row-major / C-contiguous
1 constant LAY-COL             \ column-major
ENUM layout DERIVE eq
  row
  col
;ENUM

: LAYOUT>N ( layout -- n )             \ named render boundary (exhaustive)
   MATCH layout
      row OF LAY-ROW ENDOF
      col OF LAY-COL ENDOF
   ;MATCH ;

: LAY-KEY ( layout -- ptr u8 n )       \ wire text render (exhaustive)
   MATCH layout
      row OF s" row" ENDOF
      col OF s" col" ENDOF
   ;MATCH ;

: LAYOUT-ROW? ( layout -- bool )       \ contiguity predicate (replaces LAY-ROW =)
   MATCH layout
      row OF true  ENDOF
      col OF false ENDOF
   ;MATCH ;

\ ---- alignment classes (recorded from the pointer, never assumed) ----
\ The `align` ENUM is the semantic type; AL-* codes remain ONLY for the ordinal
\ min-fold + key render inside sched-key (alignment classes are intrinsically
\ ordered: AL-UNKNOWN < .. < AL-16, and ALIGN>N preserves that order) and for
\ wire/test assertions. Bare-digit variant tails reject, so a4/a8/a16. DERIVE eq
\ generates MAKI-ALIGN:EQ ( align align -- bool ) (derive S1) so align can be an
\ enum FIELD of the DERIVE-eq SKEY product (dot habu-cad-adt-swap); zero behavior
\ change. >ALIGN (below) lifts a validated AL-* code back to the family.
0 constant AL-UNKNOWN          \ conservative: not measured (descriptors)
1 constant AL-BYTE             \ measured: < 4-byte aligned
2 constant AL-4                \ measured: 4-byte aligned
3 constant AL-8                \ measured: 8-byte aligned
4 constant AL-16               \ measured: 16-byte aligned
5 constant AL-N                \ range bound (wire codes)
ENUM align DERIVE eq
  unknown
  byte
  a4
  a8
  a16
;ENUM

: ALIGN>N ( align -- n )               \ named render boundary (order-preserving)
   MATCH align
      unknown OF AL-UNKNOWN ENDOF
      byte    OF AL-BYTE    ENDOF
      a4      OF AL-4       ENDOF
      a8      OF AL-8       ENDOF
      a16     OF AL-16      ENDOF
   ;MATCH ;

: ALIGN-UNKNOWN? ( align -- bool )     \ measurement predicate (replaces AL-UNKNOWN =)
   MATCH align
      unknown OF true  ENDOF
      byte    OF false ENDOF
      a4      OF false ENDOF
      a8      OF false ENDOF
      a16     OF false ENDOF
   ;MATCH ;

\ named parse boundary (the inverse of ALIGN>N): lift a validated AL-* ordinal
\ back into the align family, fail closed on an out-of-domain code. sched-key's
\ REGION-ALIGN min-folds ALIGN>N ordinals and lifts the result here for the SKEY
\ product's align field (dot habu-cad-adt-swap). The throw is defensive: the fold
\ only ever produces AL-UNKNOWN..AL-16, so a bad code is unreachable at runtime.
: >ALIGN ( n -- align )
   dup AL-UNKNOWN = if drop MAKI-ALIGN:UNKNOWN exit then
   dup AL-BYTE    = if drop MAKI-ALIGN:BYTE    exit then
   dup AL-4       = if drop MAKI-ALIGN:A4      exit then
   dup AL-8       = if drop MAKI-ALIGN:A8      exit then
   AL-16 = if MAKI-ALIGN:A16 exit then
   E-TV-ALIGN throw ;

\ ---- deferred raw-sibling storage (dot habu-size-model-proportional 3iv/3v) ------
\ The DEFER-LAYOUT-BUFFER typed columns cannot hold FAMILY-LESS cells (a plain n, a
\ raw data pointer), so their parallel raw siblings defer by hand: a DATA-BASE-RELATIVE
\ offset var + a capacity var (BYTES), grown to the largest model and REUSED across
\ models (leak bounded), copying the live prefix into the fresh region so a within-build
\ append never drops an earlier cell. This mirrors src/core/layout-buffer.f LDEFER-GROW
\ on the raw surface (cell columns pass `n CELL *` bytes). The stored value is an offset
\ (relocation-safe, like the DEFER accessor's baked offset) so no raw address persists;
\ callers reconstruct `data-base off +`. Shared by tensor-value / model-ir / cad (all
\ `require` this file); public in MAKI. The fresh tail is left as-is: every live slot is
\ written by its append before any read (the TV-U / MIR-N / P-N liveness guards).
: RAW-ENSURE ( n n ptr n ptr n -- )   \ needbytes livebytes off-var cap-var
   {: need:n live:n ov:ptr cv:ptr :}
   need cv @ <= if exit then                       \ fits the current region
   cv @ 2 * need max {: newcap:n :}                 \ doubling floor (grow-to-at-least)
   here {: nb:ptr :}
   newcap allot
   data-base ov @ + BYTE-VIEW  nb BYTE-VIEW  live BYTE-COPY   \ carry the live prefix forward
   nb data-base -  ov !  newcap cv ! ;              \ store the fresh region's offset

\ ---- strided-view math (docs/strided-views.md) ----------------------------------
\ A view reads storage at element (base + off + i*rstr + j*cstr); a contiguous
\ tensor is the DEGENERATE view (off 0, natural strides). These helpers take the
\ view's raw offset/strides plus its TYPED extents (so the raw extent extraction
\ stays inside package MAKI, off the nominal surface) and never touch the tensor
\ store, so they load here with the value vocabulary. T-GET/T-SET (maki/array.f)
\ carry the T-AT typed-access law; ROWS-RAW/COLS-RAW/DIM-RAW + MUL-DIM/SUM-DIM are
\ the private dim algebra above.

\ natural strides of a contiguous layout: row-major (cols,1), col-major (1,rows).
: VIEW-NAT ( CAD-KIND:rows CAD-KIND:cols layout -- n n ) {: r:CAD-KIND:rows c:CAD-KIND:cols lay:layout :}
   lay LAYOUT-ROW? if  c COLS-RAW  1  else  1  r ROWS-RAW  then ;

\ highest storage element a rows x cols view at (off,rstr,cstr) reads:
\ off + (rows-1)*rstr + (cols-1)*cstr, via the overflow-checked dim algebra.
: VIEW-MAXIDX ( n n n CAD-KIND:rows CAD-KIND:cols -- n ) {: off:n rstr:n cstr:n r:CAD-KIND:rows c:CAD-KIND:cols :}
   r ROWS-RAW 1- rstr MUL-DIM   c COLS-RAW 1- cstr MUL-DIM   SUM-DIM   off SUM-DIM ;

\ view construction bounds proof (SV-2/§3): offset/strides non-negative (v2 forbids
\ negative strides), extents positive, footprint within the storage's element count.
\ Fail-closed NAMED via E-MK-DIM - the dimension-overflow law ROWS-WINDOW already
\ throws - so nothing is constructed on an out-of-range view.
: VIEW-BOUNDS-CK ( n n n CAD-KIND:rows CAD-KIND:cols CAD-KIND:dim -- ) {: off:n rstr:n cstr:n r:CAD-KIND:rows c:CAD-KIND:cols elems:CAD-KIND:dim :}
   off NONNEG drop  rstr NONNEG drop  cstr NONNEG drop
   r ROWS-RAW 1 < c COLS-RAW 1 < or if E-MK-DIM throw then
   off rstr cstr r c VIEW-MAXIDX  elems DIM-RAW >= if E-MK-DIM throw then ;

\ derived contiguity classification (SV-1): is the (off,strides) the degenerate
\ row-major / col-major access for these extents? "strided" is neither.
: VIEW-CONTIG-ROW? ( n n n CAD-KIND:rows CAD-KIND:cols -- bool ) {: off:n rstr:n cstr:n r:CAD-KIND:rows c:CAD-KIND:cols :}
   off 0=  rstr c COLS-RAW =  and  cstr 1 =  and ;
: VIEW-CONTIG-COL? ( n n n CAD-KIND:rows CAD-KIND:cols -- bool ) {: off:n rstr:n cstr:n r:CAD-KIND:rows c:CAD-KIND:cols :}
   off 0=  rstr 1 =  and  cstr r ROWS-RAW =  and ;

\ materialize a rows x cols view into a contiguous row-major dst (the §3 COPY body).
: VIEW-MAT ( ptr r n n n CAD-KIND:rows CAD-KIND:cols ptr r -- ) {: p:ptr off:n rstr:n cstr:n r:CAD-KIND:rows c:CAD-KIND:cols dst:ptr :}
   r ROWS-RAW c COLS-RAW {: R:n C:n :}
   R C * 0 ?do
      i C / {: row:n :}   i C mod {: col:n :}
      p  off row rstr * + col cstr * +  T-GET   dst i T-SET
   loop ;

\ scatter-ADD the view's cotangent g (rows x cols, row-major) into the storage
\ adjoint adj at (off,rstr,cstr). ACCUMULATES (never zeroes): two views of one
\ storage sum their scatter-adds (fan-out), the calculus of a read that fans out
\ (docs/strided-views.md §4). Every index off+row*rstr+col*cstr < storage-elems was
\ PROVEN at construction (VIEW-BOUNDS-CK), so no per-element bound is re-checked.
: VIEW-SCATTER+ ( ptr r n n n CAD-KIND:rows CAD-KIND:cols ptr r -- ) {: g:ptr off:n rstr:n cstr:n r:CAD-KIND:rows c:CAD-KIND:cols adj:ptr :}
   r ROWS-RAW c COLS-RAW {: R:n C:n :}
   R C * 0 ?do
      i C / {: row:n :}   i C mod {: col:n :}
      off row rstr * + col cstr * + {: a:n :}
      adj a T-GET   g i T-GET  f+   adj a T-SET
   loop ;

;package

\ ---------------------------------------------------------------------------
\ The stateful tensor-value store + descriptor plan builder is a real subsystem
\ (mutable handle table + IR plan), so it lives in its own package TENSOR. The
\ value-type vocabulary above (dtype/layout/align enums + predicates) is shared
\ maki substrate used bare across the whole layer, so it stays package MAKI and
\ is qualified MAKI: from inside TENSOR. Model op kinds (OP-LINEAR / OP-GELU /
\ OP-N) come from the shared op registry maki/op-kind.f (still MAKI); PLINEAR/
\ PGELU record them below.
package TENSOR

public

\ Opaque nominal handle (Model-CAD V2 R3): a public arity-0 family, so no raw-n
\ public conversion exists. The private RAW>TENSOR/TENSOR>RAW pair below is the
\ only representation authority; handles carry the store generation and fail
\ closed once TV-RESET invalidates them.
NEWTYPE tensor 0

\ Live-descriptor sizing derives from the model (dot habu-size-model-proportional):
\ the columns start at TV-SEED and grow-to-largest, reused across captures. TV-CAP is
\ now the GENEROUS transactional ceiling AND the handle stride (TV-HANDLE encodes
\ gen*TV-CAP + idx), so idx < TV-CAP is the decode invariant; TV-GEN-MAX is lowered so
\ TV-CAP*TV-GEN-MAX still fits a signed cell.
$8000 constant TV-CAP          \ generous live-descriptor ceiling + handle stride (was 256 cap)
64    constant TV-SEED         \ initial column allotment; grows past this per model

private

TRUSTED: RAW>TENSOR ( n -- tensor ) ;
TRUSTED: TENSOR>RAW ( tensor -- n ) ;

\ record table: one array per field so each keeps its own cell type (TV-DATA
\ holds a pointer; TV-HAS holds n). Indexed by slot 0..TV-U-1. The enum-typed
\ and CAD-KIND cell-family columns are generative LAYOUT-BUFFER columns (the
\ only typed pointer introduction form), so a raw n (or a foreign family) can
\ never enter or leave a descriptor cell either way. A fresh column reads as
\ id 0 of its family (LAYOUT-BUFFER zero image); TV-U guards liveness.
\ The typed columns are DEFER-LAYOUT-BUFFER (deferred-offset: bound at load to
\ TV-SEED, grown per model); the family-less siblings (data pointer, has-flag, and
\ the view offset + row/col strides - all raw n) hand-defer via base+cap vars and
\ RAW-ENSURE. TV-STORE-AT (the storage-ref handle) is a typed DEFER column. All grow
\ in LOCKSTEP off TV-BOUND (the live column extent) so a single TV-ENSURE keeps them
\ parallel. A fresh column reads as id 0 of its family (DEFER zero image); TV-U
\ guards liveness. The view columns realize docs/strided-views.md SV-1.
DEFER-LAYOUT-BUFFER TV-ROWS-AT  CAD-KIND:rows           \ rows column ( n -- ptr CAD-KIND:rows )
DEFER-LAYOUT-BUFFER TV-COLS-AT  CAD-KIND:cols           \ cols column ( n -- ptr CAD-KIND:cols )
DEFER-LAYOUT-BUFFER TV-SPACE-AT CAD-KIND:address-space  \ address-space column ( n -- ptr CAD-KIND:address-space )
DEFER-LAYOUT-BUFFER TV-DT-AT  datatype   \ datatype column ( n -- ptr datatype )
DEFER-LAYOUT-BUFFER TV-LAY-AT layout  \ layout column ( n -- ptr layout )
DEFER-LAYOUT-BUFFER TV-AL-AT  align   \ align column ( n -- ptr align )
DEFER-LAYOUT-BUFFER TV-STORE-AT tensor \ storage-ref column ( n -- ptr tensor ): self = degenerate, other = view
create TV-DATA TV-SEED cells allot   variable TV-DATA-P  TV-DATA data-base - TV-DATA-P !   variable TV-DATA-CAP  TV-SEED cells TV-DATA-CAP !  \ data ptr sibling (offset)
create TV-HAS  TV-SEED cells allot   variable TV-HAS-P   TV-HAS  data-base - TV-HAS-P  !   variable TV-HAS-CAP   TV-SEED cells TV-HAS-CAP  !  \ has-flag sibling (offset)
create TV-OFF  TV-SEED cells allot   variable TV-OFF-P   TV-OFF  data-base - TV-OFF-P  !   variable TV-OFF-CAP   TV-SEED cells TV-OFF-CAP  !  \ view offset (elements) sibling
create TV-RSTR TV-SEED cells allot   variable TV-RSTR-P  TV-RSTR data-base - TV-RSTR-P !   variable TV-RSTR-CAP  TV-SEED cells TV-RSTR-CAP !  \ view row stride (elements) sibling
create TV-CSTR TV-SEED cells allot   variable TV-CSTR-P  TV-CSTR data-base - TV-CSTR-P !   variable TV-CSTR-CAP  TV-SEED cells TV-CSTR-CAP !  \ view col stride (elements) sibling
: TV-DATA-BASE ( -- ptr ptr r )  data-base TV-DATA-P @ + ;   \ reconstruct the live base
: TV-HAS-BASE  ( -- ptr a )  data-base TV-HAS-P  @ + ;
: TV-OFF-BASE  ( -- ptr a )  data-base TV-OFF-P  @ + ;
: TV-RSTR-BASE ( -- ptr a )  data-base TV-RSTR-P @ + ;
: TV-CSTR-BASE ( -- ptr a )  data-base TV-CSTR-P @ + ;
variable TV-U                          \ free counter / live count
variable TV-GEN                        \ store generation encoded into every handle
variable TV-BOUND                      \ live column extent (grow-to-largest high-water)

\ bind the typed columns + prime the bound to TV-SEED (grow-to-largest from here)
TV-SEED TV-ROWS-AT-BIND  TV-SEED TV-COLS-AT-BIND  TV-SEED TV-SPACE-AT-BIND
TV-SEED TV-DT-AT-BIND    TV-SEED TV-LAY-AT-BIND   TV-SEED TV-AL-AT-BIND
TV-SEED TV-STORE-AT-BIND
TV-SEED TV-BOUND !

\ grow every TV column to hold slot index n (doubling; copy-on-grow preserves live cells)
: TV-ENSURE ( n -- ) {: n:n :}
   n TV-BOUND @ < if exit then
   TV-BOUND @ {: old:n :}
   old 2 * n 1+ max {: nb:n :}
   nb TV-ROWS-AT-GROW  nb TV-COLS-AT-GROW  nb TV-SPACE-AT-GROW
   nb TV-DT-AT-GROW    nb TV-LAY-AT-GROW   nb TV-AL-AT-GROW   nb TV-STORE-AT-GROW
   nb cells old cells TV-DATA-P TV-DATA-CAP MAKI:RAW-ENSURE
   nb cells old cells TV-HAS-P  TV-HAS-CAP  MAKI:RAW-ENSURE
   nb cells old cells TV-OFF-P  TV-OFF-CAP  MAKI:RAW-ENSURE
   nb cells old cells TV-RSTR-P TV-RSTR-CAP MAKI:RAW-ENSURE
   nb cells old cells TV-CSTR-P TV-CSTR-CAP MAKI:RAW-ENSURE
   nb TV-BOUND ! ;

$FFFFFFFFFF constant TV-GEN-MAX   \ lowered so TV-GEN-MAX * TV-CAP fits a signed cell

\ ---- alignment measurement ------------------------------------------------
\ Record the pointer's real base alignment. FFI:>CELL (lib/ffi-abi.f) is the audited
\ pointer->cell cast; the low bits are exact, so no data-base assumption is made.
: TV-ALIGN-CLASS ( ptr a -- align ) {: p:ptr :}
   p FFI:>CELL {: a:n :}
   a 15 and 0= if MAKI-ALIGN:A16  exit then
   a 7  and 0= if MAKI-ALIGN:A8   exit then
   a 3  and 0= if MAKI-ALIGN:A4   exit then
   MAKI-ALIGN:BYTE ;

\ ---- slot allocation + shared field tail -----------------------------------
: TV-SLOT+ ( -- n )                    \ allocate the next slot index (fail closed)
   TV-U @ TV-CAP >= if E-TV-FULL throw then        \ generous ceiling (transactional)
   TV-U @ TV-ENSURE                                \ derive column size from the model
   TV-U @ dup 1+ TV-U ! ;

: TV-FIELDS! ( ptr r CAD-KIND:rows CAD-KIND:cols n n -- )   \ data rows cols has idx
   {: base:ptr rows:CAD-KIND:rows cols:CAD-KIND:cols has:n idx:n :}
   base  TV-DATA-BASE idx cells + !
   rows  idx TV-ROWS-AT !
   cols  idx TV-COLS-AT !
   has   TV-HAS-BASE  idx cells + ! ;

\ ---- view fields (docs/strided-views.md SV-1): storage-ref + offset + strides ---
\ Every tensor carries a view descriptor; a contiguous tensor is the DEGENERATE
\ view (storage-ref = its own handle, offset 0, natural strides). off/rstr/cstr are
\ raw element counts (family-less siblings); storage-ref is a typed handle.
: TV-VFIELDS! ( tensor n n n n -- )   \ storage off rstr cstr idx
   {: store:tensor off:n rstr:n cstr:n idx:n :}
   store  idx TV-STORE-AT !
   off    TV-OFF-BASE  idx cells + !
   rstr   TV-RSTR-BASE idx cells + !
   cstr   TV-CSTR-BASE idx cells + ! ;

\ ---- slot index -> generation-carrying handle ------------------------------
: TV-HANDLE ( n -- tensor )
   TV-GEN @ TV-CAP * + RAW>TENSOR ;

\ ---- handle -> validated slot index (rejects stale generations) ------------
: TV-IX ( tensor -- n ) {: t:tensor :}
   t TENSOR>RAW {: raw:n :}
   raw 0 < if E-TV-HANDLE throw then
   raw TV-CAP / TV-GEN @ <> if E-TV-HANDLE throw then
   raw TV-CAP mod {: idx:n :}
   idx 0 < idx TV-U @ >= or if E-TV-HANDLE throw then
   idx ;

: TV-NEXT-GEN ( -- )
   TV-GEN @ TV-GEN-MAX >= if E-TV-GEN throw then
   TV-GEN @ 1+ TV-GEN ! ;

\ ---- generic n-field read (data pointer handled separately) ----------------
: TV-N@ ( tensor ptr a -- n ) {: t:tensor base:ptr :}
   base t TV-IX cells + @ ;

public

\ ---- constructors ----------------------------------------------------------
\ dtype/layout arrive as family values and store from the stack into the typed
\ slots before the cell fields bind; a bad tag is a checker reject, so the old
\ E-MK-DTYPE/E-TV-LAYOUT validation is unrepresentable here. Alignment is
\ measured from the real data pointer. Eager float pointers originate on the
\ host; device-space constructors must require provenance-bearing allocator
\ results, not a caller-selected label.
: TV-NEW-HOST ( ptr r CAD-KIND:rows CAD-KIND:cols datatype layout -- tensor )
   TV-SLOT+ {: idx:n :}
   idx TV-LAY-AT !                      \ layout (top)
   idx TV-DT-AT !                       \ dtype
   MAKI:SPACE-HOST idx TV-SPACE-AT !
   {: base:ptr rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   base TV-ALIGN-CLASS idx TV-AL-AT !
   base rows cols 1 idx TV-FIELDS!
   idx TV-HANDLE {: self:tensor :}                 \ degenerate view: storage-ref = self,
   rows cols  idx TV-LAY-AT @  MAKI:VIEW-NAT {: rstr:n cstr:n :}   \ offset 0, natural strides
   self 0 rstr cstr idx TV-VFIELDS!
   self ;

\ TV-NEW defaults dtype f32 + row-major (the eager host-array convention).
: TV-NEW ( ptr r CAD-KIND:rows CAD-KIND:cols -- tensor )
   MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW TV-NEW-HOST ;

\ TV-DESC builds a planning descriptor: typed facts only, no buffer.
\ Alignment is unknown (conservative) and TV-DATA@ fails closed. The data
\ slot stores data-base purely as a never-read placeholder (HAS=0 guards it).
: TV-DESC ( CAD-KIND:rows CAD-KIND:cols datatype layout CAD-KIND:address-space -- tensor )
   TV-SLOT+ {: idx:n :}
   idx TV-SPACE-AT !                    \ address space (top)
   idx TV-LAY-AT !                      \ layout
   idx TV-DT-AT !                       \ dtype
   MAKI-ALIGN:UNKNOWN idx TV-AL-AT !
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   data-base rows cols 0 idx TV-FIELDS!
   idx TV-HANDLE {: self:tensor :}                 \ degenerate view (bufferless, never read
   rows cols  idx TV-LAY-AT @  MAKI:VIEW-NAT {: rstr:n cstr:n :}   \ as storage; keeps the class contiguous)
   self 0 rstr cstr idx TV-VFIELDS!
   self ;

\ ---- accessors (one per recorded fact; each returns its family) -------------
: TV-ROWS@   ( tensor -- CAD-KIND:rows )  TV-IX TV-ROWS-AT @ ;
: TV-COLS@   ( tensor -- CAD-KIND:cols )  TV-IX TV-COLS-AT @ ;
: TV-SPACE@  ( tensor -- CAD-KIND:address-space )  TV-IX TV-SPACE-AT @ ;
: TV-DTYPE@  ( tensor -- datatype )   TV-IX TV-DT-AT  @ ;
: TV-LAYOUT@ ( tensor -- layout )  TV-IX TV-LAY-AT @ ;
: TV-ALIGN@  ( tensor -- align )   TV-IX TV-AL-AT  @ ;

\ ---- view descriptor reads (docs/strided-views.md SV-1): raw offset/strides + storage-ref
: TV-OFF@   ( tensor -- n ) {: t:tensor :}  TV-OFF-BASE  t TV-IX cells + @ ;
: TV-RSTR@  ( tensor -- n ) {: t:tensor :}  TV-RSTR-BASE t TV-IX cells + @ ;
: TV-CSTR@  ( tensor -- n ) {: t:tensor :}  TV-CSTR-BASE t TV-IX cells + @ ;
: TV-STORE@ ( tensor -- tensor ) {: t:tensor :}  t TV-IX TV-STORE-AT @ dup TV-IX drop ;   \ stored handle re-validates
\ storage-ref = SELF for the degenerate view; a view names OTHER storage. This is
\ the read-only predicate (SV-3): a view is a READ descriptor, no write-through.
: TV-VIEW? ( tensor -- bool ) {: t:tensor :}  t TV-STORE@ TENSOR>RAW  t TENSOR>RAW  <> ;

: TV-ELEMS ( tensor -- CAD-KIND:dim )
   {: t:tensor :}
   t TV-ROWS@ t TV-COLS@ MAKI:SHAPE-ELEMS ;
: TV-HAS-DATA? ( tensor -- bool )  TV-HAS-BASE TV-N@ 0= 0= ;

\ TV-DATA@ hands out the FLAT contiguous buffer, so it fails closed on a view (a
\ view has no flat buffer of its own extents; §3 - remedy = the COPY op / TV-MATERIALIZE).
\ The degenerate (non-view) path is unchanged - bit-identical to before SV-1.
: TV-DATA@ ( tensor -- ptr r ) {: t:tensor :}
   t TV-HAS-DATA? 0= if E-TV-NODATA throw then
   t TV-VIEW? if E-TV-VIEW throw then
   TV-DATA-BASE t TV-IX cells + @ ;

\ ---- handle identity (no raw-n public conversion exists) -------------------
: TV-EQUAL? ( tensor tensor -- bool )
   TENSOR>RAW swap TENSOR>RAW swap = ;

private
\ raw storage data pointer, UNGUARDED (= the storage base for a view). Internal to
\ the strided read/write/scatter seam; TV-DATA@ is the guarded public flat accessor.
: TV-STORE-PTR@ ( tensor -- ptr r ) {: t:tensor :}  TV-DATA-BASE t TV-IX cells + @ ;

\ install a view slot: data = storage ptr, extents/dtype/layout/space recorded, and
\ the view descriptor (storage-ref, offset, strides) set. The high-level
\ constructors prove the bounds BEFORE calling this, so it does not re-check.
: TV-NEW-VIEW ( ptr r tensor CAD-KIND:rows CAD-KIND:cols datatype layout CAD-KIND:address-space n n n -- tensor )
   {: base:ptr store:tensor rows:CAD-KIND:rows cols:CAD-KIND:cols dt:datatype lay:layout sp:CAD-KIND:address-space off:n rstr:n cstr:n :}
   TV-SLOT+ {: idx:n :}
   sp  idx TV-SPACE-AT !
   lay idx TV-LAY-AT !
   dt  idx TV-DT-AT !
   base TV-ALIGN-CLASS idx TV-AL-AT !
   base rows cols 1 idx TV-FIELDS!
   store off rstr cstr idx TV-VFIELDS!
   idx TV-HANDLE ;

public

\ ---- derived contiguity classification (SV-1: the layout enum's role) -------
: TV-CONTIG-ROW? ( tensor -- bool ) {: t:tensor :}
   t TV-OFF@ t TV-RSTR@ t TV-CSTR@ t TV-ROWS@ t TV-COLS@ MAKI:VIEW-CONTIG-ROW? ;
: TV-CONTIG-COL? ( tensor -- bool ) {: t:tensor :}
   t TV-OFF@ t TV-RSTR@ t TV-CSTR@ t TV-ROWS@ t TV-COLS@ MAKI:VIEW-CONTIG-COL? ;
: TV-STRIDED? ( tensor -- bool ) {: t:tensor :}
   t TV-CONTIG-ROW? t TV-CONTIG-COL? or 0= ;

\ ---- element seam: strided read (SV-4/§5) + guarded write (SV-3 immutability) ---
: TV-AT@ ( tensor n n -- r ) {: t:tensor i:n j:n :}
   t TV-STORE-PTR@  t TV-OFF@  i t TV-RSTR@ *  +  j t TV-CSTR@ *  +  T-GET ;

\ TV-AT! writes storage the caller OWNS (the degenerate view) and FAILS CLOSED
\ (E-TV-VIEW) on any view: there is NO write-through-view op and none may be added
\ (docs/strided-views.md §2). A future write-view proposal reopens §2's review bar -
\ it cannot slip in silently, because this write seam rejects it red-first.
: TV-AT! ( r tensor n n -- ) {: v:r t:tensor i:n j:n :}
   t TV-VIEW? if E-TV-VIEW throw then
   t TV-HAS-DATA? 0= if E-TV-NODATA throw then
   v  t TV-STORE-PTR@  t TV-OFF@  i t TV-RSTR@ *  +  j t TV-CSTR@ *  +  T-SET ;

\ ---- view constructors (SV-2): each bounds-proved at construction, fail-closed ---
\ WINDOW: a contiguous row slice [r0,r1) - the EX-BIND generalization. Inherits the
\ base strides; the offset advances by r0 base-row-steps. ROWS-WINDOW proves
\ 0<=r0<=r1<=rows (E-MK-DIM); VIEW-BOUNDS-CK proves the footprint fits the storage.
: TV-WINDOW ( tensor n n -- tensor ) {: base:tensor r0:n r1:n :}
   base TV-HAS-DATA? 0= if E-TV-NODATA throw then
   base TV-STORE@ {: store:tensor :}
   base TV-RSTR@ {: brstr:n :}  base TV-CSTR@ {: bcstr:n :}
   base TV-OFF@  r0 brstr *  +  {: soff:n :}
   r0 r1 base TV-ROWS@ MAKI:ROWS-WINDOW {: vrows:CAD-KIND:rows :}
   base TV-COLS@ {: vcols:CAD-KIND:cols :}
   soff brstr bcstr vrows vcols  store TV-ELEMS  MAKI:VIEW-BOUNDS-CK
   store TV-STORE-PTR@ store vrows vcols base TV-DTYPE@ base TV-LAYOUT@ base TV-SPACE@ soff brstr bcstr TV-NEW-VIEW ;

\ TRANSPOSE-VIEW: swap extents AND strides (no copy); the contiguity class flips.
: TV-TRANSPOSE-VIEW ( tensor -- tensor ) {: base:tensor :}
   base TV-HAS-DATA? 0= if E-TV-NODATA throw then
   base TV-STORE@ {: store:tensor :}
   base TV-RSTR@ {: brstr:n :}  base TV-CSTR@ {: bcstr:n :}
   base TV-ROWS@ base TV-COLS@ MAKI:TRANSPOSE-SHAPE {: vrows:CAD-KIND:rows vcols:CAD-KIND:cols :}
   base TV-OFF@ {: soff:n :}
   base TV-LAYOUT@ MAKI:LAYOUT-ROW? if MAKI-LAYOUT:COL else MAKI-LAYOUT:ROW then {: vlay:layout :}
   soff bcstr brstr vrows vcols  store TV-ELEMS  MAKI:VIEW-BOUNDS-CK
   store TV-STORE-PTR@ store vrows vcols base TV-DTYPE@ vlay base TV-SPACE@ soff bcstr brstr TV-NEW-VIEW ;

\ HEAD-SPLIT (SV-6 form): head h of a (rows x C) buffer with C = H*hd. Over a
\ CONTIGUOUS base this is the doc's "offset h*hd, row stride H*hd (=C), col stride 1";
\ written in the base's own strides so it composes over any base (reducing to that
\ formula for a contiguous row-major base).
: TV-HEAD-SPLIT ( tensor n n n -- tensor ) {: base:tensor h:n H:n hd:n :}
   base TV-HAS-DATA? 0= if E-TV-NODATA throw then
   H 1 < hd 1 < or  h 0 < or  h H >= or if E-MK-DIM throw then
   base TV-COLS@ H hd * MAKI:COLS-IS? 0= if E-MK-DIM throw then   \ C = H*hd
   base TV-STORE@ {: store:tensor :}
   base TV-RSTR@ {: brstr:n :}  base TV-CSTR@ {: bcstr:n :}
   base TV-OFF@  h hd * bcstr *  +  {: soff:n :}
   base TV-ROWS@ {: vrows:CAD-KIND:rows :}
   1 hd MAKI:SHAPE nip {: vcols:CAD-KIND:cols :}
   soff brstr bcstr vrows vcols  store TV-ELEMS  MAKI:VIEW-BOUNDS-CK
   store TV-STORE-PTR@ store vrows vcols base TV-DTYPE@ base TV-LAYOUT@ base TV-SPACE@ soff brstr bcstr TV-NEW-VIEW ;

\ general VIEW: explicit offset (relative to the base's storage origin) + extents +
\ strides. WINDOW / TRANSPOSE-VIEW / HEAD-SPLIT are conveniences over this.
: TV-VIEW ( tensor n n n n n -- tensor ) {: base:tensor off:n vr:n vc:n rstr:n cstr:n :}
   base TV-HAS-DATA? 0= if E-TV-NODATA throw then
   base TV-STORE@ {: store:tensor :}
   base TV-OFF@ off + {: soff:n :}
   vr vc MAKI:SHAPE {: vrows:CAD-KIND:rows vcols:CAD-KIND:cols :}
   soff rstr cstr vrows vcols  store TV-ELEMS  MAKI:VIEW-BOUNDS-CK
   store TV-STORE-PTR@ store vrows vcols base TV-DTYPE@ base TV-LAYOUT@ base TV-SPACE@ soff rstr cstr TV-NEW-VIEW ;

\ ---- materialize (§3 COPY remedy) + scatter-add adjoint (SV-4) --------------
\ TV-MATERIALIZE strided-reads the view into a caller-owned contiguous row-major dst.
: TV-MATERIALIZE ( tensor ptr r -- ) {: t:tensor dst:ptr :}
   t TV-STORE-PTR@ t TV-OFF@ t TV-RSTR@ t TV-CSTR@ t TV-ROWS@ t TV-COLS@ dst MAKI:VIEW-MAT ;

\ TV-VIEW-ADJOINT+ scatter-ADDS the view's cotangent g (contiguous, the view's
\ extents) into the storage adjoint adj at the view's (offset,strides). ACCUMULATES
\ (fan-out: multiple views of one storage sum); the out-of-view contribution is
\ structurally zero (only footprint indices are touched). docs/strided-views.md §4.
: TV-VIEW-ADJOINT+ ( tensor ptr r ptr r -- ) {: t:tensor g:ptr adj:ptr :}
   g t TV-OFF@ t TV-RSTR@ t TV-CSTR@ t TV-ROWS@ t TV-COLS@ adj MAKI:VIEW-SCATTER+ ;

\ ---- store lifecycle -------------------------------------------------------
: TV-RESET ( -- )  TV-NEXT-GEN 0 TV-U ! ;      \ clears store; stale handles reject
: TV-COUNT ( -- n )  TV-U @ ;

\ Trip the generous ceiling transactionally (before any allot), else ensure the
\ columns hold n live descriptors. The append words already size on demand, so
\ this is only the ceiling assertion / an optional pre-size, never required.
: TV-BIND ( n -- ) {: n:n :}
   n TV-CAP > if E-TV-FULL throw then
   n 0 > if n 1- TV-ENSURE then ;

public

\ ---- plan store (descriptor-mode IR: an ordered list of op records) -------
\ Each op keeps op-kind, output tensor, and a (offset,count) window into a flat
\ input-tensor pool. A pending record is staged by PLAN-OP-BEGIN / PLAN-IN+ and
\ committed by PLAN-OP+, so any input arity records with a fixed-arity wordset.
\ Op-count / operand-pool sizing derives from the model (dot habu-size-model-
\ proportional): the columns start at their SEED and grow-to-largest, reused across
\ captures; PLAN-CAP / PLAN-INCAP are now the GENEROUS transactional ceilings.
$8000 constant PLAN-CAP         \ generous op-count ceiling (was 64 cap)
32    constant PLAN-SEED        \ initial op-column allotment; grows past this
$8000 constant PLAN-INCAP       \ generous operand-pool ceiling (was 256 cap)

private

128   constant PLAN-IN-SEED     \ initial operand-pool allotment; grows past this

\ Op-indexed columns (dot habu-cad-adt-swap keeps the typed provenance): the op-kind
\ + output-tensor columns are DEFER-LAYOUT-BUFFER; the three window/attr siblings are
\ family-less and hand-defer via base+cap vars. All five grow in LOCKSTEP off P-BOUND.
DEFER-LAYOUT-BUFFER P-KIND-AT opkind \ op-kind column ( n -- ptr opkind )
DEFER-LAYOUT-BUFFER P-OUT-AT tensor  \ output tensor column ( n -- ptr tensor )
create P-INOFF PLAN-SEED cells allot  variable P-INOFF-P  P-INOFF data-base - P-INOFF-P !  variable P-INOFF-CAP  PLAN-SEED cells P-INOFF-CAP !  \ input window start (offset)
create P-INCNT PLAN-SEED cells allot  variable P-INCNT-P  P-INCNT data-base - P-INCNT-P !  variable P-INCNT-CAP  PLAN-SEED cells P-INCNT-CAP !  \ input window length (offset)
create P-ATTR  PLAN-SEED cells allot  variable P-ATTR-P   P-ATTR  data-base - P-ATTR-P  !  variable P-ATTR-CAP   PLAN-SEED cells P-ATTR-CAP  !  \ movement attrs (offset)
: P-INOFF-BASE ( -- ptr a )  data-base P-INOFF-P @ + ;   \ reconstruct the live bases
: P-INCNT-BASE ( -- ptr a )  data-base P-INCNT-P @ + ;
: P-ATTR-BASE  ( -- ptr a )  data-base P-ATTR-P  @ + ;
variable P-N                            \ committed op count
variable P-BOUND                        \ live op-column extent (grow-to-largest)
DEFER-LAYOUT-BUFFER P-INS-AT tensor  \ flat input-tensor pool ( n -- ptr tensor )
variable P-INS-U
variable P-INS-BOUND                     \ live operand-pool extent (grow-to-largest)
1 LAYOUT-BUFFER PEND-KIND opkind        \ pending record staging (typed slot, fixed 1)
variable PEND-OFF
variable PEND-CNT
variable PEND-ATTR                       \ pending attrs (0 unless a movement appender sets it)
variable PEND-ON                        \ 1 while a record is being staged

\ bind the typed plan columns + prime the bounds (grow-to-largest from the seeds)
PLAN-SEED P-KIND-AT-BIND  PLAN-SEED P-OUT-AT-BIND  PLAN-SEED P-BOUND !
PLAN-IN-SEED P-INS-AT-BIND  PLAN-IN-SEED P-INS-BOUND !

\ grow the op-indexed columns to hold op index n (doubling; copy-on-grow preserves)
: P-ENSURE ( n -- ) {: n:n :}
   n P-BOUND @ < if exit then
   P-BOUND @ {: old:n :}
   old 2 * n 1+ max {: nb:n :}
   nb P-KIND-AT-GROW  nb P-OUT-AT-GROW
   nb cells old cells P-INOFF-P P-INOFF-CAP MAKI:RAW-ENSURE
   nb cells old cells P-INCNT-P P-INCNT-CAP MAKI:RAW-ENSURE
   nb cells old cells P-ATTR-P  P-ATTR-CAP  MAKI:RAW-ENSURE
   nb P-BOUND ! ;

\ grow the operand pool to hold operand index n (doubling; copy-on-grow preserves)
: P-INS-ENSURE ( n -- ) {: n:n :}
   n P-INS-BOUND @ < if exit then
   P-INS-BOUND @ 2 * n 1+ max {: nb:n :}
   nb P-INS-AT-GROW  nb P-INS-BOUND ! ;

: PLAN-IX ( n -- n ) {: idx:n :}               \ validate a committed-op index
   idx 0 < idx P-N @ >= or if E-TV-PLAN-IDX throw then
   idx ;

\ the pending staging cell is a one-slot generative buffer behind this typed slot
: PEND-KIND-AT  ( -- ptr opkind )    0 PEND-KIND ;

public

: PLAN-RESET ( -- )
   0 P-N !  0 P-INS-U !  0 PEND-ON ! ;

: PLAN-N@ ( -- n )  P-N @ ;

\ Ceiling pins (transactional, before any allot) / optional pre-size for the op
\ columns and the operand pool. Self-managing append still sizes on demand.
: PLAN-BIND ( n -- ) {: n:n :}
   n PLAN-CAP > if E-TV-PLAN-FULL throw then
   n 0 > if n 1- P-ENSURE then ;
: PLAN-IN-BIND ( n -- ) {: n:n :}
   n PLAN-INCAP > if E-TV-PLAN-FULL throw then
   n 0 > if n 1- P-INS-ENSURE then ;

\ open a record with the op-kind family; a bad tag is a checker reject (the old
\ E-TV-OPKIND range validation is unrepresentable). The family rides the stack
\ into the typed pending slot before the bookkeeping counters set.
: PLAN-OP-BEGIN ( opkind -- )
   PEND-ON @ if E-TV-PLAN-STATE throw then
   PEND-KIND-AT !
   P-INS-U @ PEND-OFF !  0 PEND-CNT !  0 PEND-ATTR !  1 PEND-ON ! ;

: PLAN-IN+ ( tensor -- ) {: t:tensor :}        \ stage one input for the open record
   PEND-ON @ 0= if E-TV-PLAN-STATE throw then
   t TV-IX drop                                \ stale/forged handles reject here
   P-INS-U @ PLAN-INCAP >= if E-TV-PLAN-FULL throw then   \ generous ceiling (transactional)
   P-INS-U @ P-INS-ENSURE                      \ derive operand-pool size from the model
   t P-INS-U @ P-INS-AT !
   P-INS-U @ 1+ P-INS-U !
   PEND-CNT @ 1+ PEND-CNT ! ;

: PLAN-ATTR! ( n -- ) {: attr:n :}             \ stage the movement attrs for the open record
   PEND-ON @ 0= if E-TV-PLAN-STATE throw then
   attr PEND-ATTR ! ;

: PLAN-OP+ ( tensor -- ) {: out:tensor :}      \ commit the open record with its output
   PEND-ON @ 0= if E-TV-PLAN-STATE throw then
   out TV-IX drop                              \ stale/forged handles reject here
   P-N @ PLAN-CAP >= if E-TV-PLAN-FULL throw then         \ generous ceiling (transactional)
   P-N @ P-ENSURE                              \ derive op-column size from the model
   P-N @ {: idx:n :}
   PEND-KIND-AT @  idx P-KIND-AT !          \ op-kind family (staged by PLAN-OP-BEGIN)
   out          idx P-OUT-AT !
   PEND-OFF @   P-INOFF-BASE idx cells + !
   PEND-CNT @   P-INCNT-BASE idx cells + !
   PEND-ATTR @  P-ATTR-BASE  idx cells + !
   idx 1+ P-N !
   0 PEND-ON ! ;

: PLAN-OP@ ( n -- opkind )   PLAN-IX P-KIND-AT @ ;
: PLAN-OUT@ ( n -- tensor )
   PLAN-IX P-OUT-AT @ dup TV-IX drop ;         \ stored handles re-validate on read
: PLAN-ATTR@ ( n -- n )      PLAN-IX cells P-ATTR-BASE  + @ ;
: PLAN-IN-COUNT@ ( n -- n )  PLAN-IX cells P-INCNT-BASE + @ ;

: PLAN-IN@ ( n n -- tensor ) {: idx:n k:n :}   \ k-th input tensor of op idx
   idx PLAN-IX drop
   k 0 < k P-INCNT-BASE idx cells + @ >= or if E-TV-PLAN-IDX throw then
   P-INOFF-BASE idx cells + @  k +  P-INS-AT @ dup TV-IX drop ;

\ ---- descriptor-mode model ops (append IR, do not compute) -----------------
\ Output shape/dtype are inferred and recorded; both ops return a descriptor.
: PLINEAR ( tensor tensor tensor -- tensor ) {: x:tensor w:tensor b:tensor :}
   x TV-ROWS@ w TV-COLS@ x TV-DTYPE@ MAKI-LAYOUT:ROW x TV-SPACE@ TV-DESC {: y:tensor :}
   MAKI-OPKIND:LINEAR PLAN-OP-BEGIN
   x PLAN-IN+  w PLAN-IN+  b PLAN-IN+
   y PLAN-OP+
   y ;

: PGELU ( tensor -- tensor ) {: x:tensor :}    \ elementwise: same shape/layout
   x TV-ROWS@ x TV-COLS@ x TV-DTYPE@ x TV-LAYOUT@ x TV-SPACE@ TV-DESC {: y:tensor :}
   MAKI-OPKIND:GELU PLAN-OP-BEGIN
   x PLAN-IN+
   y PLAN-OP+
   y ;

private

\ Typed descriptor facts cross into the legacy eager array kernel only here.
\ The checked caller validates every nominal role before this adapter.
\ Retirement owner: habu-epic-model-cad-70b629a9.
TRUSTED: TYPED-LINEAR ( ptr r ptr r ptr r ptr r CAD-KIND:rows CAD-KIND:cols CAD-KIND:cols -- )
   MAKI:LINEAR ;

public

\ ---- eager interop proof ---------------------------------------------------
\ Unpack tensor values into the cells eager LINEAR wants, run it in place, and
\ wrap the result back. Inner dims must agree (X cols = W rows) or fail closed.
: TV-LINEAR ( tensor tensor tensor tensor -- tensor )   \ X W b Yout -> Yout
   {: x:tensor w:tensor b:tensor y:tensor :}
   x TV-COLS@ w TV-ROWS@ MAKI:INNER-EQUAL? 0= if E-TV-SHAPE throw then
   x TV-DATA@  w TV-DATA@  b TV-DATA@  y TV-DATA@
   x TV-ROWS@ x TV-COLS@ w TV-COLS@ TYPED-LINEAR
   y ;

;package
