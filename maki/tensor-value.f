\ maki/tensor-value.f - the unified single-slot tensor value + its plan mode.
\
\ CAD-PLAN section 3 prerequisite. Today's eager maki ops pass a tensor as many
\ stack cells (maki/linear.f: LINEAR ( ptr a ptr a ptr a ptr a n n n -- ); data,
\ shape and dtype travel apart), so they cannot be re-typed onto descriptors. A
\ `tensor` is instead ONE stack slot: an opaque nominal handle indexing a
\ module-owned record table that carries data pointer, 2D shape (rows cols),
\ dtype, layout, address space, and the alignment class recorded from the pointer.
\
\ Representation hiding follows maki/report.f: a `tensor` is an opaque handle;
\ every constructor/accessor takes or returns `tensor` plus typed field
\ values, so the record layout never leaks. Unlike report's single live object this
\ table is multi-instance: a fixed-capacity table with a free counter (TV-U);
\ TV-RESET bumps the store generation, so every outstanding handle from an
\ earlier generation fails closed (Model-CAD V2 R3: stale handles reject).
\
\ Plan mode (CAD-PLAN section 3) is the descriptor-vocabulary base: PLINEAR/PGELU
\ do not compute, they append IR records (op-kind, input tensors, output tensor)
\ to a plan the fusion planner (cad-1) will consume. TV-LINEAR is the eager
\ interop proof: it unpacks tensor values into the cells the existing eager
\ LINEAR wants, runs it, and wraps the result back. v1 is the mechanism only - no
\ fusion, no cost model.
\
\ Facts are recorded, never assumed (CAD-PLAN section 4.1): the alignment class is
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
\ -5048 (E-TV-OPKIND) retired: PLAN-OP-BEGIN takes an `opkind` family, so an
\ out-of-range op-kind is a checker reject; the code stays reserved to tensor-value.
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

end-package

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
TYPEFAMILY tensor 0

256 constant TV-CAP            \ max live tensor values (store capacity contract)

private

TRUSTED: RAW>TENSOR ( n -- tensor ) ;
TRUSTED: TENSOR>RAW ( tensor -- n ) ;

\ record table: one array per field so each keeps its own cell type (TV-DATA
\ holds a pointer; TV-HAS holds n). Indexed by slot 0..TV-U-1. The enum-typed
\ columns are generative LAYOUT-BUFFER columns (the only typed-layout pointer
\ introduction form); the CAD-KIND cell-family columns publish typed slot
\ accessors over plain cell arrays, so a raw n (or a foreign family) can never
\ enter or leave a descriptor cell either way.
create TV-DATA TV-CAP cells allot      \ data pointer (materialized tensors only)
create TV-ROWS TV-CAP cells allot
create TV-COLS TV-CAP cells allot
create TV-SPACE TV-CAP cells allot     \ address-space fact
TV-CAP LAYOUT-BUFFER TV-DT-AT  dtype   \ dtype column ( n -- ptr dtype )
TV-CAP LAYOUT-BUFFER TV-LAY-AT layout  \ layout column ( n -- ptr layout )
TV-CAP LAYOUT-BUFFER TV-AL-AT  align   \ align column ( n -- ptr align )
create TV-HAS  TV-CAP cells allot      \ 1 = has data buffer, 0 = descriptor
variable TV-U                          \ free counter / live count
variable TV-GEN                        \ store generation encoded into every handle

$7FFFFFFFFFFFFF constant TV-GEN-MAX

\ typed slot accessors for the CAD-KIND cell-family columns
: TV-ROWS-AT ( n -- ptr CAD-KIND:rows )  cells TV-ROWS + ;
: TV-COLS-AT ( n -- ptr CAD-KIND:cols )  cells TV-COLS + ;
: TV-SPACE-AT ( n -- ptr CAD-KIND:address-space )  cells TV-SPACE + ;

\ ---- alignment measurement ------------------------------------------------
\ Record the pointer's real base alignment. P>N (lib/ffi-abi.f) is the audited
\ pointer->cell cast; the low bits are exact, so no data-base assumption is made.
: TV-ALIGN-CLASS ( ptr a -- align ) {: p:ptr :}
   p P>N {: a:n :}
   a 15 and 0= if MAKI-ALIGN:A16  exit then
   a 7  and 0= if MAKI-ALIGN:A8   exit then
   a 3  and 0= if MAKI-ALIGN:A4   exit then
   MAKI-ALIGN:BYTE ;

\ ---- slot allocation + shared field tail -----------------------------------
: TV-SLOT+ ( -- n )                    \ allocate the next slot index (fail closed)
   TV-U @ TV-CAP >= if E-TV-FULL throw then
   TV-U @ dup 1+ TV-U ! ;

: TV-FIELDS! ( ptr a CAD-KIND:rows CAD-KIND:cols n n -- )   \ data rows cols has idx
   {: base:ptr rows:CAD-KIND:rows cols:CAD-KIND:cols has:n idx:n :}
   base  TV-DATA idx cells + !
   rows  idx TV-ROWS-AT !
   cols  idx TV-COLS-AT !
   has   TV-HAS  idx cells + ! ;

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
\ measured from the real data pointer. Untyped eager pointers originate on the
\ host; device-space constructors must require provenance-bearing allocator
\ results, not a caller-selected label.
: TV-NEW-HOST ( ptr a CAD-KIND:rows CAD-KIND:cols dtype layout -- tensor )
   TV-SLOT+ {: idx:n :}
   idx TV-LAY-AT !                      \ layout (top)
   idx TV-DT-AT !                       \ dtype
   MAKI:SPACE-HOST idx TV-SPACE-AT !
   {: base:ptr rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   base TV-ALIGN-CLASS idx TV-AL-AT !
   base rows cols 1 idx TV-FIELDS!
   idx TV-HANDLE ;

\ TV-NEW defaults dtype f32 + row-major (the eager host-array convention).
: TV-NEW ( ptr a CAD-KIND:rows CAD-KIND:cols -- tensor )
   MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW TV-NEW-HOST ;

\ TV-DESC builds a planning descriptor: typed facts only, no buffer.
\ Alignment is unknown (conservative) and TV-DATA@ fails closed. The data
\ slot stores data-base purely as a never-read placeholder (HAS=0 guards it).
: TV-DESC ( CAD-KIND:rows CAD-KIND:cols dtype layout CAD-KIND:address-space -- tensor )
   TV-SLOT+ {: idx:n :}
   idx TV-SPACE-AT !                    \ address space (top)
   idx TV-LAY-AT !                      \ layout
   idx TV-DT-AT !                       \ dtype
   MAKI-ALIGN:UNKNOWN idx TV-AL-AT !
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   data-base rows cols 0 idx TV-FIELDS!
   idx TV-HANDLE ;

\ ---- accessors (one per recorded fact; each returns its family) -------------
: TV-ROWS@   ( tensor -- CAD-KIND:rows )  TV-IX TV-ROWS-AT @ ;
: TV-COLS@   ( tensor -- CAD-KIND:cols )  TV-IX TV-COLS-AT @ ;
: TV-SPACE@  ( tensor -- CAD-KIND:address-space )  TV-IX TV-SPACE-AT @ ;
: TV-DTYPE@  ( tensor -- dtype )   TV-IX TV-DT-AT  @ ;
: TV-LAYOUT@ ( tensor -- layout )  TV-IX TV-LAY-AT @ ;
: TV-ALIGN@  ( tensor -- align )   TV-IX TV-AL-AT  @ ;

: TV-ELEMS ( tensor -- CAD-KIND:dim )
   {: t:tensor :}
   t TV-ROWS@ t TV-COLS@ MAKI:SHAPE-ELEMS ;
: TV-HAS-DATA? ( tensor -- bool )  TV-HAS TV-N@ 0= 0= ;

: TV-DATA@ ( tensor -- ptr a ) {: t:tensor :}
   t TV-HAS-DATA? 0= if E-TV-NODATA throw then
   TV-DATA t TV-IX cells + @ ;

\ ---- handle identity (no raw-n public conversion exists) -------------------
: TV-EQUAL? ( tensor tensor -- bool )
   TENSOR>RAW swap TENSOR>RAW swap = ;

\ ---- store lifecycle -------------------------------------------------------
: TV-RESET ( -- )  TV-NEXT-GEN 0 TV-U ! ;      \ clears store; stale handles reject
: TV-COUNT ( -- n )  TV-U @ ;

public

\ ---- plan store (descriptor-mode IR: an ordered list of op records) -------
\ Each op keeps op-kind, output tensor, and a (offset,count) window into a flat
\ input-tensor pool. A pending record is staged by PLAN-OP-BEGIN / PLAN-IN+ and
\ committed by PLAN-OP+, so any input arity records with a fixed-arity wordset.
64  constant PLAN-CAP           \ max ops per plan (plan capacity contract)

private

256 constant PLAN-INCAP         \ max total input slots across the plan

\ typed op-kind column (dot habu-cad-adt-swap): a generative LAYOUT-BUFFER
\ column owning extent, stride, bounds, and family provenance, so a raw n or a
\ foreign family can never enter or leave. An opkind cannot bind into a local,
\ so it rides the stack.
PLAN-CAP LAYOUT-BUFFER P-KIND-AT opkind \ op-kind column ( n -- ptr opkind )
create P-OUT   PLAN-CAP cells allot     \ output tensor
create P-INOFF PLAN-CAP cells allot     \ input window start in P-INS
create P-INCNT PLAN-CAP cells allot     \ input window length
create P-ATTR  PLAN-CAP cells allot     \ movement attrs (packed; 0 for compute ops)
variable P-N                            \ committed op count
create P-INS   PLAN-INCAP cells allot   \ flat input-tensor pool
variable P-INS-U
1 LAYOUT-BUFFER PEND-KIND opkind        \ pending record staging (typed slot)
variable PEND-OFF
variable PEND-CNT
variable PEND-ATTR                       \ pending attrs (0 unless a movement appender sets it)
variable PEND-ON                        \ 1 while a record is being staged

: PLAN-IX ( n -- n ) {: idx:n :}               \ validate a committed-op index
   idx 0 < idx P-N @ >= or if E-TV-PLAN-IDX throw then
   idx ;

\ the pending staging cell is a one-slot generative buffer behind this typed slot
: PEND-KIND-AT  ( -- ptr opkind )    0 PEND-KIND ;

public

: PLAN-RESET ( -- )
   0 P-N !  0 P-INS-U !  0 PEND-ON ! ;

: PLAN-N@ ( -- n )  P-N @ ;

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
   P-INS-U @ PLAN-INCAP >= if E-TV-PLAN-FULL throw then
   t P-INS P-INS-U @ cells + !
   P-INS-U @ 1+ P-INS-U !
   PEND-CNT @ 1+ PEND-CNT ! ;

: PLAN-ATTR! ( n -- ) {: attr:n :}             \ stage the movement attrs for the open record
   PEND-ON @ 0= if E-TV-PLAN-STATE throw then
   attr PEND-ATTR ! ;

: PLAN-OP+ ( tensor -- ) {: out:tensor :}      \ commit the open record with its output
   PEND-ON @ 0= if E-TV-PLAN-STATE throw then
   out TV-IX drop                              \ stale/forged handles reject here
   P-N @ PLAN-CAP >= if E-TV-PLAN-FULL throw then
   P-N @ {: idx:n :}
   PEND-KIND-AT @  idx P-KIND-AT !          \ op-kind family (staged by PLAN-OP-BEGIN)
   out          P-OUT   idx cells + !
   PEND-OFF @   P-INOFF idx cells + !
   PEND-CNT @   P-INCNT idx cells + !
   PEND-ATTR @  P-ATTR  idx cells + !
   idx 1+ P-N !
   0 PEND-ON ! ;

: PLAN-OP@ ( n -- opkind )   PLAN-IX P-KIND-AT @ ;
: PLAN-OUT@ ( n -- tensor )
   PLAN-IX cells P-OUT + @ dup TV-IX drop ;    \ stored handles re-validate on read
: PLAN-ATTR@ ( n -- n )      PLAN-IX cells P-ATTR  + @ ;
: PLAN-IN-COUNT@ ( n -- n )  PLAN-IX cells P-INCNT + @ ;

: PLAN-IN@ ( n n -- tensor ) {: idx:n k:n :}   \ k-th input tensor of op idx
   idx PLAN-IX drop
   k 0 < k P-INCNT idx cells + @ >= or if E-TV-PLAN-IDX throw then
   P-INS  P-INOFF idx cells + @  k +  cells + @ dup TV-IX drop ;

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
TRUSTED: TYPED-LINEAR ( ptr a ptr a ptr a ptr a CAD-KIND:rows CAD-KIND:cols CAD-KIND:cols -- )
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

end-package
