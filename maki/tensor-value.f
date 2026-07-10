\ maki/tensor-value.f - the unified single-slot tensor value + its plan mode.
\
\ CAD-PLAN section 3 prerequisite. Today's eager maki ops pass a tensor as many
\ stack cells (maki/linear.f: LINEAR ( ptr a ptr a ptr a ptr a n n n -- ); data,
\ shape and dtype travel apart), so they cannot be re-typed onto descriptors. A
\ `tensor` is instead ONE stack slot: an opaque DEFTYPE handle indexing a
\ module-owned record table that carries data pointer, 2D shape (rows cols),
\ dtype, layout tag, and the alignment class recorded from the actual pointer.
\
\ Representation hiding follows maki/report.f: a `tensor` is an opaque handle;
\ every constructor/accessor takes or returns `tensor` plus primitive field
\ values, so the record layout never leaks and the store can swap to an ADT
\ family later without touching a caller. Unlike report's single live object this
\ table is multi-instance: a fixed-capacity table with a free counter (TV-U);
\ TV-RESET clears it and invalidates outstanding handles.
\
\ Plan mode (CAD-PLAN section 3) is the descriptor-vocabulary base: PLINEAR/PGELU
\ do not compute, they append IR records (op-kind, input tensors, output tensor)
\ to a plan the fusion planner (cad-1) will consume. TV-LINEAR is the eager
\ interop proof: it unpacks tensor values into the cells the existing eager
\ LINEAR wants, runs it, and wraps the result back. v1 is the mechanism only - no
\ fusion, no cost model.
\
\ Facts are recorded, never assumed (CAD-PLAN section 4.1): the alignment class is
\ measured from the real pointer at construction; dtype tags come from
\ maki/tensor.f's set; a descriptor with no buffer records AL-UNKNOWN, never a
\ guess. Fail closed: bad dtype/layout tag, bad handle, missing data, shape
\ mismatch, plan-builder misuse and every capacity are named throws, never
\ defaulted. maki owns -5000..-5099; tensor-value uses -5040..-5049 (loss.f owns -5030..-5039).

require maki/tensor.f
require maki/linear.f
require maki/op-kind.f
require lib/ffi-abi.f

-5040 constant E-TV-FULL        \ tensor store capacity exceeded
-5041 constant E-TV-HANDLE      \ handle slot index out of range
-5042 constant E-TV-LAYOUT      \ layout tag out of range
-5043 constant E-TV-NODATA      \ data requested from a descriptor (no buffer)
-5044 constant E-TV-SHAPE       \ shape mismatch (eager linear inner dim)
-5045 constant E-TV-PLAN-FULL   \ plan op or input-pool capacity exceeded
-5046 constant E-TV-PLAN-IDX    \ plan index out of range
-5047 constant E-TV-PLAN-STATE  \ plan builder used out of order
-5048 constant E-TV-OPKIND      \ op-kind out of range

package MAKI
public

\ ---- layout tags (v1: contiguity order only; strides arrive with cad-1) ----
0 constant LAY-ROW              \ row-major / C-contiguous
1 constant LAY-COL             \ column-major
2 constant LAY-N               \ range bound

\ Real ENUM co-located with LAY-* (dot habu-cad-adt-swap; capability S1). LAY-*
\ stay the public/wire/table-index vocabulary; model-ir.f stores this enum behind
\ its unchanged n accessors (>LAYOUT / LAYOUT>N / LAY-KEY). Order tracks LAY-ROW/COL.
ENUM layout
  row
  col
;ENUM

\ ---- alignment classes (recorded from the pointer, never assumed) ----
0 constant AL-UNKNOWN          \ conservative: not measured (descriptors)
1 constant AL-BYTE             \ measured: < 4-byte aligned
2 constant AL-4                \ measured: 4-byte aligned
3 constant AL-8                \ measured: 8-byte aligned
4 constant AL-16               \ measured: 16-byte aligned
5 constant AL-N                \ range bound

\ Real ENUM co-located with AL-* (dot habu-cad-adt-swap; capability S1). AL-* stay
\ the public/table-index vocabulary (align has no wire/text form); model-ir.f
\ stores this enum behind its unchanged n accessors (>ALIGN / ALIGN>N). Bare-digit
\ tails reject, so a4/a8/a16 map to AL-4/AL-8/AL-16. Order tracks AL-UNKNOWN..AL-16.
ENUM align
  unknown
  byte
  a4
  a8
  a16
;ENUM

: AL-VALID? ( n -- bool ) {: al:n :}  al 0 < 0=  al AL-N <  and ;

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

DEFTYPE tensor                 \ opaque single-cell handle; internals swap to an ADT later
                               \ (public: TENSOR:tensor>N is the audited handle-inspection cast)

256 constant TV-CAP            \ max live tensor values (store capacity contract)

private


\ record table: one create-array per field so each keeps its own cell type
\ (TV-DATA holds a pointer; the rest hold n). Indexed by slot 0..TV-U-1.
create TV-DATA TV-CAP cells allot      \ data pointer (materialized tensors only)
create TV-ROWS TV-CAP cells allot
create TV-COLS TV-CAP cells allot
create TV-DT   TV-CAP cells allot      \ dtype tag (maki/tensor.f DT-*)
create TV-LAY  TV-CAP cells allot      \ layout tag (LAY-*)
create TV-AL   TV-CAP cells allot      \ alignment class (AL-*)
create TV-HAS  TV-CAP cells allot      \ 1 = has data buffer, 0 = descriptor
variable TV-U                          \ free counter / live count

\ ---- alignment measurement ------------------------------------------------
\ Record the pointer's real base alignment. P>N (lib/ffi-abi.f) is the audited
\ pointer->cell cast; the low bits are exact, so no data-base assumption is made.
: TV-ALIGN-CLASS ( ptr a -- n ) {: p:ptr :}
   p P>N {: a:n :}
   a 15 and 0= if MAKI:AL-16  exit then
   a 7  and 0= if MAKI:AL-8   exit then
   a 3  and 0= if MAKI:AL-4   exit then
   MAKI:AL-BYTE ;

\ ---- store commit (validates every tag before writing a slot) -------------
: TV-COMMIT ( ptr a n n n n n n -- tensor )    \ data rows cols dtype layout align has
   {: base:ptr rows:n cols:n dt:n lay:n al:n has:n :}
   dt MAKI:DT-VALID? 0= if E-MK-DTYPE throw then
   lay 0 < lay MAKI:LAY-N >= or if E-TV-LAYOUT throw then
   TV-U @ TV-CAP >= if E-TV-FULL throw then
   TV-U @ {: idx:n :}
   base  TV-DATA idx cells + !
   rows  TV-ROWS idx cells + !
   cols  TV-COLS idx cells + !
   dt    TV-DT   idx cells + !
   lay   TV-LAY  idx cells + !
   al    TV-AL   idx cells + !
   has   TV-HAS  idx cells + !
   idx 1+ TV-U !
   idx >tensor ;

\ ---- handle -> validated slot index ---------------------------------------
: TV-IX ( tensor -- n ) {: t:tensor :}
   t tensor>N {: idx:n :}
   idx 0 < idx TV-U @ >= or if E-TV-HANDLE throw then
   idx ;

\ ---- generic n-field read/write (data pointer handled separately) ---------
: TV-N@ ( tensor ptr a -- n ) {: t:tensor base:ptr :}
   base t TV-IX cells + @ ;
: TV-N! ( n tensor ptr a -- ) {: v:n t:tensor base:ptr :}
   v base t TV-IX cells + ! ;

public

\ ---- constructors ----------------------------------------------------------
\ TV-NEW-AS is the explicit form; alignment is measured from the data pointer.
: TV-NEW-AS ( ptr a n n n n -- tensor )        \ data rows cols dtype layout
   {: base:ptr rows:n cols:n dt:n lay:n :}
   base rows cols dt lay  base TV-ALIGN-CLASS  1  TV-COMMIT ;

\ TV-NEW defaults dtype f32 + row-major (the eager host-array convention).
: TV-NEW ( ptr a n n -- tensor )               \ data rows cols
   MAKI:DT-F32 MAKI:LAY-ROW TV-NEW-AS ;

\ TV-DESC builds a planning descriptor: shape/dtype/layout only, no buffer.
\ Alignment is AL-UNKNOWN (conservative) and TV-DATA@ fails closed. The data
\ slot stores data-base purely as a never-read placeholder (HAS=0 guards it).
: TV-DESC ( n n n n -- tensor )                \ rows cols dtype layout
   {: rows:n cols:n dt:n lay:n :}
   data-base rows cols dt lay  MAKI:AL-UNKNOWN  0  TV-COMMIT ;

\ ---- accessors (one per recorded fact) ------------------------------------
: TV-ROWS@   ( tensor -- n )  TV-ROWS TV-N@ ;
: TV-COLS@   ( tensor -- n )  TV-COLS TV-N@ ;
: TV-DTYPE@  ( tensor -- n )  TV-DT   TV-N@ ;
: TV-LAYOUT@ ( tensor -- n )  TV-LAY  TV-N@ ;
: TV-ALIGN@  ( tensor -- n )  TV-AL   TV-N@ ;
: TV-ELEMS   ( tensor -- n )  {: t:tensor :}  t TV-ROWS@ t TV-COLS@ * ;
: TV-HAS-DATA? ( tensor -- bool )  TV-HAS TV-N@ 0= 0= ;

: TV-DATA@ ( tensor -- ptr a ) {: t:tensor :}
   t TV-HAS-DATA? 0= if E-TV-NODATA throw then
   TV-DATA t TV-IX cells + @ ;

\ ---- settable dtype / layout (validated) ----------------------------------
: TV-DTYPE! ( tensor n -- tensor ) {: t:tensor dt:n :}
   dt MAKI:DT-VALID? 0= if E-MK-DTYPE throw then
   dt t TV-DT TV-N!  t ;
: TV-LAYOUT! ( tensor n -- tensor ) {: t:tensor lay:n :}
   lay 0 < lay MAKI:LAY-N >= or if E-TV-LAYOUT throw then
   lay t TV-LAY TV-N!  t ;

\ ---- store lifecycle -------------------------------------------------------
: TV-RESET ( -- )  0 TV-U ! ;                  \ clears store; invalidates handles
: TV-COUNT ( -- n )  TV-U @ ;

public

\ ---- plan store (descriptor-mode IR: an ordered list of op records) -------
\ Each op keeps op-kind, output tensor, and a (offset,count) window into a flat
\ input-tensor pool. A pending record is staged by PLAN-OP-BEGIN / PLAN-IN+ and
\ committed by PLAN-OP+, so any input arity records with a fixed-arity wordset.
64  constant PLAN-CAP           \ max ops per plan (plan capacity contract)

private

256 constant PLAN-INCAP         \ max total input slots across the plan

create P-KIND  PLAN-CAP cells allot     \ op-kind (OP-*)
create P-OUT   PLAN-CAP cells allot     \ output tensor
create P-INOFF PLAN-CAP cells allot     \ input window start in P-INS
create P-INCNT PLAN-CAP cells allot     \ input window length
create P-ATTR  PLAN-CAP cells allot     \ movement attrs (packed; 0 for compute ops)
variable P-N                            \ committed op count
create P-INS   PLAN-INCAP cells allot   \ flat input-tensor pool
variable P-INS-U
variable PEND-KIND                      \ pending record staging
variable PEND-OFF
variable PEND-CNT
variable PEND-ATTR                       \ pending attrs (0 unless a movement appender sets it)
variable PEND-ON                        \ 1 while a record is being staged

: PLAN-IX ( n -- n ) {: idx:n :}               \ validate a committed-op index
   idx 0 < idx P-N @ >= or if E-TV-PLAN-IDX throw then
   idx ;

public

: PLAN-RESET ( -- )
   0 P-N !  0 P-INS-U !  0 PEND-ON ! ;

: PLAN-N@ ( -- n )  P-N @ ;

: PLAN-OP-BEGIN ( n -- ) {: k:n :}             \ open a record with op-kind k
   PEND-ON @ if E-TV-PLAN-STATE throw then
   k 0 < k MAKI:OP-N >= or if E-TV-OPKIND throw then
   k PEND-KIND !  P-INS-U @ PEND-OFF !  0 PEND-CNT !  0 PEND-ATTR !  1 PEND-ON ! ;

: PLAN-IN+ ( tensor -- ) {: t:tensor :}        \ stage one input for the open record
   PEND-ON @ 0= if E-TV-PLAN-STATE throw then
   P-INS-U @ PLAN-INCAP >= if E-TV-PLAN-FULL throw then
   t P-INS P-INS-U @ cells + !
   P-INS-U @ 1+ P-INS-U !
   PEND-CNT @ 1+ PEND-CNT ! ;

: PLAN-ATTR! ( n -- ) {: attr:n :}             \ stage the movement attrs for the open record
   PEND-ON @ 0= if E-TV-PLAN-STATE throw then
   attr PEND-ATTR ! ;

: PLAN-OP+ ( tensor -- ) {: out:tensor :}      \ commit the open record with its output
   PEND-ON @ 0= if E-TV-PLAN-STATE throw then
   P-N @ PLAN-CAP >= if E-TV-PLAN-FULL throw then
   P-N @ {: idx:n :}
   PEND-KIND @  P-KIND  idx cells + !
   out          P-OUT   idx cells + !
   PEND-OFF @   P-INOFF idx cells + !
   PEND-CNT @   P-INCNT idx cells + !
   PEND-ATTR @  P-ATTR  idx cells + !
   idx 1+ P-N !
   0 PEND-ON ! ;

: PLAN-OP@ ( n -- n )        PLAN-IX cells P-KIND  + @ ;
: PLAN-OUT@ ( n -- tensor )  PLAN-IX cells P-OUT   + @ ;
: PLAN-ATTR@ ( n -- n )      PLAN-IX cells P-ATTR  + @ ;
: PLAN-IN-COUNT@ ( n -- n )  PLAN-IX cells P-INCNT + @ ;

: PLAN-IN@ ( n n -- tensor ) {: idx:n k:n :}   \ k-th input tensor of op idx
   idx PLAN-IX drop
   k 0 < k P-INCNT idx cells + @ >= or if E-TV-PLAN-IDX throw then
   P-INS  P-INOFF idx cells + @  k +  cells + @ ;

\ ---- descriptor-mode model ops (append IR, do not compute) -----------------
\ Output shape/dtype are inferred and recorded; both ops return a descriptor.
: PLINEAR ( tensor tensor tensor -- tensor ) {: x:tensor w:tensor b:tensor :}
   x TV-ROWS@ w TV-COLS@ x TV-DTYPE@ MAKI:LAY-ROW TV-DESC {: y:tensor :}
   MAKI:OP-LINEAR PLAN-OP-BEGIN
   x PLAN-IN+  w PLAN-IN+  b PLAN-IN+
   y PLAN-OP+
   y ;

: PGELU ( tensor -- tensor ) {: x:tensor :}    \ elementwise: same shape/layout
   x TV-ROWS@ x TV-COLS@ x TV-DTYPE@ x TV-LAYOUT@ TV-DESC {: y:tensor :}
   MAKI:OP-GELU PLAN-OP-BEGIN
   x PLAN-IN+
   y PLAN-OP+
   y ;

\ ---- eager interop proof ---------------------------------------------------
\ Unpack tensor values into the cells eager LINEAR wants, run it in place, and
\ wrap the result back. Inner dims must agree (X cols = W rows) or fail closed.
: TV-LINEAR ( tensor tensor tensor tensor -- tensor )   \ X W b Yout -> Yout
   {: x:tensor w:tensor b:tensor y:tensor :}
   x TV-COLS@ w TV-ROWS@ <> if E-TV-SHAPE throw then
   x TV-DATA@  w TV-DATA@  b TV-DATA@  y TV-DATA@
   x TV-ROWS@  x TV-COLS@  w TV-COLS@  MAKI:LINEAR
   y ;

end-package
