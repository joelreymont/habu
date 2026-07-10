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

require lib/prelude.f
require maki/tensor.f
require maki/linear.f
require maki/op-kind.f
require lib/ffi-abi.f

-5040 constant E-TV-FULL        \ tensor store capacity exceeded
-5041 constant E-TV-HANDLE      \ handle slot index out of range
\ -5042 (E-TV-LAYOUT) retired: the layout family makes an out-of-range tag a
\ checker reject; the code stays reserved to tensor-value.
-5043 constant E-TV-NODATA      \ data requested from a descriptor (no buffer)
-5044 constant E-TV-SHAPE       \ shape mismatch (eager linear inner dim)
-5045 constant E-TV-PLAN-FULL   \ plan op or input-pool capacity exceeded
-5046 constant E-TV-PLAN-IDX    \ plan index out of range
-5047 constant E-TV-PLAN-STATE  \ plan builder used out of order
-5048 constant E-TV-OPKIND      \ op-kind out of range

package MAKI
public

\ ---- layout tags (v1: contiguity order only; strides arrive with cad-1) ----
\ The `layout` ENUM is the semantic type carried through construction, storage,
\ and every consumer (dot habu-cad-adt-swap, corrected plan). LAY-* codes remain
\ ONLY as the wire/hash vocabulary at the named boundaries below.
0 constant LAY-ROW              \ row-major / C-contiguous
1 constant LAY-COL             \ column-major
ENUM layout
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
\ wire/test assertions. Bare-digit variant tails reject, so a4/a8/a16.
0 constant AL-UNKNOWN          \ conservative: not measured (descriptors)
1 constant AL-BYTE             \ measured: < 4-byte aligned
2 constant AL-4                \ measured: 4-byte aligned
3 constant AL-8                \ measured: 8-byte aligned
4 constant AL-16               \ measured: 16-byte aligned
5 constant AL-N                \ range bound (wire codes)
ENUM align
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
create TV-DT   TV-CAP cells allot      \ dtype (family value; typed slot TV-DT-AT)
create TV-LAY  TV-CAP cells allot      \ layout (family value; typed slot TV-LAY-AT)
create TV-AL   TV-CAP cells allot      \ align (family value; typed slot TV-AL-AT)
create TV-HAS  TV-CAP cells allot      \ 1 = has data buffer, 0 = descriptor
variable TV-U                          \ free counter / live count

\ typed slot addresses: the descriptor columns are reachable only through these,
\ so a raw n (or a foreign family) can never enter or leave a descriptor cell.
: TV-DT-AT  ( n -- ptr dtype )   cells TV-DT  + ;
: TV-LAY-AT ( n -- ptr layout )  cells TV-LAY + ;
: TV-AL-AT  ( n -- ptr align )   cells TV-AL  + ;

\ ---- alignment measurement ------------------------------------------------
\ Record the pointer's real base alignment. P>N (lib/ffi-abi.f) is the audited
\ pointer->cell cast; the low bits are exact, so no data-base assumption is made.
: TV-ALIGN-CLASS ( ptr a -- align ) {: p:ptr :}
   p P>N {: a:n :}
   a 15 and 0= if MAKI-ALIGN:A16  exit then
   a 7  and 0= if MAKI-ALIGN:A8   exit then
   a 3  and 0= if MAKI-ALIGN:A4   exit then
   MAKI-ALIGN:BYTE ;

\ ---- slot allocation + shared n-field tail ---------------------------------
: TV-SLOT+ ( -- n )                    \ allocate the next slot index (fail closed)
   TV-U @ TV-CAP >= if E-TV-FULL throw then
   TV-U @ dup 1+ TV-U ! ;

: TV-FIELDS! ( ptr a n n n n -- )      \ data rows cols has idx -- (the n-typed columns)
   {: base:ptr rows:n cols:n has:n idx:n :}
   base  TV-DATA idx cells + !
   rows  TV-ROWS idx cells + !
   cols  TV-COLS idx cells + !
   has   TV-HAS  idx cells + ! ;

\ ---- handle -> validated slot index ---------------------------------------
: TV-IX ( tensor -- n ) {: t:tensor :}
   t tensor>N {: idx:n :}
   idx 0 < idx TV-U @ >= or if E-TV-HANDLE throw then
   idx ;

\ ---- generic n-field read (data pointer handled separately) ----------------
: TV-N@ ( tensor ptr a -- n ) {: t:tensor base:ptr :}
   base t TV-IX cells + @ ;

public

\ ---- constructors ----------------------------------------------------------
\ dtype/layout arrive as family values and store from the stack into the typed
\ slots before the n fields bind (families cannot bind into locals); a bad tag
\ is a checker reject, so the old E-MK-DTYPE/E-TV-LAYOUT validation is
\ unrepresentable here. Alignment is measured from the real data pointer.
: TV-NEW-AS ( ptr a n n dtype layout -- tensor )   \ data rows cols dtype layout
   TV-SLOT+ {: idx:n :}
   idx TV-LAY-AT !                      \ layout (top)
   idx TV-DT-AT !                       \ dtype
   {: base:ptr rows:n cols:n :}
   base TV-ALIGN-CLASS idx TV-AL-AT !
   base rows cols 1 idx TV-FIELDS!
   idx >tensor ;

\ TV-NEW defaults dtype f32 + row-major (the eager host-array convention).
: TV-NEW ( ptr a n n -- tensor )               \ data rows cols
   MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW TV-NEW-AS ;

\ TV-DESC builds a planning descriptor: shape/dtype/layout only, no buffer.
\ Alignment is unknown (conservative) and TV-DATA@ fails closed. The data
\ slot stores data-base purely as a never-read placeholder (HAS=0 guards it).
: TV-DESC ( n n dtype layout -- tensor )       \ rows cols dtype layout
   TV-SLOT+ {: idx:n :}
   idx TV-LAY-AT !                      \ layout (top)
   idx TV-DT-AT !                       \ dtype
   MAKI-ALIGN:UNKNOWN idx TV-AL-AT !
   {: rows:n cols:n :}
   data-base rows cols 0 idx TV-FIELDS!
   idx >tensor ;

\ ---- accessors (one per recorded fact; descriptor facts return families) ----
: TV-ROWS@   ( tensor -- n )       TV-ROWS TV-N@ ;
: TV-COLS@   ( tensor -- n )       TV-COLS TV-N@ ;
: TV-DTYPE@  ( tensor -- dtype )   TV-IX TV-DT-AT  @ ;
: TV-LAYOUT@ ( tensor -- layout )  TV-IX TV-LAY-AT @ ;
: TV-ALIGN@  ( tensor -- align )   TV-IX TV-AL-AT  @ ;

: TV-ELEMS   ( tensor -- n )  {: t:tensor :}  t TV-ROWS@ t TV-COLS@ * ;
: TV-HAS-DATA? ( tensor -- bool )  TV-HAS TV-N@ 0= 0= ;

: TV-DATA@ ( tensor -- ptr a ) {: t:tensor :}
   t TV-HAS-DATA? 0= if E-TV-NODATA throw then
   TV-DATA t TV-IX cells + @ ;

\ ---- settable dtype / layout ------------------------------------------------
\ The new value arrives as a family (a bad tag is a checker reject); it swaps
\ over the handle so the handle can bind while the family stores from the stack.
: TV-DTYPE! ( tensor dtype -- tensor )
   swap {: t:tensor :}  t TV-IX TV-DT-AT !  t ;
: TV-LAYOUT! ( tensor layout -- tensor )
   swap {: t:tensor :}  t TV-IX TV-LAY-AT !  t ;

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
   x TV-ROWS@ w TV-COLS@ x TV-DTYPE@ MAKI-LAYOUT:ROW TV-DESC {: y:tensor :}
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
