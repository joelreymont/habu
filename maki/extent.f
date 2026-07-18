\ maki/extent.f - nominal tensor extents + the extent-typed index value family.
\
\ docs/golden-syntax.md candidate B on the TFAM substrate (docs/extent-substrate.md).
\ An EXTENT declares a nominal tensor dimension: `128 EXTENT: #M` mints a
\ package-scoped arity-0 TFAM cell family from the `#M` surface name, binds the
\ runtime extent value (`#M ( -- n )` pushes 128), and derives the explicit
\ crossing `>#M ( n -- ix<extm> )` that lifts a plain index into the extent's
\ index space. `ix` is the arity-1 index value family (docs/golden-syntax.md:59):
\ a value of type `ix<extm>` is a real cell whose PHANTOM type argument names the
\ extent it indexes, so a row index and a column index are DISTINCT nominal types
\ - flipping them is an author-time checker reject, not a runtime bug.
\
\ MANGLING (#name -> lowercase family tail): strip the leading '#', lowercase the
\ remainder, prefix `ext` (#M -> extm, #K -> extk, #SEQ -> extseq). The result
\ satisfies TDECL-RESERVED? (src/core/sumtype.f): >=4 bytes so never a single
\ letter, never an atom prefix (extent-/space-/... all carry a hyphen), never a
\ builtin/CT-role/keyword tail. A tail collision (two names folding to one tail)
\ throws E-TFAM-DUP through CHECKER-DEFFAMILY - a NAMED reject, never a silent
\ rename.
\
\ CROSSING BOUNDARY: `>#name` (per-extent, PINS the extent) and `IX>N` (generic
\ projection) are TRUSTED casts - the same sanctioned nominal-cast pattern
\ src/core/roles.f uses for idx/len/fd (one cell at runtime, retyped for the
\ checker). The checker cannot yet express a checked injection into a parametric
\ cell family; that gap is the roles.f role-cast gap, tracked project-wide.
\
\ WHAT THE CROSSING DOES AND DOES NOT GUARANTEE (be honest here):
\   - Explicit: a bare n is never an ix<extm> without a `>#name` call
\     (maki/extent-tensor-test.f locks that reject).
\   - Range-guarded at runtime: `>#name` throws E-EXT-RANGE when n is outside
\     [0, extent), so an out-of-range index - including a corrupt value a gather
\     reads back and re-injects - never reaches an accessor as a typed index.
\   - NOT author-time bound to a specific extent: every injector accepts ANY
\     plain n, so `i >#M` inside a `#K 0 ?DO` loop is STATICALLY ACCEPTED - the
\     loop counter -> index crossing is not checked against the extent the loop
\     iterates. docs/golden-syntax.md:68's "`#K ?DO` yields idx<#K>" author-time
\     induction binding is NOT implemented here; that is follow-up dot
\     habu-extent-bound-loop-a70a49b3. The author-time flip protection this dot
\     DOES deliver is on ACCESSOR CALLS: feeding an ix of the wrong extent to an
\     accessor is a checker reject (the `ix<#N> where ix<#M>` case).
\ maki -> habu only; maki owns -5031..-5034 here.

require lib/prelude.f                 \ true/false
require maki/array.f                 \ T-AT: the ptr+offset address word the accessors reuse
require lib/string.f                 \ STR=, BYTE-COPY: registry name storage

-5031 constant E-EXT-NAME     \ EXTENT: surface name missing, empty, or not '#'-prefixed
-5032 constant E-EXT-UNDECL   \ TENSOR:/ITENSOR: referenced an extent no EXTENT: declared
-5033 constant E-EXT-CAP      \ extent registry or generated-source buffer capacity exceeded
-5034 constant E-EXT-RANGE    \ a crossing into an extent's index space is outside [0, extent)
-5035 constant E-EXT-VALUE    \ extent size below 1, or a negative value handed to the decimal emitter

package MAKI

public

\ ix<extent> is the index value family: arity 1, one cell, the extent a phantom arg.
TYPEFAMILY ix 1

\ IX>N projects any index value back to a plain cell. Generic over the extent
\ (type var `e`), so one word serves every extent - the projection direction is
\ always sound (a nominal cell IS a cell). The reverse `>#name` is per-extent so
\ the target extent is pinned, never inferred.
TRUSTED: IX>N ( ix<e> -- n ) ;

private

\ ---- generated-source codegen buffer (build the ": ... ;" / "TRUSTED: ... ;"
\ text each definer evaluates). Shared by EXTENT: and the tensor accessor definers
\ (maki/extent-tensor.f, same package). ----------------------------------------
$400 constant XG-CAP
create XG-BUF XG-CAP allot
variable XG-U

: XG-RESET ( -- )  0 XG-U ! ;
: XG-C ( n -- ) {: c:n :}                          \ append one byte
   XG-U @ 1 + XG-CAP > if E-EXT-CAP throw then
   c XG-BUF XG-U @ + c!  XG-U @ 1 + XG-U ! ;
: XG+ ( ptr u8 n -- ) {: a:ptr u:n :}              \ append a string
   0 begin dup u < while  dup a + c@ XG-C  1 +  repeat drop ;
: XG-INT ( n -- ) {: v:n :}                        \ append a non-negative decimal
   v 0 < if E-EXT-VALUE throw then                 \ fail closed: never emit garbage digits for a negative
   v 10 >= if v 10 / RECURSE then  v 10 mod 48 + XG-C ;
: XG$ ( -- ptr u8 n )  XG-BUF XG-U @ ;

\ the one metaprogramming boundary: `evaluate` cannot be checker-typed, so the
\ audited TRUSTED wrapper compiles the constructed text with the check hook active
\ (roles.f DTC-EVAL / maki/cad.f CAP-COMPILE-RUN pattern). Every generated body is
\ certified by that hook; the definer itself adds no unchecked code.
TRUSTED: XG-EVAL ( -- )  XG$ evaluate ;

\ ---- extent registry: surface name -> (mangled tail, runtime value). The tensor
\ accessor definers look an extent up by its surface name to bake the row-major
\ stride (value) and emit the accessor signature (tail). -----------------------
64 constant XR-CAP                                 \ max declared extents
32 constant XR-NAME-CAP                            \ max surface / tail bytes
create XR-SURF XR-CAP XR-NAME-CAP * allot
create XR-SLEN XR-CAP cells allot
create XR-TAIL XR-CAP XR-NAME-CAP * allot
create XR-TLEN XR-CAP cells allot
create XR-VAL  XR-CAP cells allot
variable XR-N

: XR-SURF-SLOT ( n -- ptr a )  XR-NAME-CAP *  XR-SURF + ;
: XR-TAIL-SLOT ( n -- ptr a )  XR-NAME-CAP *  XR-TAIL + ;

public

: XR-SURF@ ( n -- ptr u8 n ) {: i:n :}  i XR-SURF-SLOT  i cells XR-SLEN + @ ;
: XR-TAIL@ ( n -- ptr u8 n ) {: i:n :}  i XR-TAIL-SLOT  i cells XR-TLEN + @ ;
: XR-VAL@  ( n -- n )  cells XR-VAL + @ ;

\ resolve a surface name to its registry slot; false = never declared.
: XR-FIND ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   XR-N @ 0 ?do
      a u  i XR-SURF@ STR= if  i true  unloop exit  then
   loop  0 false ;

\ resolve or fail closed: the value+tail a tensor definer needs for one extent.
: XR-REQUIRE ( ptr u8 n -- n )
   XR-FIND 0= if E-EXT-UNDECL throw then ;

private

: XR-ADD ( n ptr u8 n ptr u8 n -- ) {: val:n sa:ptr su:n ta:ptr tu:n :}
   XR-N @ XR-CAP >= if E-EXT-CAP throw then
   su XR-NAME-CAP > tu XR-NAME-CAP > or if E-EXT-CAP throw then
   XR-N @ {: i:n :}
   sa i XR-SURF-SLOT su BYTE-COPY  su i cells XR-SLEN + !
   ta i XR-TAIL-SLOT tu BYTE-COPY  tu i cells XR-TLEN + !
   val i cells XR-VAL + !
   i 1 + XR-N ! ;

\ ---- #name -> lowercase family tail (extm/extk/...) --------------------------
create XM-BUF XR-NAME-CAP allot
variable XM-U

: XM-LC ( n -- n ) {: c:n :}  c 65 >= c 90 <= and if c 32 + else c then ;
: XM-C ( n -- ) {: c:n :}
   XM-U @ XR-NAME-CAP >= if E-EXT-NAME throw then
   c XM-BUF XM-U @ + c!  XM-U @ 1 + XM-U ! ;
: X-MANGLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 2 < if E-EXT-NAME throw then                  \ need '#' + at least one char
   a c@ 35 <> if E-EXT-NAME throw then             \ require the leading '#'
   0 XM-U !
   [char] e XM-C  [char] x XM-C  [char] t XM-C
   u 1 ?do  a i + c@ XM-LC XM-C  loop              \ lowercase the tail, skip '#'
   XM-BUF XM-U @ ;

public

\ EXTENT: - `value EXTENT: #NAME`. Mints the extent family, binds the runtime
\ extent value word `#NAME ( -- n )`, and derives the injector `>#NAME
\ ( n -- ix<tail> )`. Top-level-interpret-only, like SUMTYPE / DEFTYPE (it parses
\ the input stream and mutates the type registry - side effects a ( n -- ) row
\ does not model). The generated constant + injector text is certified by the
\ check hook through XG-EVAL.
: EXTENT: ( n -- )
   {: val:n :}
   parse-name {: sa:ptr su:n :}
   su 0= if E-EXT-NAME throw then
   \ an extent of size < 1 has no valid index; reject BEFORE minting anything (the
   \ name is already consumed, so the guard leaves no dangling token and no family).
   val 1 < if E-EXT-VALUE throw then
   sa su X-MANGLE {: ta:ptr tu:n :}
   ta tu s" 0" CHECKER-DEFFAMILY                   \ arity-0 family; E-TFAM-DUP on collision
   val sa su ta tu XR-ADD
   XG-RESET
   s" : " XG+  sa su XG+  s"  ( -- n ) " XG+  val XG-INT  s"  ; " XG+
   \ the injector is the single choke point every crossing into this extent's
   \ index space passes through, so it runtime-guards the bound: n in [0, extent).
   \ TRUSTED because the n -> ix<tail> retype is not checker-expressible; the
   \ range check is ordinary runtime logic inside that boundary.
   s" TRUSTED: >" XG+  sa su XG+  s"  ( n -- ix<" XG+  ta tu XG+
      s" > ) dup 0 < over " XG+  val XG-INT  s"  >= or if E-EXT-RANGE throw then ;" XG+
   XG-EVAL ;

;package
