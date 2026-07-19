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
require lib/string.f                 \ STR=, BYTE-COPY, ASCII-LOWER: registry name storage + tail fold
require lib/codegen.f                \ CODEGEN:BUFFER-E: the shared generated-source byte buffer
require lib/adt/option.f             \ option<xr-slot>: XR-FIND returns a present/absent slot
require lib/type/deftype.f           \ DEFTYPE: the registry slot index + length columns are their own types

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

\ Registry data-layer nominals (deftype.f). The extent registry is a set of
\ parallel arrays; making its slot index and its two string-length columns their
\ own checker types stops the swaps a raw `n` hides. `xr-slot` is the row index: a
\ rank, a loop counter, or a tensor-registry slot can no longer pose as an extent
\ slot without the explicit `>XR-SLOT` crossing. `xr-surf-len` and `xr-tail-len`
\ are the surface-name and tail lengths as DISTINCT types, so a name accessor that
\ reads the wrong length column is an author-time reject, not a silent bug.
DEFTYPE XR-SLOT
DEFTYPE XR-SURF-LEN
DEFTYPE XR-TAIL-LEN

private

\ ---- generated-source codegen buffer (build the ": ... ;" / "TRUSTED: ... ;"
\ text each definer evaluates). Shared by EXTENT: and the tensor accessor definers
\ (maki/extent-tensor.f, same package). The append mechanics live in package CODEGEN
\ (lib/codegen.f); these thin words bind them to this file's XG-BUFFER instance,
\ minted with the E-EXT-CAP / E-EXT-VALUE throw codes its callers already expect. ---
$1000 constant XG-CAP                              \ headroom for SPEC:-generated word bodies
XG-CAP E-EXT-CAP E-EXT-VALUE CODEGEN:BUFFER-E XG-BUFFER

: XG-RESET ( -- )  XG-BUFFER CODEGEN:RESET ;
: XG+ ( ptr u8 n -- )  XG-BUFFER CODEGEN:APPEND-STRING ;   \ append a string
: XG-INT ( n -- )  XG-BUFFER CODEGEN:APPEND-DECIMAL ;      \ append a non-negative decimal (negative -> E-EXT-VALUE)
: XG$ ( -- ptr u8 n )  XG-BUFFER CODEGEN:CONTENTS ;

\ the one metaprogramming boundary: `evaluate` cannot be checker-typed, so the
\ audited TRUSTED wrapper compiles the constructed text with the check hook active
\ (lib/type/deftype.f NG-EVAL / maki/cad.f CAP-COMPILE-RUN pattern). Every generated body is
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

\ column byte-slot bases: the one place raw cells-offset math lives, hidden behind
\ the `xr-slot` index so a bare n can never address a name column.
: XR-SURF-PTR ( xr-slot -- ptr a )  XR-SLOT>N XR-NAME-CAP *  XR-SURF + ;
: XR-TAIL-PTR ( xr-slot -- ptr a )  XR-SLOT>N XR-NAME-CAP *  XR-TAIL + ;

public

\ one accessor per column, each with a distinct typed effect. The surface-name and
\ tail lengths are DISTINCT nominals, so reading the wrong length column rejects.
: XR-SLEN@ ( xr-slot -- xr-surf-len )  XR-SLOT>N cells XR-SLEN + @ >XR-SURF-LEN ;
: XR-TLEN@ ( xr-slot -- xr-tail-len )  XR-SLOT>N cells XR-TLEN + @ >XR-TAIL-LEN ;
: XR-VAL@  ( xr-slot -- n )            XR-SLOT>N cells XR-VAL + @ ;
: XR-SURF@ ( xr-slot -- ptr u8 n ) {: s:xr-slot :}  s XR-SURF-PTR  s XR-SLEN@ XR-SURF-LEN>N ;
: XR-TAIL@ ( xr-slot -- ptr u8 n ) {: s:xr-slot :}  s XR-TAIL-PTR  s XR-TLEN@ XR-TAIL-LEN>N ;

\ resolve a surface name to its registry slot; absent = option<xr-slot> none, so a
\ caller that forgets the not-found branch fails certification.
: XR-FIND ( ptr u8 n -- option<xr-slot> ) {: a:ptr u:n :}
   XR-N @ 0 ?do
      a u  i >XR-SLOT XR-SURF@ STR= if  i >XR-SLOT OPTION:SOME  unloop exit  then
   loop  OPTION:NONE ;

\ resolve or fail closed: the slot a tensor definer needs for one extent.
: XR-REQUIRE ( ptr u8 n -- xr-slot )
   XR-FIND MATCH option
      none OF E-EXT-UNDECL throw ENDOF
      some OF ENDOF
   ;MATCH ;

private

: XR-ADD ( n ptr u8 n ptr u8 n -- ) {: val:n sa:ptr su:n ta:ptr tu:n :}
   XR-N @ XR-CAP >= if E-EXT-CAP throw then
   su XR-NAME-CAP > tu XR-NAME-CAP > or if E-EXT-CAP throw then
   XR-N @ >XR-SLOT {: i:xr-slot :}
   sa i XR-SURF-PTR su BYTE-COPY  su i XR-SLOT>N cells XR-SLEN + !
   ta i XR-TAIL-PTR tu BYTE-COPY  tu i XR-SLOT>N cells XR-TLEN + !
   val i XR-SLOT>N cells XR-VAL + !
   XR-N @ 1 + XR-N ! ;

\ ---- #name -> lowercase family tail (extm/extk/...) --------------------------
\ A second CODEGEN buffer, kept separate from XG-BUFFER because EXTENT: reads the
\ mangled tail out of here while it builds the generated word text in XG-BUFFER. The
\ capacity throw stays E-EXT-NAME (a surface name too long for the tail buffer), the
\ code the old hand-rolled append raised. The tail fold is lib/string.f ASCII-LOWER.
XR-NAME-CAP E-EXT-NAME E-EXT-NAME CODEGEN:BUFFER-E XM-BUFFER

: X-MANGLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 2 < if E-EXT-NAME throw then                  \ need '#' + at least one char
   a c@ 35 <> if E-EXT-NAME throw then             \ require the leading '#'
   XM-BUFFER CODEGEN:RESET
   [char] e XM-BUFFER CODEGEN:APPEND-BYTE
   [char] x XM-BUFFER CODEGEN:APPEND-BYTE
   [char] t XM-BUFFER CODEGEN:APPEND-BYTE
   u 1 ?do  a i + c@ ASCII-LOWER XM-BUFFER CODEGEN:APPEND-BYTE  loop   \ lowercase the tail, skip '#'
   XM-BUFFER CODEGEN:CONTENTS ;

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
