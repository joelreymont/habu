\ structure-certify-suite.f — checker certify suite for STRUCTURE construct /
\ access, driven END-TO-END from the STRUCTURE grammar surface (dot
\ habu-checker-type-structure-d996215b, the STRUCTURE twin of the ENUM certify
\ leaf in the checker-certify-unified chain). Run BY THE ENGINE over stdin, like
\ test/structure-decl-suite.f (the definer parses the live input stream and
\ mutates the type registry, so it resolves only at top-level interpret):
\     bin/hb < test/structure-certify-suite.f
\
\ This is the CERTIFY complement to the two whitebox STRUCTURE suites and the
\ field-projection suite — it adds NO grammar, NO generator, NO checker word; it
\ pins the checker's construct/access CONTRACT through real STRUCTURE syntax:
\   - test/structure-decl-suite.f owns the declaration/rollback front end and the
\     end-to-end MAKE/UNMAKE seam for cell/byte/generic fields;
\   - test/structure-make-suite.f owns the whitebox generator + role/arity matrix;
\   - test/field-proj-suite.f owns the field-ACCESS armed window (the accessor the
\     generate-field lane will emit; see the accessor note at the file end).
\ Everything here is verified against the checker as it types the LEGACY product
\ path (test/type-ctor-suite.f item 15, sumtype.f TDECL-PROD-WORDS): a STRUCTURE
\ is a single-shape PRODUCT, so its generated FAMILY:MAKE/UNMAKE type through the
\ same shared ctor machinery — this suite proves the STRUCTURE grammar reaches it
\ IDENTICALLY across the slices type-ctor-suite pins for PRODUCT, on the field
\ shapes only the STRUCTURE front end can declare.
\
\ Certify slices (dot Desc), each red-first (a positive that certifies AND a
\ negative that rejects, so a checker regression flips a case):
\   1. MAKE/UNMAKE exact inverses across field kinds the decl-suite lacks: a wide
\      nested product field, a ptr field, a nested sum-with-payload field.
\   2. Whole-layout widths: a wide nested field makes MAKE consume the whole
\      bundle (not its flattened cells), UNMAKE yields it, round-trips preserve
\      declaration order + values.
\   3. Generic field schema instantiation: concrete + WIDE instantiation certify,
\      wrong-role/flattened reject, a linear instantiation stays fail-closed.
\   4. Pointer / value roles: a ptr field certifies; value-for-ptr and
\      ptr-for-value both reject.
\   5. Malformed / forged construct rejects BEFORE runtime: raw cells cannot forge
\      a bundle, wrong arity rejects, a foreign-family bundle cannot alias, a
\      bundle cannot split without UNMAKE; the whole-bundle transport is allowed.
\   6. Checker diagnostics: a malformed construct emits E-MISMATCH naming the
\      generated constructor token and the exact field-derived expected types.
\   7. Constructor-package protection: the STRUCTURE-derived package is
\      closed-but-callable — reopen/undefine reject case-insensitively, the words
\      stay callable — matching the shipped sum/product ctor protection.
\ A failure prints F<index> + detail; REPORT exits 1 on any fail.

require test/checker-assert.f

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;
: T-TRUE ( bool -- ) {: b:bool :}
   #CASE @ 1 + #CASE !
   b 0= if T-FAIL s" assert: expected true" type cr then ;

\ --- boundary shims (the test/structure-decl-suite.f idiom): the STRUCTURE
\ opener and evaluate are reached at top level through named trusted forwarders.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: TRY ( ptr u8 n -- n ) ['] EV catch ;

\ --- diagnostic capture (the test/type-match-suite.f idiom): a JSON-mode render
\ into a fixed buffer, asserted by substring.
variable MDT-I
: MDT-CONTAINS? ( ptr u8 n ptr u8 n -- bool ) {: h:ptr hu:n n:ptr nu:n :}
   hu nu < if 0 0= 0= exit then
   0 MDT-I !
   begin MDT-I @ nu + hu <= while
      h MDT-I @ + nu  n nu CORE-STR= if 0 0= exit then
      MDT-I @ 1 + MDT-I !
   repeat 0 0= 0= ;
create SCG-BUF 8192 allot
: SCG< ( ptr u8 n -- )   \ run one candidate with JSON diags captured; assert rejected
   SCG-BUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;
: SCG? ( ptr u8 n -- )   \ assert the captured diag contains the needle
   #CASE @ 1 + #CASE !
   DIAG-BUFFER$ 2swap MDT-CONTAINS? 0= if
      T-FAIL s" assert: diag needle missing" type cr
   then ;
: SCG-END ( -- ) 0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF ;

\ ---------------------------------------------------------------------------
\ 1. Exact-inverse MAKE/UNMAKE across the field kinds the decl-suite does not
\    cover, from real STRUCTURE syntax: a WIDE nested product field, a ptr
\    field, and a nested sum-with-payload field. Each MAKE/UNMAKE pair certifies
\    as declaration-order inverses; a flattened/rolewrong call rejects.
\ ---------------------------------------------------------------------------
s" STRUCTURE scinr 0 FIELD a n FIELD b n ;STRUCTURE" EV           \ width-2 product leaf
s" STRUCTURE scoutr 0 FIELD w scinr FIELD t n ;STRUCTURE" EV      \ wide nested field + a cell
s" M1 ( scinr n -- scoutr ) SCOUTR:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" U1 ( scoutr -- scinr n ) SCOUTR:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

s" STRUCTURE scpf 0 FIELD p ptr n FIELD k n ;STRUCTURE" EV
s" M2 ( ptr n n -- scpf ) SCPF:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" U2 ( scpf -- ptr n n ) SCPF:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

s" SUMTYPE scnr 0 VARIANT ok n ;VARIANT VARIANT err n ;VARIANT ;SUMTYPE" EV
s" STRUCTURE schold 0 FIELD r scnr FIELD t n ;STRUCTURE" EV       \ sum-with-payload field
s" M3 ( scnr n -- schold ) SCHOLD:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" U3 ( schold -- scnr n ) SCHOLD:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ ---------------------------------------------------------------------------
\ 2. Whole-layout widths. The wide nested field makes MAKE consume the WHOLE
\    inner bundle, not its flattened cells; UNMAKE yields the whole bundle; and
\    MAKE->UNMAKE is a physical no-op preserving declaration order + values.
\ ---------------------------------------------------------------------------
s" WW1 ( n n n -- scoutr ) SCOUTR:MAKE" CHECK-QUIET-CANDIDATE! 0 T=   \ flattened (a b t) is not (scinr t)
s" WW2 ( n n -- scoutr ) SCOUTR:MAKE" CHECK-QUIET-CANDIDATE! 0 T=     \ two cells is not a bundle + cell
s" WW3 ( scoutr -- n n n ) SCOUTR:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=  \ UNMAKE yields the bundle, not cells
: SCRT ( -- n n n ) 1 2 SCINR:MAKE 9 SCOUTR:MAKE SCOUTR:UNMAKE >r SCINR:UNMAKE r> ;
SCRT 9 T= 2 T= 1 T=                                  \ t=9 (top), b=2, a=1 round-trip bit-identical

\ ---------------------------------------------------------------------------
\ 3. Generic field schema instantiation from real syntax. A structure with a
\    parameter field certifies at a concrete AND a WIDE instantiation; a
\    wrong-role or flattened arg rejects; the concrete-cell and generic-param
\    forms round-trip as inverses; a LINEAR instantiation stays fail-closed at
\    MAKE, UNMAKE, and whole-bundle transport (v1 discipline). (A WIDE UNMAKE at
\    a concrete multi-cell instantiation lowers through the layout-cap lane's
\    hidden-field expansion, certified there; only the WIDE MAKE is pinned here.)
\ ---------------------------------------------------------------------------
s" STRUCTURE scg 1 FIELD u a FIELD z n ;STRUCTURE" EV
s" G1 ( n n -- scg<n> ) SCG:MAKE" CHECK-QUIET-CANDIDATE! -1 T=            \ concrete a=n
s" G2 ( ptr u8 n -- scg<n> ) SCG:MAKE" CHECK-QUIET-CANDIDATE! 0 T=        \ wrong role: u=n not ptr u8
s" G3 ( scinr n -- scg<scinr> ) SCG:MAKE" CHECK-QUIET-CANDIDATE! -1 T=    \ WIDE instantiation a=scinr
s" G4 ( n n n -- scg<scinr> ) SCG:MAKE" CHECK-QUIET-CANDIDATE! 0 T=       \ flattened inner rejects
s" GUC ( scg<n> -- n n ) SCG:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=         \ concrete-cell UNMAKE inverts MAKE
s" GUG ( scg<a> -- a n ) SCG:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=         \ generic-form UNMAKE (open param)
: SCGRT ( -- n n ) 42 7 SCG:MAKE SCG:UNMAKE ;
SCGRT 7 T= 42 T=                                     \ MAKE->UNMAKE round-trips u=42 z=7
s" GL1 ( own n -- scg<own> ) SCG:MAKE" CHECK-QUIET-CANDIDATE! 0 T=        \ linear inst fail-closed
s" GL2 ( scg<own> -- scg<own> )" CHECK-QUIET-CANDIDATE! 0 T=              \ linear transport fail-closed
s" GL3 ( scg<own> -- own n ) SCG:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ 4. Pointer / value roles. A ptr field certifies against ptr T; a plain cell
\    where ptr T is required, and ptr T where a cell is required, both reject.
\ ---------------------------------------------------------------------------
s" STRUCTURE scrole 0 FIELD p ptr n FIELD v n ;STRUCTURE" EV
s" R1 ( ptr n n -- scrole ) SCROLE:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" R2 ( n n -- scrole ) SCROLE:MAKE" CHECK-QUIET-CANDIDATE! 0 T=          \ value where ptr n required
s" R3 ( ptr n ptr n -- scrole ) SCROLE:MAKE" CHECK-QUIET-CANDIDATE! 0 T=  \ ptr where value required

\ ---------------------------------------------------------------------------
\ 5. Malformed / forged construct rejects BEFORE runtime. Raw cells cannot
\    fabricate a bundle (the pending window is closed), wrong arity rejects, a
\    foreign-family bundle cannot alias, and a bundle cannot split without
\    UNMAKE; the whole-bundle transport of a structure value IS allowed.
\ ---------------------------------------------------------------------------
s" STRUCTURE scpt 0 FIELD x n FIELD y n ;STRUCTURE" EV
s" STRUCTURE scqt 0 FIELD x n FIELD y n ;STRUCTURE" EV                    \ same shape, different identity
s" F1 ( n n -- scpt )" CHECK-QUIET-CANDIDATE! 0 T=                        \ empty body cannot forge
s" F2 ( n -- scpt ) 0" CHECK-QUIET-CANDIDATE! 0 T=                        \ a raw literal cannot forge
s" F3 ( n -- scpt ) SCPT:MAKE" CHECK-QUIET-CANDIDATE! 0 T=                \ one arg, needs two
s" F4 ( n n n -- scpt ) SCPT:MAKE" CHECK-QUIET-CANDIDATE! 0 T=            \ three args, needs two
s" F5 ( scpt -- n n )" CHECK-QUIET-CANDIDATE! 0 T=                        \ cannot split without UNMAKE
s" F6 ( scpt -- scqt ) SCPT:UNMAKE SCQT:MAKE" CHECK-QUIET-CANDIDATE! -1 T=  \ round via cells is the only cross
s" F7 ( scpt -- scqt )" CHECK-QUIET-CANDIDATE! 0 T=                       \ a scpt bundle is not a scqt
s" F8 ( n n -- scqt ) SCPT:MAKE" CHECK-QUIET-CANDIDATE! 0 T=              \ SCPT:MAKE makes scpt, not scqt
s" F9 ( scpt -- scpt scpt ) dup" CHECK-QUIET-CANDIDATE! -1 T=            \ whole-bundle transport allowed

\ ---------------------------------------------------------------------------
\ 6. Checker diagnostics: a malformed construct is rejected with a structured
\    E-MISMATCH that names the generated constructor token and renders the exact
\    field-derived expected types (proof the reject is field-shape-specific, not
\    a bare arity count).
\ ---------------------------------------------------------------------------
s" STRUCTURE scdg 0 FIELD x n FIELD y ptr n ;STRUCTURE" EV
s" D1 ( n -- scdg ) SCDG:MAKE" SCG<
s\" \"code\":\"E-MISMATCH\"" SCG?
s\" \"token\":\"SCDG:MAKE\"" SCG?
s\" \"expected\":\"n ptr n \"" SCG?                               \ the exact field-derived shape (x:n, y:ptr n)
SCG-END

\ ---------------------------------------------------------------------------
\ 7. Constructor-package protection. The STRUCTURE-derived package is
\    closed-but-callable: reopening it (any case) and undefining a generated word
\    reject with E-CTOR-PROTECTED, and the words stay callable afterwards —
\    matching the shipped sum/product ctor protection.
\ ---------------------------------------------------------------------------
s" STRUCTURE scprot 0 FIELD x n FIELD y n ;STRUCTURE" EV
s" package scprot" TRY E-CTOR-PROTECTED T=
s" package SCPROT" TRY E-CTOR-PROTECTED T=                        \ case-insensitive
s" undefine SCPROT:MAKE" TRY E-CTOR-PROTECTED T=
s" undefine scprot:unmake" TRY E-CTOR-PROTECTED T=
s" package scok ;package" TRY 0 T=                                \ an ordinary package still opens
: SCPRT ( -- n n ) 3 4 SCPROT:MAKE SCPROT:UNMAKE ;               \ still callable after the rejects
SCPRT 4 T= 3 T=

\ ---------------------------------------------------------------------------
\ 8. In-package public structure: the derived package name is (PKG-TAIL), the
\    generated words are addressable from global scope, and declaring-package
\    state survives generation.
\ ---------------------------------------------------------------------------
package scpkg
public
STRUCTURE prow 0 FIELD v n FIELD w n ;STRUCTURE
private
;package
s" IPK ( n n -- scpkg:prow ) SCPKG-PROW:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" IPU ( scpkg:prow -- n n ) SCPKG-PROW:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ ---------------------------------------------------------------------------
\ ACCESSOR NOTE (report + follow-up). The generate-field lane will emit sealed
\ FAMILY:FIELD accessor words ( ptr family<args> -- ptr field-type ) that fire
\ the checker's field-projection armed window (src/core/checker.f, pinned by
\ test/field-proj-suite.f). When that lands, this suite gains a section proving
\ each accessor certifies its projected pointee type from real STRUCTURE syntax
\ (positive read-back per field kind: cell, ptr, wide-nested, generic-inst) and
\ that a forged offset / foreign-family pointer / role mismatch rejects — the
\ grammar-surface certify of the access half, mirroring §1-6 for the construct
\ half. The armed window and its direct negatives already exist; only the
\ end-to-end grammar fixtures wait on the generated accessor words.
\ ---------------------------------------------------------------------------

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" structure-certify-suite: failures" 1 die ;
REPORT
