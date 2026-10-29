\ structure-certify-suite.f — the checker's construct/access contract for
\ STRUCTURE through real STRUCTURE syntax. The definer parses the live input and
\ resolves only at top-level interpret, so the engine runs it over stdin, like
\ test/structure-decl-suite.f:
\     bin/hb < test/structure-certify-suite.f
\ test/structure-decl-suite.f owns the declaration and rollback front end.

require src/lib/string.f
require src/lib/test.f

T-RESET

\ Diagnostic capture as in test/type-match-suite.f: a JSON-mode render into a
\ fixed buffer, asserted by substring.
create SCG-BUF 8192 allot
: SCG< ( ptr u8 n -- )   \ run one candidate with JSON diags captured; assert rejected
   SCG-BUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;
: SCG? ( ptr u8 n -- )   \ assert the captured diag contains the needle
   T-NEXT
   DIAG-BUFFER$ 2swap CONTAINS? 0= if
      s" diag needle missing" T-ASSERT-DETAIL
   then ;
: SCG-END ( -- ) 0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF ;

\ Each section pairs a positive that certifies with a negative that rejects.
\ A STRUCTURE is a single-shape PRODUCT, so its FAMILY:MAKE/UNMAKE type through
\ the shared ctor machinery (test/type-ctor-suite.f, src/core/sumtype.f
\ TDECL-PROD-WORDS).
\ 1. MAKE/UNMAKE are exact inverses across field kinds the decl suite lacks; a
\ flattened or role-wrong call rejects.
s" STRUCTURE scinr FIELD a n FIELD b n ;STRUCTURE" evaluate-closed           \ width-2 product leaf
s" STRUCTURE scoutr FIELD w scinr FIELD t n ;STRUCTURE" evaluate-closed      \ wide nested field + a cell
s" M1 ( scinr n -- scoutr ) SCOUTR:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" U1 ( scoutr -- scinr n ) SCOUTR:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

s" STRUCTURE scpf FIELD p ptr n FIELD k n ;STRUCTURE" evaluate-closed
s" M2 ( ptr n n -- scpf ) SCPF:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" U2 ( scpf -- ptr n n ) SCPF:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

s" SUMTYPE scnr VARIANT ok n ;VARIANT VARIANT err n ;VARIANT ;SUMTYPE" evaluate-closed
s" STRUCTURE schold FIELD r scnr FIELD t n ;STRUCTURE" evaluate-closed       \ sum-with-payload field
s" M3 ( scnr n -- schold ) SCHOLD:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" U3 ( schold -- scnr n ) SCHOLD:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

s" ENUM scne VARIANT red ;VARIANT VARIANT blue ;VARIANT ;ENUM" evaluate-closed   \ payload-free enum
s" STRUCTURE scef FIELD e scne FIELD t n ;STRUCTURE" evaluate-closed        \ width-1 nested-family field
s" M4 ( scne n -- scef ) SCEF:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" U4 ( scef -- scne n ) SCEF:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" M4N ( n n -- scef ) SCEF:MAKE" CHECK-QUIET-CANDIDATE! 0 T=     \ a plain cell is not the enum

\ 2. A wide nested field makes MAKE consume the whole inner bundle, not its
\ cells; MAKE then UNMAKE is a physical no-op.
s" WW1 ( n n n -- scoutr ) SCOUTR:MAKE" CHECK-QUIET-CANDIDATE! 0 T=   \ flattened (a b t) is not (scinr t)
s" WW2 ( n n -- scoutr ) SCOUTR:MAKE" CHECK-QUIET-CANDIDATE! 0 T=     \ two cells is not a bundle + cell
s" WW3 ( scoutr -- n n n ) SCOUTR:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=  \ UNMAKE yields the bundle, not cells
: SCRT ( -- n n n ) 1 2 SCINR:MAKE 9 SCOUTR:MAKE SCOUTR:UNMAKE >r SCINR:UNMAKE r> ;
SCRT 9 T= 2 T= 1 T=                                  \ t=9 (top), b=2, a=1 round-trip bit-identical

\ 3. Generic field schema: concrete and WIDE instantiations certify; wrong-role
\    or flattened arguments reject; a LINEAR instantiation stays fail-closed at
\    MAKE, UNMAKE and transport. A WIDE UNMAKE at a concrete multi-cell
\    instantiation lowers through hidden-field expansion and is not pinned here.
s" STRUCTURE scg<a> FIELD u a FIELD z n ;STRUCTURE" evaluate-closed
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

\ 4. Pointer and value roles: value-for-ptr and ptr-for-value both reject.
s" STRUCTURE scrole FIELD p ptr n FIELD v n ;STRUCTURE" evaluate-closed
s" R1 ( ptr n n -- scrole ) SCROLE:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" R2 ( n n -- scrole ) SCROLE:MAKE" CHECK-QUIET-CANDIDATE! 0 T=          \ value where ptr n required
s" R3 ( ptr n ptr n -- scrole ) SCROLE:MAKE" CHECK-QUIET-CANDIDATE! 0 T=  \ ptr where value required

\ 5. Forged constructs reject before runtime; whole-bundle transport of a
\ structure value is allowed.
s" STRUCTURE scpt FIELD x n FIELD y n ;STRUCTURE" evaluate-closed
s" STRUCTURE scqt FIELD x n FIELD y n ;STRUCTURE" evaluate-closed                    \ same shape, different identity
s" F1 ( n n -- scpt )" CHECK-QUIET-CANDIDATE! 0 T=                        \ empty body cannot forge
s" F2 ( n -- scpt ) 0" CHECK-QUIET-CANDIDATE! 0 T=                        \ a raw literal cannot forge
s" F3 ( n -- scpt ) SCPT:MAKE" CHECK-QUIET-CANDIDATE! 0 T=                \ one arg, needs two
s" F4 ( n n n -- scpt ) SCPT:MAKE" CHECK-QUIET-CANDIDATE! 0 T=            \ three args, needs two
s" F5 ( scpt -- n n )" CHECK-QUIET-CANDIDATE! 0 T=                        \ cannot split without UNMAKE
s" F6 ( scpt -- scqt ) SCPT:UNMAKE SCQT:MAKE" CHECK-QUIET-CANDIDATE! -1 T=  \ round via cells is the only cross
s" F7 ( scpt -- scqt )" CHECK-QUIET-CANDIDATE! 0 T=                       \ a scpt bundle is not a scqt
s" F8 ( n n -- scqt ) SCPT:MAKE" CHECK-QUIET-CANDIDATE! 0 T=              \ SCPT:MAKE makes scpt, not scqt
s" F9 ( scpt -- scpt scpt ) dup" CHECK-QUIET-CANDIDATE! -1 T=            \ whole-bundle transport allowed

\ 6. A malformed construct's E-MISMATCH names the generated constructor and the
\    field-derived expected types: the reject is field-shape-specific, not an
\    arity count.
s" STRUCTURE scdg FIELD x n FIELD y ptr n ;STRUCTURE" evaluate-closed
s" D1 ( n -- scdg ) SCDG:MAKE" SCG<
s\" \"code\":\"E-MISMATCH\"" SCG?
s\" \"token\":\"SCDG:MAKE\"" SCG?
s\" \"expected\":\"n ptr n \"" SCG?                               \ the exact field-derived shape (x:n, y:ptr n)
SCG-END

\ 7. The derived package is closed but callable: reopening it (any case) or
\    undefining a generated word throws E-CTOR-PROTECTED, as for sum and product
\    constructors.
s" STRUCTURE scprot FIELD x n FIELD y n ;STRUCTURE" evaluate-closed
s" package scprot" TEST-EVAL:RC E-CTOR-PROTECTED T=
s" package SCPROT" TEST-EVAL:RC E-CTOR-PROTECTED T=               \ case-insensitive
s" undefine SCPROT:MAKE" TEST-EVAL:RC E-CTOR-PROTECTED T=
s" undefine scprot:unmake" TEST-EVAL:RC E-CTOR-PROTECTED T=
s" package scok ;package" TEST-EVAL:RC 0 T=                       \ an ordinary package still opens
: SCPRT ( -- n n ) 3 4 SCPROT:MAKE SCPROT:UNMAKE ;               \ still callable after the rejects
SCPRT 4 T= 3 T=

\ 8. In-package public structure: the derived package is (PKG-TAIL), its words
\    are addressable from global scope, and the declaring package's state
\    survives generation.
package scpkg
public
STRUCTURE prow FIELD v n FIELD w n ;STRUCTURE
private
;package
s" IPK ( n n -- scpkg:prow ) SCPKG-PROW:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" IPU ( scpkg:prow -- n n ) SCPKG-PROW:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ 9. Derived equality and hash. A declaration checks its whole generated plan in
\    one candidate scope before one evaluate publishes it; HASH calls the
\    structure's UNMAKE, which binds for the later rows of its scope
\    (src/core/checker.f CK-PENDING-SYM). A scope whose rows did not bind
\    refuses the declaration (`bad structure declaration`, 70).
s" STRUCTURE scder DERIVE eq hash FIELD x n ;STRUCTURE" TEST-EVAL:RC 0 T=
: SCDEQ ( n n -- bool ) {: a:n b:n :} a SCDER:MAKE b SCDER:MAKE SCDER:EQ ;
: SCDHS ( n n -- bool ) {: a:n b:n :} a SCDER:MAKE SCDER:HASH b SCDER:MAKE SCDER:HASH = ;
5 5 SCDEQ TTRUE
5 6 SCDEQ TFALSE
5 5 SCDHS TTRUE

T-REPORT
