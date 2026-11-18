split-str
\ structure-decl-suite.f - behavior and rollback suite for the STRUCTURE
\ declaration front end (src/core/structure-decl.f, package STRUCTURE-DECL).
\ It asserts reject codes, rendered diagnostics, field-row counts and the
\ generated words. A failure prints F<index> and detail; T-REPORT exits 1.

require src/habu/verify-source.f
require test/replay-scope.f
require test/decl-diag-capture.f   \ DECL-DIAG: the check tool's own declaration-packet capture
require src/lib/test.f
require src/lib/test/rejection-text.f
require src/lib/vector.f
require src/lib/heap.f
require src/lib/string.f

\ Capture every reject's declaration diagnostic from the start; the last
\ section asserts the rendered lines.
DECL-DIAG:PROSE

T-RESET

variable B   variable PFB

\ 6. A duplicate field name rejects through the field record (E-TFAM-DUP 7102)
\    and the provisional field rows roll back. hb's own type entries are read
\    first: a declaration reads every pending one before it opens.
TFAM:READ-PENDING
TYPE-FIELD:COUNT PFB !
DECL-DIAG:PROSE
s" STRUCTURE dupf FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `dupf`\n" DECL-DIAG:HAS? TTRUE
s" z" s" duplicate field name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each field of the structure its own name\n" DECL-DIAG:HAS? TTRUE
TYPE-FIELD:COUNT PFB @ T=                             \ committed field rows retired

\ 7. Grammar / name / arity / type / policy / derive / terminator rejects, each
\    at the offending token with its reason.
DECL-DIAG:PROSE
s" STRUCTURE field ;STRUCTURE" TEST-EVAL:RC 70 T=            \ reserved keyword name
s\" error[E-BAD-DECLARATION]: bad structure declaration `field`\n" DECL-DIAG:HAS? TTRUE
s" field" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: pick another name: a built-in type such as `n` cannot name a family\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE n ;STRUCTURE" TEST-EVAL:RC 70 T=                \ single-letter type name
s\" error[E-BAD-DECLARATION]: bad structure declaration `n`\n" DECL-DIAG:HAS? TTRUE
s" n" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: pick another name: a built-in type such as `n` cannot name a family\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE ptr FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=    \ the pointer constructor's tail
s\" error[E-BAD-DECLARATION]: bad structure declaration `ptr`\n" DECL-DIAG:HAS? TTRUE
s" ptr" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: pick another name: a built-in type such as `n` cannot name a family\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE Bad ;STRUCTURE" TEST-EVAL:RC 70 T=              \ upper-case name (case)
s\" error[E-BAD-DECLARATION]: bad structure declaration `Bad`\n" DECL-DIAG:HAS? TTRUE
s" Bad" s" name must be a lowercase family tail" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write the name as a lowercase tail with no package qualifier, as in `point`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE foo<q,q> ;STRUCTURE" TEST-EVAL:RC 70 T=              \ duplicate parameter
s\" error[E-BAD-DECLARATION]: bad structure declaration `foo<q,q>`\n" DECL-DIAG:HAS? TTRUE
s" foo<q,q>" s" invalid type parameter header" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name each type parameter once in the header, as in `STRUCTURE box<a>` or `STRUCTURE pair<a,b>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE foo<a,> ;STRUCTURE" TEST-EVAL:RC 70 T=             \ trailing comma in parameter list
s\" error[E-BAD-DECLARATION]: bad structure declaration `foo<a,>`\n" DECL-DIAG:HAS? TTRUE
s" foo<a,>" s" invalid type parameter header" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name each type parameter once in the header, as in `STRUCTURE box<a>` or `STRUCTURE pair<a,b>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE foo FIELD a nope ;STRUCTURE" TEST-EVAL:RC 70 T=  \ unresolved field type
s\" error[E-BAD-DECLARATION]: bad structure declaration `foo`\n" DECL-DIAG:HAS? TTRUE
s" nope" s" unknown type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name a type declared before this declaration: `n`, `^u8`, a header parameter, a" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE foo FIELD a Q ;STRUCTURE" TEST-EVAL:RC 70 T=    \ upper-case single-letter type
s\" error[E-BAD-DECLARATION]: bad structure declaration `foo`\n" DECL-DIAG:HAS? TTRUE
s" Q" s" unknown type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: list the parameter in the header, as in `STRUCTURE box<a>`, before a field names it\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE foo FIELD a a ;STRUCTURE" TEST-EVAL:RC 70 T=    \ parameter outside declared arity
s\" error[E-BAD-DECLARATION]: bad structure declaration `foo`\n" DECL-DIAG:HAS? TTRUE
s" a" s" unknown type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: list the parameter in the header, as in `STRUCTURE box<a>`, before a field names it\n" DECL-DIAG:HAS? TTRUE
NEWTYPE qreq
NEWTYPE qres
DECL-DIAG:PROSE
s" STRUCTURE qrollback FIELD handler [ qreq qres ] ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `qrollback`\n" DECL-DIAG:HAS? TTRUE
s" ]" s" the quotation type needs `--` here" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write the quotation type as `[ in -- out ]`\n" DECL-DIAG:HAS? TTRUE
s" STRUCTURE qrollback FIELD handler [ qreq qres -- ] ;STRUCTURE" TEST-EVAL:RC 0 T=
s" QROLLBACK-RT ( [ qreq qres -- ] -- qrollback ) QROLLBACK:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
\ A field, payload or base may point at a quotation; the code a field's
\ pointer reaches runs, and an unclosed quotation is rejected at its `[`.
STRUCTURE sdqptr DERIVE addr FIELD run ^[ n -- n ] ;STRUCTURE
TYPED-VARIABLE SDQ-CODE [ n -- n ]
1 TYPED-BUFFER SDQ-REC sdqptr
: SDQ-RUN ( n -- n )
   [: 3 * ;] SDQ-CODE !
   SDQ-CODE 0 SDQ-REC SDQPTR:RUN !
   0 SDQ-REC SDQPTR:RUN @ @ execute ;
5 SDQ-RUN 15 T=
s" ENUM sdqenum VARIANT one FIELD run ^[ n -- n ] ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" NEWTYPE sdqbase ^[ n -- n ]" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s\" NEWTYPE sdqwrap option<n,\n  n>" TEST-EVAL:RC 70 T=
s" option<n," s" type has too many arguments" DECL-DIAG:CAUSE? TTRUE
s\" NEWTYPE sdqok option<\n  n>" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" STRUCTURE sdqopen FIELD run ^[ n -- n ;STRUCTURE" TEST-EVAL:RC 70 T=
s" [" s" the quotation type is not closed" DECL-DIAG:CAUSE? TTRUE
\ A quotation's tokens are a signature's: `[` and `]` need no blank beside
\ them, and a rejection stands at the element or application it rejects.
s" STRUCTURE sdqcaret FIELD run ^[n -- n] ;STRUCTURE" TEST-EVAL:RC 0 T=
s" SDQCARET-RT ( ^[ n -- n ] -- sdqcaret ) SDQCARET:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" STRUCTURE sdqclose FIELD run [ n -- n] ;STRUCTURE" TEST-EVAL:RC 0 T=
s" SDQCLOSE-RT ( [ n -- n ] -- sdqclose ) SDQCLOSE:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" STRUCTURE sdqopt FIELD run option<[ n -- n]> ;STRUCTURE" TEST-EVAL:RC 0 T=
s" SDQOPT-RT ( option<[ n -- n ]> -- sdqopt ) SDQOPT:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
DECL-DIAG:PROSE
s" STRUCTURE rqc FIELD f [ n option<n,n> -- n ] ;STRUCTURE" TEST-EVAL:RC 70 T=
s" option<n,n>" s" type has too many arguments" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE rqm FIELD f [ n , n -- ] ;STRUCTURE" TEST-EVAL:RC 70 T=
s" ," s" malformed type application" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE rqx FIELD f option<n>x ;STRUCTURE" TEST-EVAL:RC 70 T=
s" x" s" unexpected bytes after a whole type" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s\" STRUCTURE rvk2\n  FIELD f option<n>[ n --\n    n ]\n;STRUCTURE" TEST-EVAL:RC 70 T=
s" [ n --" s" unexpected bytes after a whole type" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE rqe FIELD f option<[ n -- n ],n> ;STRUCTURE" TEST-EVAL:RC 70 T=
s" option<[ n -- n ],n>" s" type has too many arguments" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE foo VARIANT q ;VARIANT ;STRUCTURE" TEST-EVAL:RC 70 T=  \ mixed legacy token
s\" error[E-BAD-DECLARATION]: bad structure declaration `foo`\n" DECL-DIAG:HAS? TTRUE
s" VARIANT" s" unexpected token in structure declaration" DECL-DIAG:CAUSE? TTRUE
s\"   = help: start each clause with `FIELD`, `POLICY`, `DERIVE` or `OPAQUE`, and end with `;STRUCTURE`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE foo FIELD x n" TEST-EVAL:RC 70 T=               \ missing ;STRUCTURE
s\" error[E-BAD-DECLARATION]: bad structure declaration `foo`\n" DECL-DIAG:HAS? TTRUE
s" foo" s" missing ;STRUCTURE" DECL-DIAG:CAUSE? TTRUE
s\"   = help: end the declaration with `;STRUCTURE` after its last field\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE foo POLICY nope FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T= \ unknown layout policy
s\" error[E-BAD-DECLARATION]: bad structure declaration `foo`\n" DECL-DIAG:HAS? TTRUE
s" nope" s" unknown layout policy" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE foo DERIVE nope FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T= \ unknown derive feature
s\" error[E-BAD-DECLARATION]: bad structure declaration `foo`\n" DECL-DIAG:HAS? TTRUE
s" nope" s" unknown derive feature" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name `eq`, `hash`, `addr` or `init` after `DERIVE`\n" DECL-DIAG:HAS? TTRUE

\ A throw out of the text that opened a BEGIN-STRUCTURE abandons it, so the next
\ text opens its own; a text that ends with one still open rejects it.
s" BEGIN-STRUCTURE P NO-SUCH-WORD" TEST-EVAL:RC 70 T=
s" BEGIN-STRUCTURE Q END-STRUCTURE" TEST-EVAL:RC 0 T=
s" BEGIN-STRUCTURE R" TEST-EVAL:RC 70 T=

\ An effect reads the token `ptr` ahead of any family, so the tail is reserved
\ ahead of the global `ptr` family's duplicate check (7102), and a package may
\ not shadow it as it may shadow any other global family.
package SDPTR
DECL-DIAG:PROSE
s" STRUCTURE ptr FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `ptr`\n" DECL-DIAG:HAS? TTRUE
s" ptr" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: pick another name: a built-in type such as `n` cannot name a family\n" DECL-DIAG:HAS? TTRUE
;package

\ 8. A duplicate family name rejects (E-TFAM-DUP 7102 from TFAM-DECL).
s" STRUCTURE twice FIELD x n ;STRUCTURE" evaluate-closed
DECL-DIAG:PROSE
s" STRUCTURE twice FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `twice`\n" DECL-DIAG:HAS? TTRUE
s" twice" s" duplicate family" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: this package already declares a family with this name\n" DECL-DIAG:HAS? TTRUE

\ 10. A public STRUCTURE with fields generates a sealed MAKE/UNMAKE ctor package;
\     MAKE then UNMAKE round-trips the fields bit-identically in declaration order.
s" STRUCTURE tri FIELD a n FIELD b n FIELD c n ;STRUCTURE" evaluate-closed
: TRIRT ( n n n -- n n n ) TRI:MAKE TRI:UNMAKE ;      \ callable sealed ctor words
11 22 33 TRIRT 33 T= 22 T= 11 T=                      \ declaration order + values round-trip bit-identically

\ 11. MAKE/UNMAKE are exact type inverses across a generic, cell and byte field
\     mix at a concrete instantiation (a = n).
s" STRUCTURE abc<a> FIELD p a FIELD k n FIELD c char ;STRUCTURE" evaluate-closed
s" ABCRT ( n n char -- n n char ) ABC:MAKE ABC:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
\ a generic-only structure round-trips its parameter at a concrete instantiation.
s" STRUCTURE gp<a> FIELD v a ;STRUCTURE" evaluate-closed
: GPRT ( n -- n ) GP:MAKE GP:UNMAKE ;
42 GPRT 42 T=

\ The STRUCTURE parser consumes the shared declaration alphabet. A maximum-arity
\ declaration accepts g and z while f/n/r stay scalar fields.
s" STRUCTURE sdmap<a,b,c,d,e,g,h,i,j,k,l,m,o,p,q,s,t,u,v,w,x,y,z> FIELD p00 a FIELD p01 b FIELD p02 c FIELD p03 d FIELD p04 e FIELD p05 g FIELD flag f FIELD integer n FIELD real r FIELD last z ;STRUCTURE" evaluate-closed
s" SDMAPRT ( n n n n n n bool n r char -- n n n n n n bool n r char ) SDMAP:MAKE SDMAP:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" SDMAPBAD ( n n n n n bool n n r char -- n n n n n bool n n r char ) SDMAP:MAKE SDMAP:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=

\ 12. Only a structure with fields gets a ctor package (SD-MAKEABLE?): a zero-field
\     family declares, and its MAKE is an unknown word (0).
DECL-DIAG:PROSE
s" STRUCTURE badf FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 70 T=  \ duplicate field rejects
s\" error[E-BAD-DECLARATION]: bad structure declaration `badf`\n" DECL-DIAG:HAS? TTRUE
s" z" s" duplicate field name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each field of the structure its own name\n" DECL-DIAG:HAS? TTRUE
s" STRUCTURE opaque ;STRUCTURE" evaluate-closed                  \ zero-field opaque one-cell family (docs/forth.md "Structures And Enums")
s" SDOPQMK ( -- opaque ) OPAQUE:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOPQLD ( ^opaque -- opaque ) @" CHECK-QUIET-CANDIDATE! -1 T=   \ its one cell loads

\ 13. A PRIVATE structure with fields generates FAMILY-MAKE/FAMILY-UNMAKE
\     (src/core/type-family.f TF-CTOR-PRIV$) into the declaring package's
\     private wordlist; a public family gets FAMILY:MAKE in its global ctor
\     namespace. A colon name cannot land in a private wordlist: the engine
\     routes a qualified definition to the namespace's public wid, and the
\     checker records a single-colon name as the package's public symbol.
package SDPRIVTEST
private
STRUCTURE sdpkw DERIVE addr FIELD export n FIELD i n ;STRUCTURE   \ private accessors SDPKW-EXPORT, SDPKW-I
STRUCTURE hidden FIELD x n FIELD y n ;STRUCTURE
: HRT ( n n -- n n ) HIDDEN-MAKE HIDDEN-UNMAKE ;      \ callable from the owning package
: HAFTER ( -- n ) 7 ;                                 \ the package still defines privately
public
: HGO ( n n -- n n ) HRT ;
: HTAIL ( -- n ) HAFTER ;
;package
11 22 SDPRIVTEST:HGO 22 T= 11 T=                      \ declaration order + values round-trip
SDPRIVTEST:HTAIL 7 T=                                 \ generation did not seal the private wordlist

\ A second package and the global scope cannot resolve them: the probe is refused
\ as an undefined word (0). Contrasts: the same probe certifies inside
\ the declaring package, and case 10's public family certifies from every scope.
package SDPRIVTEST
private
s" SDPI ( n n -- n n ) HIDDEN-MAKE HIDDEN-UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
public
;package
package SDPRIVOTHER
private
s" SDPO ( n n -- n n ) HIDDEN-MAKE HIDDEN-UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDPT ( n n n -- n n n ) TRI:MAKE TRI:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
public
;package
s" SDPG ( n n -- n n ) HIDDEN-MAKE HIDDEN-UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDPGT ( n n n -- n n n ) TRI:MAKE TRI:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ While its package is open, a private product's MAKE and UNMAKE refuse undefine
\ by name, E-CTOR-PROTECTED; a word only sharing their prefix does not.
package SDPRIVTEST
private
DECL-DIAG:PROSE
s" undefine HIDDEN-MAKE" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `HIDDEN-MAKE`" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" undefine HIDDEN-UNMAKE" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `HIDDEN-UNMAKE`" DECL-DIAG:HAS? TTRUE
: HIDDEN-SIZE ( -- n ) 3 ;                            \ the family tail, a member nobody generates
HIDDEN-SIZE 3 T=
s" undefine HIDDEN-SIZE" TEST-EVAL:RC 0 T=            \ so it is the package's own word
public
;package

\ A private SUM generates its variants' words into the declaring package's
\ private wordlist; COLOUR-MAKE, a product's member spelling no row of colour
\ names, stays the package's own.
package SDPRIVSUM
private
SUMTYPE colour VARIANT red ;VARIANT VARIANT green ;VARIANT ;SUMTYPE
: COLOUR-MAKE ( -- n ) 6 ;
COLOUR-MAKE 6 T=
s" undefine COLOUR-MAKE" TEST-EVAL:RC 0 T=
STRUCTURE veil ;STRUCTURE                           \ an opaque product publishes no member pair
: VEIL-MAKE ( -- n ) 4 ;
VEIL-MAKE 4 T=
s" undefine VEIL-MAKE" TEST-EVAL:RC 0 T=
public
;package

\ A public family in part A:B has its constructor package in the part A:B-F of
\ A: its words are B-F:TAIL inside A and its parts, and nothing outside A
\ reaches them, A:B-F:TAIL included.
package SDPARTFAM
package BB
public
STRUCTURE inner FIELD x n ;STRUCTURE
private
;package
: AGO ( n -- n ) BB-INNER:MAKE BB-INNER:UNMAKE ;
package CC
public
: CGO ( n -- n ) BB-INNER:MAKE BB-INNER:UNMAKE ;
private
;package
public
: GO ( n -- n ) AGO ;
: PART-GO ( n -- n ) CC:CGO ;
;package
4 SDPARTFAM:GO 4 T=
4 SDPARTFAM:PART-GO 4 T=
s" 4 BB-INNER:MAKE" TEST-EVAL:RC 70 T=
s" 4 SDPARTFAM:BB-INNER:MAKE" TEST-EVAL:RC 70 T=

\ A public family whose field has the part's type derives eq through the part's
\ family's EQ.
package SDPARTEQ
package BB
public
STRUCTURE inner DERIVE eq FIELD x n ;STRUCTURE
private
;package
public
STRUCTURE outer DERIVE eq FIELD i BB:inner ;STRUCTURE
: SAME? ( n n -- bool )
   swap BB-INNER:MAKE SDPARTEQ-OUTER:MAKE
   swap BB-INNER:MAKE SDPARTEQ-OUTER:MAKE  SDPARTEQ-OUTER:EQ ;
;package
4 4 SDPARTEQ:SAME? TTRUE
4 5 SDPARTEQ:SAME? TFALSE

\ A part's constructor package is judged by the part's row, not by a sealed
\ top-level package of the same name: BODY-ITEMS is an engine package.
package SDPARTSEAL
package BODY
public
STRUCTURE items FIELD x n ;STRUCTURE
private
;package
public
: GO ( n -- n ) BODY-ITEMS:MAKE BODY-ITEMS:UNMAKE ;
;package
6 SDPARTSEAL:GO 6 T=

\ Neither a top-level package nor a part of a public family's top package may
\ bear the family's constructor package name; whichever comes second is rejected.
package SDRESA
package SDRESA-PT ;package
public
DECL-DIAG:PROSE
s" STRUCTURE pt FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
s" pt" s" a part of this package already bears its constructor package's name" DECL-DIAG:CAUSE? TTRUE
private
;package
package SDRESB
public
STRUCTURE pt FIELD x n ;STRUCTURE
private
;package
\ A part opens only in the text that opens its package.
DECL-DIAG:PROSE
s" package SDRESB package SDRESB-PT ;package ;package" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `SDRESB:SDRESB-PT` bears the name of the constructor package of the family `SDRESB:pt`" DECL-DIAG:HAS? TTRUE
s" as `package SDRESB-PT-WORDS`" DECL-DIAG:HAS? TTRUE
s" SDRESB-PT" s" constructor package" DECL-DIAG:CAUSE? TTRUE
package SDRESC
package BB-INNER ;package
package BB
public
DECL-DIAG:PROSE
s" STRUCTURE inner FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
s" inner" s" a part of this package already bears its constructor package's name" DECL-DIAG:CAUSE? TTRUE
private
;package
;package
package SDRESH-PT ;package
package SDRESH
public
DECL-DIAG:PROSE
s" STRUCTURE pt FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
s" pt" s" a package already bears its constructor package's name" DECL-DIAG:CAUSE? TTRUE
s" = word: SDRESH-PT" DECL-DIAG:HAS? TTRUE
s" = help: rename this family or that package" DECL-DIAG:HAS? TTRUE
private
;package
: SDRESJ-PT:HELLO ( -- n ) 1 ;
package SDRESJ
public
DECL-DIAG:PROSE
s" STRUCTURE pt FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
s" pt" s" a package already bears its constructor package's name" DECL-DIAG:CAUSE? TTRUE
private
;package
package SDRESI
public
STRUCTURE pt FIELD x n ;STRUCTURE
private
;package
DECL-DIAG:PROSE
s" package SDRESI-PT ;package" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `SDRESI-PT` bears the name of the constructor package of the family `SDRESI:pt`" DECL-DIAG:HAS? TTRUE
s" SDRESI-PT" s" constructor package" DECL-DIAG:CAUSE? TTRUE
\ A SUMTYPE before it leaves no name the STRUCTURE's rejection reads.
package SDRESG
package SDRESG-PT ;package
public
SUMTYPE stale VARIANT one n ;VARIANT ;SUMTYPE
DECL-DIAG:PROSE
s" STRUCTURE pt FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
s" pt" s" a part of this package already bears its constructor package's name" DECL-DIAG:CAUSE? TTRUE
s" = word:" DECL-DIAG:HAS? TFALSE
private
;package
\ A part named as its package: its family's constructor package `SDRESE:SDRESE-PT`
\ and the package's own family's `SDRESE-PT` share a name, in either order.
package SDRESE
package SDRESE
public
STRUCTURE pt FIELD y n FIELD z n ;STRUCTURE
private
;package
public
DECL-DIAG:PROSE
s" STRUCTURE pt FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
s" pt" s" a family of a part of this package already makes a constructor package of this name" DECL-DIAG:CAUSE? TTRUE
private
;package
package SDRESF
public
STRUCTURE pt FIELD x n ;STRUCTURE
private
package SDRESF
public
DECL-DIAG:PROSE
s" STRUCTURE pt FIELD y n FIELD z n ;STRUCTURE" TEST-EVAL:RC 70 T=
s" pt" s" a family of the enclosing package already makes a constructor package of this name" DECL-DIAG:CAUSE? TTRUE
private
;package
;package
\ A part family's constructor package is protected as a top-level one is.
package SDRESD
package BB
public
STRUCTURE inner FIELD x n ;STRUCTURE
private
;package
DECL-DIAG:PROSE
s" : BB-INNER:EXTRA ( -- ) ;" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `BB-INNER:EXTRA`" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" undefine BB-INNER:MAKE" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `BB-INNER:MAKE`" DECL-DIAG:HAS? TTRUE
;package
DECL-DIAG:PROSE
s" package SDRESD package BB-INNER ;package ;package" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `SDRESD:BB-INNER`" DECL-DIAG:HAS? TTRUE
s" BB-INNER" s" constructor package" DECL-DIAG:CAUSE? TTRUE

\ Another package's own part of a constructor package's name is that part: its
\ words are no constructor package's.
package SDRESZ
package SDRESB-PT ;package
s" : SDRESB-PT:Q ( -- n ) 1 ;" TEST-EVAL:RC 0 T=
;package

\ 14. A rejected PRIVATE declaration rolls back its field rows and its words: the
\     same name declares again in the package.
package SDPRIVROLL
private
TYPE-FIELD:COUNT PFB !
DECL-DIAG:PROSE
s" STRUCTURE roll FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `roll`\n" DECL-DIAG:HAS? TTRUE
s" z" s" duplicate field name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each field of the structure its own name\n" DECL-DIAG:HAS? TTRUE
TYPE-FIELD:COUNT PFB @ T=                             \ committed field rows retired
STRUCTURE roll FIELD z n ;STRUCTURE                 \ the name is free again
: RRT ( n -- n ) ROLL-MAKE ROLL-UNMAKE ;
public
: RGO ( n -- n ) RRT ;
;package
9 SDPRIVROLL:RGO 9 T=                                 \ the re-declaration generated working words

\ A field in a part `A:B` names A's private family bare, as a word there calls
\ A's private words.
package SDPARENT
private
STRUCTURE sdinner FIELD x n ;STRUCTURE
package SDPART
s" STRUCTURE sdouter FIELD y sdinner ;STRUCTURE" TEST-EVAL:RC 0 T=
s" : SDPGET ( n -- n ) SDINNER-MAKE SDOUTER-MAKE SDOUTER-UNMAKE SDINNER-UNMAKE ; 5 SDPGET"
TEST-EVAL:N 5 T=
;package
;package

\ Reject diagnostics. STRUCTURE prints its rejection through package REJECTION;
\ the assertions read the bytes captured through REJECTION:CAPTURE /
\ REJECTION:BUFFER$. This proves the channel, not a check-tool run. Each
\ case asserts the header line, the token line and the help line.

\ one case per reject reason this suite pins, each rendering family + token +
\ reason and stopping the load with status 70.
DECL-DIAG:PROSE
s" STRUCTURE Sdgcase ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `Sdgcase`\n" DECL-DIAG:HAS? TTRUE
s" Sdgcase" s" name must be a lowercase family tail" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write the name as a lowercase tail with no package qualifier, as in `point`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `n`\n" DECL-DIAG:HAS? TTRUE
s" n" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: pick another name: a built-in type such as `n` cannot name a family\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE sdgdup FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgdup`\n" DECL-DIAG:HAS? TTRUE
s" z" s" duplicate field name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each field of the structure its own name\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE sdgar<q,q> ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgar<q,q>`\n" DECL-DIAG:HAS? TTRUE
s" sdgar<q,q>" s" invalid type parameter header" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name each type parameter once in the header, as in `STRUCTURE box<a>` or `STRUCTURE pair<a,b>`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE sdgbad FIELD a nosuchtype ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgbad`\n" DECL-DIAG:HAS? TTRUE
s" nosuchtype" s" unknown type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name a type declared before this declaration: `n`, `^u8`, a header parameter, a" DECL-DIAG:HAS? TTRUE

\ inside an application the record stands at the argument it judges.
DECL-DIAG:PROSE
s" STRUCTURE sdgu<a> FIELD x option<b> ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgu`\n" DECL-DIAG:HAS? TTRUE
s" b" s" unknown type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: list the parameter in the header, as in `STRUCTURE box<a>`, before a field names it\n" DECL-DIAG:HAS? TTRUE

\ A pointer field is spelled ^T: `ptr` is an unknown type.
DECL-DIAG:PROSE
s" STRUCTURE sdgptr FIELD a ptr u8 ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgptr`\n" DECL-DIAG:HAS? TTRUE
s" ptr" s" unknown type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: spell a pointer `^T`, `^` joined to the type it points at, as in `FIELD next ^node`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE sdgpol POLICY nosuch ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgpol`\n" DECL-DIAG:HAS? TTRUE
s" nosuch" s" unknown layout policy" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE sdgdrv DERIVE nosuch ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgdrv`\n" DECL-DIAG:HAS? TTRUE
s" nosuch" s" unknown derive feature" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name `eq`, `hash`, `addr` or `init` after `DERIVE`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE sdgtag DERIVE tag FIELD a n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgtag`\n" DECL-DIAG:HAS? TTRUE
s" tag" s" derive tag requires an enum" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE sdgstray VARIANT ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgstray`\n" DECL-DIAG:HAS? TTRUE
s" VARIANT" s" unexpected token in structure declaration" DECL-DIAG:CAUSE? TTRUE
s\"   = help: start each clause with `FIELD`, `POLICY`, `DERIVE` or `OPAQUE`, and end with `;STRUCTURE`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE sdgfn FIELD make n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgfn`\n" DECL-DIAG:HAS? TTRUE
s" make" s" reserved field name" DECL-DIAG:CAUSE? TTRUE

\ a terminator that never arrives anchors on the family.
DECL-DIAG:PROSE
s" STRUCTURE sdgnoend FIELD a n" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgnoend`\n" DECL-DIAG:HAS? TTRUE
s" sdgnoend" s" missing ;STRUCTURE" DECL-DIAG:CAUSE? TTRUE
s\"   = help: end the declaration with `;STRUCTURE` after its last field\n" DECL-DIAG:HAS? TTRUE

\ A self-referential field is refused during type resolution, before a width
\ query can enter the unfinished family.
DECL-DIAG:PROSE
s" STRUCTURE sdgself FIELD selffld sdgself ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgself`\n" DECL-DIAG:HAS? TTRUE
s" sdgself" s" type cannot recursively name its owner" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name the structure being declared only beneath `^`, as in `FIELD next ^node`\n" DECL-DIAG:HAS? TTRUE

\ no family leaks from one declaration into the next: a declaration that fails
\ before its own name is read reports an empty family, not sdgself.
DECL-DIAG:PROSE
s" STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration\n" DECL-DIAG:HAS? TTRUE
s" " s" missing name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write the family name after `STRUCTURE`, as in `STRUCTURE point`\n" DECL-DIAG:HAS? TTRUE
s" sdgself" DECL-DIAG:HAS? TFALSE

\ A type parameter used in two kinds is rejected at its later use.
DECL-DIAG:PROSE
s" STRUCTURE sdgkind<a,q> FIELD v read-view<a,q,n> FIELD x a ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgkind`\n" DECL-DIAG:HAS? TTRUE
s" a" s" `a` is a value type here, but field `v` uses it as a scope" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each kind its own header parameter, as in `STRUCTURE box<a,p,q>` with `FIELD x a` and `FIELD v read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdgkind<a,q> FIELD x a FIELD v read-view<a,q,n> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgkind`\n" DECL-DIAG:HAS? TTRUE
s" a" s" `a` is a scope here, but field `x` uses it as a value type" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:TEXT$ s\" = evaluated: STRUCTURE sdgkind<a,q> FIELD x a FIELD v read-view<a,q,n> ;STRUCTURE\n                                                                ^" REJECTION-TEXT:HAS? TTRUE
s\"   = help: give each kind its own header parameter, as in `STRUCTURE box<a,p,q>` with `FIELD x a` and `FIELD v read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdgtype<a,q> FIELD x a FIELD w read-view<q,q,a> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgtype`\n" DECL-DIAG:HAS? TTRUE
s" a" s" `a` is an element type here, but field `x` uses it as a value type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each kind its own header parameter, as in `STRUCTURE box<a,p,q>` with `FIELD x a` and `FIELD v read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
\ The label carries a 255-byte field name, the longest a name may be, whole.
DECL-DIAG:PROSE
s" STRUCTURE sdglong<a,q> FIELD fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff a FIELD v read-view<a,q,n> ;STRUCTURE" TEST-EVAL:RC 70 T=
s" a" s" `a` is a scope here, but field `fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff` uses it as a value type" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdgheld<a,q> FIELD p ^a FIELD w read-view<a,q,n> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgheld`\n" DECL-DIAG:HAS? TTRUE
s" a" s" `a` is a scope here, but field `p` uses it as a pointee or quotation element" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each kind its own header parameter, as in `STRUCTURE box<a,p,q>` with `FIELD x a` and `FIELD v read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
s" STRUCTURE sdgok<a,q> FIELD p ^a FIELD w read-view<q,q,a> ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" STRUCTURE sdgptee<a,q> FIELD w read-view<a,q,n> FIELD p ^a ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s" a" s" `a` is a pointee or quotation element here, but field `w` uses it as a scope" DECL-DIAG:CAUSE? TTRUE
\ A pointer reaches the declared family through another family's arguments.
DECL-DIAG:PROSE
s" STRUCTURE sdgnest<q> FIELD next ^option<sdgnest<n>> FIELD w read-view<q,q,n> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgnest`\n" DECL-DIAG:HAS? TTRUE
s" sdgnest" s" a pointer cannot reach `sdgnest`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
s\"   = help: keep the view out of the family a pointer reaches, as in `STRUCTURE node<a>` with `FIELD next ^node<a>`, and `STRUCTURE walk<a,p,q>` with `FIELD at ^node<a>` and `FIELD w read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
\ A pointer at a family with a scope or region parameter is rejected at its
\ name, the declared family included, whether the parameter comes before or after.
DECL-DIAG:PROSE
s" STRUCTURE sdgprv<p,q> FIELD x ^read-view<p,q,n> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgprv`\n" DECL-DIAG:HAS? TTRUE
s" read-view" s" a pointer cannot reach `read-view`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
s\"   = help: hold the view in the field itself, as in `STRUCTURE box<p,q>` with `FIELD x read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdgparg<p,q> FIELD x SPAN:span<^read-view<p,q,n>> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgparg`\n" DECL-DIAG:HAS? TTRUE
s" read-view" s" a pointer cannot reach `read-view`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
s\"   = help: hold the view in the field itself, as in `STRUCTURE box<p,q>` with `FIELD x read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdgnode<q> FIELD next ^sdgnode<n> FIELD w read-view<q,q,n> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgnode`\n" DECL-DIAG:HAS? TTRUE
s" sdgnode" s" a pointer cannot reach `sdgnode`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:TEXT$ s\" = evaluated: STRUCTURE sdgnode<q> FIELD next ^sdgnode<n> FIELD w read-view<q,q,n> ;STRUCTURE\n                                              ^^^^^^^" REJECTION-TEXT:HAS? TTRUE
s\"   = help: keep the view out of the family a pointer reaches, as in `STRUCTURE node<a>` with `FIELD next ^node<a>`, and `STRUCTURE walk<a,p,q>` with `FIELD at ^node<a>` and `FIELD w read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdgnodeq<a,q> FIELD next ^sdgnodeq<a,q> FIELD w read-view<q,q,a> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgnodeq`\n" DECL-DIAG:HAS? TTRUE
s" sdgnodeq" s" a pointer cannot reach `sdgnodeq`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:TEXT$ s\" = evaluated: STRUCTURE sdgnodeq<a,q> FIELD next ^sdgnodeq<a,q> FIELD w read-view<q,q,a> ;STRUCTURE\n                                                 ^^^^^^^^" REJECTION-TEXT:HAS? TTRUE
s\"   = help: keep the view out of the family a pointer reaches, as in `STRUCTURE node<a>` with `FIELD next ^node<a>`, and `STRUCTURE walk<a,p,q>` with `FIELD at ^node<a>` and `FIELD w read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdgafter<q> FIELD w read-view<q,q,n> FIELD next ^sdgafter<q> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgafter`\n" DECL-DIAG:HAS? TTRUE
s" sdgafter" s" a pointer cannot reach `sdgafter`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:TEXT$ s\" = evaluated: STRUCTURE sdgafter<q> FIELD w read-view<q,q,n> FIELD next ^sdgafter<q> ;STRUCTURE\n                                                                        ^^^^^^^^" REJECTION-TEXT:HAS? TTRUE
s\"   = help: keep the view out of the family a pointer reaches, as in `STRUCTURE node<a>` with `FIELD next ^node<a>`, and `STRUCTURE walk<a,p,q>` with `FIELD at ^node<a>` and `FIELD w read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
\ Beneath a pointer a family's arguments are judged as the pointee itself.
DECL-DIAG:PROSE
s" STRUCTURE sdgopt<p,q> FIELD x ^option<read-view<p,q,n>> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgopt`\n" DECL-DIAG:HAS? TTRUE
s" read-view" s" a pointer cannot reach `read-view`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
s\"   = help: hold the view in the field itself, as in `STRUCTURE box<p,q>` with `FIELD x read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
\ Beneath a family only rendered source declares, the check judges its
\ arguments the same.
DECL-DIAG:PROSE
: SDG-LATE ( -- ) s\" s\" STRUCTURE sdglate<a> FIELD y ^a ;STRUCTURE\" evaluate\nSTRUCTURE sdgfwd<p,q> FIELD x ^sdglate<read-view<p,q,n>> ;STRUCTURE" VERIFY:SOURCE-BUF ;
' SDG-LATE catch 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgfwd`\n" DECL-DIAG:HAS? TTRUE
\ CAUSE? reads the note only TEST-EVAL writes: the place and label stand for it.
s\"  --> <input>:2:40\n" DECL-DIAG:HAS? TTRUE
s\" ^ a pointer cannot reach `read-view`, which takes a scope or region\n" DECL-DIAG:HAS? TTRUE
s\"   = help: hold the view in the field itself, as in `STRUCTURE box<p,q>` with `FIELD x read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
\ A pointer reaches no user family with a scope parameter either: rejected at
\ its name.
DECL-DIAG:PROSE
s" STRUCTURE sdgscoped<p> FIELD v read-view<p,p,u8> ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" STRUCTURE sdgtoscoped<p> FIELD q ^sdgscoped<p> ;STRUCTURE" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgtoscoped`\n" DECL-DIAG:HAS? TTRUE
s" sdgscoped" s" a pointer cannot reach `sdgscoped`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
s\"   = help: hold the view in the field itself, as in `STRUCTURE box<p,q>` with `FIELD x read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
\ A concrete type where a scope or region parameter goes rejects at that
\ argument, helped by its slot.
REJECTION:CAPTURE
: SDG-RV ( -- ) s" STRUCTURE sdgrv FIELD x read-view<n,n,n> ;STRUCTURE" VERIFY:SOURCE-BUF ;
' SDG-RV catch 70 T=
REJECTION:BUFFER$ S\" error[E-BAD-DECLARATION]: bad structure declaration `sdgrv`\n --> <input>:1:35\n  |\n1 | STRUCTURE sdgrv FIELD x read-view<n,n,n> ;STRUCTURE\n  |                                   ^ type argument must be a scope parameter\n  = help: pass a parameter the header lists, as in `STRUCTURE box<p,q>` with `FIELD v read-view<p,q,u8>`\n\n" CONTAINS? TTRUE
REJECTION:CAPTURE
: SDG-MV ( -- ) s" STRUCTURE sdgmv<p,q> FIELD x mut-view<p,q,n> ;STRUCTURE" VERIFY:SOURCE-BUF ;
' SDG-MV catch 70 T=
REJECTION:BUFFER$ S\" error[E-BAD-DECLARATION]: bad structure declaration `sdgmv`\n --> <input>:1:43\n  |\n1 | STRUCTURE sdgmv<p,q> FIELD x mut-view<p,q,n> ;STRUCTURE\n  |                                           ^ type argument must be a region parameter\n  = help: pass a parameter the header lists, as in `STRUCTURE box<p,q,s>` with `FIELD v mut-view<p,q,s,u8>`\n\n" CONTAINS? TTRUE
\ A type spaced across one line is marked for its whole written span.
: SDG-SPACED ( -- ) s" STRUCTURE sdgsp FIELD f option<n,    n> ;STRUCTURE" VERIFY:SOURCE-BUF ;
DECL-DIAG:PROSE
' SDG-SPACED catch 70 T=
s\"  ^^^^^^^^^^^^^^^ type has too many arguments\n" DECL-DIAG:HAS? TTRUE

\ A field may point at its own family: three linked records walk round, MAKE and
\ UNMAKE certify, and a pointer to a pointer or to a span of itself declares.
STRUCTURE sdsnode DERIVE addr FIELD id n FIELD next ^sdsnode ;STRUCTURE
3 TYPED-BUFFER SDS-NODES sdsnode
: SDS-LINK ( -- )
   11 0 SDS-NODES SDSNODE:ID !  22 1 SDS-NODES SDSNODE:ID !  33 2 SDS-NODES SDSNODE:ID !
   1 SDS-NODES 0 SDS-NODES SDSNODE:NEXT !
   2 SDS-NODES 1 SDS-NODES SDSNODE:NEXT !
   0 SDS-NODES 2 SDS-NODES SDSNODE:NEXT ! ;
: SDS-AT ( n -- n )                     \ the id k links on from the first record
   {: k:n :}
   0 SDS-NODES k 0 ?do SDSNODE:NEXT @ loop SDSNODE:ID @ ;
SDS-LINK
0 SDS-AT 11 T=  1 SDS-AT 22 T=  2 SDS-AT 33 T=  3 SDS-AT 11 T=
s" SDSMK ( n ^sdsnode -- sdsnode ) SDSNODE:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" SDSUN ( sdsnode -- n ^sdsnode ) SDSNODE:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" STRUCTURE sdsback FIELD id n FIELD back ^^sdsback ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdspspan FIELD id n FIELD s ^SPAN:span<sdspspan> ;STRUCTURE" TEST-EVAL:RC 0 T=

\ A parametric family pointing at itself: pairing two of its applications ends,
\ a generic and a concrete accessor certify, and a mismatched one is refused.
STRUCTURE sdsplist<a> DERIVE addr FIELD v a FIELD next ^sdsplist<a> ;STRUCTURE
s" SDSPG ( ^sdsplist<a> -- ^sdsplist<a> ) SDSPLIST:NEXT @" CHECK-QUIET-CANDIDATE! -1 T=
s" SDSPC ( ^sdsplist<n> -- ^sdsplist<n> ) SDSPLIST:NEXT @" CHECK-QUIET-CANDIDATE! -1 T=
s" SDSPX ( ^sdsplist<n> -- ^sdsplist<f> ) SDSPLIST:NEXT @" CHECK-QUIET-CANDIDATE! 0 T=
STRUCTURE sdsplistn<a> DERIVE addr FIELD v a FIELD next ^sdsplistn<n> ;STRUCTURE
s" SDSNG ( ^sdsplistn<a> -- ^sdsplistn<n> ) SDSPLISTN:NEXT @" CHECK-QUIET-CANDIDATE! -1 T=
s" SDSNC ( ^sdsplistn<n> -- ^sdsplistn<n> ) SDSPLISTN:NEXT @" CHECK-QUIET-CANDIDATE! -1 T=

\ An addr family's parameter takes a record through its words and generic ones.
package SDVEC
public
STRUCTURE sdvbox<t> OPAQUE DERIVE addr FIELD k n FIELD j n ;STRUCTURE
ENUM sdvshade
   VARIANT dark FIELD k n ;VARIANT
   VARIANT light FIELD k n ;VARIANT
;ENUM
TYPED-VARIABLE SDV-BOX sdvbox<sdvshade>
: SDV-K ( ^sdvbox<sdvshade> -- ^n ) SDVBOX-K ;
: SDV-CNT ( ^sdvbox<t> -- n ) SDVBOX-K @ ;
: SDV-SHADE-CNT ( ^sdvbox<sdvshade> -- n ) SDV-CNT ;
: SDV-PICK ( ^VEC:vector<sdvshade> n -- ^sdvshade ) VEC:AT ;
: SDV-STRIDE? ( ^VEC:vector<sdvshade> -- bool )
   {: v:^ :}
   v 1 SDV-PICK v 0 SDV-PICK -  [type] sdvshade width>n = ;
: SDV-RUN ( -- n bool )
   7 SDV-BOX SDV-K !  SDV-BOX SDV-SHADE-CNT
   HEAP:ALLOCATOR 2 [type] sdvshade VEC:NEW {: v:^ :}
   v SDV-STRIDE?  v VEC:DELETE ;
;package
SDVEC:SDV-RUN TTRUE 7 T=

\ The declaring package's own `*` does not reach its structure's AT: another
\ package reads element 2 of three through it.
package SDSTARP
: * ( n n -- n ) + ;
public
STRUCTURE trio DERIVE addr FIELD a n FIELD b n FIELD c n ;STRUCTURE
;package
package SDSTARQ
3 TYPED-BUFFER SDSTAR-BUF SDSTARP:trio
: SDSTAR-PUT ( n n -- )
   {: v:n i:n :}
   v v 1 + v 2 + SDSTARP-TRIO:MAKE i SDSTAR-BUF ! ;
public
: SDSTAR-THIRD ( -- n )
   100 0 SDSTAR-PUT  200 1 SDSTAR-PUT  300 2 SDSTAR-PUT
   0 SDSTAR-BUF 2 SDSTARP-TRIO:AT SDSTARP-TRIO:A @ ;
;package
SDSTARQ:SDSTAR-THIRD 300 T=

\ Its own family as another family's argument is rejected, a span's too,
\ though a span's base only points at its element.
DECL-DIAG:PROSE
s" STRUCTURE sdsvspan FIELD s SPAN:span<sdsvspan> ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdsvspan`\n" DECL-DIAG:HAS? TTRUE
s" sdsvspan" s" type cannot recursively name its owner" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name the structure being declared only beneath `^`, as in `FIELD next ^node`\n" DECL-DIAG:HAS? TTRUE

\ A family argument may be a pointer to the declared family; a span of pointers stored in
\ the field comes back.
STRUCTURE sdpnode DERIVE addr
   FIELD kind n
   FIELD kept SPAN:span<^sdpnode>
;STRUCTURE
2 TYPED-BUFFER SDP-NODES sdpnode
2 TYPED-BUFFER SDP-REFS ^sdpnode
: SDP-KEPT ( -- n n )
   7 1 SDP-NODES SDPNODE:KIND !
   1 SDP-NODES 1 SDP-REFS !
   0 SDP-REFS 2 cells SPAN:MAKE 0 SDP-NODES SDPNODE:KEPT !
   0 SDP-NODES SDPNODE:KEPT @ SPAN-SPAN:UNMAKE {: b:^ l:n :}
   b 1 cells + @ SDPNODE:KIND @ l ;
SDP-KEPT 2 cells T= 7 T=

\ A type left open at `;STRUCTURE` is unclosed and `^` with no pointee a pointer without
\ one; a type's tokens may stand on two lines and hold blanks, as a declaration's do.
DECL-DIAG:PROSE
s" STRUCTURE sdpopen FIELD kept SPAN:span<^ ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdpopen`\n" DECL-DIAG:HAS? TTRUE
s" SPAN:span<^" s" unclosed type application" DECL-DIAG:CAUSE? TTRUE
s\"   = help: close each `<` of the type with `>` before the next clause\n" DECL-DIAG:HAS? TTRUE
s\" STRUCTURE sdpline FIELD kept SPAN:span<\n   ^sdpline> ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdpspaced FIELD kept SPAN:span< ^sdpspaced > ;STRUCTURE" TEST-EVAL:RC 0 T=
\ A type that runs past its line is marked up to that line's end.
DECL-DIAG:PROSE
s\" STRUCTURE rqml FIELD f option<n,\n  n> ;STRUCTURE" TEST-EVAL:RC 70 T=
s" option<n," s" type has too many arguments" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdpbare FIELD kept SPAN:span<^> ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdpbare`\n" DECL-DIAG:HAS? TTRUE
s" ^" s" pointer without a pointee" DECL-DIAG:CAUSE? TTRUE
s\"   = help: spell a pointer `^T`, `^` joined to the type it points at, as in `FIELD next ^node`\n" DECL-DIAG:HAS? TTRUE

\ A pointer to its own family is a pointer to a linear value when the family
\ holds one, in either field order: judged once the fields are committed.
package SDSELF
public
DEFLINEAR SDSELF:tok
;package
DECL-DIAG:PROSE
s" STRUCTURE sdslnode FIELD next ^sdslnode FIELD t SDSELF:tok ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdslnode`\n" DECL-DIAG:HAS? TTRUE
s" sdslnode" s" type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdslnode2 FIELD t SDSELF:tok FIELD next ^sdslnode2 ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdslnode2`\n" DECL-DIAG:HAS? TTRUE
s" sdslnode2" s" type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

\ hostile declarations: a field spelled like a fragment of the message is
\ reported as the token it is, and exactly one rejection is emitted.
DECL-DIAG:PROSE
s" STRUCTURE sdgforge FIELD duplicate n FIELD duplicate n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgforge`\n" DECL-DIAG:HAS? TTRUE
s" duplicate" s" duplicate field name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each field of the structure its own name\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
DECL-DIAG:TEXT$ REJECTION:PATH$ CONTAINS? TTRUE   \ the record names the loading file

\ a family name spelled like one of the JSON packet's own keys keeps key and
\ value distinguishable, and a token carrying a double quote is escaped rather
\ than closing the JSON string early.
DECL-DIAG:JSON
s" STRUCTURE token FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" \"message\":\"bad structure declaration `token`\"" DECL-DIAG:HAS? TTRUE
s\" \"token\":\"z\"" DECL-DIAG:HAS? TTRUE
s\" \"label\":\"duplicate field name\"" DECL-DIAG:HAS? TTRUE
s\" \"help\":\"give each field of the structure its own name\"" DECL-DIAG:HAS? TTRUE

DECL-DIAG:JSON
s\" STRUCTURE sdgquote FIELD aq\"b n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" \"token\":\"aq\\\"b\"" DECL-DIAG:HAS? TTRUE
s\" \"message\":\"bad structure declaration `sdgquote`\"" DECL-DIAG:HAS? TTRUE

\ accepted declarations stay silent.
DECL-DIAG:PROSE
s" STRUCTURE sdgok1 FIELD a n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdgok2 ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdgok3<a> FIELD a a ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdgok4 DERIVE eq FIELD a n ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:SILENT? TTRUE

\ the armed duplicate-family reason, which no case above reaches: it is armed at
\ SD-REGISTER, immediately before the registry call that can raise it.
DECL-DIAG:PROSE
s" STRUCTURE sdgtwice FIELD a n ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" STRUCTURE sdgtwice FIELD b n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgtwice`\n" DECL-DIAG:HAS? TTRUE
s" sdgtwice" s" duplicate family" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: this package already declares a family with this name\n" DECL-DIAG:HAS? TTRUE

\ SD-CLOSE arms no reason of its own: the generator's rejects are raised past the
\ last token this front end holds, so they are answered from the packet's code
\ table. The over-long span cap and its "..." marker belong to the shared packet
\ and are pinned once, in test/enum-decl-suite.f section 22h.
DECL-DIAG:PROSE
s" STRUCTURE sdgvrsv FIELD z sdgvrsv ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdgvrsv`\n" DECL-DIAG:HAS? TTRUE
s" sdgvrsv" s" type cannot recursively name its owner" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name the structure being declared only beneath `^`, as in `FIELD next ^node`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:OFF

\ TYPE-DECL:REPLAY-STRUCTURE - registering a STRUCTURE from tokens a tool already lexed
\ (src/habu/verify-source.f), so a later declaration can name the family as a payload
\ type. Registration includes the MAKE/UNMAKE variant rows, their constructor
\ package and checked effects; replay emits no runtime code.
package struct-replay-test
public

\ The replay entry under `catch` on copies of both strings, so a reject answers
\ its code as TEST-EVAL:RC does for the live entry.
: RP-EV ( str str -- ) TYPE-DECL:REPLAY-STRUCTURE TFALSE 2drop ;
: RP-TRY ( str str -- n )
   [: 2over 2over RP-EV ;] catch {: rc:n :} 2drop 2drop rc ;
: DICT-RECS ( -- n ) ndict@ ;

variable RP-DICT
: DICT-MARK ( -- ) DICT-RECS RP-DICT ! ;
: DICT-SAME ( -- ) DICT-RECS RP-DICT @ T= ;

\ A replayed STRUCTURE registers its family: a checked MAKE call resolves in the
\ scope it was replayed in, and so does a field typed by that family.
: RP-SD ( -- )
   s" rpsd" s" FIELD one n FIELD two n ;STRUCTURE" RP-TRY 0 T=
   s" S1 ( n n -- rpsd ) RPSD:MAKE" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" S1BAD ( n -- rpsd ) RPSD:MAKE" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" rpsdpay" s" FIELD h rpsd ;STRUCTURE" RP-TRY 0 T= ;

\ Header clauses replay too; derived EQ calls rows only the replay recorded, so
\ it runs in the check tool's replay scope (test/replay-scope.f).
: RP-SD-HEADER ( -- )
   s" rpsdh" s" POLICY packed-tag DERIVE eq FIELD one n ;STRUCTURE"
   RP-TRY 0 T= ;

\ A replayed BUFFER record (src/habu/verify-source.f) has the live one's MAKE.
: RP-SDBUF ( -- )
   s" rpsdbuf" s" FIELD a n BUFFER b 13 FIELD c n ;STRUCTURE" RP-TRY 0 T=
   s" SB1 ( n n n n -- rpsdbuf ) RPSDBUF:MAKE" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" SB1BAD ( n n n n n -- rpsdbuf ) RPSDBUF:MAKE" VERIFY:CANDIDATE-IN-SCOPE 0 T= ;

private
;package

\ The native dictionary does not move (DICT-SAME).
struct-replay-test:DICT-MARK
' struct-replay-test:RP-SD CHECK:SCOPED
struct-replay-test:DICT-SAME

' struct-replay-test:RP-SD-HEADER TOOL-REPLAY:RUN

\ A malformed replayed STRUCTURE reports through the same renderer as a live one.
DECL-DIAG:PROSE
s" rpsdbad" s" FIELD one nope ;STRUCTURE" struct-replay-test:RP-TRY 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `rpsdbad`\n" DECL-DIAG:HAS? TTRUE
s\" ^ unknown type\n" DECL-DIAG:HAS? TTRUE
s\" = word: nope\n" DECL-DIAG:HAS? TTRUE
s\"   = help: name a type declared before this declaration: `n`, `^u8`, a header parameter, a" DECL-DIAG:HAS? TTRUE

\ A buffer with no terminator rejects through the front end's own gate.
DECL-DIAG:PROSE
s" rpsdnoend" s" FIELD one n" struct-replay-test:RP-TRY 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `rpsdnoend`\n" DECL-DIAG:HAS? TTRUE
s\" ^ missing ;STRUCTURE\n" DECL-DIAG:HAS? TTRUE
s\" = word: rpsdnoend\n" DECL-DIAG:HAS? TTRUE
s\"   = help: end the declaration with `;STRUCTURE` after its last field\n" DECL-DIAG:HAS? TTRUE

\ A zero-length name reaches the missing-name gate.
DECL-DIAG:PROSE
s" " s" FIELD one n ;STRUCTURE" struct-replay-test:RP-TRY 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration\n" DECL-DIAG:HAS? TTRUE
s\" ^ missing name\n" DECL-DIAG:HAS? TTRUE
s\"   = help: write the family name after `STRUCTURE`, as in `STRUCTURE point`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ A rejected replay leaves the stream closed, so the next LIVE declaration reads
\ the input source again.
DECL-DIAG:PROSE
DECL-DIAG:PROSE
s" rpsddangle" s" FIELD" struct-replay-test:RP-TRY 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `rpsddangle`\n" DECL-DIAG:HAS? TTRUE
s\" ^ missing field name\n" DECL-DIAG:HAS? TTRUE
s\"   = help: write a name and a type after `FIELD`, as in `FIELD x n`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF
s" STRUCTURE rpsdafter FIELD one n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ A field may spell a keyword and a family name may contain one, but a public
\ family's field under DERIVE addr names its accessor and takes the definer rule.
DECL-DIAG:PROSE
s" STRUCTURE iffy FIELD if n FIELD variable n FIELD i n ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:SILENT? TTRUE
DECL-DIAG:OFF
DECL-DIAG:PROSE
s" STRUCTURE sdkwa DERIVE addr FIELD x n FIELD if n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-RESERVED-DEFINITION]: `if` is a keyword and cannot name a definition\n" DECL-DIAG:HAS? TTRUE
s" if" s" reserved keyword" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:OFF

\ A field may name a linear family at any depth, and the structure then owns it
\ (TFAM-CONCRETE-LINEAR?); test/type-linear-suite.f checks how such values are used.
package SDLIN
public
DEFLINEAR SDLIN:tok                                   \ the linear owner these fixtures nest
;package

\ depth 1: a field naming the linear con itself.
s" STRUCTURE sdlbox FIELD t SDLIN:tok FIELD k n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ depth 2: a field naming that linear family.
TYPE-FIELD:COUNT B !
s" STRUCTURE sdlouter FIELD inner sdlbox FIELD z n ;STRUCTURE" TEST-EVAL:RC 0 T=
B @ TFAM:PF-NAME$ s" inner" CORE-STR= TTRUE
B @ TYPE-FIELD:BYTES@ 2 cells T=                      \ the field is the whole bundle, not one cell
B @ 1 + TFAM:PF-BYTE-OFF@ 2 cells T=                  \ z sits after it

\ depth 3: the walk recurses again rather than stopping one level down.
s" STRUCTURE sdldeep FIELD d sdlouter ;STRUCTURE" TEST-EVAL:RC 0 T=

\ a sum reached through a product counts the same way.
s" ENUM sdlsum VARIANT hold FIELD m sdlbox ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" STRUCTURE sdlviasum FIELD e sdlsum ;STRUCTURE" TEST-EVAL:RC 0 T=

\ a nested chain with no linear value anywhere declares too.
s" STRUCTURE sdlplain FIELD v n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdlplainer FIELD inner sdlplain ;STRUCTURE" TEST-EVAL:RC 0 T=

\ wrong role: the same word in the FIELD NAME position is a name, never a type.
s" STRUCTURE sdlrole FIELD sdlbox n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ reordering: naming a family before it is declared is still an unknown type,
\ so acceptance comes from resolution and not from the spelling alone.
DECL-DIAG:PROSE
s" STRUCTURE sdlfwd FIELD m sdllater ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlfwd`\n" DECL-DIAG:HAS? TTRUE
s" sdllater" s" unknown type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name a type declared before this declaration: `n`, `^u8`, a header parameter, a" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ A name that resolves says why it cannot be a field type: a parametric family
\ named bare. A name that resolves to nothing reports unknown.
s" STRUCTURE sdlgen<a> FIELD a a ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" STRUCTURE sdlgenuse FIELD m sdlgen ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlgenuse`\n" DECL-DIAG:HAS? TTRUE
s" sdlgen" s" type is parametric and needs type arguments" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give the family its type arguments, as in `box<n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ A field may not name a pointer to a linear value. A pointer is a non-owning
\ boundary, so the linearity walk stops at the pointer node and the structure
\ would read non-linear, copying and dropping freely with a linear resource
\ behind the address. TFCL-NODE?, the node walk the linearity accounting
\ trusts, refuses the con and family spellings alike at every depth:
\ `^^sdlbox` rejects on the inner recursion, anchoring the diagnostic on
\ the family that owns the resource. A pointer to something that owns nothing
\ (`^n`, `^sdlplain`) stays legal.

\ the two spellings side by side. Naming the value owns it; pointing at it cannot.
s" STRUCTURE sdlowns FIELD m sdlbox ;STRUCTURE" TEST-EVAL:RC 0 T=

DECL-DIAG:PROSE
s" STRUCTURE sdlptr FIELD p ^sdlbox ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlptr`\n" DECL-DIAG:HAS? TTRUE
s" sdlbox" s" type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

\ the con spelling launders the same way, so the same rule refuses it, and the
\ diagnostic names the con it found rather than some enclosing family.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrcon FIELD p ^SDLIN:tok ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlptrcon`\n" DECL-DIAG:HAS? TTRUE
s" SDLIN:tok" s" type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

\ depth: a second pointer does not launder past the rule. The inner recursion
\ rejects first, so the token names the family that owns the resource.
DECL-DIAG:PROSE
s" STRUCTURE sdlptr2 FIELD p ^^sdlbox ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlptr2`\n" DECL-DIAG:HAS? TTRUE
s" sdlbox" s" type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

\ reaching the resource through a nested family or through a sum is still reaching
\ it, so a pointer to either is refused for the same reason.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrdeep FIELD p ^sdldeep ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlptrdeep`\n" DECL-DIAG:HAS? TTRUE
s" sdldeep" s" type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdlptrsum FIELD p ^sdlsum ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlptrsum`\n" DECL-DIAG:HAS? TTRUE
s" sdlsum" s" type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

\ a field anywhere in the body is checked, not just the first one, and the token
\ names the offending field's type rather than the declaration's first field.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrlate FIELD a n FIELD p ^sdlbox FIELD c n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlptrlate`\n" DECL-DIAG:HAS? TTRUE
s" sdlbox" s" type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

\ `^` lets a field name its own family only inside the pointee: by value after
\ it, or in the declaration after a refused pointee, the name is rejected.
DECL-DIAG:PROSE
s" STRUCTURE sdlpquot FIELD q [ ^sdlpquot sdlpquot -- ] ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlpquot`\n" DECL-DIAG:HAS? TTRUE
s" sdlpquot" s" type cannot recursively name its owner" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name the structure being declared only beneath `^`, as in `FIELD next ^node`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdlpleak FIELD next ^Q ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlpleak`\n" DECL-DIAG:HAS? TTRUE
s" Q" s" unknown type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: list the parameter in the header, as in `STRUCTURE box<a>`, before a field names it\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdlpafter FIELD me sdlpafter ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlpafter`\n" DECL-DIAG:HAS? TTRUE
s" sdlpafter" s" type cannot recursively name its owner" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name the structure being declared only beneath `^`, as in `FIELD next ^node`\n" DECL-DIAG:HAS? TTRUE

\ the controls. A pointer to a family that owns nothing declares, so the rejects
\ above answer the POINTEE's linearity and not the word `^`.
s" STRUCTURE sdlptrok FIELD p ^sdlplain FIELD k n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdlptrok2 FIELD p ^^sdlplain ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdlptrn FIELD p ^n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ wrong role: the same words in the FIELD NAME position are names, never types.
\ Neither resolves and neither is refused.
s" STRUCTURE sdlptrrole FIELD ptr n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdlptrrole2 FIELD sdlbox n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ hostile comments. A declaration body has no comment syntax: `(` and `\` are
\ ordinary tokens, so the declaration rejects the unexpected token, not a
\ pointer to a linear value.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrpar FIELD v n ( FIELD p ^sdlbox ) ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlptrpar`\n" DECL-DIAG:HAS? TTRUE                                  \ the paren itself was the token
s" (" s" unexpected token in structure declaration" DECL-DIAG:CAUSE? TTRUE
s\"   = help: start each clause with `FIELD`, `POLICY`, `DERIVE` or `OPAQUE`, and end with `;STRUCTURE`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdlptrbsl FIELD v n \ FIELD p ^sdlbox" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlptrbsl`\n" DECL-DIAG:HAS? TTRUE                                  \ and so was the backslash
s\" \\" s" unexpected token in structure declaration" DECL-DIAG:CAUSE? TTRUE
s\"   = help: start each clause with `FIELD`, `POLICY`, `DERIVE` or `OPAQUE`, and end with `;STRUCTURE`\n" DECL-DIAG:HAS? TTRUE
\ and trailing text after the offending field cannot suppress the reject, because
\ the field is resolved before anything following it is read.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrtail FIELD p ^sdlbox ( note ) ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdlptrtail`\n" DECL-DIAG:HAS? TTRUE
s" sdlbox" s" type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:OFF

\ 15. DERIVE addr: one accessor per field plus AT / BYTES / CELLS, in the
\     family's spelling and wordlist, generated at commit because an accessor
\     takes a committed field id (src/core/structure-decl.f). The checker
\     derives each accessor's family, offset, extent, role and schema from the
\     field id, so the declared output type is judged.
DECL-DIAG:PROSE                                       \ this section rejects too: keep the output clean
STRUCTURE sdaddr
   DERIVE addr
   FIELD a n
   FIELD p ^u8
   FIELD c n
;STRUCTURE
2 TYPED-BUFFER SDA-BUF sdaddr

SDADDR:BYTES 24 T=                                    \ the size words BEGIN-STRUCTURE published
SDADDR:CELLS 3 T=

: SDA-W ( n str n -- ) {: av:n pv:^ cv:n i:n :} av pv cv SDADDR:MAKE i SDA-BUF ! ;
: SDA-A ( n -- n ) SDA-BUF SDADDR:A @ ;
: SDA-C ( n -- n ) SDA-BUF SDADDR:C @ ;
: SDA-P ( n -- ^u8 ) SDA-BUF SDADDR:P @ ;
: SDA-UA ( n -- n ) SDA-BUF @ SDADDR:UNMAKE {: av:n pv:^ cv:n :} av ;
: SDA-UC ( n -- n ) SDA-BUF @ SDADDR:UNMAKE {: av:n pv:^ cv:n :} cv ;
: SDA-UP ( n -- ^u8 ) SDA-BUF @ SDADDR:UNMAKE {: av:n pv:^ cv:n :} pv ;

10 NULL-PTR 30 0 SDA-W
40 NULL-PTR 60 1 SDA-W
\ a field read through an accessor equals the same field read through UNMAKE,
\ for a scalar field, a pointer field, and the last field of the record
0 SDA-A 0 SDA-UA T=
0 SDA-C 0 SDA-UC T=
0 SDA-P 0 SDA-UP = TTRUE
1 SDA-A 40 T=
1 SDA-C 60 T=

\ AT strides by the committed width and PRESERVES the family, so the accessor of
\ the same family reads the next record; the stride is signed both ways.
: SDA-NEXT ( -- n ) 0 SDA-BUF 1 SDADDR:AT SDADDR:A @ ;
: SDA-PREV ( -- n ) 1 SDA-BUF -1 SDADDR:AT SDADDR:C @ ;
SDA-NEXT 40 T=
SDA-PREV 30 T=

\ the declared output type is the FIELD's type: a pointer field projects as
\ `^^u8` and nothing else, and a scalar field does not coerce.
s" SDAX1 ( ^sdaddr -- ^^u8 ) SDADDR:P" CHECK-QUIET-CANDIDATE! -1 T=
s" SDAX2 ( ^sdaddr -- ^n ) SDADDR:P" CHECK-QUIET-CANDIDATE! 0 T=
s" SDAX3 ( ^sdaddr -- ^n ) SDADDR:A" CHECK-QUIET-CANDIDATE! -1 T=
s" SDAX4 ( ^sdaddr -- ^r ) SDADDR:A" CHECK-QUIET-CANDIDATE! 0 T=
\ and the accessor of one family is not the accessor of another
s" STRUCTURE sdaddr2 DERIVE addr FIELD a n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" SDAX5 ( ^sdaddr2 -- ^n ) SDADDR:A" CHECK-QUIET-CANDIDATE! 0 T=

\ a GENERIC family projects at the caller's instantiation
STRUCTURE sdagen<a>
   DERIVE addr
   FIELD v a
   FIELD k n
;STRUCTURE
1 TYPED-BUFFER SDAG-BUF sdagen<n>
: SDAG-W ( n n n -- ) {: vv:n kv:n i:n :} vv kv SDAGEN:MAKE i SDAG-BUF ! ;
: SDAG-V ( n -- n ) SDAG-BUF SDAGEN:V @ ;
77 88 0 SDAG-W
0 SDAG-V 77 T=
s" SDAG1 ( ^sdagen<a> -- ^a ) SDAGEN:V" CHECK-QUIET-CANDIDATE! -1 T=
s" SDAG2 ( ^sdagen<a> -- ^n ) SDAGEN:V" CHECK-QUIET-CANDIDATE! 0 T=

\ A family argument instantiates the accessor too, at the instance's layout:
\ K lies past a two-cell V.
STRUCTURE sdaw1 FIELD z n ;STRUCTURE                 \ one cell
STRUCTURE sdaw2 FIELD z n FIELD w n ;STRUCTURE       \ two cells
s" SDAW1 ( ^sdagen<sdaw1> -- ^sdaw1 ) SDAGEN:V" CHECK-QUIET-CANDIDATE! -1 T=
s" SDAW2 ( ^sdagen<sdaw2> -- ^n ) SDAGEN:K" CHECK-QUIET-CANDIDATE! -1 T=

\ `record-at` scales by the caller's count, so it refuses a pointee whose
\ instantiated width is not the committed width: an argument wider than one
\ cell moves every field past the first, which F:AT's baked multiplier and an
\ accessor's baked offset assume away. A one-cell argument is not wide, so the
\ refusal is about width, not about a family argument.
s" SDAW3 ( ^sdagen<sdaw2> n -- ^sdagen<sdaw2> ) record-at" CHECK-QUIET-CANDIDATE! 0 T=
s" SDAW4 ( ^sdagen<sdaw1> n -- ^sdagen<sdaw1> ) record-at" CHECK-QUIET-CANDIDATE! -1 T=
s" SDAW5 ( ^sdaddr n -- ^sdaddr ) record-at" CHECK-QUIET-CANDIDATE! -1 T=
\ off a layout pointee the plain row answers, and it is `cells +` on any pointer.
s" SDAW6 ( ^u8 n -- ^u8 ) record-at" CHECK-QUIET-CANDIDATE! -1 T=
\ the wide instantiation itself stays a perfectly ordinary type: what is refused
\ is the address surface over it, not the pointer.
s" SDAW7 ( ^sdagen<sdaw2> -- n ) drop 0" CHECK-QUIET-CANDIDATE! -1 T=

\ A field may hold an option of a multi-cell STRUCTURE: option's cell parameter
\ takes the layout-kind argument, as a variant payload's does. SOME and NONE go
\ through the generated accessor and read back by MATCH, and the fields around
\ the wide one keep their values. The options are built by words that declare
\ option<sdqpair>: a bare OPTION:NONE or OPTION:SOME inside a body stays the
\ open one-cell option<a> and does not meet the wide site.
STRUCTURE sdqpair FIELD x n FIELD y n ;STRUCTURE
: SDQ-ABSENT ( -- option<sdqpair> ) OPTION:NONE ;
: SDQ-PRESENT ( n n -- option<sdqpair> ) SDQPAIR:MAKE OPTION:SOME ;
: SDQ-OPEN ( option<sdqpair> -- n )
   MATCH option
      none OF -1 ENDOF
      some OF SDQPAIR:UNMAKE {: xv:n yv:n :} xv 100 * yv + ENDOF
   ;MATCH ;
s" STRUCTURE sdqrec DERIVE addr FIELD k n FIELD res option<sdqpair> FIELD z n ;STRUCTURE"
TEST-EVAL:RC 0 T=
s" 1 TYPED-BUFFER SDQ-BUF sdqrec" TEST-EVAL:RC 0 T=
s" : SDQ-INIT ( -- ) 11 SDQ-ABSENT 22 SDQREC:MAKE 0 SDQ-BUF ! ;" TEST-EVAL:RC 0 T=
s" : SDQ-SOME ( n n -- ) SDQ-PRESENT 0 SDQ-BUF SDQREC:RES ! ;" TEST-EVAL:RC 0 T=
s" : SDQ-NONE ( -- ) SDQ-ABSENT 0 SDQ-BUF SDQREC:RES ! ;" TEST-EVAL:RC 0 T=
s" : SDQ-READ ( -- n ) 0 SDQ-BUF SDQREC:RES @ SDQ-OPEN ;" TEST-EVAL:RC 0 T=
s" : SDQ-SIDES ( -- n ) 0 SDQ-BUF SDQREC:K @ 100 * 0 SDQ-BUF SDQREC:Z @ + ;" TEST-EVAL:RC 0 T=
s" SDQ-INIT SDQ-READ" TEST-EVAL:N -1 T=
s" 3 4 SDQ-SOME SDQ-READ" TEST-EVAL:N 304 T=
s" SDQ-SIDES" TEST-EVAL:N 1122 T=
s" SDQ-NONE SDQ-READ" TEST-EVAL:N -1 T=
s" SDQ-SIDES" TEST-EVAL:N 1122 T=
\ the same field without the addr surface
s" STRUCTURE sdqplain FIELD res option<sdqpair> ;STRUCTURE" TEST-EVAL:RC 0 T=

\ a field named like a generated member is refused AT ITS OWN TOKEN, because the
\ generator would otherwise render a name it had already defined.
DECL-DIAG:PROSE
s" STRUCTURE sdabad DERIVE addr FIELD at n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdabad`\n" DECL-DIAG:HAS? TTRUE
s" at" s" field name is a generated member name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the field: under `DERIVE addr` the names `make`, `unmake`, `at`, `bytes`, `cells`," DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdabad2 DERIVE addr FIELD make n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdabad2`\n" DECL-DIAG:HAS? TTRUE
s" make" s" field name is a generated member name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the field: under `DERIVE addr` the names `make`, `unmake`, `at`, `bytes`, `cells`," DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdabad3 DERIVE addr FIELD cells n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdabad3`\n" DECL-DIAG:HAS? TTRUE
s" cells" s" field name is a generated member name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the field: under `DERIVE addr` the names `make`, `unmake`, `at`, `bytes`, `cells`," DECL-DIAG:HAS? TTRUE
\ without the addr surface the same names are ordinary fields
s" STRUCTURE sdaok FIELD at n FIELD cells n ;STRUCTURE" TEST-EVAL:RC 0 T=
\ and a family with no field has no address to take
DECL-DIAG:PROSE
s" STRUCTURE sdaempty DERIVE addr ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdaempty`\n" DECL-DIAG:HAS? TTRUE
s" sdaempty" s" derived fields require a family with fields" DECL-DIAG:CAUSE? TTRUE
s\"   = help: add a `FIELD` to the structure, or drop `addr` and `init` from `DERIVE`\n" DECL-DIAG:HAS? TTRUE

\ the generated words are the family's own: a stray tail in its reserved
\ constructor package is still refused, and an accessor cannot be undefined.
DECL-DIAG:PROSE
s" undefine SDADDR:A" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `SDADDR:A`" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" undefine SDADDR:AT" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `SDADDR:AT`" DECL-DIAG:HAS? TTRUE

\ a PRIVATE family takes the private spelling, in the declaring package only.
package SDAPRIV
private
STRUCTURE hrec
   DERIVE addr
   FIELD x n
   FIELD y n
;STRUCTURE
1 TYPED-BUFFER HB hrec
: HW ( n n n -- ) {: xv:n yv:n i:n :} xv yv HREC-MAKE i HB ! ;
: HX ( n -- n ) HB HREC-X @ ;
: HY ( n -- n ) HB HREC-Y @ ;
: HNEXT ( n -- ^SDAPRIV:hrec ) HB 0 HREC-AT ;
s" HXI ( ^hrec -- ^n ) HREC-X" CHECK-QUIET-CANDIDATE! -1 T=   \ certifies HERE
public
: HGO ( -- n n n ) 5 6 0 HW 0 HX 0 HY HREC-BYTES ;
;package
SDAPRIV:HGO 16 T= 6 T= 5 T=

package SDAOTHER
private
\ The accessor name reaches nothing here: SDAP1 is refused as an undefined word
\ (0), as its diagnostic says. Its signature names no private type, which would
\ be refused too; SDAP2 refuses for that type.
DECL-DIAG:PROSE
s" SDAP1 ( n -- n ) HREC-X" CHECK-CANDIDATE! 0 T=
s\" error[E-UNDEFINED]: cannot find word `HREC-X`\n" DECL-DIAG:HAS? TTRUE
s\"  --> <input>:1:18\n" DECL-DIAG:HAS? TTRUE
s" SDAP2 ( ^SDAPRIV:hrec -- n ) drop 0" CHECK-QUIET-CANDIDATE! 0 T=
public
;package

\ A private family may derive eq and hash too, in the same private spelling.
package SDAPRIVDRV
private
STRUCTURE drec
   DERIVE eq hash
   FIELD u n
;STRUCTURE
public
: DEQ ( -- bool ) 4 DREC-MAKE 4 DREC-MAKE DREC-EQ ;
: DNE ( -- bool ) 4 DREC-MAKE 5 DREC-MAKE DREC-EQ ;
: DHS ( -- bool ) 4 DREC-MAKE DREC-HASH 4 DREC-MAKE DREC-HASH = ;
;package
SDAPRIVDRV:DEQ TTRUE
SDAPRIVDRV:DNE TFALSE
SDAPRIVDRV:DHS TTRUE

\ a rejected addr declaration rolls the whole thing back, accessors included:
\ the name is free and re-declaring it publishes working words.
TYPE-FIELD:COUNT PFB !
DECL-DIAG:PROSE
s" STRUCTURE sdaroll DERIVE addr FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdaroll`\n" DECL-DIAG:HAS? TTRUE
s" z" s" duplicate field name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each field of the structure its own name\n" DECL-DIAG:HAS? TTRUE
TYPE-FIELD:COUNT PFB @ T=
s" STRUCTURE sdaroll DERIVE addr FIELD z n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" SDAR1 ( ^sdaroll -- ^n ) SDAROLL:Z" CHECK-QUIET-CANDIDATE! -1 T=

\ A package's own `*`, RECORD-AT, `+` or `swap`, defined before a STRUCTURE with
\ DERIVE addr, captures none of the words it makes: each reads its own field
\ and AT element. Each is undefined before the storage, whose definer spells it.
package SDACAPM
: * ( n n -- n ) + ;
STRUCTURE cp DERIVE addr FIELD a n FIELD b n ;STRUCTURE
undefine *
2 TYPED-BUFFER CB cp
public
: GO ( -- n n n n )
   11 0 CB CP-A !  22 0 CB CP-B !  33 1 CB CP-A !  44 1 CB CP-B !
   0 CB 1 CP-AT CP-B @  0 CB CP-A @  CP-BYTES  CP-CELLS ;
;package
SDACAPM:GO 2 T= 16 T= 11 T= 44 T=

package SDACAPR
8 constant RECORD-AT
STRUCTURE cp DERIVE addr FIELD a n FIELD b n ;STRUCTURE
undefine RECORD-AT
2 TYPED-BUFFER CB cp
public
: GO ( -- n n n n )
   11 0 CB CP-A !  22 0 CB CP-B !  33 1 CB CP-A !  44 1 CB CP-B !
   0 CB 1 CP-AT CP-B @  0 CB CP-A @  CP-BYTES  CP-CELLS ;
;package
SDACAPR:GO 2 T= 16 T= 11 T= 44 T=

package SDACAPP
: + ( n n -- n ) - ;
STRUCTURE cp DERIVE addr FIELD a n FIELD b n ;STRUCTURE
undefine +
2 TYPED-BUFFER CB cp
public
: GO ( -- n n n n )
   11 0 CB CP-A !  22 0 CB CP-B !  33 1 CB CP-A !  44 1 CB CP-B !
   0 CB 1 CP-AT CP-B @  0 CB CP-A @  CP-BYTES  CP-CELLS ;
;package
SDACAPP:GO 2 T= 16 T= 11 T= 44 T=

package SDACAPS
: swap ( n n -- n n ) ;
STRUCTURE cp DERIVE addr FIELD a n FIELD b n ;STRUCTURE
undefine swap
2 TYPED-BUFFER CB cp
public
: GO ( -- n n n n )
   11 0 CB CP-A !  22 0 CB CP-B !  33 1 CB CP-A !  44 1 CB CP-B !
   0 CB 1 CP-AT CP-B @  0 CB CP-A @  CP-BYTES  CP-CELLS ;
;package
SDACAPS:GO 2 T= 16 T= 11 T= 44 T=

\ 16. OPAQUE is refused after a field, twice, on a fieldless family, in an ENUM and
\     outside a package; each reject keeps the field rows at their mark and renders.
: REJ-MARK ( -- ) TYPE-FIELD:COUNT PFB ! ;
: REJ-SAME ( -- ) TYPE-FIELD:COUNT PFB @ T= ;
package SDOPQ
public
STRUCTURE obox OPAQUE FIELD x n FIELD y n ;STRUCTURE
;package

\ A private family may say OPAQUE too: the placement is already private, so the
\ clause changes nothing and is admitted.
package SDOPQPRIV
private
s" STRUCTURE opriv OPAQUE FIELD x n ;STRUCTURE" TEST-EVAL:RC 0 T=
public
;package

\ The definer rejects, each at its token, each with the field rows at their mark.
package SDOPQREJ
public
DECL-DIAG:PROSE
REJ-MARK
s" STRUCTURE olate FIELD x n OPAQUE ;STRUCTURE" TEST-EVAL:RC 70 T=
REJ-SAME
s\" error[E-BAD-DECLARATION]: bad structure declaration `olate`\n" DECL-DIAG:HAS? TTRUE
s" OPAQUE" s" header clause after the first field" DECL-DIAG:CAUSE? TTRUE
s\"   = help: move the `POLICY`, `DERIVE` and `OPAQUE` clauses before the first `FIELD`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
REJ-MARK
s" STRUCTURE otwice OPAQUE OPAQUE FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
REJ-SAME
s\" error[E-BAD-DECLARATION]: bad structure declaration `otwice`\n" DECL-DIAG:HAS? TTRUE
s" OPAQUE" s" a second OPAQUE clause in one declaration" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
REJ-MARK
s" STRUCTURE obare OPAQUE ;STRUCTURE" TEST-EVAL:RC 70 T=
REJ-SAME
s\" error[E-BAD-DECLARATION]: bad structure declaration `obare`\n" DECL-DIAG:HAS? TTRUE
s" obare" s" opaque requires a family with fields" DECL-DIAG:CAUSE? TTRUE
s\"   = help: add a `FIELD` to the structure, or drop `OPAQUE`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
REJ-MARK
s" ENUM oshade OPAQUE VARIANT dark ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
REJ-SAME
s\" error[E-BAD-DECLARATION]: bad enum declaration `oshade`\n" DECL-DIAG:HAS? TTRUE
s" OPAQUE" s" unexpected token in enum declaration" DECL-DIAG:CAUSE? TTRUE
;package

\ Outside any package there is no private wordlist for the pair to land in: the
\ clause refuses before any name is checked, the loader goes on, the name is free.
DECL-DIAG:PROSE
REJ-MARK
s" STRUCTURE oloose OPAQUE FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
REJ-SAME
s\" error[E-BAD-DECLARATION]: bad structure declaration `oloose`\n" DECL-DIAG:HAS? TTRUE
s" OPAQUE" s" OPAQUE requires a family declared in a package" DECL-DIAG:CAUSE? TTRUE
s" STRUCTURE oloose FIELD x n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ 17. What OPAQUE does: the family's generated words — MAKE/UNMAKE and every
\     DERIVE member — take section 13's private-family placement, bare
\     FAMILY-MEMBER in the declaring package's private wordlist, protected by
\     name, with no constructor namespace stamped; the type stays nameable from
\     any package and the declaration's own section is restored after
\     generation. A reject after generation retires the privately placed words.
package SDOPQ
public
STRUCTURE oaddr OPAQUE DERIVE addr FIELD x n FIELD y n ;STRUCTURE
STRUCTURE oeq OPAQUE DERIVE eq hash FIELD u n ;STRUCTURE
STRUCTURE oinit OPAQUE DERIVE init FIELD a n FIELD b n ;STRUCTURE
: OAFTER ( -- n ) 8 ;                                 \ defined after generation: still public
: ORT ( n n -- n n ) OBOX-MAKE OBOX-UNMAKE ;          \ section 16's family, callable bare
: OEQT ( -- bool ) 4 OEQ-MAKE 4 OEQ-MAKE OEQ-EQ ;
: OEQF ( -- bool ) 4 OEQ-MAKE 5 OEQ-MAKE OEQ-EQ ;
: OHSH ( -- bool ) 4 OEQ-MAKE OEQ-HASH 4 OEQ-MAKE OEQ-HASH = ;
s" SDOI ( n n -- n n ) OBOX-MAKE OBOX-UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" SDOX ( ^oaddr -- ^n ) OADDR-X" CHECK-QUIET-CANDIDATE! -1 T=
s" SDON ( mut-view<a,b,c,init<d,oinit>> -- mut-view<a,b,c,init<d,oinit>> n ) OINIT-A@" CHECK-QUIET-CANDIDATE! -1 T=
private                                               \ the pair lives in the private wordlist
DECL-DIAG:PROSE
s" undefine OBOX-MAKE" TEST-EVAL:RC 70 T=           \ protected by name there
s" error[E-CTOR-PROTECTED]: `OBOX-MAKE`" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" undefine OBOX-UNMAKE" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `OBOX-UNMAKE`" DECL-DIAG:HAS? TTRUE
public
;package
11 22 SDOPQ:ORT 22 T= 11 T=
SDOPQ:OAFTER 8 T=
SDOPQ:OEQT TTRUE
SDOPQ:OEQF TFALSE
SDOPQ:OHSH TTRUE

\ Another package names the type and reaches none of the words, under any
\ spelling: the probe is refused as an undefined word (0), as section 13's SDPO is.
package SDOPQOTHER
private
s" SDOO ( n n -- n n ) OBOX-MAKE OBOX-UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOQ ( n n -- n n ) SDOPQ-OBOX:MAKE SDOPQ-OBOX:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOC ( n n -- n n ) SDOPQ:OBOX-MAKE SDOPQ:OBOX-UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOE ( SDOPQ:oeq SDOPQ:oeq -- bool ) SDOPQ-OEQ:EQ" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOA ( ^SDOPQ:oaddr -- ^n ) SDOPQ-OADDR:X" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOB ( mut-view<a,b,c,init<d,SDOPQ:oinit>> -- mut-view<a,b,c,init<d,SDOPQ:oinit>> n ) OINIT-A@" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOT ( SDOPQ:obox -- SDOPQ:obox )" CHECK-QUIET-CANDIDATE! -1 T=
public
;package

\ A reject after generation (DERIVE init refuses a non-canonical record at
\ commit, after MAKE/UNMAKE were placed privately) retires the privately
\ placed words: the name is free and re-declaring it works.
package SDOPQ
public
REJ-MARK
DECL-DIAG:PROSE
s" STRUCTURE oroll<a> OPAQUE DERIVE init FIELD value a FIELD tail n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `oroll`\n" DECL-DIAG:HAS? TTRUE
s" oroll" s" derive init requires a canonical fixed-cell record" DECL-DIAG:CAUSE? TTRUE
s\"   = help: drop `init` from `DERIVE`, or give every field a fixed cell width\n" DECL-DIAG:HAS? TTRUE
REJ-SAME
STRUCTURE oroll OPAQUE FIELD z n ;STRUCTURE
: ORR ( n -- n ) OROLL-MAKE OROLL-UNMAKE ;
;package
9 SDOPQ:ORR 9 T=

\ A reject inside generation: a private OCLASH-X already holds the name DERIVE
\ addr generates for field x, so the declaration rejects after its
\ MAKE/UNMAKE were placed. The colliding word still answers; once it is undefined
\ the same declaration succeeds and its pair round-trips.
package SDOPQ
private
: OCLASH-X ( -- n ) 77 ;
public
REJ-MARK
DECL-DIAG:PROSE
s" STRUCTURE oclash OPAQUE DERIVE addr FIELD x n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `oclash`\n" DECL-DIAG:HAS? TTRUE
s" oclash" s" generated declaration already defined" DECL-DIAG:CAUSE? TTRUE
REJ-SAME
private
OCLASH-X 77 T=
undefine OCLASH-X
public
STRUCTURE oclash OPAQUE DERIVE addr FIELD x n ;STRUCTURE
: OCR ( n -- n ) OCLASH-MAKE OCLASH-UNMAKE ;
;package
17 SDOPQ:OCR 17 T=

\ 18. BUFFER name N: N bytes inline from the next slot, in whole cells; the record
\     stays whole cells and the fields around the bytes keep theirs. The count
\     is a number literal above 0, judged at declaration. DERIVE addr answers
\     the bytes as a `SPAN:span<u8>` of N bytes from the field's address.
DECL-DIAG:PROSE                                       \ this section rejects too: keep the output clean
STRUCTURE sdbuf
   DERIVE addr
   FIELD a n
   BUFFER b 13
   FIELD c n
;STRUCTURE
1 TYPED-BUFFER SDB-BUF sdbuf

SDBUF:CELLS 4 T=                                      \ a, ceil(13 / CELL) for b, c
SDBUF:BYTES SDBUF:CELLS CELL * T=
: SDB-GAP ( -- n ) 0 SDB-BUF SDBUF:C NULL-PTR -  0 SDB-BUF SDBUF:A NULL-PTR -  - ;
SDB-GAP 3 CELL * T=                                   \ c lies 3 cells after a
: SDB-BYTES ( -- str ) 0 SDB-BUF SDBUF:B SPAN:$ ;
: SDB-N ( -- n ) 0 SDB-BUF SDBUF:B SPAN:LEN ;
SDB-N 13 T=
: SDB-AT ( -- n ) SDB-BYTES drop NULL-PTR -  0 SDB-BUF NULL-PTR - - ;
SDB-AT CELL T=                                        \ b starts one cell into the record
: SDB-W ( -- ) 5 0 0 6 SDBUF:MAKE 0 SDB-BUF ! ;      \ b's cells are MAKE's middle two
: SDB-FILL ( -- ) SDB-BYTES {: p:^ u:n :} u 0 ?do 255 p i + c! loop ;
: SDB-LAST ( -- n ) SDB-BYTES drop 12 + c@ ;
SDB-W SDB-FILL
SDB-LAST 255 T=
0 SDB-BUF SDBUF:A @ 5 T=                              \ 13 bytes through b leave a and c
0 SDB-BUF SDBUF:C @ 6 T=

s" STRUCTURE sdbhex BUFFER b $10 ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" STRUCTURE sdbzero BUFFER b 0 ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdbzero`\n" DECL-DIAG:HAS? TTRUE
s" 0" s" buffer byte count must be a number above 0" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give the byte count as a number literal above 0, as in `BUFFER bytes 16`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdbword BUFFER b many ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdbword`\n" DECL-DIAG:HAS? TTRUE
s" many" s" buffer byte count must be a number above 0" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give the byte count as a number literal above 0, as in `BUFFER bytes 16`\n" DECL-DIAG:HAS? TTRUE
13 constant SDB-LEN
DECL-DIAG:PROSE
s" STRUCTURE sdbname BUFFER b SDB-LEN ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdbname`\n" DECL-DIAG:HAS? TTRUE
s" SDB-LEN" s" buffer byte count must be a number above 0" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give the byte count as a number literal above 0, as in `BUFFER bytes 16`\n" DECL-DIAG:HAS? TTRUE

\ DERIVE eq, hash and init are each rejected with a buffer.
DECL-DIAG:PROSE
s" STRUCTURE sdbeq DERIVE eq FIELD a n BUFFER b 13 ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdbeq`\n" DECL-DIAG:HAS? TTRUE
s" b" s" derive eq cannot compare a buffer" DECL-DIAG:CAUSE? TTRUE
s\"   = help: drop `eq` from `DERIVE`, or hold the bytes in `FIELD`s\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdbhash DERIVE hash BUFFER b 13 ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdbhash`\n" DECL-DIAG:HAS? TTRUE
s" b" s" derive hash cannot hash a buffer" DECL-DIAG:CAUSE? TTRUE
s\"   = help: drop `hash` from `DERIVE`, or hold the bytes in `FIELD`s\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdbinit DERIVE init BUFFER b 13 ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdbinit`\n" DECL-DIAG:HAS? TTRUE
s" sdbinit" s" derive init requires a canonical fixed-cell record" DECL-DIAG:CAUSE? TTRUE
s\"   = help: drop `init` from `DERIVE`, or give every field a fixed cell width\n" DECL-DIAG:HAS? TTRUE

\ `buffer` is a clause keyword, so it names no family and no field.
DECL-DIAG:PROSE
s" STRUCTURE buffer FIELD a n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `buffer`\n" DECL-DIAG:HAS? TTRUE
s" buffer" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: pick another name: a built-in type such as `n` cannot name a family\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdbfld FIELD buffer n ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdbfld`\n" DECL-DIAG:HAS? TTRUE
s" buffer" s" reserved field name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the field: a declaration word such as `field` or a generated word such as `make`, `tag` or `eq` is taken\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ MAKE takes one item per FIELD and one per buffer cell, and its effect holds
\ EFFECT-DEPTH-MAX (4096) at most: the clause that passes it is rejected.
package SDBIG
$10000 constant CAP            \ bytes: 4097 fields of 13 bytes and the head
create SRC CAP allot
TYPED-VARIABLE SRC-U len
: S+ ( str -- ) SRC CAP SRC-U BUF-APPEND ;
: S+C ( n -- ) SRC CAP SRC-U BUF-APPEND-C ;
: S+LETTER ( n -- ) 26 mod 97 + S+C ;
\ Field I is `fXYZ`, I in base 26 as three letters.
: S+FIELD ( n -- )
   {: i:n :}
   s"  FIELD f" S+
   i 676 / S+LETTER  i 26 / S+LETTER  i S+LETTER
   s"  n" S+ ;
public
: FIELDS ( ^u8 n n -- )
   {: na:^ nu:n k:n :}
   SRC-U BUF-RESET  s" STRUCTURE " S+  na nu S+
   k 0 ?do i S+FIELD loop ;
: SRC+ ( str -- ) S+ ;
: SRC$ ( -- ^u8 n ) SRC SRC-U @ LEN>N ;
;package

\ 4095 fields and one cell of bytes are exactly 4096 items.
s" sdbfull" 4095 SDBIG:FIELDS  s"  BUFFER b 8 ;STRUCTURE" SDBIG:SRC+
SDBIG:SRC$ TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" STRUCTURE sdbover FIELD a n BUFFER b 32761 ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdbover`\n" DECL-DIAG:HAS? TTRUE
s" 32761" s" record takes more than 4096 items to make" DECL-DIAG:CAUSE? TTRUE
s\"   = help: split the record: MAKE takes one item per `FIELD` and one per buffer cell, at most 4096\n" DECL-DIAG:HAS? TTRUE
\ A count past the room is rejected before its cells are counted.
DECL-DIAG:PROSE
s" STRUCTURE sdbhuge BUFFER b $7FFFFFFFFFFFFFF0 ;STRUCTURE" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdbhuge`\n" DECL-DIAG:HAS? TTRUE
s" $7FFFFFFFFFFFFFF0" s" record takes more than 4096 items to make" DECL-DIAG:CAUSE? TTRUE
s\"   = help: split the record: MAKE takes one item per `FIELD` and one per buffer cell, at most 4096\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" sdbmany" 4097 SDBIG:FIELDS  s"  ;STRUCTURE" SDBIG:SRC+
SDBIG:SRC$ TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad structure declaration `sdbmany`\n" DECL-DIAG:HAS? TTRUE
s" fgbo" s" record takes more than 4096 items to make" DECL-DIAG:CAUSE? TTRUE
s\"   = help: split the record: MAKE takes one item per `FIELD` and one per buffer cell, at most 4096\n" DECL-DIAG:HAS? TTRUE
\ A generated word whose effect is too deep to record has no signature text:
\ its rejection quotes none. SDGDEEP:MAKE takes 4095 `n` fields and one
\ `option<n>`, 4097 cells, one past EFFECT-DEPTH-MAX.
DECL-DIAG:PROSE
s" sdgdeep" 4095 SDBIG:FIELDS  s"  FIELD qopt option<n> ;STRUCTURE" SDBIG:SRC+
SDBIG:SRC$ TEST-EVAL:RC 70 T=
s\" error[E-BAD-STORED-SIGNATURE]: the signature of `SDGDEEP:MAKE` cannot be recorded\n" DECL-DIAG:HAS? TTRUE
s\"   = reason: effect too deep to record (depth 4097, at most 4096)\n" DECL-DIAG:HAS? TTRUE
s" = signature:" DECL-DIAG:HAS? TFALSE
DECL-DIAG:OFF
\ A pointee's label holds its whole name, however long
\ (sdgtoscoped has a short one).
1000 constant SDGW-U
: SDGW-Q ( -- ) SDGW-U 7 - 0 ?do s" q" SDBIG:SRC+ loop ;
s" sdgwide" 0 SDBIG:FIELDS  SDGW-Q  s" <p> FIELD v read-view<p,p,u8> ;STRUCTURE" SDBIG:SRC+
SDBIG:SRC$ TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" sdgwideto<p> FIELD q ^sdgwide" 0 SDBIG:FIELDS  SDGW-Q  s" <p> ;STRUCTURE" SDBIG:SRC+
SDBIG:SRC$ TEST-EVAL:RC 70 T=
s" a pointer cannot reach `sdgwideqqqq" DECL-DIAG:HAS? TTRUE
s" qqqq`, which takes a scope or region" DECL-DIAG:HAS? TTRUE
7109 DECL-REJECT:REASON$ nip
s" a pointer cannot reach `" nip SDGW-U +  s" `, which takes a scope or region" nip + T=
DECL-DIAG:OFF
\ An application's arguments wait while a later one is read: the field type
\ `sdnest<n,sdnest<n,…n>>` nested 300 deep holds 300 at once.
s" STRUCTURE sdnest<a,b> DERIVE addr FIELD x a FIELD y b ;STRUCTURE" TEST-EVAL:RC 0 T=
300 constant SDNEST-K
: SDNEST-ARGS ( -- )
   SDNEST-K 0 ?do s" sdnest<n," SDBIG:SRC+ loop  s" n" SDBIG:SRC+
   SDNEST-K 0 ?do s" >" SDBIG:SRC+ loop ;
s" sdndeep DERIVE addr FIELD d " 0 SDBIG:FIELDS  SDNEST-ARGS  s"  ;STRUCTURE" SDBIG:SRC+
SDBIG:SRC$ TEST-EVAL:RC 0 T=
: SDND-GO ( n ^sdndeep -- n )
   dup >r SDNDEEP:D SDNEST:Y SDNEST:X !  r> SDNDEEP:D SDNEST:Y SDNEST:X @ ;
\ A buffer of it checks each nested instance once.
1 TYPED-BUFFER SDND-REC sdndeep
7 0 SDND-REC SDND-GO 7 T=

' struct-replay-test:RP-SDBUF CHECK:SCOPED

T-REPORT
