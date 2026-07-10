\ type-family-suite.f — behavior suite for the package-scoped TFAM/SUMV/product/
\ layout/SCHEMA registries (src/core/type-family.f, src/core/type-schema.f). Run
\ BY THE ENGINE over stdin (registry words resolve only at top-level interpret,
\ never inside a checked ':' body), like test/engine-suite.f:
\     bin/hb < test/type-family-suite.f
\ The harness words below are ordinary checked definitions (public words only);
\ every registry op is a top-level interpret line so the checker-internal words
\ stay in scope. A failure prints F<index> + detail; REPORT exits 1 on any fail.

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
: T$= ( ptr u8 n ptr u8 n -- ) {: ga:ptr gu:n wa:ptr wu:n :}
   #CASE @ 1 + #CASE !
   gu wu <> if
      T-FAIL s" assert string len: expected " type wu . s" got " type gu . cr exit
   then
   0 begin dup gu < while
      dup ga + c@  over wa + c@ <> if
         drop T-FAIL s" assert string byte mismatch" type cr exit
      then
      1+
   repeat drop ;
\ TSNE ( ga gu wa wu -- ) : assert two strings are NOT byte-identical.
: TSNE ( ptr u8 n ptr u8 n -- ) {: ga:ptr gu:n wa:ptr wu:n :}
   #CASE @ 1 + #CASE !
   gu wu <> if exit then
   0 begin dup gu < while
      dup ga + c@  over wa + c@ <> if drop exit then
      1+
   repeat drop
   T-FAIL s" assert strings differ: both " type ga gu type cr ;

\ catch-code stash (TC) + result-flag stash (FOUNDF) + id/node scratch.
variable TC     variable FOUNDF
variable FID    variable PID    variable AID    variable PTID   variable CLID
variable VOK    variable VERR   variable FX     variable NP     variable NC
variable NA     variable R1     variable L0     variable NQ
variable NPTR

\ clean slate (nothing declares families during prefix load, but be explicit).
TFAM-RESET
SCHEMA-RESET

\ F4 (dot habu-tfam-nested-param-09fa2004): TFAM-RESET must de-register the
\ internal `field` family, else its reserved id (normally 15 — the 16th family)
\ dangles and a later family that lands on id 15 is misclassified as a record
\ field. After reset FIELD-FAM is -1; declaring 16 fresh families puts the 16th
\ on id 15, yet field stays de-registered, so no misclassification is possible.
FIELD-FAM @ -1 T=
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a0" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a1" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a2" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a3" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a4" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a5" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a6" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a7" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a8" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a9" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" aa" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" ab" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" ac" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" ad" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" ae" 1 TK-CELL TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" af" 1 TK-CELL TFAM-DECL VOK !
VOK @ 15 T=              \ the 16th fresh family occupies the field family's normal id
FIELD-FAM @ -1 T=        \ yet field stays de-registered — id 15 is not a field param
TFAM-RESET               \ restore the clean slate for the rest of the suite
SCHEMA-RESET

\ ---------------------------------------------------------------------------
\ 1. add / find / arity / kind / visibility / name / package readback.
\ ---------------------------------------------------------------------------
s" pkga" CHECKER-PACKAGE-PRIVATE s" opt"  1 TK-SUM     TFAM-DECL FID !
s" pkgb" CHECKER-PACKAGE-PUBLIC  s" res"  2 TK-SUM     TFAM-DECL PID !
s" pkga" CHECKER-PACKAGE-PRIVATE s" res"  0 TK-ENUM    TFAM-DECL AID !
s" pkgc" CHECKER-PACKAGE-PUBLIC  s" pt"   0 TK-PRODUCT TFAM-DECL PTID !
s" pkgc" CHECKER-PACKAGE-PUBLIC  s" cl"   0 TK-CELL    TFAM-DECL CLID !

FID @ TFAM-ARITY@ 1 T=
PID @ TFAM-ARITY@ 2 T=
AID @ TFAM-ARITY@ 0 T=
FID @ TFAM-KIND@ TK-SUM T=
AID @ TFAM-KIND@ TK-ENUM T=
PTID @ TFAM-KIND@ TK-PRODUCT T=
FID @ TFAM-VIS@ CHECKER-PACKAGE-PRIVATE T=
PID @ TFAM-VIS@ CHECKER-PACKAGE-PUBLIC T=
FID @ TFAM-NAME$ s" opt" T$=
FID @ TFAM-PKG$  s" pkga" T$=

\ kind predicates
FID @ TFAM-SUM? -1 T=      FID @ TFAM-ENUM? 0 T=      FID @ TFAM-LAYOUT? -1 T=
AID @ TFAM-ENUM? -1 T=     AID @ TFAM-SUM? 0 T=
PTID @ TFAM-PRODUCT? -1 T= PTID @ TFAM-LAYOUT? -1 T=
CLID @ TFAM-CELL? -1 T=    CLID @ TFAM-LAYOUT? 0 T=

\ defaults from TFAM-DECL
FID @ TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=
FID @ TFAM-TAGW@ TAGW-CELL T=
FID @ TFAM-SLOTS@ 0 T=

\ ---------------------------------------------------------------------------
\ 2. qualified (exact-package) vs unqualified (active-scope) lookup.
\ ---------------------------------------------------------------------------
s" pkga" s" opt"  TFAM-FIND-IN FOUNDF !  FID @ T=  FOUNDF @ -1 T=
s" pkga" s" nope" TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ 0 T=
s" pkga" s" opt"  TFAM-RESOLVE FOUNDF !  FID @ T=  FOUNDF @ -1 T=
\ pkgc has no 'res' of its own, so resolve reaches pkgb's PUBLIC res (not pkga's
\ private res) — own-package-first + public-elsewhere.
s" pkgc" s" res"  TFAM-RESOLVE FOUNDF !  PID @ T=  FOUNDF @ -1 T=

\ ---------------------------------------------------------------------------
\ 3. public / private isolation.
\ ---------------------------------------------------------------------------
s" pkgb" s" opt" TFAM-RESOLVE FOUNDF ! drop  FOUNDF @ 0 T=
s" res" TFAM-FIND-PUBLIC FOUNDF !  PID @ T=  FOUNDF @ -1 T=
s" opt" TFAM-FIND-PUBLIC FOUNDF ! drop  FOUNDF @ 0 T=

\ ---------------------------------------------------------------------------
\ 4. same tail across different packages -> distinct ids, both findable.
\ ---------------------------------------------------------------------------
AID @ PID @ = 0 T=
s" pkga" s" res" TFAM-FIND-IN FOUNDF !  AID @ T=  FOUNDF @ -1 T=
s" pkgb" s" res" TFAM-FIND-IN FOUNDF !  PID @ T=  FOUNDF @ -1 T=
AID @ TFAM-ARITY@ 0 T=    PID @ TFAM-ARITY@ 2 T=

\ ---------------------------------------------------------------------------
\ 5. duplicate rejection within a package (throws E-TFAM-DUP).
\    stack before catch: pkg-a pkg-u vis name-a name-u arity kind  (7 cells)
\ ---------------------------------------------------------------------------
s" pkga" CHECKER-PACKAGE-PRIVATE s" opt" 1 TK-SUM ' TFAM-DECL catch
   TC ! 2drop 2drop 2drop drop  TC @ E-TFAM-DUP T=

\ ---------------------------------------------------------------------------
\ 6. uppercase / mixed-case rejection at the declaration boundary.
\ ---------------------------------------------------------------------------
s" result"  TF-CANON? -1 T=
s" opt-2"   TF-CANON? -1 T=
s" a-b-c"   TF-CANON? -1 T=       \ internal single hyphens are fine
s" Result"  TF-CANON? 0 T=
s" reSult"  TF-CANON? 0 T=
s" RESULT"  TF-CANON? 0 T=
s" 123"     TF-CANON? 0 T=
s" @x"      TF-CANON? 0 T=
\ internal-only single hyphens: leading / trailing / doubled '-' reject
\ (item 8's '-'->'--' constructor-package escaping depends on this canon).
s" -a"      TF-CANON? 0 T=
s" a-"      TF-CANON? 0 T=
s" a--b"    TF-CANON? 0 T=
s" -"       TF-CANON? 0 T=
s" pkga" CHECKER-PACKAGE-PRIVATE s" Result" 0 TK-SUM ' TFAM-DECL catch
   TC ! 2drop 2drop 2drop drop  TC @ E-TFAM-CASE T=
s" pkga" CHECKER-PACKAGE-PRIVATE s" MiXeD" 0 TK-SUM ' TFAM-DECL catch
   TC ! 2drop 2drop 2drop drop  TC @ E-TFAM-CASE T=

\ ---------------------------------------------------------------------------
\ 7. no hidden-field ('@name') lookup from public signatures.
\ ---------------------------------------------------------------------------
s" @opt.slot0" TF-HIDDEN? -1 T=
s" @res.tag"   TF-HIDDEN? -1 T=       \ item-7 tag row shape is hidden too
s" opt"        TF-HIDDEN? 0 T=
s" pkga" s" @opt.slot0" TFAM-RESOLVE FOUNDF ! drop  FOUNDF @ 0 T=
s" pkgb" s" @res.tag"   TFAM-RESOLVE FOUNDF ! drop  FOUNDF @ 0 T=

\ ---------------------------------------------------------------------------
\ 8. field setters / getters (record stores layout/slots/ranges/tagw/span/pk).
\ ---------------------------------------------------------------------------
FID @ TL-PACKED-TAG TFAM-LAYOUT!   FID @ TFAM-LAYOUT-POLICY@ TL-PACKED-TAG T=
FID @ 3 TFAM-SLOTS!                FID @ TFAM-SLOTS@ 3 T=
FID @ 16 TFAM-TAGW!                FID @ TFAM-TAGW@ 16 T=
FID @ 5 9 TFAM-VAR-RANGE!          FID @ TFAM-VAR-START@ 5 T=  FID @ TFAM-VAR-COUNT@ 9 T=
FID @ 2 4 TFAM-FLD-RANGE!          FID @ TFAM-FLD-START@ 2 T=  FID @ TFAM-FLD-COUNT@ 4 T=
FID @ 7 TFAM-SCHEMA-ROOT!          FID @ TFAM-SCHEMA-ROOT@ 7 T=
FID @ 40 6 TFAM-SPAN!              FID @ TFAM-SPAN@ 6 T= 40 T=
FID @ 0 TFAM-PK@ PK-CELL T=
FID @ 0 PK-TYPE TFAM-PK!           FID @ 0 TFAM-PK@ PK-TYPE T=
PID @ 0 TFAM-PK@ PK-CELL T=        PID @ 1 TFAM-PK@ PK-CELL T=

\ ---------------------------------------------------------------------------
\ 9. SCHEMA nodes: valid builders, malformed rejection, root pool + growth.
\    SCH nodes seed cap 4, roots seed cap 4 -> add >4 of each to force a grow.
\ ---------------------------------------------------------------------------
0 SCHEMA-PARAM NP !    NP @ SCHEMA-TAG@ SCH-PARAM T=   NP @ SCHEMA-A@ 0 T=
1 SCHEMA-CON   NC !    NC @ SCHEMA-TAG@ SCH-CON T=     NC @ SCHEMA-A@ 1 T=
FID @ 0 1 SCHEMA-APP NA !   NA @ SCHEMA-TAG@ SCH-APP T=   NA @ SCHEMA-C@ 1 T=
NP @ SCHEMA-PARAM? -1 T=    NC @ SCHEMA-CON? -1 T=       NA @ SCHEMA-APP? -1 T=
1 SCHEMA-PARAM drop   2 SCHEMA-CON drop   3 SCHEMA-PARAM drop   \ >4 nodes -> SCH grew
SCHEMA-N@ 7 T=                                          \ ids 1..6 created (nil is 0)
\ malformed tag rejected (tag a b c = 4 cells before catch)
999 0 0 0 ' SCHEMA-NEW catch   TC ! 2drop 2drop  TC @ E-SCHEMA-BAD T=
\ malformed paramref (negative index) rejected (1 cell before catch)
-1 ' SCHEMA-PARAM catch   TC ! drop  TC @ E-SCHEMA-BAD T=
\ root pool: 5 roots > seed cap 4 -> SCH-ROOT grew
NP @ SCHEMA-ROOT+ R1 !   R1 @ SCHEMA-ROOT@ NP @ T=
NC @ SCHEMA-ROOT+ drop   NA @ SCHEMA-ROOT+ drop
NP @ SCHEMA-ROOT+ drop   NC @ SCHEMA-ROOT+ drop
SCHEMA-ROOT-N@ 5 T=

\ SC-QUOT quotation payload node (dot habu-tfam-4-remainder): four row roots
\ (din,dout,rin,rout) + hasr flag, round-trip read-back, hasr normalization, and
\ malformed-row rejection (a child that is not a live schema node).
NP @ NC @ NA @ NP @ -1 SCHEMA-QUOT NQ !
NQ @ SCHEMA-TAG@ SCH-QUOT T=   NQ @ SCHEMA-QUOT? -1 T=
NQ @ SCHEMA-PARAM? 0 T=        NQ @ SCHEMA-C@ SCH-QUOT-ROWS T=
NQ @ SCHEMA-QUOT-HASR@ -1 T=
NQ @ SCHEMA-QUOT-DIN@  NP @ T=   NQ @ SCHEMA-QUOT-DOUT@ NC @ T=
NQ @ SCHEMA-QUOT-RIN@  NA @ T=   NQ @ SCHEMA-QUOT-ROUT@ NP @ T=
NC @ NC @ NC @ NC @ 0 SCHEMA-QUOT SCHEMA-QUOT-HASR@ 0 T=   \ hasr normalizes to 0
\ malformed row = nil node (0) rejected (5 args before catch: din dout rin rout hasr)
NP @ NC @ NA @ 0 -1 ' SCHEMA-QUOT catch   TC ! 2drop 2drop drop  TC @ E-SCHEMA-BAD T=
\ malformed row = out-of-range node rejected
NP @ NC @ NA @ 99999 -1 ' SCHEMA-QUOT catch   TC ! 2drop 2drop drop  TC @ E-SCHEMA-BAD T=

\ SC-PTR pointer payload node (PLAN item 6, docs §8 SC-PTR): child round-trip,
\ nesting, predicate discrimination, and malformed-child rejection.
NC @ SCHEMA-PTR NPTR !
NPTR @ SCHEMA-TAG@ SCH-PTR T=   NPTR @ SCHEMA-PTR? -1 T=
NPTR @ SCHEMA-CON? 0 T=         NPTR @ SCHEMA-A@ NC @ T=
NPTR @ SCHEMA-PTR SCHEMA-A@ NPTR @ T=       \ ptr ptr X nests
NC @ SCHEMA-PTR? 0 T=
\ malformed child = nil node (0) / out-of-range node rejected (1 cell before catch)
0 ' SCHEMA-PTR catch   TC ! drop  TC @ E-SCHEMA-BAD T=
99999 ' SCHEMA-PTR catch   TC ! drop  TC @ E-SCHEMA-BAD T=

\ ---------------------------------------------------------------------------
\ 10. SUMV variants: add, per-family key, dup rejection, cross-family reuse.
\    SUMV-ADD ( fam name-a name-u tag sch-start sch-count paycells -- id )
\ ---------------------------------------------------------------------------
FID @ s" ok"  0 0 0 0 SUMV-ADD VOK !    VOK @ SUMV-FAM@ FID @ T=   VOK @ SUMV-TAG@ 0 T=
FID @ s" err" 1 0 0 1 SUMV-ADD VERR !   VERR @ SUMV-NAME$ s" err" T$=   VERR @ SUMV-PAYCELLS@ 1 T=
PID @ s" ok"  0 0 0 0 SUMV-ADD drop     \ same 'ok' tail under a different family is fine
PID @ s" err" 1 0 0 0 SUMV-ADD drop
AID @ s" red"   0 0 0 0 SUMV-ADD drop
AID @ s" green" 1 0 0 0 SUMV-ADD drop   \ 6 variants > seed cap 4 -> SUMV grew
FID @ s" ok" SUMV-FIND FOUNDF !  VOK @ T=  FOUNDF @ -1 T=
PID @ s" ok" SUMV-FIND FOUNDF ! drop  FOUNDF @ -1 T=
FID @ s" none" SUMV-FIND FOUNDF ! drop  FOUNDF @ 0 T=
FID @ s" ok" 0 0 0 0 ' SUMV-ADD catch   TC ! 2drop 2drop 2drop drop  TC @ E-TFAM-DUP T=

\ ---------------------------------------------------------------------------
\ 11. product fields: add, per-family key, dup rejection.
\    PF-ADD ( fam name-a name-u sch slot -- id )
\ ---------------------------------------------------------------------------
PTID @ s" x" 0 0 PF-ADD FX !            FX @ PF-FAM@ PTID @ T=   FX @ PF-SLOT@ 0 T=
PTID @ s" y" 0 1 PF-ADD drop
PTID @ s" z" 0 2 PF-ADD drop
PID  @ s" a" 0 0 PF-ADD drop
PID  @ s" b" 0 1 PF-ADD drop            \ 5 fields > seed cap 4 -> PF grew
PTID @ s" x" PF-FIND FOUNDF !  FX @ T=  FOUNDF @ -1 T=
PTID @ s" q" PF-FIND FOUNDF ! drop  FOUNDF @ 0 T=
PTID @ s" x" 0 0 ' PF-ADD catch   TC ! 2drop 2drop drop  TC @ E-TFAM-DUP T=

\ ---------------------------------------------------------------------------
\ 12. layout records: one per family, keyed by family; dup rejection.
\    LAY-ADD ( fam policy size align tagw -- id )
\ ---------------------------------------------------------------------------
FID  @ TL-STACK-CELL-TAG 16 8 8 LAY-ADD L0 !   L0 @ LAY-FAM@ FID @ T=   L0 @ LAY-SIZE@ 16 T=
PID  @ TL-PACKED-TAG     24 8 4 LAY-ADD drop
AID  @ TL-STACK-CELL-TAG  8 8 8 LAY-ADD drop
PTID @ TL-BOXED           8 8 8 LAY-ADD drop
CLID @ TL-CUSTOM          8 8 8 LAY-ADD drop    \ 5 layouts > seed cap 4 -> LAY grew
FID @ LAY-FIND FOUNDF !  L0 @ T=  FOUNDF @ -1 T=
CLID @ LAY-FIND FOUNDF !  LAY-POLICY@ TL-CUSTOM T=  FOUNDF @ -1 T=
FID @ TL-STACK-CELL-TAG 8 8 8 ' LAY-ADD catch   TC ! 2drop 2drop drop  TC @ E-TFAM-DUP T=

\ ---------------------------------------------------------------------------
\ 12b. constructor package-name derivation (PLAN Package Shape, docs §12; item 8).
\    TF-CTOR-PKG$ ( pkg-a pkg-u tail-a tail-u -- ctor-a ctor-u ): uppercase the
\    package segment and family tail, escape a literal '-' inside the segment as
\    '--', join package-then-tail with a single '-'; when the escaped spelling
\    exceeds the 16-byte inline dictionary name limit, the name is `T` + the
\    first 16 lowercase hex digits of SHA-256 over the length-prefixed segment
\    list + `-` + the uppercase tail. Pure, injective, stable (no alloc-order id).
\ ---------------------------------------------------------------------------
variable CPA   variable CPU   variable CQA   variable CQU
\ top level: bare uppercased tail, no separator.
s" " s" result" TF-CTOR-PKG$ s" RESULT" T$=
\ in-package: PKG-TAIL.
s" pkg" s" result" TF-CTOR-PKG$ s" PKG-RESULT" T$=
s" opt" s" some"   TF-CTOR-PKG$ s" OPT-SOME" T$=
\ digits pass through unchanged.
s" v2" s" ok"      TF-CTOR-PKG$ s" V2-OK" T$=
\ injectivity across the hyphen boundary: every joined segment (package AND
\ tail) escapes '-' as '--', so all three hyphen splits stay distinct:
\   a-b + c  ->  A--B-C      a + b-c  ->  A-B--C      "" + a-b-c -> A--B--C
s" a-b" s" c"      TF-CTOR-PKG$ s" A--B-C" T$=
s" a"   s" b-c"    TF-CTOR-PKG$ s" A-B--C" T$=
s" "    s" a-b-c"  TF-CTOR-PKG$ s" A--B--C" T$=
\ determinism: identical inputs -> byte-identical output.
s" pkg" s" result" TF-CTOR-PKG$ s" PKG-RESULT" T$=

\ SHA-256 fallback: escaped `VERYLONGPACKAGENAME-RESULT` is 26 bytes > 16, so the
\ derived name is `T` + 16 hex + `-RESULT` = 24 bytes. Structure asserted here;
\ the exact hash goldens (determinism + injectivity + algorithm pin) follow.
s" verylongpackagename" s" result" TF-CTOR-PKG$ CPU ! CPA !
CPU @ 24 T=
CPA @ 1 s" T" T$=                           \ prefix marker
CPA @ 17 + 1 s" -" T$=                      \ separator after the 16-hex hash
CPA @ 18 + 6 s" RESULT" T$=                 \ uppercase family tail suffix
\ every hash byte is a lowercase hex digit (0-9 a-f).
: HEXLC? ( n -- bool ) {: c:n :}
   c 48 >= c 57 <= and   c 97 >= c 102 <= and   or ;
: HEX16? ( ptr u8 -- bool ) {: p:ptr :}
   0 begin dup 16 < while
      dup p + c@ HEXLC? 0= if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;
CPA @ 1 + HEX16? -1 T=
\ TF-CTOR-PKG$ returns a pointer into the shared derivation buffer, so intern a
\ stable copy of the first result before deriving again.
variable CPOFF
CPA @ CPU @ TF-INTERN CPOFF !
\ determinism: the same long input reproduces the same derived name.
s" verylongpackagename" s" result" TF-CTOR-PKG$ CQU ! CQA !
CQA @ CQU @  CPOFF @ CPU @ TF-OFF$  T$=
\ injectivity: a different long package hashes to a different name (the hash
\ region separates inputs that share length and tail).
s" verylongpackagenamx" s" result" TF-CTOR-PKG$ CQU ! CQA !
CQA @ CQU @  CPOFF @ CPU @ TF-OFF$  TSNE   \ NOT equal to the first long name
\ exact golden pins the pinned algorithm byte-for-byte:
\ SHA-256(0x13 "verylongpackagename") = 92a8624462e75ea4... (independent impl).
s" verylongpackagename" s" result" TF-CTOR-PKG$ s" T92a8624462e75ea4-RESULT" T$=
\ a long family tail with an empty package: fallback hashes the empty segment
\ list, tail still appended (verylongfamilyname = 18 bytes > 16).
s" " s" verylongfamilyname" TF-CTOR-PKG$ CQU ! CQA !
CQU @ 36 T=                                 \ T(1)+16 hex+ -(1)+VERYLONGFAMILYNAME(18)
CQA @ 1 s" T" T$=
CQA @ 1 + HEX16? -1 T=
CQA @ 18 + 18 s" VERYLONGFAMILYNAME" T$=
\ empty segment list golden: SHA-256("") = e3b0c44298fc1c14... (FIPS-180 constant).
s" " s" verylongfamilyname" TF-CTOR-PKG$ s" Te3b0c44298fc1c14-VERYLONGFAMILYNAME" T$=

\ SV.CTOR-PKG metadata slot: friend writer/reader round-trip through the pool.
\ VOK is a live variant id from section 10; storing its constructor package name
\ leaves the other variant fields untouched.
variable RPK
s" RESULT" TF-INTERN RPK !
VOK @ SUMV-CTOR-PKG$ nip 0 T=               \ unset variants report an empty name
VOK @ RPK @ 6 SUMV-CTOR-PKG!
VOK @ SUMV-CTOR-PKG$ s" RESULT" T$=
VOK @ SUMV-TAG@ 0 T=                        \ tag field intact after the CTOR write

\ ---------------------------------------------------------------------------
\ 13. grow across the TFAM record / string / param-kind seed caps, then prove
\    family id 0 survives every relocation.
\ ---------------------------------------------------------------------------
s" pkgd" CHECKER-PACKAGE-PUBLIC s" tree"  1 TK-SUM     TFAM-DECL drop
s" pkgd" CHECKER-PACKAGE-PUBLIC s" list"  1 TK-SUM     TFAM-DECL drop
s" pkgd" CHECKER-PACKAGE-PUBLIC s" maybe" 1 TK-SUM     TFAM-DECL drop
s" pkge" CHECKER-PACKAGE-PUBLIC s" pair"  2 TK-PRODUCT TFAM-DECL drop
FID @ TFAM-NAME$ s" opt" T$=
FID @ TFAM-PKG$  s" pkga" T$=
FID @ TFAM-ARITY@ 1 T=
FID @ TFAM-KIND@ TK-SUM T=
FID @ 0 TFAM-PK@ PK-TYPE T=
s" pkgd" s" tree" TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=
TFAM-N@ 9 T=

\ ---------------------------------------------------------------------------
\ 14. snapshot persist/restore: run the exact words CHECKER-SNAPSHOT-PREPARE
\    invokes and prove every store reads back identically after the bake.
\ ---------------------------------------------------------------------------
TFAM-SNAPSHOT-PERSIST
SCHEMA-SNAPSHOT-PERSIST
FID @ TFAM-NAME$ s" opt" T$=
FID @ TFAM-ARITY@ 1 T=
FID @ TFAM-KIND@ TK-SUM T=
FID @ 0 TFAM-PK@ PK-TYPE T=
FID @ TFAM-SLOTS@ 3 T=
s" pkgb" s" res" TFAM-FIND-IN FOUNDF ! PID @ T= FOUNDF @ -1 T=
FID @ s" ok" SUMV-FIND FOUNDF ! VOK @ T= FOUNDF @ -1 T=
PTID @ s" x" PF-FIND FOUNDF ! FX @ T= FOUNDF @ -1 T=
FID @ LAY-FIND FOUNDF ! LAY-SIZE@ 16 T= FOUNDF @ -1 T=
R1 @ SCHEMA-ROOT@ SCHEMA-TAG@ SCH-PARAM T=
NA @ SCHEMA-TAG@ SCH-APP T=
\ SC-QUOT node (NQ, built in section 9: din=NP dout=NC rin=NA rout=NP hasr=-1)
\ survives the bake: tag, row roots, and hasr read back from the persisted node
\ arena + root pool (destruction review finding 3).
NQ @ SCHEMA-TAG@ SCH-QUOT T=
NQ @ SCHEMA-QUOT-DIN@  NP @ T=
NQ @ SCHEMA-QUOT-ROUT@ NP @ T=
NQ @ SCHEMA-QUOT-HASR@ -1 T=

\ ---------------------------------------------------------------------------
\ 15. ambiguous unqualified public resolution: two OTHER-package publics sharing
\    a tail throw E-TFAM-AMBIG; an own-package match still wins without ambiguity;
\    qualified (exact-package) access resolves both distinctly. (dot 2a)
\ ---------------------------------------------------------------------------
variable AX  variable AY
s" pkgx" CHECKER-PACKAGE-PUBLIC s" amb" 1 TK-SUM TFAM-DECL AX !
s" pkgy" CHECKER-PACKAGE-PUBLIC s" amb" 1 TK-SUM TFAM-DECL AY !
\ unqualified resolve from a third package: two publics tie -> throw
s" pkgz" s" amb" ' TFAM-RESOLVE catch  TC ! 2drop 2drop  TC @ E-TFAM-AMBIG T=
\ bare cross-package public lookup throws on the same tie
s" amb" ' TFAM-FIND-PUBLIC catch  TC ! 2drop  TC @ E-TFAM-AMBIG T=
\ own-package family wins without ambiguity (each resolves to its own amb)
s" pkgx" s" amb" TFAM-RESOLVE FOUNDF !  AX @ T=  FOUNDF @ -1 T=
s" pkgy" s" amb" TFAM-RESOLVE FOUNDF !  AY @ T=  FOUNDF @ -1 T=
\ qualified (exact-package) access still resolves both distinctly, no throw
s" pkgx" s" amb" TFAM-FIND-IN FOUNDF !  AX @ T=  FOUNDF @ -1 T=
s" pkgy" s" amb" TFAM-FIND-IN FOUNDF !  AY @ T=  FOUNDF @ -1 T=
\ a single public tail (no tie) still resolves cleanly through FIND-PUBLIC
s" pkgx" CHECKER-PACKAGE-PUBLIC s" solo" 0 TK-ENUM TFAM-DECL drop
s" solo" TFAM-FIND-PUBLIC FOUNDF ! drop  FOUNDF @ -1 T=

\ ---------------------------------------------------------------------------
\ item 10 slice 1: TFL-* compiler-facing lowering surface (dot
\ habu-tfam-10-native design A) — pure folded resolution + tag/pad metadata
\ the native construct/MATCH emitters call by name at token positions. Same
\ scope rules as the checker friend XTs (owner-only construct, signature-scope
\ match), no diagnostic latch, no checker-row effect.
\ ---------------------------------------------------------------------------
variable LID   variable LVID
SUMTYPE lres 0
  VARIANT lok  n   ;VARIANT
  VARIANT lerr n n ;VARIANT
  VARIANT lnil     ;VARIANT
;SUMTYPE
s" " s" lres" TFAM-FIND-IN FOUNDF !  LID !  FOUNDF @ -1 T=
\ construct one-shot -> ( tag pads ok ); pads = M-p with M = 2 (widest payload)
s" lres" s" lok"  TFL-CON? FOUNDF !  1 T=  0 T=  FOUNDF @ -1 T=
s" lres" s" lerr" TFL-CON? FOUNDF !  0 T=  1 T=  FOUNDF @ -1 T=
s" lres" s" lnil" TFL-CON? FOUNDF !  2 T=  2 T=  FOUNDF @ -1 T=
\ raw engine tokens fold: uppercase spellings agree with the declaration
s" LRES" s" LOK" TFL-CON? FOUNDF !  1 T=  0 T=  FOUNDF @ -1 T=
\ misses fail pure (no throw, no diagnostic): unknown family/variant, cell kind
s" nosuch" s" lok" TFL-CON? FOUNDF !  0 T=  0 T=  FOUNDF @ 0 T=
s" lres" s" nope"  TFL-CON? FOUNDF !  0 T=  0 T=  FOUNDF @ 0 T=
s" span" s" lok"   TFL-CON? FOUNDF !  0 T=  0 T=  FOUNDF @ 0 T=
\ owner-only construct scope: pkgx's public solo does NOT construct from here
s" solo" TFL-CON-FAM? FOUNDF ! drop  FOUNDF @ 0 T=
\ match resolution is signature scope: own ("" top level), unique public,
\ qualified; ambiguous publics and non-sum kinds fail pure
s" lres" TFL-MATCH-FAM? FOUNDF !  LID @ T=  FOUNDF @ -1 T=
s" solo" TFL-MATCH-FAM? FOUNDF ! drop  FOUNDF @ -1 T=
s" pkgx:amb" TFL-MATCH-FAM? FOUNDF !  AX @ T=  FOUNDF @ -1 T=
s" amb"  TFL-MATCH-FAM? FOUNDF ! drop  FOUNDF @ 0 T=
s" span" TFL-MATCH-FAM? FOUNDF ! drop  FOUNDF @ 0 T=
\ variant resolve + per-variant metadata (folded)
s" LERR" LID @ TFL-VAR? FOUNDF !  LVID !  FOUNDF @ -1 T=
LVID @ SUMV-TAG@ 1 T=
LID @ LVID @ TFL-VPADS 0 T=
s" zzz" LID @ TFL-VAR? FOUNDF ! drop  FOUNDF @ 0 T=
\ variant one-shot for a resolved fam (the engine's state-2 bridge call)
s" lnil" LID @ TFL-CVAR? FOUNDF !  2 T=  2 T=  FOUNDF @ -1 T=
s" nope" LID @ TFL-CVAR? FOUNDF !  0 T=  0 T=  FOUNDF @ 0 T=
s" TFL-SURFACE" type cr

\ ---------------------------------------------------------------------------
\ packed ABI descriptor (docs §22.2, policy TL-PACKED-TAG). PACKED-NARROW picks
\ the smallest byte tag width holding a K-variant tag; PACKED-DESC composes
\ ( size align tagw ) with cell payloads (align CELL) and the narrowed tag placed
\ last, SIZE the aligned record stride. Computed for ANY family regardless of its
\ declared policy (the accept-flip that populates LAY on POLICY packed-tag is a
\ later sub-slice); private families (package pkpk) keep the protected-WID seal
\ cap untouched (dot habu-seal-protwid-cap-6f1c9d2b).
\ ---------------------------------------------------------------------------
0 PACKED-NARROW 0 T=
1 PACKED-NARROW 1 T=
256 PACKED-NARROW 1 T=
257 PACKED-NARROW 2 T=
65536 PACKED-NARROW 2 T=
65537 PACKED-NARROW 4 T=
1 32 lshift PACKED-NARROW 4 T=
1 32 lshift 1 + PACKED-NARROW 8 T=
variable PSZ  variable PAL  variable PTW  variable PKI
package pkpk
ENUM pkpke red green blue ;ENUM
SUMTYPE pkpks 1 VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE
PRODUCT pkpkp 0 FIELD x n FIELD y n ;PRODUCT
end-package
\ enum (3 variants, no payload): tag-only u8 -> size 1 align 1 tagw 1
s" pkpk" s" pkpke" TFAM-FIND-IN drop PKI !
PKI @ PACKED-DESC PTW ! PAL ! PSZ !
PSZ @ 1 T=   PAL @ 1 T=   PTW @ 1 T=
\ sum (2 variants, M=1 cell): tag u8 after one cell -> align_up(8+1,8)=16, align 8, tagw 1
s" pkpk" s" pkpks" TFAM-FIND-IN drop PKI !
PKI @ PACKED-DESC PTW ! PAL ! PSZ !
PSZ @ 16 T=  PAL @ 8 T=   PTW @ 1 T=
\ product (2 cell fields, no tag): align_up(16,8)=16, align 8, tagw 0
s" pkpk" s" pkpkp" TFAM-FIND-IN drop PKI !
PKI @ PACKED-DESC PTW ! PAL ! PSZ !
PSZ @ 16 T=  PAL @ 8 T=   PTW @ 0 T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-family-suite: failures" 1 die ;
REPORT
