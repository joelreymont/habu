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
: T-PF-DROP ( n n n ptr u8 n n n n n n n n -- )
   2drop 2drop 2drop 2drop 2drop 2drop ;

\ catch-code stash (TC) + result-flag stash (FOUNDF) + id/node scratch.
variable TC     variable FOUNDF
variable FID    variable PID    variable AID    variable PTID   variable CLID
variable VOK    variable VERR   variable FX     variable NP     variable NC
variable NA     variable R1     variable L0     variable NQ
variable NPTR   variable WBX
\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): checker-internal colon
\ words probed at top level go through named trusted shims.
TRUSTED: TWX-CHECKER-SNAPSHOT-PREPARE ( -- ) CHECKER-SNAPSHOT-PREPARE ;
TRUSTED: TWX-FRESH ( -- n ) FRESH ;
TRUSTED: TWX-LAY-ADD ( n n n n n -- n ) LAY-ADD ;
TRUSTED: TWX-LAY-ALIGN@ ( n -- n ) LAY-ALIGN@ ;
TRUSTED: TWX-LAY-FAM@ ( n -- n ) LAY-FAM@ ;
TRUSTED: TWX-LAY-FIND ( n -- n bool ) LAY-FIND ;
TRUSTED: TWX-LAY-POLICY@ ( n -- n ) LAY-POLICY@ ;
TRUSTED: TWX-LAY-SIZE@ ( n -- n ) LAY-SIZE@ ;
TRUSTED: TWX-LAY-TAGW@ ( n -- n ) LAY-TAGW@ ;
TRUSTED: TWX-PACKED-DESC ( n -- n n n ) PACKED-DESC ;
TRUSTED: TWX-PACKED-NARROW ( n -- n ) PACKED-NARROW ;
TRUSTED: TWX-PF-BEGIN ( -- n ) PF-BEGIN ;
TRUSTED: TWX-PF-ADD ( n n n ptr u8 n n n n n n n n -- n ) PF-ADD ;
TRUSTED: TWX-PF-COMMIT ( n -- ) PF-COMMIT ;
TRUSTED: TWX-PF-ROLLBACK ( n -- ) PF-ROLLBACK ;
TRUSTED: TWX-SCHEMA-A@ ( n -- n ) SCHEMA-A@ ;
TRUSTED: TWX-SCHEMA-APP ( n n n -- n ) SCHEMA-APP ;
TRUSTED: TWX-SCHEMA-APP? ( n -- bool ) SCHEMA-APP? ;
TRUSTED: TWX-SCHEMA-C@ ( n -- n ) SCHEMA-C@ ;
TRUSTED: TWX-SCHEMA-CON ( n -- n ) SCHEMA-CON ;
TRUSTED: TWX-SCHEMA-CON? ( n -- bool ) SCHEMA-CON? ;
TRUSTED: TWX-SCHEMA-NEW ( n n n n -- n ) SCHEMA-NEW ;
TRUSTED: TWX-SCHEMA-PARAM ( n -- n ) SCHEMA-PARAM ;
TRUSTED: TWX-SCHEMA-PARAM? ( n -- bool ) SCHEMA-PARAM? ;
TRUSTED: TWX-SCHEMA-PTR ( n -- n ) SCHEMA-PTR ;
TRUSTED: TWX-SCHEMA-PTR? ( n -- bool ) SCHEMA-PTR? ;
TRUSTED: TWX-SCHEMA-QUOT ( n n n n n -- n ) SCHEMA-QUOT ;
TRUSTED: TWX-SCHEMA-QUOT-DIN@ ( n -- n ) SCHEMA-QUOT-DIN@ ;
TRUSTED: TWX-SCHEMA-QUOT-DOUT@ ( n -- n ) SCHEMA-QUOT-DOUT@ ;
TRUSTED: TWX-SCHEMA-QUOT-HASR@ ( n -- n ) SCHEMA-QUOT-HASR@ ;
TRUSTED: TWX-SCHEMA-QUOT-RIN@ ( n -- n ) SCHEMA-QUOT-RIN@ ;
TRUSTED: TWX-SCHEMA-QUOT-ROUT@ ( n -- n ) SCHEMA-QUOT-ROUT@ ;
TRUSTED: TWX-SCHEMA-QUOT? ( n -- bool ) SCHEMA-QUOT? ;
TRUSTED: TWX-SCHEMA-RESET ( -- ) SCHEMA-RESET ;
TRUSTED: TWX-SCHEMA-ROOT+ ( n -- n ) SCHEMA-ROOT+ ;
TRUSTED: TWX-SCHEMA-ROOT@ ( n -- n ) SCHEMA-ROOT@ ;
TRUSTED: TWX-SCHEMA-SNAPSHOT-PERSIST ( -- ) SCHEMA-SNAPSHOT-PERSIST ;
TRUSTED: TWX-SCHEMA-TAG@ ( n -- n ) SCHEMA-TAG@ ;
TRUSTED: TWX-SUMV-ADD ( n ptr u8 n n n n n -- n ) SUMV-ADD ;
TRUSTED: TWX-SUMV-CTOR-PKG! ( n n n -- ) SUMV-CTOR-PKG! ;
TRUSTED: TWX-SUMV-FAM@ ( n -- n ) SUMV-FAM@ ;
TRUSTED: TWX-SUMV-FIND ( n ptr u8 n -- n bool ) SUMV-FIND ;
TRUSTED: TWX-SUMV-PAYCELLS@ ( n -- n ) SUMV-PAYCELLS@ ;
TRUSTED: TWX-SUMV-TAG@ ( n -- n ) SUMV-TAG@ ;
TRUSTED: TWX-TF-CANON? ( ptr u8 n -- bool ) TF-CANON? ;
TRUSTED: TWX-TF-CTOR-PKG$ ( ptr u8 n ptr u8 n -- ptr u8 n ) TF-CTOR-PKG$ ;
TRUSTED: TWX-TF-HIDDEN? ( ptr u8 n -- bool ) TF-HIDDEN? ;
TRUSTED: TWX-TF-INTERN ( ptr u8 n -- n ) TF-INTERN ;
TRUSTED: TWX-TF-OFF$ ( n n -- ptr u8 n ) TF-OFF$ ;
TRUSTED: TWX-TFAM-CELL? ( n -- bool ) TFAM-CELL? ;
TRUSTED: TWX-TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: TWX-TFAM-ENUM? ( n -- bool ) TFAM-ENUM? ;
TRUSTED: TWX-TFAM-FIND-IN ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN ;
TRUSTED: TWX-TFAM-FIND-PUBLIC ( ptr u8 n -- n bool ) TFAM-FIND-PUBLIC ;
TRUSTED: TWX-TFAM-FLD-COUNT@ ( n -- n ) TFAM-FLD-COUNT@ ;
TRUSTED: TWX-TFAM-FLD-RANGE! ( n n n -- ) TFAM-FLD-RANGE! ;
TRUSTED: TWX-TFAM-FLD-START@ ( n -- n ) TFAM-FLD-START@ ;
TRUSTED: TWX-TFAM-LAYOUT! ( n n -- ) TFAM-LAYOUT! ;
TRUSTED: TWX-TFAM-LAYOUT-POLICY@ ( n -- n ) TFAM-LAYOUT-POLICY@ ;
TRUSTED: TWX-TFAM-LAYOUT? ( n -- bool ) TFAM-LAYOUT? ;
TRUSTED: TWX-TFAM-PK! ( n n n -- ) TFAM-PK! ;
TRUSTED: TWX-TFAM-PK@ ( n n -- n ) TFAM-PK@ ;
TRUSTED: TWX-TFAM-PKG$ ( n -- ptr u8 n ) TFAM-PKG$ ;
TRUSTED: TWX-TFAM-PRODUCT? ( n -- bool ) TFAM-PRODUCT? ;
TRUSTED: TWX-TFAM-RESET ( -- ) TFAM-RESET ;
TRUSTED: TWX-TFAM-RESOLVE ( ptr u8 n ptr u8 n -- n bool ) TFAM-RESOLVE ;
TRUSTED: TWX-TFAM-SCHEMA-ROOT! ( n n -- ) TFAM-SCHEMA-ROOT! ;
TRUSTED: TWX-TFAM-SCHEMA-ROOT@ ( n -- n ) TFAM-SCHEMA-ROOT@ ;
TRUSTED: TWX-TFAM-SLOTS! ( n n -- ) TFAM-SLOTS! ;
TRUSTED: TWX-TFAM-SLOTS@ ( n -- n ) TFAM-SLOTS@ ;
TRUSTED: TWX-TFAM-SNAPSHOT-PERSIST ( -- ) TFAM-SNAPSHOT-PERSIST ;
TRUSTED: TWX-TFAM-SPAN! ( n n n -- ) TFAM-SPAN! ;
TRUSTED: TWX-TFAM-SPAN@ ( n -- n n ) TFAM-SPAN@ ;
TRUSTED: TWX-TFAM-SUM? ( n -- bool ) TFAM-SUM? ;
TRUSTED: TWX-TFAM-TAGW! ( n n -- ) TFAM-TAGW! ;
TRUSTED: TWX-TFAM-TAGW@ ( n -- n ) TFAM-TAGW@ ;
TRUSTED: TWX-TFAM-VAR-RANGE! ( n n n -- ) TFAM-VAR-RANGE! ;
TRUSTED: TWX-TFAM-VIS@ ( n -- n ) TFAM-VIS@ ;
TRUSTED: TWX-TFL-CON-FAM? ( ptr u8 n -- n bool ) TFL-CON-FAM? ;
TRUSTED: TWX-TFL-CON? ( ptr u8 n ptr u8 n -- n n bool ) TFL-CON? ;
TRUSTED: TWX-TFL-CVAR? ( ptr u8 n n -- n n bool ) TFL-CVAR? ;
TRUSTED: TWX-TFL-MATCH-FAM? ( ptr u8 n -- n bool ) TFL-MATCH-FAM? ;
TRUSTED: TWX-TFL-VAR? ( ptr u8 n n -- n bool ) TFL-VAR? ;
TRUSTED: TWX-TFL-VPADS ( n n -- n ) TFL-VPADS ;
\ layout-cap slice 1: build resolved T-PARAM terms directly (bypassing the sig
\ parser, which rejects a layout arg in a cell param) to unit-test arg-aware width.
TRUSTED: TWX-T-WIDTH ( n -- n ) T-WIDTH ;
TRUSTED: TWX-MK-NULLARY ( n -- n ) {: fam:n :}       \ 0-arg term of family fam
   PARAM-SCR-N @ fam TFAM-NAME$ fam MK-PARAM ;
TRUSTED: TWX-MK-UNARY ( n n -- n ) {: arg:n fam:n :}  \ fam<arg> term
   PARAM-SCR-N @ {: base:n :}
   arg PARAM-SCR+
   base fam TFAM-NAME$ fam MK-PARAM ;


\ Explicit pre-checker layouts: offsets/accessors assert during prefix load;
\ this pins each private record's stride, alignment, and pointer-role metadata.
SCH-REC 4 cells T=
SCH-REC-ALIGN CELL T=
SCH-REC-PTR-MASK 0 T=
SCH-RBF-REC 2 cells T=
SCH-RBF-REC-ALIGN CELL T=
SCH-RBF-REC-PTR-MASK 0 T=
TF-REC 19 cells T=
TF-REC-ALIGN CELL T=
TF-REC-PTR-MASK 0 T=
SUMV-REC 10 cells T=
SUMV-REC-ALIGN CELL T=
SUMV-REC-PTR-MASK 0 T=
PF-REC 11 cells T=
PF-REC-ALIGN CELL T=
PF-REC-PTR-MASK 0 T=
PF-TX-REC 3 cells T=
PF-TX-REC-ALIGN CELL T=
PF-TX-REC-PTR-MASK 0 T=
LAY-REC 5 cells T=
LAY-REC-ALIGN CELL T=
LAY-REC-PTR-MASK 0 T=
TF-RBF-REC 6 cells T=
TF-RBF-REC-ALIGN CELL T=
TF-RBF-REC-PTR-MASK 0 T=


\ clean slate (nothing declares families during prefix load, but be explicit).
TWX-TFAM-RESET
TWX-SCHEMA-RESET

\ F4 (dot habu-tfam-nested-param-09fa2004): TWX-TFAM-RESET must de-register the
\ internal `field` family, else its reserved id (normally 15 — the 16th family)
\ dangles and a later family that lands on id 15 is misclassified as a record
\ field. After reset FIELD-FAM is -1; declaring 16 fresh families puts the 16th
\ on id 15, yet field stays de-registered, so no misclassification is possible.
FIELD-FAM @ -1 T=
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a0" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a1" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a2" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a3" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a4" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a5" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a6" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a7" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a8" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" a9" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" aa" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" ab" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" ac" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" ad" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" ae" 1 TK-CELL TWX-TFAM-DECL drop
s" pkgf4" CHECKER-PACKAGE-PUBLIC s" af" 1 TK-CELL TWX-TFAM-DECL VOK !
VOK @ 15 T=              \ the 16th fresh family occupies the field family's normal id
FIELD-FAM @ -1 T=        \ yet field stays de-registered — id 15 is not a field param
TWX-TFAM-RESET               \ restore the clean slate for the rest of the suite
TWX-SCHEMA-RESET

\ ---------------------------------------------------------------------------
\ 1. add / find / arity / kind / visibility / name / package readback.
\ ---------------------------------------------------------------------------
s" pkga" CHECKER-PACKAGE-PRIVATE s" opt"  1 TK-SUM     TWX-TFAM-DECL FID !
s" pkgb" CHECKER-PACKAGE-PUBLIC  s" res"  2 TK-SUM     TWX-TFAM-DECL PID !
s" pkga" CHECKER-PACKAGE-PRIVATE s" res"  0 TK-ENUM    TWX-TFAM-DECL AID !
s" pkgc" CHECKER-PACKAGE-PUBLIC  s" pt"   0 TK-PRODUCT TWX-TFAM-DECL PTID !
s" pkgc" CHECKER-PACKAGE-PUBLIC  s" cl"   0 TK-CELL    TWX-TFAM-DECL CLID !

FID @ TFAM-ARITY@ 1 T=
PID @ TFAM-ARITY@ 2 T=
AID @ TFAM-ARITY@ 0 T=
FID @ TFAM-KIND@ TK-SUM T=
AID @ TFAM-KIND@ TK-ENUM T=
PTID @ TFAM-KIND@ TK-PRODUCT T=
FID @ TWX-TFAM-VIS@ CHECKER-PACKAGE-PRIVATE T=
PID @ TWX-TFAM-VIS@ CHECKER-PACKAGE-PUBLIC T=
FID @ TFAM-NAME$ s" opt" T$=
FID @ TWX-TFAM-PKG$  s" pkga" T$=

\ kind predicates
FID @ TWX-TFAM-SUM? -1 T=      FID @ TWX-TFAM-ENUM? 0 T=      FID @ TWX-TFAM-LAYOUT? -1 T=
AID @ TWX-TFAM-ENUM? -1 T=     AID @ TWX-TFAM-SUM? 0 T=
PTID @ TWX-TFAM-PRODUCT? -1 T= PTID @ TWX-TFAM-LAYOUT? -1 T=
CLID @ TWX-TFAM-CELL? -1 T=    CLID @ TWX-TFAM-LAYOUT? 0 T=

\ defaults from TWX-TFAM-DECL
FID @ TWX-TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=
FID @ TWX-TFAM-TAGW@ TAGW-CELL T=
FID @ TWX-TFAM-SLOTS@ 0 T=

\ ---------------------------------------------------------------------------
\ 2. qualified (exact-package) vs unqualified (active-scope) lookup.
\ ---------------------------------------------------------------------------
s" pkga" s" opt"  TWX-TFAM-FIND-IN FOUNDF !  FID @ T=  FOUNDF @ -1 T=
s" pkga" s" nope" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ 0 T=
s" pkga" s" opt"  TWX-TFAM-RESOLVE FOUNDF !  FID @ T=  FOUNDF @ -1 T=
\ pkgc has no 'res' of its own, so resolve reaches pkgb's PUBLIC res (not pkga's
\ private res) — own-package-first + public-elsewhere.
s" pkgc" s" res"  TWX-TFAM-RESOLVE FOUNDF !  PID @ T=  FOUNDF @ -1 T=

\ ---------------------------------------------------------------------------
\ 3. public / private isolation.
\ ---------------------------------------------------------------------------
s" pkgb" s" opt" TWX-TFAM-RESOLVE FOUNDF ! drop  FOUNDF @ 0 T=
s" res" TWX-TFAM-FIND-PUBLIC FOUNDF !  PID @ T=  FOUNDF @ -1 T=
s" opt" TWX-TFAM-FIND-PUBLIC FOUNDF ! drop  FOUNDF @ 0 T=

\ ---------------------------------------------------------------------------
\ 4. same tail across different packages -> distinct ids, both findable.
\ ---------------------------------------------------------------------------
AID @ PID @ = 0 T=
s" pkga" s" res" TWX-TFAM-FIND-IN FOUNDF !  AID @ T=  FOUNDF @ -1 T=
s" pkgb" s" res" TWX-TFAM-FIND-IN FOUNDF !  PID @ T=  FOUNDF @ -1 T=
AID @ TFAM-ARITY@ 0 T=    PID @ TFAM-ARITY@ 2 T=

\ ---------------------------------------------------------------------------
\ 5. duplicate rejection within a package (throws E-TFAM-DUP).
\    stack before catch: pkg-a pkg-u vis name-a name-u arity kind  (7 cells)
\ ---------------------------------------------------------------------------
s" pkga" CHECKER-PACKAGE-PRIVATE s" opt" 1 TK-SUM ' TWX-TFAM-DECL catch
   TC ! 2drop 2drop 2drop drop  TC @ E-TFAM-DUP T=

\ ---------------------------------------------------------------------------
\ 6. uppercase / mixed-case rejection at the declaration boundary.
\ ---------------------------------------------------------------------------
s" result"  TWX-TF-CANON? -1 T=
s" opt-2"   TWX-TF-CANON? -1 T=
s" a-b-c"   TWX-TF-CANON? -1 T=       \ internal single hyphens are fine
s" Result"  TWX-TF-CANON? 0 T=
s" reSult"  TWX-TF-CANON? 0 T=
s" RESULT"  TWX-TF-CANON? 0 T=
s" 123"     TWX-TF-CANON? 0 T=
s" @x"      TWX-TF-CANON? 0 T=
\ internal-only single hyphens: leading / trailing / doubled '-' reject
\ (item 8's '-'->'--' constructor-package escaping depends on this canon).
s" -a"      TWX-TF-CANON? 0 T=
s" a-"      TWX-TF-CANON? 0 T=
s" a--b"    TWX-TF-CANON? 0 T=
s" -"       TWX-TF-CANON? 0 T=
s" pkga" CHECKER-PACKAGE-PRIVATE s" Result" 0 TK-SUM ' TWX-TFAM-DECL catch
   TC ! 2drop 2drop 2drop drop  TC @ E-TFAM-CASE T=
s" pkga" CHECKER-PACKAGE-PRIVATE s" MiXeD" 0 TK-SUM ' TWX-TFAM-DECL catch
   TC ! 2drop 2drop 2drop drop  TC @ E-TFAM-CASE T=

\ ---------------------------------------------------------------------------
\ 7. no hidden-field ('@name') lookup from public signatures.
\ ---------------------------------------------------------------------------
s" @opt.slot0" TWX-TF-HIDDEN? -1 T=
s" @res.tag"   TWX-TF-HIDDEN? -1 T=       \ item-7 tag row shape is hidden too
s" opt"        TWX-TF-HIDDEN? 0 T=
s" pkga" s" @opt.slot0" TWX-TFAM-RESOLVE FOUNDF ! drop  FOUNDF @ 0 T=
s" pkgb" s" @res.tag"   TWX-TFAM-RESOLVE FOUNDF ! drop  FOUNDF @ 0 T=

\ ---------------------------------------------------------------------------
\ 8. field setters / getters (record stores layout/slots/ranges/tagw/span/pk).
\ ---------------------------------------------------------------------------
FID @ TL-PACKED-TAG TWX-TFAM-LAYOUT!   FID @ TWX-TFAM-LAYOUT-POLICY@ TL-PACKED-TAG T=
FID @ 3 TWX-TFAM-SLOTS!                FID @ TWX-TFAM-SLOTS@ 3 T=
FID @ 16 TWX-TFAM-TAGW!                FID @ TWX-TFAM-TAGW@ 16 T=
FID @ 5 9 TWX-TFAM-VAR-RANGE!          FID @ TFAM-VAR-START@ 5 T=  FID @ TFAM-VAR-COUNT@ 9 T=
FID @ 2 4 TWX-TFAM-FLD-RANGE!          FID @ TWX-TFAM-FLD-START@ 2 T=  FID @ TWX-TFAM-FLD-COUNT@ 4 T=
FID @ 7 TWX-TFAM-SCHEMA-ROOT!          FID @ TWX-TFAM-SCHEMA-ROOT@ 7 T=
FID @ 40 6 TWX-TFAM-SPAN!              FID @ TWX-TFAM-SPAN@ 6 T= 40 T=
FID @ 0 TWX-TFAM-PK@ PK-CELL T=
FID @ 0 PK-TYPE TWX-TFAM-PK!           FID @ 0 TWX-TFAM-PK@ PK-TYPE T=
PID @ 0 TWX-TFAM-PK@ PK-CELL T=        PID @ 1 TWX-TFAM-PK@ PK-CELL T=

\ ---------------------------------------------------------------------------
\ 9. SCHEMA nodes: valid builders, malformed rejection, root pool + growth.
\    SCH nodes seed cap 4, roots seed cap 4 -> add >4 of each to force a grow.
\ ---------------------------------------------------------------------------
0 TWX-SCHEMA-PARAM NP !    NP @ TWX-SCHEMA-TAG@ SCH-PARAM T=   NP @ TWX-SCHEMA-A@ 0 T=
1 TWX-SCHEMA-CON   NC !    NC @ TWX-SCHEMA-TAG@ SCH-CON T=     NC @ TWX-SCHEMA-A@ 1 T=
FID @ 0 1 TWX-SCHEMA-APP NA !   NA @ TWX-SCHEMA-TAG@ SCH-APP T=   NA @ TWX-SCHEMA-C@ 1 T=
NP @ TWX-SCHEMA-PARAM? -1 T=    NC @ TWX-SCHEMA-CON? -1 T=       NA @ TWX-SCHEMA-APP? -1 T=
1 TWX-SCHEMA-PARAM drop   2 TWX-SCHEMA-CON drop   3 TWX-SCHEMA-PARAM drop   \ >4 nodes -> SCH grew
SCHEMA-N@ 7 T=                                          \ ids 1..6 created (nil is 0)
\ malformed tag rejected (tag a b c = 4 cells before catch)
999 0 0 0 ' TWX-SCHEMA-NEW catch   TC ! 2drop 2drop  TC @ E-SCHEMA-BAD T=
\ malformed paramref (negative index) rejected (1 cell before catch)
-1 ' TWX-SCHEMA-PARAM catch   TC ! drop  TC @ E-SCHEMA-BAD T=
\ root pool: 5 roots > seed cap 4 -> SCH-ROOT grew
NP @ TWX-SCHEMA-ROOT+ R1 !   R1 @ TWX-SCHEMA-ROOT@ NP @ T=
NC @ TWX-SCHEMA-ROOT+ drop   NA @ TWX-SCHEMA-ROOT+ drop
NP @ TWX-SCHEMA-ROOT+ drop   NC @ TWX-SCHEMA-ROOT+ drop
SCHEMA-ROOT-N@ 5 T=

\ SC-QUOT quotation payload node (dot habu-tfam-4-remainder): four row roots
\ (din,dout,rin,rout) + hasr flag, round-trip read-back, hasr normalization, and
\ malformed-row rejection (a child that is not a live schema node).
NP @ NC @ NA @ NP @ -1 TWX-SCHEMA-QUOT NQ !
NQ @ TWX-SCHEMA-TAG@ SCH-QUOT T=   NQ @ TWX-SCHEMA-QUOT? -1 T=
NQ @ TWX-SCHEMA-PARAM? 0 T=        NQ @ TWX-SCHEMA-C@ SCH-QUOT-ROWS T=
NQ @ TWX-SCHEMA-QUOT-HASR@ -1 T=
NQ @ TWX-SCHEMA-QUOT-DIN@  NP @ T=   NQ @ TWX-SCHEMA-QUOT-DOUT@ NC @ T=
NQ @ TWX-SCHEMA-QUOT-RIN@  NA @ T=   NQ @ TWX-SCHEMA-QUOT-ROUT@ NP @ T=
NC @ NC @ NC @ NC @ 0 TWX-SCHEMA-QUOT TWX-SCHEMA-QUOT-HASR@ 0 T=   \ hasr normalizes to 0
\ malformed row = nil node (0) rejected (5 args before catch: din dout rin rout hasr)
NP @ NC @ NA @ 0 -1 ' TWX-SCHEMA-QUOT catch   TC ! 2drop 2drop drop  TC @ E-SCHEMA-BAD T=
\ malformed row = out-of-range node rejected
NP @ NC @ NA @ 99999 -1 ' TWX-SCHEMA-QUOT catch   TC ! 2drop 2drop drop  TC @ E-SCHEMA-BAD T=

\ SC-PTR pointer payload node (PLAN item 6, docs §8 SC-PTR): child round-trip,
\ nesting, predicate discrimination, and malformed-child rejection.
NC @ TWX-SCHEMA-PTR NPTR !
NPTR @ TWX-SCHEMA-TAG@ SCH-PTR T=   NPTR @ TWX-SCHEMA-PTR? -1 T=
NPTR @ TWX-SCHEMA-CON? 0 T=         NPTR @ TWX-SCHEMA-A@ NC @ T=
NPTR @ TWX-SCHEMA-PTR TWX-SCHEMA-A@ NPTR @ T=       \ ptr ptr X nests
NC @ TWX-SCHEMA-PTR? 0 T=
\ malformed child = nil node (0) / out-of-range node rejected (1 cell before catch)
0 ' TWX-SCHEMA-PTR catch   TC ! drop  TC @ E-SCHEMA-BAD T=
99999 ' TWX-SCHEMA-PTR catch   TC ! drop  TC @ E-SCHEMA-BAD T=

\ ---------------------------------------------------------------------------
\ 10. SUMV variants: add, per-family key, dup rejection, cross-family reuse.
\    TWX-SUMV-ADD ( fam name-a name-u tag sch-start sch-count paycells -- id )
\ ---------------------------------------------------------------------------
FID @ s" ok"  0 0 0 0 TWX-SUMV-ADD VOK !    VOK @ TWX-SUMV-FAM@ FID @ T=   VOK @ TWX-SUMV-TAG@ 0 T=
FID @ s" err" 1 0 0 1 TWX-SUMV-ADD VERR !   VERR @ SUMV-NAME$ s" err" T$=   VERR @ TWX-SUMV-PAYCELLS@ 1 T=
PID @ s" ok"  0 0 0 0 TWX-SUMV-ADD drop     \ same 'ok' tail under a different family is fine
PID @ s" err" 1 0 0 0 TWX-SUMV-ADD drop
AID @ s" red"   0 0 0 0 TWX-SUMV-ADD drop
AID @ s" green" 1 0 0 0 TWX-SUMV-ADD drop   \ 6 variants > seed cap 4 -> SUMV grew
FID @ s" ok" TWX-SUMV-FIND FOUNDF !  VOK @ T=  FOUNDF @ -1 T=
PID @ s" ok" TWX-SUMV-FIND FOUNDF ! drop  FOUNDF @ -1 T=
FID @ s" none" TWX-SUMV-FIND FOUNDF ! drop  FOUNDF @ 0 T=
FID @ s" ok" 0 0 0 0 ' TWX-SUMV-ADD catch   TC ! 2drop 2drop 2drop drop  TC @ E-TFAM-DUP T=

\ ---------------------------------------------------------------------------
\ 11. shared fields: atomic tx add, committed reflection, dup rejection.
\ ---------------------------------------------------------------------------
variable PFTX   variable PFOUT   variable PFIN
variable PFBASE variable PFSTR   variable PFSCH
variable PFBAD  variable PFAPP   variable PFARG
PF-N@ FX !
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" x" 1 0 1 0 CELL CELL PF-FLAGS-NONE TWX-PF-ADD PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" y" 1 1 1 CELL CELL CELL PF-FLAGS-NONE TWX-PF-ADD PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" z" 1 2 1 2 cells CELL CELL PF-FLAGS-NONE TWX-PF-ADD PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" a" 1 3 1 3 cells CELL CELL PF-FLAGS-NONE TWX-PF-ADD PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" b" 1 4 1 4 cells CELL CELL PF-FLAGS-NONE TWX-PF-ADD PFTX !
PF-N@ FX @ T=                                  \ provisional ids are not reflected
PFTX @ TWX-PF-COMMIT
PF-N@ FX @ 5 + T=                              \ 5 fields > seed cap 4 -> PF grew
PTID @ PF-NO-VARIANT s" x" PF-FIND FOUNDF !  FX @ T=  FOUNDF @ -1 T=
PTID @ PF-NO-VARIANT s" q" PF-FIND FOUNDF ! drop  FOUNDF @ 0 T=
FX @ PF-FAM@ PTID @ T=   FX @ PF-VAR@ PF-NO-VARIANT T=
FX @ PF-SLOT@ 0 T=       FX @ PF-CELLS@ 1 T=
FX @ PF-BYTE-OFF@ 0 T=   FX @ PF-BYTES@ CELL T=
FX @ PF-ALIGN@ CELL T=    FX @ PF-FLAGS@ PF-FLAGS-NONE T=
FX @ PF-NAME$ s" x" T$=
PTID @ PF-NO-VARIANT 0 PF-EACH FOUNDF ! FX @ T= FOUNDF @ -1 T=

TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" x" 1 5 1 5 cells CELL CELL PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! 2drop 2drop 2drop 2drop 2drop 2drop
TC @ E-TFAM-DUP T=
PFTX @ TWX-PF-ROLLBACK

\ names are reserved independently of layout, and owner/variant membership is
\ validated before any row or interned string becomes visible.
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" make" 1 5 1 5 cells CELL CELL PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-NAME T=
PFTX @ TWX-PF-ROLLBACK

TWX-PF-BEGIN PFTX !
PFTX @ FID @ PF-NO-VARIANT s" absent" 1 0 1 0 4 4 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-OWNER T=
PFTX @ TWX-PF-ROLLBACK

TWX-PF-BEGIN PFTX !
PFTX @ PTID @ VOK @ s" wrong-owner" 1 5 1 40 CELL CELL PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-OWNER T=
PFTX @ TWX-PF-ROLLBACK

\ Optional variant ids are part of the key. Packed metadata is explicit and
\ need not use CELL byte size/alignment.
TWX-PF-BEGIN PFTX !
PFTX @ FID @ VOK @ s" value" 1 0 1 0 4 4 PF-FLAGS-NONE TWX-PF-ADD PFTX !
PFTX @ FID @ VERR @ s" value" 1 0 1 0 2 2 PF-FLAGS-NONE TWX-PF-ADD PFTX !
PFTX @ TWX-PF-COMMIT
FID @ VOK @ s" value" PF-FIND FOUNDF ! PFOUT !  FOUNDF @ -1 T=
FID @ VERR @ s" value" PF-FIND FOUNDF ! PFIN !   FOUNDF @ -1 T=
PFOUT @ PFIN @ = 0 T=
PFOUT @ PF-VAR@ VOK @ T=  PFOUT @ PF-BYTES@ 4 T=  PFOUT @ PF-ALIGN@ 4 T=
PFIN @ PF-VAR@ VERR @ T=  PFIN @ PF-BYTES@ 2 T=   PFIN @ PF-ALIGN@ 2 T=

\ Recursive schema validation: owner param bounds, concrete liveness, malformed
\ PTR/QUOT shapes, APP family/arity/root/kind/visibility, and a valid APP.
0 TWX-SCHEMA-PARAM TWX-SCHEMA-ROOT+ PFBAD !
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" bad-param" PFBAD @ 20 1 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TWX-PF-ROLLBACK

99999 TWX-SCHEMA-CON TWX-SCHEMA-ROOT+ PFBAD !
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" bad-con" PFBAD @ 20 1 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TWX-PF-ROLLBACK

SCH-PTR SCHEMA-N@ 0 0 TWX-SCHEMA-NEW TWX-SCHEMA-ROOT+ PFBAD !
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" bad-ptr" PFBAD @ 20 1 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TWX-PF-ROLLBACK

SCH-QUOT 2 0 SCH-QUOT-ROWS TWX-SCHEMA-NEW TWX-SCHEMA-ROOT+ PFBAD !
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" bad-quot" PFBAD @ 20 1 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TWX-PF-ROLLBACK

99999 0 0 TWX-SCHEMA-APP TWX-SCHEMA-ROOT+ PFBAD !
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" dead-app" PFBAD @ 20 1 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TWX-PF-ROLLBACK

PID @ 0 1 TWX-SCHEMA-APP TWX-SCHEMA-ROOT+ PFBAD !
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" arity-app" PFBAD @ 20 1 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TWX-PF-ROLLBACK

PID @ SCHEMA-ROOT-N@ 2 TWX-SCHEMA-APP TWX-SCHEMA-ROOT+ PFBAD !
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" range-app" PFBAD @ 20 1 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TWX-PF-ROLLBACK

FID @ 1 1 TWX-SCHEMA-APP TWX-SCHEMA-ROOT+ PFBAD !
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" private-app" PFBAD @ 20 1 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TWX-PF-ROLLBACK

CC-N TWX-SCHEMA-CON TWX-SCHEMA-ROOT+ PFARG !
CC-N TWX-SCHEMA-CON TWX-SCHEMA-ROOT+ drop
PID @ PFARG @ 2 TWX-SCHEMA-APP TWX-SCHEMA-ROOT+ PFAPP !
PID @ 0 PK-LAYOUT TWX-TFAM-PK!
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" kind-app" PFAPP @ 20 1 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TWX-PF-ROLLBACK
PID @ 0 PK-CELL TWX-TFAM-PK!
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" valid-app" PFAPP @ 20 1 100 3 1 PF-FLAGS-NONE
   TWX-PF-ADD PFTX !
PFTX @ TWX-PF-ROLLBACK

\ Common explicit-layout checks apply under every registered policy.
PTID @ TL-STACK-CELL-TAG TWX-TFAM-LAYOUT!
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" stack-bad" 1 20 0 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TWX-PF-ROLLBACK

PTID @ TL-PACKED-TAG TWX-TFAM-LAYOUT!
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" packed-bad" 1 20 1 102 3 3 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TWX-PF-ROLLBACK

PTID @ TL-NICHE TWX-TFAM-LAYOUT!
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" niche-bad" 1 20 1 101 3 4 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TWX-PF-ROLLBACK

PTID @ TL-BOXED TWX-TFAM-LAYOUT!
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" boxed-bad" 1 20 2 104 8 8 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TWX-PF-ROLLBACK

PTID @ TL-CUSTOM TWX-TFAM-LAYOUT!
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" custom-bad" 1 20 1 PF-MAX-N 2 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TWX-PF-ROLLBACK

TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" flag-bad" 1 20 1 101 3 1 1
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-FLAGS T=  PFTX @ TWX-PF-ROLLBACK

TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" overlap" 1 0 1 100 3 1 PF-FLAGS-NONE
   ' TWX-PF-ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TWX-PF-ROLLBACK

\ A valid custom-policy row proves byte metadata is independent of cell slots.
TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" custom-ok" 1 20 1 101 3 1 PF-FLAGS-NONE
   TWX-PF-ADD PFTX !
PFTX @ TWX-PF-ROLLBACK
PTID @ TL-STACK-CELL-TAG TWX-TFAM-LAYOUT!

\ Nested commit remains provisional. Outer rollback restores both high-waters;
\ the next outer commit reuses the retired id and string space.
PF-N@ PFBASE !  TF-STR-U @ PFSTR !
TWX-PF-BEGIN PFOUT !
PFOUT @ PTID @ PF-NO-VARIANT s" outer" 1 20 1 100 3 1 PF-FLAGS-NONE TWX-PF-ADD PFOUT !
PF-N@ PFBASE @ T=  TF-STR-U @ PFSTR @ > -1 T=
TWX-PF-BEGIN PFIN !
PFIN @ PTID @ PF-NO-VARIANT s" inner" 1 21 1 103 3 1 PF-FLAGS-NONE TWX-PF-ADD PFIN !
PFIN @ TWX-PF-COMMIT
PF-N@ PFBASE @ T=
PTID @ PF-NO-VARIANT s" inner" PF-FIND FOUNDF ! drop  FOUNDF @ 0 T=
PFOUT @ TWX-PF-ROLLBACK
PF-N@ PFBASE @ T=  TF-STR-U @ PFSTR @ T=
PTID @ PF-NO-VARIANT s" outer" PF-FIND FOUNDF ! drop  FOUNDF @ 0 T=
PTID @ PF-NO-VARIANT s" inner" PF-FIND FOUNDF ! drop  FOUNDF @ 0 T=

TWX-PF-BEGIN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" reuse" 1 20 1 100 3 1 PF-FLAGS-NONE TWX-PF-ADD PFTX !
PFTX @ TWX-PF-COMMIT
PTID @ PF-NO-VARIANT s" reuse" PF-FIND FOUNDF ! PFBASE @ T=  FOUNDF @ -1 T=

\ Strict LIFO tokens reject stale/non-top commit without corrupting either frame.
TWX-PF-BEGIN PFOUT !  TWX-PF-BEGIN PFIN !
PFOUT @ ' TWX-PF-COMMIT catch TC ! drop  TC @ E-PF-TX T=
PFIN @ TWX-PF-ROLLBACK  PFOUT @ TWX-PF-ROLLBACK

\ ---------------------------------------------------------------------------
\ 12. layout records: one per family, keyed by family; dup rejection.
\    TWX-LAY-ADD ( fam policy size align tagw -- id )
\ ---------------------------------------------------------------------------
FID  @ TL-STACK-CELL-TAG 16 8 8 TWX-LAY-ADD L0 !   L0 @ TWX-LAY-FAM@ FID @ T=   L0 @ TWX-LAY-SIZE@ 16 T=
PID  @ TL-PACKED-TAG     24 8 4 TWX-LAY-ADD drop
AID  @ TL-STACK-CELL-TAG  8 8 8 TWX-LAY-ADD drop
PTID @ TL-BOXED           8 8 8 TWX-LAY-ADD drop
CLID @ TL-CUSTOM          8 8 8 TWX-LAY-ADD drop    \ 5 layouts > seed cap 4 -> LAY grew
FID @ TWX-LAY-FIND FOUNDF !  L0 @ T=  FOUNDF @ -1 T=
CLID @ TWX-LAY-FIND FOUNDF !  TWX-LAY-POLICY@ TL-CUSTOM T=  FOUNDF @ -1 T=
FID @ TL-STACK-CELL-TAG 8 8 8 ' TWX-LAY-ADD catch   TC ! 2drop 2drop drop  TC @ E-TFAM-DUP T=

\ ---------------------------------------------------------------------------
\ 12b. constructor package-name derivation (PLAN Package Shape, docs §12; item 8).
\    TWX-TF-CTOR-PKG$ ( pkg-a pkg-u tail-a tail-u -- ctor-a ctor-u ): uppercase the
\    package segment and family tail, escape a literal '-' inside the segment as
\    '--', join package-then-tail with a single '-'; when the escaped spelling
\    exceeds the 32-byte readability cap (TF-CTOR-NAME-LIMIT; raised from 16 by
\    dot habu-raise-or-alias-5d2a6b70), the name is `T` + the first 16 lowercase
\    hex digits of SHA-256 over the length-prefixed segment list + `-` + the
\    uppercase tail. Pure, injective, stable (no alloc-order id).
\ ---------------------------------------------------------------------------
variable CPA   variable CPU   variable CQA   variable CQU
\ top level: bare uppercased tail, no separator.
s" " s" result" TWX-TF-CTOR-PKG$ s" RESULT" T$=
\ in-package: PKG-TAIL.
s" pkg" s" result" TWX-TF-CTOR-PKG$ s" PKG-RESULT" T$=
s" opt" s" some"   TWX-TF-CTOR-PKG$ s" OPT-SOME" T$=
\ digits pass through unchanged.
s" v2" s" ok"      TWX-TF-CTOR-PKG$ s" V2-OK" T$=
\ injectivity across the hyphen boundary: every joined segment (package AND
\ tail) escapes '-' as '--', so all three hyphen splits stay distinct:
\   a-b + c  ->  A--B-C      a + b-c  ->  A-B--C      "" + a-b-c -> A--B--C
s" a-b" s" c"      TWX-TF-CTOR-PKG$ s" A--B-C" T$=
s" a"   s" b-c"    TWX-TF-CTOR-PKG$ s" A-B--C" T$=
s" "    s" a-b-c"  TWX-TF-CTOR-PKG$ s" A--B--C" T$=
\ determinism: identical inputs -> byte-identical output.
s" pkg" s" result" TWX-TF-CTOR-PKG$ s" PKG-RESULT" T$=

\ Readable band 16 < len <= 32 (raised from 16 by dot
\ habu-raise-or-alias-5d2a6b70): the escaped form is injective at every length
\ and the runtime/AOT dictionary stores long names (DNAME-EXT), so an escaped
\ spelling up to 32 bytes keeps its READABLE form -- the real EVID/POLICY
\ presence-slot ctor packages (EVID-CERTIFY--SLOT=18, POLICY-PROMOTE--POLICY=22)
\ are now constructable by name. These three folded to opaque SHA before the raise:
s" verylongpackagename" s" result" TWX-TF-CTOR-PKG$ s" VERYLONGPACKAGENAME-RESULT" T$=   \ 26
s" " s" verylongfamilyname" TWX-TF-CTOR-PKG$ s" VERYLONGFAMILYNAME" T$=                    \ 18
\ exactly 32 bytes stays readable (the boundary is len <= 32):
s" abcdefghijklmno" s" pqrstuvwxyzabcde" TWX-TF-CTOR-PKG$ s" ABCDEFGHIJKLMNO-PQRSTUVWXYZABCDE" T$=  \ 15+1+16

\ SHA-256 fallback fires only PAST 32 bytes now. escaped
\ `VERYLONGPACKAGENAME-RESULTRESULTR` is 33 bytes > 32, so the derived name is
\ `T` + 16 hex + `-RESULTRESULTR` = 31 bytes (the hash covers only the package
\ segment list; the tail is appended raw). Structure asserted here; the exact
\ hash goldens (determinism + injectivity + algorithm pin) follow.
s" verylongpackagename" s" resultresultr" TWX-TF-CTOR-PKG$ CPU ! CPA !
CPU @ 31 T=
CPA @ 1 s" T" T$=                           \ prefix marker
CPA @ 17 + 1 s" -" T$=                      \ separator after the 16-hex hash
CPA @ 18 + 13 s" RESULTRESULTR" T$=         \ uppercase family tail suffix (appended raw)
\ every hash byte is a lowercase hex digit (0-9 a-f).
: HEXLC? ( n -- bool ) {: c:n :}
   c 48 >= c 57 <= and   c 97 >= c 102 <= and   or ;
: HEX16? ( ptr u8 -- bool ) {: p:ptr :}
   0 begin dup 16 < while
      dup p + c@ HEXLC? 0= if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;
CPA @ 1 + HEX16? -1 T=
\ TWX-TF-CTOR-PKG$ returns a pointer into the shared derivation buffer, so intern a
\ stable copy of the first result before deriving again.
variable CPOFF
CPA @ CPU @ TWX-TF-INTERN CPOFF !
\ determinism: the same long input reproduces the same derived name.
s" verylongpackagename" s" resultresultr" TWX-TF-CTOR-PKG$ CQU ! CQA !
CQA @ CQU @  CPOFF @ CPU @ TWX-TF-OFF$  T$=
\ injectivity: a different long package hashes to a different name (the hash
\ region separates inputs that share length and tail).
s" verylongpackagenamx" s" resultresultr" TWX-TF-CTOR-PKG$ CQU ! CQA !
CQA @ CQU @  CPOFF @ CPU @ TWX-TF-OFF$  TSNE   \ NOT equal to the first long name
\ exact golden pins the pinned algorithm byte-for-byte (hash covers the package
\ segment list only, so the longer tail keeps the verylongpackagename golden):
\ SHA-256(0x13 "verylongpackagename") = 92a8624462e75ea4... (independent impl).
s" verylongpackagename" s" resultresultr" TWX-TF-CTOR-PKG$ s" T92a8624462e75ea4-RESULTRESULTR" T$=
\ a long family tail with an empty package: fallback hashes the empty segment
\ list, tail still appended (33-byte top-level tail > 32).
s" " s" abcdefghijklmnopqrstuvwxyzabcdefg" TWX-TF-CTOR-PKG$ CQU ! CQA !
CQU @ 51 T=                                 \ T(1)+16 hex+ -(1)+33-byte tail
CQA @ 1 s" T" T$=
CQA @ 1 + HEX16? -1 T=
CQA @ 18 + 33 s" ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFG" T$=
\ empty segment list golden: SHA-256("") = e3b0c44298fc1c14... (FIPS-180 constant).
s" " s" abcdefghijklmnopqrstuvwxyzabcdefg" TWX-TF-CTOR-PKG$ s" Te3b0c44298fc1c14-ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFG" T$=

\ SV.CTOR-PKG metadata slot: friend writer/reader round-trip through the pool.
\ VOK is a live variant id from section 10; storing its constructor package name
\ leaves the other variant fields untouched.
variable RPK
s" RESULT" TWX-TF-INTERN RPK !
VOK @ SUMV-CTOR-PKG$ nip 0 T=               \ unset variants report an empty name
VOK @ RPK @ 6 TWX-SUMV-CTOR-PKG!
VOK @ SUMV-CTOR-PKG$ s" RESULT" T$=
VOK @ TWX-SUMV-TAG@ 0 T=                        \ tag field intact after the CTOR write

\ ---------------------------------------------------------------------------
\ 13. grow across the TFAM record / string / param-kind seed caps, then prove
\    family id 0 survives every relocation.
\ ---------------------------------------------------------------------------
s" pkgd" CHECKER-PACKAGE-PUBLIC s" tree"  1 TK-SUM     TWX-TFAM-DECL drop
s" pkgd" CHECKER-PACKAGE-PUBLIC s" list"  1 TK-SUM     TWX-TFAM-DECL drop
s" pkgd" CHECKER-PACKAGE-PUBLIC s" maybe" 1 TK-SUM     TWX-TFAM-DECL drop
s" pkge" CHECKER-PACKAGE-PUBLIC s" pair"  2 TK-PRODUCT TWX-TFAM-DECL drop
FID @ TFAM-NAME$ s" opt" T$=
FID @ TWX-TFAM-PKG$  s" pkga" T$=
FID @ TFAM-ARITY@ 1 T=
FID @ TFAM-KIND@ TK-SUM T=
FID @ 0 TWX-TFAM-PK@ PK-TYPE T=
s" pkgd" s" tree" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=
TFAM-N@ 9 T=

\ ---------------------------------------------------------------------------
\ 14. snapshot persist/restore: run the exact words TWX-CHECKER-SNAPSHOT-PREPARE
\    invokes and prove every store reads back identically after the bake.
\ ---------------------------------------------------------------------------
TWX-TFAM-SNAPSHOT-PERSIST
TWX-SCHEMA-SNAPSHOT-PERSIST
FID @ TFAM-NAME$ s" opt" T$=
FID @ TFAM-ARITY@ 1 T=
FID @ TFAM-KIND@ TK-SUM T=
FID @ 0 TWX-TFAM-PK@ PK-TYPE T=
FID @ TWX-TFAM-SLOTS@ 3 T=
s" pkgb" s" res" TWX-TFAM-FIND-IN FOUNDF ! PID @ T= FOUNDF @ -1 T=
FID @ s" ok" TWX-SUMV-FIND FOUNDF ! VOK @ T= FOUNDF @ -1 T=
PTID @ PF-NO-VARIANT s" x" PF-FIND FOUNDF ! FX @ T= FOUNDF @ -1 T=
FID @ TWX-LAY-FIND FOUNDF ! TWX-LAY-SIZE@ 16 T= FOUNDF @ -1 T=
R1 @ TWX-SCHEMA-ROOT@ TWX-SCHEMA-TAG@ SCH-PARAM T=
NA @ TWX-SCHEMA-TAG@ SCH-APP T=
\ SC-QUOT node (NQ, built in section 9: din=NP dout=NC rin=NA rout=NP hasr=-1)
\ survives the bake: tag, row roots, and hasr read back from the persisted node
\ arena + root pool (destruction review finding 3).
NQ @ TWX-SCHEMA-TAG@ SCH-QUOT T=
NQ @ TWX-SCHEMA-QUOT-DIN@  NP @ T=
NQ @ TWX-SCHEMA-QUOT-ROUT@ NP @ T=
NQ @ TWX-SCHEMA-QUOT-HASR@ -1 T=

\ ---------------------------------------------------------------------------
\ 15. ambiguous unqualified public resolution: two OTHER-package publics sharing
\    a tail throw E-TFAM-AMBIG; an own-package match still wins without ambiguity;
\    qualified (exact-package) access resolves both distinctly. (dot 2a)
\ ---------------------------------------------------------------------------
variable AX  variable AY
s" pkgx" CHECKER-PACKAGE-PUBLIC s" amb" 1 TK-SUM TWX-TFAM-DECL AX !
s" pkgy" CHECKER-PACKAGE-PUBLIC s" amb" 1 TK-SUM TWX-TFAM-DECL AY !
\ unqualified resolve from a third package: two publics tie -> throw
s" pkgz" s" amb" ' TWX-TFAM-RESOLVE catch  TC ! 2drop 2drop  TC @ E-TFAM-AMBIG T=
\ bare cross-package public lookup throws on the same tie
s" amb" ' TWX-TFAM-FIND-PUBLIC catch  TC ! 2drop  TC @ E-TFAM-AMBIG T=
\ own-package family wins without ambiguity (each resolves to its own amb)
s" pkgx" s" amb" TWX-TFAM-RESOLVE FOUNDF !  AX @ T=  FOUNDF @ -1 T=
s" pkgy" s" amb" TWX-TFAM-RESOLVE FOUNDF !  AY @ T=  FOUNDF @ -1 T=
\ qualified (exact-package) access still resolves both distinctly, no throw
s" pkgx" s" amb" TWX-TFAM-FIND-IN FOUNDF !  AX @ T=  FOUNDF @ -1 T=
s" pkgy" s" amb" TWX-TFAM-FIND-IN FOUNDF !  AY @ T=  FOUNDF @ -1 T=
\ a single public tail (no tie) still resolves cleanly through FIND-PUBLIC
s" pkgx" CHECKER-PACKAGE-PUBLIC s" solo" 0 TK-ENUM TWX-TFAM-DECL drop
s" solo" TWX-TFAM-FIND-PUBLIC FOUNDF ! drop  FOUNDF @ -1 T=

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
s" " s" lres" TWX-TFAM-FIND-IN FOUNDF !  LID !  FOUNDF @ -1 T=
\ construct one-shot -> ( tag pads ok ); pads = M-p with M = 2 (widest payload)
s" lres" s" lok"  TWX-TFL-CON? FOUNDF !  1 T=  0 T=  FOUNDF @ -1 T=
s" lres" s" lerr" TWX-TFL-CON? FOUNDF !  0 T=  1 T=  FOUNDF @ -1 T=
s" lres" s" lnil" TWX-TFL-CON? FOUNDF !  2 T=  2 T=  FOUNDF @ -1 T=
\ raw engine tokens fold: uppercase spellings agree with the declaration
s" LRES" s" LOK" TWX-TFL-CON? FOUNDF !  1 T=  0 T=  FOUNDF @ -1 T=
\ misses fail pure (no throw, no diagnostic): unknown family/variant, cell kind
s" nosuch" s" lok" TWX-TFL-CON? FOUNDF !  0 T=  0 T=  FOUNDF @ 0 T=
s" lres" s" nope"  TWX-TFL-CON? FOUNDF !  0 T=  0 T=  FOUNDF @ 0 T=
s" span" s" lok"   TWX-TFL-CON? FOUNDF !  0 T=  0 T=  FOUNDF @ 0 T=
\ owner-only construct scope: pkgx's public solo does NOT construct from here
s" solo" TWX-TFL-CON-FAM? FOUNDF ! drop  FOUNDF @ 0 T=
\ match resolution is signature scope: own ("" top level), unique public,
\ qualified; ambiguous publics and non-sum kinds fail pure
s" lres" TWX-TFL-MATCH-FAM? FOUNDF !  LID @ T=  FOUNDF @ -1 T=
s" solo" TWX-TFL-MATCH-FAM? FOUNDF ! drop  FOUNDF @ -1 T=
s" pkgx:amb" TWX-TFL-MATCH-FAM? FOUNDF !  AX @ T=  FOUNDF @ -1 T=
s" amb"  TWX-TFL-MATCH-FAM? FOUNDF ! drop  FOUNDF @ 0 T=
s" span" TWX-TFL-MATCH-FAM? FOUNDF ! drop  FOUNDF @ 0 T=
\ variant resolve + per-variant metadata (folded)
s" LERR" LID @ TWX-TFL-VAR? FOUNDF !  LVID !  FOUNDF @ -1 T=
LVID @ TWX-SUMV-TAG@ 1 T=
LID @ LVID @ TWX-TFL-VPADS 0 T=
s" zzz" LID @ TWX-TFL-VAR? FOUNDF ! drop  FOUNDF @ 0 T=
\ variant one-shot for a resolved fam (the engine's state-2 bridge call)
s" lnil" LID @ TWX-TFL-CVAR? FOUNDF !  2 T=  2 T=  FOUNDF @ -1 T=
s" nope" LID @ TWX-TFL-CVAR? FOUNDF !  0 T=  0 T=  FOUNDF @ 0 T=
s" TFL-SURFACE" type cr

\ ---------------------------------------------------------------------------
\ packed ABI descriptor (docs §22.2, policy TL-PACKED-TAG). TWX-PACKED-NARROW picks
\ the smallest byte tag width holding a K-variant tag; TWX-PACKED-DESC composes
\ ( size align tagw ) with cell payloads (align CELL) and the narrowed tag placed
\ last, SIZE the aligned record stride. Computed for ANY family regardless of its
\ declared policy (the accept-flip that populates LAY on POLICY packed-tag is a
\ later sub-slice); private families (package pkpk) keep the protected-WID seal
\ cap untouched (dot habu-seal-protwid-cap-6f1c9d2b).
\ ---------------------------------------------------------------------------
0 TWX-PACKED-NARROW 0 T=
1 TWX-PACKED-NARROW 1 T=
256 TWX-PACKED-NARROW 1 T=
257 TWX-PACKED-NARROW 2 T=
65536 TWX-PACKED-NARROW 2 T=
65537 TWX-PACKED-NARROW 4 T=
1 32 lshift TWX-PACKED-NARROW 4 T=
1 32 lshift 1 + TWX-PACKED-NARROW 8 T=
variable PSZ  variable PAL  variable PTW  variable PKI
package pkpk
ENUM pkpke red green blue ;ENUM
SUMTYPE pkpks 1 VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE
PRODUCT pkpkp 0 FIELD x n FIELD y n ;PRODUCT
;package
\ enum (3 variants, no payload): tag-only u8 -> size 1 align 1 tagw 1
s" pkpk" s" pkpke" TWX-TFAM-FIND-IN drop PKI !
PKI @ TWX-PACKED-DESC PTW ! PAL ! PSZ !
PSZ @ 1 T=   PAL @ 1 T=   PTW @ 1 T=
\ sum (2 variants, M=1 cell): tag u8 after one cell -> align_up(8+1,8)=16, align 8, tagw 1
s" pkpk" s" pkpks" TWX-TFAM-FIND-IN drop PKI !
PKI @ TWX-PACKED-DESC PTW ! PAL ! PSZ !
PSZ @ 16 T=  PAL @ 8 T=   PTW @ 1 T=
\ product (2 cell fields, no tag): align_up(16,8)=16, align 8, tagw 0
s" pkpk" s" pkpkp" TWX-TFAM-FIND-IN drop PKI !
PKI @ TWX-PACKED-DESC PTW ! PAL ! PSZ !
PSZ @ 16 T=  PAL @ 8 T=   PTW @ 0 T=

\ ---------------------------------------------------------------------------
\ packed-tag ACCEPT (item 16 sub-slice 2, docs §22.0/§22.2). `POLICY packed-tag`
\ declares: the family row carries TL-PACKED-TAG and the close bakes the
\ TWX-PACKED-DESC memory descriptor into the LAY registry. packed is a MEMORY-ABI
\ descriptor ONLY: the stack representation is IDENTICAL to stack-cell-tag, so a
\ packed family and its stack-cell-tag twin construct, MATCH, and transport
\ (dup/drop via nip) exactly alike — pinned differentially below. Private
\ families (package pkac) keep the protected-WID seal cap untouched.
\ ---------------------------------------------------------------------------
variable PQA  variable PQB  variable PQL  variable PQF
package pkac
SUMTYPE pkacp 0 POLICY packed-tag
  VARIANT lo n ;VARIANT
  VARIANT hi n ;VARIANT
;SUMTYPE
SUMTYPE pkacs 0 POLICY stack-cell-tag
  VARIANT lo n ;VARIANT
  VARIANT hi n ;VARIANT
;SUMTYPE
ENUM pkace POLICY packed-tag red green blue ;ENUM
PRODUCT pkacr 0 POLICY packed-tag FIELD x n FIELD y n ;PRODUCT

\ policy readback + stack-width identity with the stack-cell-tag twin
s" pkac" s" pkacp" TWX-TFAM-FIND-IN drop PQA !
s" pkac" s" pkacs" TWX-TFAM-FIND-IN drop PQB !
PQA @ TWX-TFAM-LAYOUT-POLICY@ TL-PACKED-TAG T=
PQB @ TWX-TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=
PQA @ TFAM-WIDTH@ PQB @ TFAM-WIDTH@ T=
\ the close baked one LAY row for the packed family; values == TWX-PACKED-DESC
PQA @ TWX-LAY-FIND PQF ! PQL !
PQF @ -1 T=
PQL @ TWX-LAY-POLICY@ TL-PACKED-TAG T=
PQA @ TWX-PACKED-DESC PTW ! PAL ! PSZ !
PQL @ TWX-LAY-SIZE@ PSZ @ T=   PQL @ TWX-LAY-ALIGN@ PAL @ T=   PQL @ TWX-LAY-TAGW@ PTW @ T=
\ the stack-cell-tag twin bakes NO row
PQB @ TWX-LAY-FIND PQF ! drop
PQF @ 0 T=
\ packed enum + product descriptors baked at close
s" pkac" s" pkace" TWX-TFAM-FIND-IN drop PQA !
PQA @ TWX-TFAM-LAYOUT-POLICY@ TL-PACKED-TAG T=
PQA @ TWX-LAY-FIND PQF ! PQL !
PQF @ -1 T=
PQL @ TWX-LAY-SIZE@ 1 T=   PQL @ TWX-LAY-ALIGN@ 1 T=   PQL @ TWX-LAY-TAGW@ 1 T=
s" pkac" s" pkacr" TWX-TFAM-FIND-IN drop PQA !
PQA @ TWX-LAY-FIND PQF ! PQL !
PQF @ -1 T=
PQL @ TWX-LAY-SIZE@ 16 T=  PQL @ TWX-LAY-ALIGN@ 8 T=   PQL @ TWX-LAY-TAGW@ 0 T=

\ differential stack-shape identity: the same construct -> dup -> MATCH-the-copy
\ -> nip-the-original round trip on the packed family and on its twin.
: PKAC-P-RT ( n -- n )
   construct pkacp lo dup
   MATCH pkacp lo OF ENDOF hi OF ENDOF ;MATCH
   nip ;
: PKAC-S-RT ( n -- n )
   construct pkacs lo dup
   MATCH pkacs lo OF ENDOF hi OF ENDOF ;MATCH
   nip ;
41 PKAC-P-RT 41 T=
41 PKAC-S-RT 41 T=
7 PKAC-P-RT 7 PKAC-S-RT T=
;package

\ ---------------------------------------------------------------------------
\ boxed / niche-null stack width = 1 (docs §18 WIDTH(boxed)=1, §22.3 niche one
\ cell). No declaration accepts boxed/niche yet (both reject at the POLICY
\ clause), so this shared W=1 metadata is exercised through the direct
\ TWX-TFAM-LAYOUT! mutator, exactly like the packed descriptor (LAY) unit tests.
\ A multi-slot SUM (default width slots+1) collapses to 1 under boxed and under
\ niche, but keeps its cell width under stack-cell-tag AND packed (packed is a
\ MEMORY-ABI descriptor only, §22.2) — the regression guard that the branch
\ fires for boxed/niche alone.
\ ---------------------------------------------------------------------------
s" pkgw" CHECKER-PACKAGE-PRIVATE s" wbx" 1 TK-SUM TWX-TFAM-DECL WBX !
WBX @ 2 TWX-TFAM-SLOTS!                                       \ 2 payload slots
WBX @ TWX-TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=            \ default policy
WBX @ TFAM-WIDTH@ 3 T=                                        \ slots + tag
WBX @ TL-BOXED       TWX-TFAM-LAYOUT!   WBX @ TFAM-WIDTH@ 1 T=
WBX @ TL-NICHE       TWX-TFAM-LAYOUT!   WBX @ TFAM-WIDTH@ 1 T=
WBX @ TL-PACKED-TAG  TWX-TFAM-LAYOUT!   WBX @ TFAM-WIDTH@ 3 T=  \ packed keeps cell width
WBX @ TL-STACK-CELL-TAG TWX-TFAM-LAYOUT!   WBX @ TFAM-WIDTH@ 3 T=  \ restored

\ ---------------------------------------------------------------------------
\ arg-aware INSTANTIATED width (layout-cap slice 1, docs §18). T-WIDTH walks a
\ resolved layout term's variant/product schemas, substituting each param slot by
\ its arg's width, instead of the declared "every param is one cell" TFAM-WIDTH@.
\ A width-1 arg reproduces the declared width (behaviour-preserving groundwork,
\ what every cell-kinded corpus shape uses); a wide layout arg widens the sum
\ payload — the degenerate case the declared family-only width gets wrong. The
\ probe shapes stay rejected at the sig layer (slice 1 adds NO new accepts); these
\ terms are built directly through MK-PARAM to exercise the width function alone.
\ ---------------------------------------------------------------------------
variable IWP1  variable IWP3  variable IWOPT  variable IWT
package pkiw
ENUM pkiw1 red green ;ENUM                                         \ layout, width 1 (tag only)
PRODUCT pkiw3 0 FIELD a n FIELD b n FIELD c n ;PRODUCT             \ layout, width 3 (three cells)
SUMTYPE pkiwo 1 VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE  \ option-like, arity 1
;package
s" pkiw" s" pkiw1" TWX-TFAM-FIND-IN drop IWP1 !
s" pkiw" s" pkiw3" TWX-TFAM-FIND-IN drop IWP3 !
s" pkiw" s" pkiwo" TWX-TFAM-FIND-IN drop IWOPT !
\ declared (params-as-cells) widths — the family-only baseline
IWP1 @ TFAM-WIDTH@ 1 T=                          \ enum: tag only
IWP3 @ TFAM-WIDTH@ 3 T=                           \ product: three cells
IWOPT @ TFAM-WIDTH@ 2 T=                          \ sum: max(none=0, some=1-as-cell) + tag
\ arg-aware width == T-WIDTH of the built terms
IWP1 @ TWX-MK-NULLARY TWX-T-WIDTH 1 T=            \ enum term self-check
IWP3 @ TWX-MK-NULLARY TWX-T-WIDTH 3 T=            \ product term self-check
\ behaviour-preserving: a width-1 layout arg reproduces the declared sum width
IWP1 @ TWX-MK-NULLARY IWOPT @ TWX-MK-UNARY TWX-T-WIDTH 2 T=       \ opt<enum1> == declared 2
\ the groundwork proof: a width-3 layout arg widens the sum payload (declared width 2 was degenerate)
IWP3 @ TWX-MK-NULLARY IWOPT @ TWX-MK-UNARY dup IWT !
IWT @ TWX-T-WIDTH 4 T=                            \ opt<pt3>: max(0, width(pt3)=3) + tag = 4
IWOPT @ TFAM-WIDTH@ 2 T=                          \ family-only width unchanged (still 2) — arg-aware differs

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-family-suite: failures" 1 die ;
REPORT
