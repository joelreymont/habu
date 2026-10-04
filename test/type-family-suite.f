\ type-family-suite.f — behavior suite for the package-scoped TFAM/SUMV/product/
\ layout/SCHEMA registries (src/core/type-family.f, src/core/type-schema.f). A
\ WHITEBOX-SUITE row: the registry words are checker internals, which the
\ unsealed engine binds by their recorded rows at top level and in a checked ':'
\ body alike. The gate runs it on that engine, which test/whitebox-engine.f
\ builds:
\     <unsealed engine> --load test/type-family-suite.f
\ The harness words below are ordinary checked definitions (public words only);
\ every registry op is a top-level interpret line, naming the internal word or a
\ checked probe of it. A failure prints F<index> + detail; REPORT exits 1 on any
\ fail.

require lib/fmt.f                        \ FMT:.INT - one-line number text

using SCHEMA-REG
using TFAM

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want FMT:.INT s"  got " type got FMT:.INT cr
   then ;
: T-TRUE ( bool -- )
   #CASE @ 1 + #CASE !
   0= if T-FAIL s" assert: expected true" type cr then ;
: T$= ( ptr u8 n ptr u8 n -- ) {: ga:ptr gu:n wa:ptr wu:n :}
   #CASE @ 1 + #CASE !
   gu wu <> if
      T-FAIL s" assert string len: expected " type wu FMT:.INT s"  got " type gu FMT:.INT cr exit
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
TYPED-VARIABLE TF-GROW-BASE ptr n
create TF-GROW-NAME 2 allot
variable FID    variable PID    variable AID    variable PTID   variable CLID
variable VOK    variable VERR   variable FX     variable NP     variable NC
variable NA     variable R1     variable L0     variable NQ
variable NPTR   variable WBX
variable NQDIN   variable NQDOUT  variable NQRIN   variable NQROUT
variable NQEMP   variable NQMUL variable NQST
\ Checker-internal words are probed at top level, by name or through a probe
\ below. The probes are checked: on the whitebox engine a body naming an
\ internal word binds that word's recorded row.
: TWX-CHECKER-CAPTURE-PREPARE ( -- ) CHECKER-CAPTURE-PREPARE ;

package TF-FIELD
public
: OPEN ( -- n ) TYPE-FIELD-OWNER:OPEN ;
: ADD ( n n n ptr u8 n n n n n n n n -- n ) TYPE-FIELD-OWNER:ADD ;
: CLOSE ( n -- ) dup TYPE-FIELD-OWNER:PREPARE drop
   dup TYPE-FIELD-OWNER:COMMIT TYPE-FIELD-OWNER:FINALIZE ;
: ROLLBACK ( n -- ) TYPE-FIELD-OWNER:ROLLBACK ;
;package

: TWX-SUMV-PAY-N ( n -- n ) SUMV-PAY-N ;
: TWX-SUMV-PAYCELLS@ ( n -- n ) SUMV-PAYCELLS@ ;
: TWX-SUMV-TAG@ ( n -- n ) SUMV-TAG@ ;
: TDP-INDEX>CHAR ( n n -- ) {: index:n char:n :}
   index TFAM-DECL-PARAM>CHAR T-TRUE char T= ;
: TDP-CHAR>INDEX ( n n -- ) {: index:n char:n :}
   char TFAM-DECL-CHAR>PARAM T-TRUE index T= ;
: TDP-PAIR ( n n -- ) {: index:n char:n :}
   index char TDP-INDEX>CHAR
   index char TDP-CHAR>INDEX ;
: TF-GROW-FAMILY ( n -- ) {: idx:n :}
   idx 26 / [char] a + TF-GROW-NAME c!
   idx 26 mod [char] a + TF-GROW-NAME 1+ c!
   s" pkgrowth" CHECKER-PACKAGE-PUBLIC TF-GROW-NAME 2 0 TK-CELL TFAM-DECL drop ;
: TF-GROW-THROUGH-CAP ( -- )
   0 begin
      dup 676 < FID @ TF-REC@ TF-GROW-BASE @ = and
   while
      dup TF-GROW-FAMILY 1+
   repeat drop ;
: TWX-TFAM-SLOTS@ ( n -- n ) TFAM-SLOTS@ ;
: TWX-TFL-CON-FAM? ( ptr u8 n -- n bool ) TFL-CON-FAM? ;
: TWX-TFL-MATCH-FAM? ( ptr u8 n -- n bool ) TFL-MATCH-FAM? ;
: TWX-TFL-VAR? ( ptr u8 n n -- n bool ) TFL-VAR? ;
: TWX-TFL-VPADS ( n n -- n ) TFL-VPADS ;
\ layout-cap slice 1: build resolved T-PARAM terms directly (bypassing the sig
\ parser, which rejects a layout arg in a cell param) to unit-test arg-aware width.
: TWX-MK-NULLARY ( n -- n ) {: fam:n :}       \ 0-arg term of family fam
   PARAM-SCR-N @ fam TFAM-NAME$ fam MK-PARAM ;
: TWX-MK-UNARY ( n n -- n ) {: arg:n fam:n :}  \ fam<arg> term
   PARAM-SCR-N @ {: base:n :}
   arg PARAM-SCR+
   base fam TFAM-NAME$ fam MK-PARAM ;
: TWX-FAMILY-WIDTH ( n -- n ) TWX-MK-NULLARY T-WIDTH ;


\ Declaration parameters use one reserved-safe positional alphabet.  These
\ direct whitebox checks pin both directions and every ordered character;
\ f/n/r are concrete scalar tokens and therefore never map to a parameter.
TFAM-DECL-PARAM-COUNT 23 T=
0  $61 TDP-PAIR
1  $62 TDP-PAIR
2  $63 TDP-PAIR
3  $64 TDP-PAIR
4  $65 TDP-PAIR
5  $67 TDP-PAIR
6  $68 TDP-PAIR
7  $69 TDP-PAIR
8  $6A TDP-PAIR
9  $6B TDP-PAIR
10 $6C TDP-PAIR
11 $6D TDP-PAIR
12 $6F TDP-PAIR
13 $70 TDP-PAIR
14 $71 TDP-PAIR
15 $73 TDP-PAIR
16 $74 TDP-PAIR
17 $75 TDP-PAIR
18 $76 TDP-PAIR
19 $77 TDP-PAIR
20 $78 TDP-PAIR
21 $79 TDP-PAIR
22 $7A TDP-PAIR
0 TFAM-DECL-PARAM>CHAR nip -1 T=
22 TFAM-DECL-PARAM>CHAR nip -1 T=
-1 TFAM-DECL-PARAM>CHAR nip 0 T=
23 TFAM-DECL-PARAM>CHAR nip 0 T=
$66 TFAM-DECL-CHAR>PARAM nip 0 T=   \ f is bool
$6E TFAM-DECL-CHAR>PARAM nip 0 T=   \ n is int
$72 TFAM-DECL-CHAR>PARAM nip 0 T=   \ r is real
$41 TFAM-DECL-CHAR>PARAM nip 0 T=   \ uppercase is never positional
$30 TFAM-DECL-CHAR>PARAM nip 0 T=   \ non-letter is never positional
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
TFAM-RESET                   \ restore the clean slate for the rest of the suite
SCHEMA-RESET

\ ---------------------------------------------------------------------------
\ 1. Families used by lookup, scope, growth and rollback checks.
\ ---------------------------------------------------------------------------
s" pkga" CHECKER-PACKAGE-PRIVATE s" opt"  1 TK-SUM     TFAM-DECL FID !
s" pkgb" CHECKER-PACKAGE-PUBLIC  s" res"  2 TK-SUM     TFAM-DECL PID !
s" pkga" CHECKER-PACKAGE-PRIVATE s" res"  0 TK-ENUM    TFAM-DECL AID !
s" pkgc" CHECKER-PACKAGE-PUBLIC  s" pt"   0 TK-PRODUCT TFAM-DECL PTID !
s" pkgc" CHECKER-PACKAGE-PUBLIC  s" cl"   0 TK-CELL    TFAM-DECL CLID !

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
s" a-b-c"   TF-CANON? -1 T=           \ internal single hyphens are fine
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
s" @res.tag"   TF-HIDDEN? -1 T=           \ item-7 tag row shape is hidden too
s" opt"        TF-HIDDEN? 0 T=
s" pkga" s" @opt.slot0" TFAM-RESOLVE FOUNDF ! drop  FOUNDF @ 0 T=
s" pkgb" s" @res.tag"   TFAM-RESOLVE FOUNDF ! drop  FOUNDF @ 0 T=

\ Distinct values retained across the growth and snapshot checks below.
FID @ 3 TFAM-SLOTS!
FID @ 0 PK-TYPE TFAM-PK!

\ ---------------------------------------------------------------------------
\ 9. SCHEMA nodes: valid builders, malformed rejection, root pool + growth.
\    SCH nodes seed cap 4, roots seed cap 4 -> add >4 of each to force a grow.
\ ---------------------------------------------------------------------------
0 SCHEMA-PARAM NP !    NP @ SCHEMA-TAG@ SCH-PARAM T=   NP @ SCHEMA-A@ 0 T=
1 SCHEMA-CON   NC !    NC @ SCHEMA-TAG@ SCH-CON T=     NC @ SCHEMA-A@ 1 T=
FID @ 0 1 SCHEMA-APP NA !   NA @ SCHEMA-TAG@ SCH-APP T=   NA @ SCHEMA-C@ 1 T=
NP @ SCHEMA-PARAM? -1 T=    NC @ SCHEMA-CON? -1 T=       NA @ SCHEMA-APP? -1 T=
1 SCHEMA-PARAM drop   2 SCHEMA-CON drop   3 SCHEMA-PARAM drop               \ >4 nodes -> SCH grew
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

\ SC-QUOT quotation payload node (dot habu-sc-quot-full-db4d0518): each effect side
\ is a full SCH-ROW node listing that side's ordered element type nodes. Build four
\ single-element rows (din=[NP] dout=[NC] rin=[NA] rout=[NP]), then a quot; verify the
\ side roots are SCH-ROW nodes with the expected elements, an empty side, a multi-type
\ side, hasr normalization, and malformed-side rejection.
NP @ SCHEMA-ROOT+ 1 SCHEMA-ROW NQDIN !
NC @ SCHEMA-ROOT+ 1 SCHEMA-ROW NQDOUT !
NA @ SCHEMA-ROOT+ 1 SCHEMA-ROW NQRIN !
NP @ SCHEMA-ROOT+ 1 SCHEMA-ROW NQROUT !
NQDIN @ SCHEMA-TAG@ SCH-ROW T=   NQDIN @ SCHEMA-ROW? -1 T=
NQDIN @ SCHEMA-ROW-COUNT@ 1 T=   NQDIN @ SCHEMA-ROW-OK? -1 T=
NQDIN @ NQDOUT @ NQRIN @ NQROUT @ -1 SCHEMA-QUOT NQ !
NQ @ SCHEMA-TAG@ SCH-QUOT T=   NQ @ SCHEMA-QUOT? -1 T=
NQ @ SCHEMA-PARAM? 0 T=        NQ @ SCHEMA-C@ SCH-QUOT-ROWS T=
NQ @ SCHEMA-QUOT-HASR@ -1 T=
NQ @ SCHEMA-QUOT-DIN@  NQDIN @ T=   NQ @ SCHEMA-QUOT-DOUT@ NQDOUT @ T=
NQ @ SCHEMA-QUOT-RIN@  NQRIN @ T=   NQ @ SCHEMA-QUOT-ROUT@ NQROUT @ T=
NQ @ SCHEMA-QUOT-DIN@  0 SCHEMA-ROW-ELEM@ NP @ T=
NQ @ SCHEMA-QUOT-DOUT@ 0 SCHEMA-ROW-ELEM@ NC @ T=
NQ @ SCHEMA-QUOT-RIN@  0 SCHEMA-ROW-ELEM@ NA @ T=
NQ @ SCHEMA-QUOT-ROUT@ 0 SCHEMA-ROW-ELEM@ NP @ T=
\ empty side (count 0) is a legal SCH-ROW; hasr normalizes to 0 through SCH-FLAG.
SCHEMA-ROOT-N@ 0 SCHEMA-ROW NQEMP !
NQEMP @ SCHEMA-ROW? -1 T=   NQEMP @ SCHEMA-ROW-COUNT@ 0 T=
NQEMP @ NQEMP @ NQEMP @ NQEMP @ 0 SCHEMA-QUOT SCHEMA-QUOT-HASR@ 0 T=
\ multi-type side: din=[NP,NC], read both elements back in order.
NP @ SCHEMA-ROOT+ NC @ SCHEMA-ROOT+ drop 2 SCHEMA-ROW NQMUL !
NQMUL @ SCHEMA-ROW-COUNT@ 2 T=
NQMUL @ 0 SCHEMA-ROW-ELEM@ NP @ T=   NQMUL @ 1 SCHEMA-ROW-ELEM@ NC @ T=
\ malformed side = not a live SCH-ROW node: a bare type node, nil (0), or oob rejected.
NQDIN @ NQDOUT @ NQRIN @ NP @    -1 ' SCHEMA-QUOT catch   TC ! 2drop 2drop drop  TC @ E-SCHEMA-BAD T=
NQDIN @ NQDOUT @ NQRIN @ 0       -1 ' SCHEMA-QUOT catch   TC ! 2drop 2drop drop  TC @ E-SCHEMA-BAD T=
NQDIN @ NQDOUT @ NQRIN @ 99999   -1 ' SCHEMA-QUOT catch   TC ! 2drop 2drop drop  TC @ E-SCHEMA-BAD T=

\ SC-PTR pointer payload node (PLAN item 6, docs §8 SC-PTR): child round-trip,
\ nesting, predicate discrimination, and malformed-child rejection.
NC @ SCHEMA-PTR NPTR !
NPTR @ SCHEMA-TAG@ SCH-PTR T=   NPTR @ SCHEMA-PTR? -1 T=
NPTR @ SCHEMA-CON? 0 T=         NPTR @ SCHEMA-A@ NC @ T=
NPTR @ SCHEMA-PTR SCHEMA-A@ NPTR @ T=               \ ptr ptr X nests
NC @ SCHEMA-PTR? 0 T=
\ malformed child = nil node (0) / out-of-range node rejected (1 cell before catch)
0 ' SCHEMA-PTR catch   TC ! drop  TC @ E-SCHEMA-BAD T=
99999 ' SCHEMA-PTR catch   TC ! drop  TC @ E-SCHEMA-BAD T=

\ ---------------------------------------------------------------------------
\ 10. SUMV variants: add, per-family key, dup rejection, cross-family reuse.
\    SUMV-ADD ( fam name-a name-u tag sch-start sch-count paycells -- id )
\ ---------------------------------------------------------------------------
FID @ s" ok"  0 0 0 0 SUMV-ADD VOK !    VOK @ SUMV-FAM@ FID @ T=   VOK @ TWX-SUMV-TAG@ 0 T=
FID @ s" err" 1 0 0 1 SUMV-ADD VERR !   VERR @ SUMV-NAME$ s" err" T$=   VERR @ TWX-SUMV-PAYCELLS@ 1 T=
PID @ s" ok"  0 0 0 0 SUMV-ADD drop         \ same 'ok' tail under a different family is fine
PID @ s" err" 1 0 0 0 SUMV-ADD drop
AID @ s" red"   0 0 0 0 SUMV-ADD drop
AID @ s" green" 1 0 0 0 SUMV-ADD drop       \ 6 variants > seed cap 4 -> SUMV grew
FID @ s" ok" SUMV-FIND FOUNDF !  VOK @ T=  FOUNDF @ -1 T=
PID @ s" ok" SUMV-FIND FOUNDF ! drop  FOUNDF @ -1 T=
FID @ s" none" SUMV-FIND FOUNDF ! drop  FOUNDF @ 0 T=
FID @ s" ok" 0 0 0 0 ' SUMV-ADD catch   TC ! 2drop 2drop 2drop drop  TC @ E-TFAM-DUP T=

\ ---------------------------------------------------------------------------
\ 11. shared fields: atomic tx add, committed reflection, dup rejection.
\ ---------------------------------------------------------------------------
variable PFTX   variable PFOUT   variable PFIN
variable PFBASE variable PFSTR   variable PFSCH
variable PFBAD  variable PFAPP   variable PFARG
TYPE-FIELD:COUNT FX !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" x" 1 0 1 0 CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" y" 1 1 1 CELL CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" z" 1 2 1 2 cells CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" a" 1 3 1 3 cells CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" b" 1 4 1 4 cells CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
TYPE-FIELD:COUNT FX @ T=                       \ provisional ids are not reflected
PFTX @ TF-FIELD:CLOSE
TYPE-FIELD:COUNT FX @ 5 + T=                   \ 5 fields > seed cap 4 -> PF grew
PTID @ TYPE-FIELD:NO-VARIANT s" x" TYPE-FIELD:FIND FOUNDF !  FX @ T=  FOUNDF @ -1 T=
PTID @ TYPE-FIELD:NO-VARIANT s" q" TYPE-FIELD:FIND FOUNDF ! drop  FOUNDF @ 0 T=
FX @ TYPE-FIELD:FAMILY@ PTID @ T=
FX @ TYPE-FIELD:VARIANT@ TYPE-FIELD:NO-VARIANT T=
FX @ TYPE-FIELD:SLOT@ 0 T=       FX @ TYPE-FIELD:CELLS@ 1 T=
FX @ TYPE-FIELD:BYTE-OFF@ 0 T=   FX @ TYPE-FIELD:BYTES@ CELL T=
FX @ TYPE-FIELD:ALIGN@ CELL T=    FX @ TYPE-FIELD:FLAGS@ PF-FLAGS-NONE T=
FX @ TYPE-FIELD:NAME$ s" x" T$=
PTID @ TYPE-FIELD:NO-VARIANT 0 TYPE-FIELD:EACH FOUNDF ! FX @ T= FOUNDF @ -1 T=
PTID @ TYPE-FIELD:NO-VARIANT -1 TYPE-FIELD:EACH FOUNDF ! drop FOUNDF @ 0 T=

TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" x" 1 5 1 5 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! 2drop 2drop 2drop 2drop 2drop 2drop
TC @ E-TFAM-DUP T=
PFTX @ TF-FIELD:ROLLBACK

\ names are reserved independently of layout, and owner/variant membership is
\ validated before any row or interned string becomes visible.
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" make" 1 5 1 5 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-NAME T=
PFTX @ TF-FIELD:ROLLBACK

TF-FIELD:OPEN PFTX !
PFTX @ FID @ PF-NO-VARIANT s" absent" 1 0 1 0 4 4 PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-OWNER T=
PFTX @ TF-FIELD:ROLLBACK

TF-FIELD:OPEN PFTX !
PFTX @ PTID @ VOK @ s" wrong-owner" 1 5 1 40 CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-OWNER T=
PFTX @ TF-FIELD:ROLLBACK

\ Optional variant ids are part of the key. Packed rows still use the current
\ canonical cell payload ABI until a distinct packed-field ABI exists.
TF-FIELD:OPEN PFTX !
PFTX @ FID @ VOK @ s" value" 1 0 1 0 CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ FID @ VERR @ s" value" 1 0 1 0 CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:CLOSE
FID @ VOK @ s" value" TYPE-FIELD:FIND FOUNDF ! PFOUT !  FOUNDF @ -1 T=
FID @ VERR @ s" value" TYPE-FIELD:FIND FOUNDF ! PFIN !   FOUNDF @ -1 T=
PFOUT @ PFIN @ = 0 T=
PFOUT @ TYPE-FIELD:VARIANT@ VOK @ T=
PFOUT @ TYPE-FIELD:BYTES@ CELL T=  PFOUT @ TYPE-FIELD:ALIGN@ CELL T=
PFIN @ TYPE-FIELD:VARIANT@ VERR @ T=
PFIN @ TYPE-FIELD:BYTES@ CELL T=   PFIN @ TYPE-FIELD:ALIGN@ CELL T=

\ The canonical payload seam selects one representation for the whole family.
\ Named rows preserve declaration order, a fieldless sibling stays empty, and
\ neither a missing named index nor a mixed legacy schema can fall back.
variable UFAM   variable UEMPTY   variable UNAMED   variable UBASE
variable USCH0  variable USCH1   variable MFAM     variable MRAW
variable MNAMED variable MBASE
CC-N SCHEMA-CON SCHEMA-ROOT+ USCH0 !
CC-BOOL SCHEMA-CON SCHEMA-ROOT+ USCH1 !
s" pkgu" CHECKER-PACKAGE-PUBLIC s" uenum" 0 TK-SUM TFAM-DECL UFAM !
UFAM @ s" empty" 0 0 0 0 SUMV-ADD UEMPTY !
UFAM @ s" named" 1 0 0 0 SUMV-ADD UNAMED !
UFAM @ UEMPTY @ 2 TFAM-VAR-RANGE!
UFAM @ 2 TFAM-SLOTS!
TYPE-FIELD:COUNT UBASE !
TF-FIELD:OPEN PFTX !
PFTX @ UFAM @ UNAMED @ s" first" USCH0 @ 0 1 0 CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ UFAM @ UNAMED @ s" second" USCH1 @ 1 1 CELL CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:CLOSE
UFAM @ UBASE @ 2 TFAM-FLD-RANGE!
UEMPTY @ TWX-SUMV-PAY-N 0 T=
UNAMED @ TWX-SUMV-PAY-N 2 T=
UEMPTY @ TWX-SUMV-PAYCELLS@ 0 T=
UNAMED @ TWX-SUMV-PAYCELLS@ 2 T=
UFAM @ TWX-FAMILY-WIDTH 3 T=
UNAMED @ 0 SUMV-PAY-ROOT USCH0 @ T=
UNAMED @ 1 SUMV-PAY-ROOT USCH1 @ T=
UNAMED @ 0 SUMV-PAY-FIELD FOUNDF ! PFOUT !
FOUNDF @ -1 T=  PFOUT @ TYPE-FIELD:NAME$ s" first" T$=
UNAMED @ 1 SUMV-PAY-FIELD FOUNDF ! PFOUT !
FOUNDF @ -1 T=  PFOUT @ TYPE-FIELD:NAME$ s" second" T$=
UNAMED @ 2 ' SUMV-PAY-ROOT catch TC ! 2drop  TC @ E-TFAM-PAYLOAD T=
\ A nonempty field count selects named representation. Corrupt bounds reject;
\ they never make the same family fall back to its legacy SUMV storage.
UFAM @ TYPE-FIELD:COUNT 1 + 1 TFAM-FLD-RANGE!
UNAMED @ ' TWX-SUMV-PAY-N catch TC ! drop  TC @ E-TFAM-PAYLOAD T=
UFAM @ ' TWX-FAMILY-WIDTH catch TC ! drop TC @ E-TFAM-PAYLOAD T=
UFAM @ UBASE @ 2 TFAM-FLD-RANGE!
UNAMED @ TWX-SUMV-PAY-N 2 T=
UFAM @ TWX-FAMILY-WIDTH 3 T=

\ A rolled-back provisional field never enters the committed payload view.
TF-FIELD:OPEN PFTX !
PFTX @ UFAM @ UNAMED @ s" provisional" USCH0 @ 2 1 2 cells CELL CELL PF-FLAGS-NONE
   TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:ROLLBACK
UNAMED @ TWX-SUMV-PAY-N 2 T=
UNAMED @ 1 SUMV-PAY-FIELD FOUNDF ! PFOUT !
FOUNDF @ -1 T=  PFOUT @ TYPE-FIELD:NAME$ s" second" T$=
\ The rollback leaves no row behind under its owner either: the same owner takes
\ the same name and layout again, and a second one is still a duplicate.
TF-FIELD:OPEN PFTX !
PFTX @ UFAM @ UNAMED @ s" provisional" USCH0 @ 2 1 2 cells CELL CELL PF-FLAGS-NONE
   TF-FIELD:ADD PFTX !
PFTX @ UFAM @ UNAMED @ s" provisional" USCH0 @ 3 1 3 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-TFAM-DUP T=
PFTX @ TF-FIELD:ROLLBACK
UNAMED @ TWX-SUMV-PAY-N 2 T=

\ If any variant carries a legacy positional schema while the family publishes
\ named rows, both variants fail at the family-level representation boundary.
s" pkgu" CHECKER-PACKAGE-PUBLIC s" umixed" 0 TK-SUM TFAM-DECL MFAM !
MFAM @ s" raw" 0 USCH0 @ 1 1 SUMV-ADD MRAW !
MFAM @ s" named" 1 0 0 0 SUMV-ADD MNAMED !
MFAM @ MRAW @ 2 TFAM-VAR-RANGE!
MFAM @ 1 TFAM-SLOTS!
TYPE-FIELD:COUNT MBASE !
TF-FIELD:OPEN PFTX !
PFTX @ MFAM @ MNAMED @ s" value" USCH0 @ 0 1 0 CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:CLOSE
MFAM @ MBASE @ 1 TFAM-FLD-RANGE!
MRAW @ ' TWX-SUMV-PAY-N catch TC ! drop  TC @ E-TFAM-PAYLOAD T=
MNAMED @ ' TWX-SUMV-PAY-N catch TC ! drop  TC @ E-TFAM-PAYLOAD T=
MFAM @ ' TWX-FAMILY-WIDTH catch TC ! drop TC @ E-TFAM-PAYLOAD T=

\ Interleaved rows still sum per variant. A layout argument forces recursive
\ width queries while the outer variant accumulators are live.
variable IFAM variable IV0 variable IV1 variable IBASE variable ISCH
0 SCHEMA-PARAM SCHEMA-ROOT+ ISCH !
s" pkgu" CHECKER-PACKAGE-PUBLIC s" interleaved" 1 TK-SUM TFAM-DECL IFAM !
IFAM @ 0 PK-CELL TFAM-PK!
IFAM @ s" twice" 0 0 0 0 SUMV-ADD IV0 !
IFAM @ s" once" 1 0 0 0 SUMV-ADD IV1 !
IFAM @ IV0 @ 2 TFAM-VAR-RANGE!
IFAM @ 2 TFAM-SLOTS!
TYPE-FIELD:COUNT IBASE !
TF-FIELD:OPEN PFTX !
PFTX @ IFAM @ IV0 @ s" a" ISCH @ 0 1 0 CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ IFAM @ IV1 @ s" b" USCH0 @ 0 1 0 CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ IFAM @ IV0 @ s" c" ISCH @ 1 1 CELL CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ IFAM @ IV1 @ s" d" ISCH @ 1 1 CELL CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:CLOSE
IFAM @ IBASE @ 4 TFAM-FLD-RANGE!
IV0 @ TWX-SUMV-PAYCELLS@ 2 T= IV1 @ TWX-SUMV-PAYCELLS@ 2 T=
UFAM @ TWX-MK-NULLARY IFAM @ TWX-MK-UNARY T-WIDTH 7 T=
\ Ownership includes the family's declared variant slice, not just SV.FAM.
IFAM @ IV1 @ 1 TFAM-VAR-RANGE!
IV0 @ ' TWX-SUMV-PAY-N catch TC ! drop TC @ E-TFAM-PAYLOAD T=
IFAM @ IV0 @ 2 TFAM-VAR-RANGE!
UFAM @ TWX-MK-NULLARY IFAM @ TWX-MK-UNARY T-WIDTH 7 T=
\ Refuse inside a nested width query, then recover on the same instantiated
\ term after restoring the inner metadata.
variable ITERM
UFAM @ TWX-MK-NULLARY IFAM @ TWX-MK-UNARY ITERM !
UFAM @ TYPE-FIELD:COUNT 1 + 1 TFAM-FLD-RANGE!
ITERM @ ' T-WIDTH catch TC ! drop TC @ E-TFAM-PAYLOAD T=
UFAM @ UBASE @ 2 TFAM-FLD-RANGE!
ITERM @ T-WIDTH 7 T=

\ Recursive schema validation: owner param bounds, concrete liveness, malformed
\ PTR/QUOT shapes, APP family/arity/root/kind/visibility, and a valid APP.
0 SCHEMA-PARAM SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" bad-param" PFBAD @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK

\ PARAM width is defined only for cell-kinded owner parameters. Layout/type
\ parameters remain fail-closed until field-width instantiation exists.
0 SCHEMA-PARAM SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ FID @ VOK @ s" type-param" PFBAD @ 1 1 CELL CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK
FID @ 0 PK-LAYOUT TFAM-PK!
TF-FIELD:OPEN PFTX !
PFTX @ FID @ VOK @ s" layout-param" PFBAD @ 1 1 CELL CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK
FID @ 0 PK-CELL TFAM-PK!
TF-FIELD:OPEN PFTX !
PFTX @ FID @ VOK @ s" cell-param" PFBAD @ 1 1 CELL CELL CELL PF-FLAGS-NONE
   TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:ROLLBACK
FID @ 0 PK-TYPE TFAM-PK!

\ Concrete-type liveness (backed by the now internal-marked checker word
\ CT-LIVE?, dot habu-internalize-field-liveness): a field whose schema is a
\ SCHEMA-CON over a LIVE concrete type validates and adds; a SCHEMA-CON over a
\ dead concrete-type code (99999) is rejected E-PF-SCHEMA. Removing the global
\ CT-LIVE? axiom must leave both outcomes unchanged.
1 SCHEMA-CON SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" live-con" PFBAD @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:ROLLBACK

99999 SCHEMA-CON SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" bad-con" PFBAD @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK

SCH-PTR SCHEMA-N@ 0 0 SCHEMA-NEW SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" bad-ptr" PFBAD @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK

SCH-QUOT 2 0 SCH-QUOT-ROWS SCHEMA-NEW SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" bad-quot" PFBAD @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK

\ a SC-QUOT with a valid hasr but sides that are live nodes yet NOT SCH-ROW nodes:
\ PF-QUOT-ROW-OK? rejects each non-row side, so ADD reports E-PF-SCHEMA.
SCHEMA-ROOT-N@ NQST !
NP @ SCHEMA-ROOT+ drop   NC @ SCHEMA-ROOT+ drop
NA @ SCHEMA-ROOT+ drop   NP @ SCHEMA-ROOT+ drop
SCH-QUOT -1 NQST @ SCH-QUOT-ROWS SCHEMA-NEW SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" nonrow-quot" PFBAD @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK

99999 0 0 SCHEMA-APP SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" dead-app" PFBAD @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK

PID @ 0 1 SCHEMA-APP SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" arity-app" PFBAD @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK

PID @ SCHEMA-ROOT-N@ 2 SCHEMA-APP SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" range-app" PFBAD @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK

FID @ 1 1 SCHEMA-APP SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" private-app" PFBAD @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK

CC-N SCHEMA-CON SCHEMA-ROOT+ PFARG !
CC-N SCHEMA-CON SCHEMA-ROOT+ drop
PID @ PFARG @ 2 SCHEMA-APP SCHEMA-ROOT+ PFAPP !
PID @ 0 PK-LAYOUT TFAM-PK!
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" kind-app" PFAPP @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK
PID @ 0 PK-CELL TFAM-PK!
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" valid-app" PFAPP @ 20 1 20 cells CELL CELL PF-FLAGS-NONE
   TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:ROLLBACK

\ A zero-arity APP has no argument-root range, so its only canonical start is
\ zero. The canonical form remains accepted.
CLID @ 1 0 SCHEMA-APP SCHEMA-ROOT+ PFBAD !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" zero-app-start" PFBAD @
   20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-SCHEMA T=  PFTX @ TF-FIELD:ROLLBACK
CLID @ 0 0 SCHEMA-APP SCHEMA-ROOT+ PFAPP !
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" zero-app" PFAPP @
   20 1 20 cells CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:ROLLBACK

\ STACK and PACKED accept only the canonical cell mapping. Other policies
\ reject otherwise-valid rows until their field ABI validators exist.
PTID @ TL-STACK-CELL-TAG TFAM-LAYOUT!
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" stack-bad" 1 20 1 21 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TF-FIELD:ROLLBACK

PTID @ TL-PACKED-TAG TFAM-LAYOUT!
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" packed-bad" 1 20 1 20 cells 2 cells CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TF-FIELD:ROLLBACK

PTID @ TL-NICHE TFAM-LAYOUT!
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" niche-bad" 1 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TF-FIELD:ROLLBACK

PTID @ TL-BOXED TFAM-LAYOUT!
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" boxed-bad" 1 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TF-FIELD:ROLLBACK

PTID @ TL-CUSTOM TFAM-LAYOUT!
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" custom-bad" 1 20 1 20 cells CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TF-FIELD:ROLLBACK
PTID @ TL-STACK-CELL-TAG TFAM-LAYOUT!

TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" flag-bad" 1 20 1 20 cells CELL CELL 1
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-FLAGS T=  PFTX @ TF-FIELD:ROLLBACK

TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" overlap" 1 0 1 0 CELL CELL PF-FLAGS-NONE
   ' TF-FIELD:ADD catch TC ! T-PF-DROP
TC @ E-PF-LAYOUT T=  PFTX @ TF-FIELD:ROLLBACK

\ Nested commit remains provisional. Outer rollback restores both high-waters;
\ the next outer commit reuses the retired id and string space.
TYPE-FIELD:COUNT PFBASE !  TF-STR-U@ PFSTR !
TF-FIELD:OPEN PFOUT !
PFOUT @ PTID @ PF-NO-VARIANT s" outer" 1 20 1 20 cells CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFOUT !
TYPE-FIELD:COUNT PFBASE @ T=  TF-STR-U@ PFSTR @ > -1 T=
TF-FIELD:OPEN PFIN !
PFIN @ PTID @ PF-NO-VARIANT s" inner" 1 21 1 21 cells CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFIN !
PFIN @ TF-FIELD:CLOSE
TYPE-FIELD:COUNT PFBASE @ T=
PTID @ TYPE-FIELD:NO-VARIANT s" inner" TYPE-FIELD:FIND FOUNDF ! drop  FOUNDF @ 0 T=
PFOUT @ TF-FIELD:ROLLBACK
TYPE-FIELD:COUNT PFBASE @ T=  TF-STR-U@ PFSTR @ T=
PTID @ TYPE-FIELD:NO-VARIANT s" outer" TYPE-FIELD:FIND FOUNDF ! drop  FOUNDF @ 0 T=
PTID @ TYPE-FIELD:NO-VARIANT s" inner" TYPE-FIELD:FIND FOUNDF ! drop  FOUNDF @ 0 T=

TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" reuse" 1 20 1 20 cells CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:CLOSE
PTID @ TYPE-FIELD:NO-VARIANT s" reuse" TYPE-FIELD:FIND FOUNDF ! PFBASE @ T=  FOUNDF @ -1 T=

\ Strict LIFO tokens reject stale/non-top commit without corrupting either frame.
TF-FIELD:OPEN PFOUT !  TF-FIELD:OPEN PFIN !
PFOUT @ ' TF-FIELD:CLOSE catch TC ! drop  TC @ E-PF-TX T=
PFIN @ TF-FIELD:ROLLBACK  PFOUT @ TF-FIELD:ROLLBACK

\ ---------------------------------------------------------------------------
\ 12. layout records: one per family, keyed by family; dup rejection.
\    LAY-ADD ( fam policy size align tagw -- id )
\ ---------------------------------------------------------------------------
FID  @ TL-STACK-CELL-TAG 16 8 8 LAY-ADD L0 !   L0 @ LAY-FAM@ FID @ T=   L0 @ LAY-SIZE@ 16 T=
PID  @ TL-PACKED-TAG     24 8 4 LAY-ADD drop
AID  @ TL-STACK-CELL-TAG  8 8 8 LAY-ADD drop
PTID @ TL-BOXED           8 8 8 LAY-ADD drop
CLID @ TL-CUSTOM          8 8 8 LAY-ADD drop        \ 5 layouts > seed cap 4 -> LAY grew
FID @ LAY-FIND FOUNDF !  L0 @ T=  FOUNDF @ -1 T=
CLID @ LAY-FIND FOUNDF !  LAY-POLICY@ TL-CUSTOM T=  FOUNDF @ -1 T=
FID @ TL-STACK-CELL-TAG 8 8 8 ' LAY-ADD catch   TC ! 2drop 2drop drop  TC @ E-TFAM-DUP T=

\ ---------------------------------------------------------------------------
\ 12b. constructor package-name derivation (PLAN Package Shape, docs §12; item 8).
\    TF-CTOR-PKG$ ( pkg-a pkg-u tail-a tail-u -- ctor-a ctor-u ): uppercase the
\    package segment and family tail, escape a literal '-' inside the segment as
\    '--', join package-then-tail with a single '-'; when the escaped spelling
\    exceeds the 32-byte readability cap (TF-CTOR-NAME-LIMIT; raised from 16 by
\    dot habu-raise-or-alias-5d2a6b70), the name is `T` + the first 16 lowercase
\    hex digits of SHA-256 over the length-prefixed segment list + `-` + the
\    uppercase tail. Pure, injective, stable (no alloc-order id).
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

\ Readable band 16 < len <= 32 (raised from 16 by dot
\ habu-raise-or-alias-5d2a6b70): the escaped form is injective at every length
\ and the runtime/AOT dictionary stores long names (DNAME-EXT), so an escaped
\ spelling up to 32 bytes keeps its READABLE form -- the real EVID/POLICY
\ presence-slot ctor packages (EVID-CERTIFY--SLOT=18, POLICY-PROMOTE--POLICY=22)
\ are now constructable by name. These three folded to opaque SHA before the raise:
s" verylongpackagename" s" result" TF-CTOR-PKG$ s" VERYLONGPACKAGENAME-RESULT" T$=       \ 26
s" " s" verylongfamilyname" TF-CTOR-PKG$ s" VERYLONGFAMILYNAME" T$=                        \ 18
\ exactly 32 bytes stays readable (the boundary is len <= 32):
s" abcdefghijklmno" s" pqrstuvwxyzabcde" TF-CTOR-PKG$ s" ABCDEFGHIJKLMNO-PQRSTUVWXYZABCDE" T$=      \ 15+1+16

\ SHA-256 fallback fires only PAST 32 bytes now. escaped
\ `VERYLONGPACKAGENAME-RESULTRESULTR` is 33 bytes > 32, so the derived name is
\ `T` + 16 hex + `-RESULTRESULTR` = 31 bytes (the hash covers only the package
\ segment list; the tail is appended raw). Structure asserted here; the exact
\ hash goldens (determinism + injectivity + algorithm pin) follow.
s" verylongpackagename" s" resultresultr" TF-CTOR-PKG$ CPU ! CPA !
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
\ TF-CTOR-PKG$ returns a pointer into the shared derivation buffer, so intern a
\ stable copy of the first result before deriving again.
variable CPOFF
CPA @ CPU @ TF-INTERN CPOFF !
\ injectivity: a different long package hashes to a different name (the hash
\ region separates inputs that share length and tail).
s" verylongpackagenamx" s" resultresultr" TF-CTOR-PKG$ CQU ! CQA !
CQA @ CQU @  CPOFF @ CPU @ TF-OFF$  TSNE       \ NOT equal to the first long name
\ exact golden pins the pinned algorithm byte-for-byte (hash covers the package
\ segment list only, so the longer tail keeps the verylongpackagename golden):
\ SHA-256(0x13 "verylongpackagename") = 92a8624462e75ea4... (independent impl).
s" verylongpackagename" s" resultresultr" TF-CTOR-PKG$ s" T92a8624462e75ea4-RESULTRESULTR" T$=
\ a long family tail with an empty package: fallback hashes the empty segment
\ list, tail still appended (33-byte top-level tail > 32).
s" " s" abcdefghijklmnopqrstuvwxyzabcdefg" TF-CTOR-PKG$ CQU ! CQA !
CQU @ 51 T=                                 \ T(1)+16 hex+ -(1)+33-byte tail
CQA @ 1 s" T" T$=
CQA @ 1 + HEX16? -1 T=
CQA @ 18 + 33 s" ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFG" T$=
\ empty segment list golden: SHA-256("") = e3b0c44298fc1c14... (FIPS-180 constant).
s" " s" abcdefghijklmnopqrstuvwxyzabcdefg" TF-CTOR-PKG$ s" Te3b0c44298fc1c14-ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFG" T$=

\ ---------------------------------------------------------------------------
\ 13. grow across the TFAM record / string / param-kind seed caps, then prove
\    family id 0 survives every relocation.
\ ---------------------------------------------------------------------------
FID @ TF-REC@ TF-GROW-BASE !
s" pkgd" CHECKER-PACKAGE-PUBLIC s" tree"  1 TK-SUM     TFAM-DECL drop
s" pkgd" CHECKER-PACKAGE-PUBLIC s" list"  1 TK-SUM     TFAM-DECL drop
s" pkgd" CHECKER-PACKAGE-PUBLIC s" maybe" 1 TK-SUM     TFAM-DECL drop
s" pkge" CHECKER-PACKAGE-PUBLIC s" pair"  2 TK-PRODUCT TFAM-DECL drop
TFAM-N@ 12 T=                      \ includes the three unified-payload fixtures
TF-GROW-THROUGH-CAP
FID @ TF-REC@ TF-GROW-BASE @ = 0 T=   \ the record arena moved
FID @ TFAM-NAME$ s" opt" T$=
FID @ TFAM-PKG$  s" pkga" T$=
FID @ TFAM-ARITY@ 1 T=
FID @ TFAM-KIND@ TK-SUM T=
FID @ 0 TFAM-PK@ PK-TYPE T=
s" pkgd" s" tree" TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=

\ ---------------------------------------------------------------------------
\ 14. snapshot persist/restore: run the exact words TWX-CHECKER-CAPTURE-PREPARE
\    invokes and prove every store reads back identically after the bake.
\ ---------------------------------------------------------------------------
TFAM-SNAPSHOT-PERSIST
SCHEMA-SNAPSHOT-PERSIST
FID @ TFAM-NAME$ s" opt" T$=
FID @ TFAM-ARITY@ 1 T=
FID @ TFAM-KIND@ TK-SUM T=
FID @ 0 TFAM-PK@ PK-TYPE T=
FID @ TWX-TFAM-SLOTS@ 3 T=
s" pkgb" s" res" TFAM-FIND-IN FOUNDF ! PID @ T= FOUNDF @ -1 T=
FID @ s" ok" SUMV-FIND FOUNDF ! VOK @ T= FOUNDF @ -1 T=
PTID @ TYPE-FIELD:NO-VARIANT s" x" TYPE-FIELD:FIND FOUNDF ! FX @ T= FOUNDF @ -1 T=
FID @ LAY-FIND FOUNDF ! LAY-SIZE@ 16 T= FOUNDF @ -1 T=
R1 @ SCHEMA-ROOT@ SCHEMA-TAG@ SCH-PARAM T=
NA @ SCHEMA-TAG@ SCH-APP T=
\ SC-QUOT node (NQ, built in section 9: din=[NP] dout=[NC] rin=[NA] rout=[NP] hasr=-1)
\ survives the bake: tag, side SCH-ROW roots, their elements, and hasr all read back
\ from the persisted node arena + root pool (destruction review finding 3).
NQ @ SCHEMA-TAG@ SCH-QUOT T=
NQ @ SCHEMA-QUOT-DIN@  NQDIN @ T=
NQ @ SCHEMA-QUOT-DIN@  0 SCHEMA-ROW-ELEM@ NP @ T=
NQ @ SCHEMA-QUOT-ROUT@ 0 SCHEMA-ROW-ELEM@ NP @ T=
NQ @ SCHEMA-QUOT-HASR@ -1 T=
\ The committed named-field view survives snapshot persistence byte-for-byte.
UEMPTY @ TWX-SUMV-PAY-N 0 T=
UNAMED @ TWX-SUMV-PAY-N 2 T=
UNAMED @ 0 SUMV-PAY-ROOT USCH0 @ T=
UNAMED @ 1 SUMV-PAY-FIELD FOUNDF ! PFOUT !
FOUNDF @ -1 T=  PFOUT @ TYPE-FIELD:NAME$ s" second" T$=

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
\ 16. A global family and package-owned families may share a tail. Resolution
\     is lexical: exact owner, exact global, then one non-global public family.
\     The exact rows keep their independent arities through snapshot persist.
\ ---------------------------------------------------------------------------
variable GSPAN  variable MSPAN  variable MLOCAL  variable OPUBLIC
s" "      CHECKER-PACKAGE-PUBLIC  s" span" 3 TK-CELL TFAM-DECL GSPAN !
s" mem"   CHECKER-PACKAGE-PUBLIC  s" span" 6 TK-CELL TFAM-DECL MSPAN !
s" mem"   CHECKER-PACKAGE-PRIVATE s" tier" 1 TK-CELL TFAM-DECL MLOCAL !
s" other" CHECKER-PACKAGE-PUBLIC  s" tier" 2 TK-CELL TFAM-DECL OPUBLIC !

\ Exact owner wins, including its private row.
s" mem" s" span" TFAM-RESOLVE FOUNDF ! MSPAN @ T= FOUNDF @ -1 T=
s" mem" s" tier" TFAM-RESOLVE FOUNDF ! MLOCAL @ T= FOUNDF @ -1 T=
\ Global is the lexical row at top level and in packages without an own row.
s" "      s" span" TFAM-RESOLVE FOUNDF ! GSPAN @ T= FOUNDF @ -1 T=
s" caller" s" span" TFAM-RESOLVE FOUNDF ! GSPAN @ T= FOUNDF @ -1 T=
\ With no own or global row, the unique package-public family is the fallback.
s" caller" s" tier" TFAM-RESOLVE FOUNDF ! OPUBLIC @ T= FOUNDF @ -1 T=
s" tier" TFAM-FIND-PUBLIC FOUNDF ! OPUBLIC @ T= FOUNDF @ -1 T=
\ Exact identities and arities never alias.
s" " s" span" TFAM-FIND-IN FOUNDF ! GSPAN @ T= FOUNDF @ -1 T=
s" mem" s" span" TFAM-FIND-IN FOUNDF ! MSPAN @ T= FOUNDF @ -1 T=
GSPAN @ MSPAN @ <> -1 T=
GSPAN @ TFAM-ARITY@ 3 T=
MSPAN @ TFAM-ARITY@ 6 T=
MLOCAL @ TFAM-ARITY@ 1 T=
OPUBLIC @ TFAM-ARITY@ 2 T=

TFAM-SNAPSHOT-PERSIST
s" " s" span" TFAM-FIND-IN FOUNDF ! GSPAN @ T= FOUNDF @ -1 T=
s" mem" s" span" TFAM-FIND-IN FOUNDF ! MSPAN @ T= FOUNDF @ -1 T=
s" caller" s" span" TFAM-RESOLVE FOUNDF ! GSPAN @ T= FOUNDF @ -1 T=
s" caller" s" tier" TFAM-RESOLVE FOUNDF ! OPUBLIC @ T= FOUNDF @ -1 T=
GSPAN @ TFAM-ARITY@ 3 T=
MSPAN @ TFAM-ARITY@ 6 T=

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
;package
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
\ packed-tag ACCEPT (item 16 sub-slice 2, docs §22.0/§22.2). `POLICY packed-tag`
\ declares: the family row carries TL-PACKED-TAG and the close bakes the
\ PACKED-DESC memory descriptor into the LAY registry. packed is a MEMORY-ABI
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
s" pkac" s" pkacp" TFAM-FIND-IN drop PQA !
s" pkac" s" pkacs" TFAM-FIND-IN drop PQB !
PQA @ TFAM-LAYOUT-POLICY@ TL-PACKED-TAG T=
PQB @ TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=
PQA @ TFAM-WIDTH@ PQB @ TFAM-WIDTH@ T=
\ the close baked one LAY row for the packed family; values == PACKED-DESC
PQA @ LAY-FIND PQF ! PQL !
PQF @ -1 T=
PQL @ LAY-POLICY@ TL-PACKED-TAG T=
PQA @ PACKED-DESC PTW ! PAL ! PSZ !
PQL @ LAY-SIZE@ PSZ @ T=   PQL @ LAY-ALIGN@ PAL @ T=   PQL @ LAY-TAGW@ PTW @ T=
\ the stack-cell-tag twin bakes NO row
PQB @ LAY-FIND PQF ! drop
PQF @ 0 T=
\ packed enum + product descriptors baked at close
s" pkac" s" pkace" TFAM-FIND-IN drop PQA !
PQA @ TFAM-LAYOUT-POLICY@ TL-PACKED-TAG T=
PQA @ LAY-FIND PQF ! PQL !
PQF @ -1 T=
PQL @ LAY-SIZE@ 1 T=   PQL @ LAY-ALIGN@ 1 T=   PQL @ LAY-TAGW@ 1 T=
s" pkac" s" pkacr" TFAM-FIND-IN drop PQA !
PQA @ LAY-FIND PQF ! PQL !
PQF @ -1 T=
PQL @ LAY-SIZE@ 16 T=  PQL @ LAY-ALIGN@ 8 T=   PQL @ LAY-TAGW@ 0 T=

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
\ TFAM-LAYOUT! mutator, exactly like the packed descriptor (LAY) unit tests.
\ A multi-slot SUM (default width slots+1) collapses to 1 under boxed and under
\ niche, but keeps its cell width under stack-cell-tag AND packed (packed is a
\ MEMORY-ABI descriptor only, §22.2) — the regression guard that the branch
\ fires for boxed/niche alone.
\ ---------------------------------------------------------------------------
s" pkgw" CHECKER-PACKAGE-PRIVATE s" wbx" 1 TK-SUM TFAM-DECL WBX !
WBX @ 2 TFAM-SLOTS!                                           \ 2 payload slots
WBX @ TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=                \ default policy
WBX @ TFAM-WIDTH@ 3 T=                                        \ slots + tag
WBX @ TL-BOXED       TFAM-LAYOUT!   WBX @ TFAM-WIDTH@ 1 T=
WBX @ TL-NICHE       TFAM-LAYOUT!   WBX @ TFAM-WIDTH@ 1 T=
WBX @ TL-PACKED-TAG  TFAM-LAYOUT!   WBX @ TFAM-WIDTH@ 3 T=      \ packed keeps cell width
WBX @ TL-STACK-CELL-TAG TFAM-LAYOUT!   WBX @ TFAM-WIDTH@ 3 T=      \ restored

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
s" pkiw" s" pkiw1" TFAM-FIND-IN drop IWP1 !
s" pkiw" s" pkiw3" TFAM-FIND-IN drop IWP3 !
s" pkiw" s" pkiwo" TFAM-FIND-IN drop IWOPT !
\ declared (params-as-cells) widths — the family-only baseline
IWP1 @ TFAM-WIDTH@ 1 T=                          \ enum: tag only
IWP3 @ TFAM-WIDTH@ 3 T=                           \ product: three cells
IWOPT @ TFAM-WIDTH@ 2 T=                          \ sum: max(none=0, some=1-as-cell) + tag
\ arg-aware width == T-WIDTH of the built terms
IWP1 @ TWX-MK-NULLARY T-WIDTH 1 T=                \ enum term self-check
IWP3 @ TWX-MK-NULLARY T-WIDTH 3 T=                \ product term self-check
\ behaviour-preserving: a width-1 layout arg reproduces the declared sum width
IWP1 @ TWX-MK-NULLARY IWOPT @ TWX-MK-UNARY T-WIDTH 2 T=           \ opt<enum1> == declared 2
\ the groundwork proof: a width-3 layout arg widens the sum payload (declared width 2 was degenerate)
IWP3 @ TWX-MK-NULLARY IWOPT @ TWX-MK-UNARY IWT !
IWT @ T-WIDTH 4 T=                                \ opt<pt3>: max(0, width(pt3)=3) + tag = 4
IWOPT @ TFAM-WIDTH@ 2 T=                          \ family-only width unchanged (still 2) — arg-aware differs

\ ---------------------------------------------------------------------------
\ 20. field-id hardening (dot habu-harden-field-tokens): every public indexed
\ reflection accessor rejects an out-of-band id with a catchable E-PF-ID throw
\ instead of a process-killing `die`; TFAM-RESET refuses to discard a live
\ field-transaction frame; and the transaction-token generation stays monotonic
\ across reset, so a completed token can never alias a transaction begun later.
\ ---------------------------------------------------------------------------
variable PROVID   variable STOK   variable NTOK   variable RTOK   variable RCNT

\ net-neutral ( -- ) probes over each public indexed accessor, reading the
\ candidate id from PROVID so a top-level `catch` reports only the throw code.
: PRB-FAM    ( -- ) PROVID @ TYPE-FIELD:FAMILY@   drop ;
: PRB-VAR    ( -- ) PROVID @ TYPE-FIELD:VARIANT@  drop ;
: PRB-NAME   ( -- ) PROVID @ TYPE-FIELD:NAME$     2drop ;
: PRB-SCH    ( -- ) PROVID @ TYPE-FIELD:SCHEMA@   drop ;
: PRB-SLOT   ( -- ) PROVID @ TYPE-FIELD:SLOT@     drop ;
: PRB-CELLS  ( -- ) PROVID @ TYPE-FIELD:CELLS@    drop ;
: PRB-BOFF   ( -- ) PROVID @ TYPE-FIELD:BYTE-OFF@ drop ;
: PRB-BYTES  ( -- ) PROVID @ TYPE-FIELD:BYTES@    drop ;
: PRB-ALIGN  ( -- ) PROVID @ TYPE-FIELD:ALIGN@    drop ;
: PRB-FLAGS  ( -- ) PROVID @ TYPE-FIELD:FLAGS@    drop ;

\ a provisional row: added inside an open transaction, so its id sits at exactly
\ the committed high-water and is NOT yet reflected. Every indexed accessor must
\ reject that guessed id with E-PF-ID rather than read the uncommitted row.
TF-FIELD:OPEN PFTX !
PFTX @ PTID @ PF-NO-VARIANT s" prov" 1 31 1 31 cells CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
TYPE-FIELD:COUNT PROVID !
' PRB-FAM   catch TC ! TC @ E-PF-ID T=
' PRB-VAR   catch TC ! TC @ E-PF-ID T=
' PRB-NAME  catch TC ! TC @ E-PF-ID T=
' PRB-SCH   catch TC ! TC @ E-PF-ID T=
' PRB-SLOT  catch TC ! TC @ E-PF-ID T=
' PRB-CELLS catch TC ! TC @ E-PF-ID T=
' PRB-BOFF  catch TC ! TC @ E-PF-ID T=
' PRB-BYTES catch TC ! TC @ E-PF-ID T=
' PRB-ALIGN catch TC ! TC @ E-PF-ID T=
' PRB-FLAGS catch TC ! TC @ E-PF-ID T=
PFTX @ TF-FIELD:ROLLBACK

\ negative and out-of-range committed ids reject the same way (no open tx, so
\ TYPE-FIELD:COUNT is exactly one past the last valid committed id).
-1 PROVID !                ' PRB-FAM catch TC ! TC @ E-PF-ID T=
TYPE-FIELD:COUNT PROVID !  ' PRB-FAM catch TC ! TC @ E-PF-ID T=
999999 PROVID !            ' PRB-FAM catch TC ! TC @ E-PF-ID T=

\ active-reset reject preserves the frame: with a transaction open, TFAM-RESET
\ throws E-PF-TX and changes nothing — the registry survives and the same token
\ still commits cleanly, proving the frame was never discarded.
TYPE-FIELD:COUNT RCNT !
TF-FIELD:OPEN RTOK !
' TFAM-RESET catch TC ! TC @ E-PF-TX T=
PTID @ TFAM-ARITY@ 0 T=              \ registry NOT wiped: PTID is still a valid family
TYPE-FIELD:COUNT RCNT @ T=           \ committed high-water unchanged
RTOK @ TF-FIELD:CLOSE                 \ frame intact: the token still matches the live top frame
TYPE-FIELD:COUNT RCNT @ T=           \ empty commit publishes nothing

\ completed token stays stale across reset + new begin: mint a token, commit it,
\ run a legal reset (depth 0), then begin afresh. The new token is strictly
\ greater — the serial did not rewind across reset — so the completed pre-reset
\ token no longer matches the top frame and committing it rejects E-PF-TX.
TF-FIELD:OPEN STOK !
STOK @ TF-FIELD:CLOSE
TFAM-RESET
TF-FIELD:OPEN NTOK !
NTOK @ STOK @ > -1 T=                \ token generation did not rewind across reset
NTOK @ STOK @ = 0 T=                 \ so a completed token can never be reissued
STOK @ ' TF-FIELD:CLOSE catch TC ! drop  TC @ E-PF-TX T=   \ stale token never aliases the new frame
NTOK @ TF-FIELD:ROLLBACK

\ ---------------------------------------------------------------------------
\ 21. field rollback zero-scrub (dot habu-make-field-rollback): a rolled-back
\ provisional row is scrubbed to canonical zero in the live arena, and snapshot
\ persistence zero-fills the unused product-field capacity, so retired bytes
\ never enter reflection or snapshot/fixpoint identity. Raw shims read/write PF
\ records directly (bypassing the committed-id bound) to observe retired slots.
\ ---------------------------------------------------------------------------
: TWX-PF-RAW@ ( n n -- n ) {: id:n off:n :} id PF-REC * PF-BASE + off cells + @ ;
: TWX-PF-RAW! ( n n n -- ) {: v:n id:n off:n :} v id PF-REC * PF-BASE + off cells + ! ;
variable RBPID   variable RBCN   variable RBIDX

\ fresh product with two committed fields (case 20's stale-token check wiped the
\ registry, so start from a clean slate).
s" pkr" CHECKER-PACKAGE-PUBLIC s" prod" 0 TK-PRODUCT TFAM-DECL RBPID !
TF-FIELD:OPEN PFTX !
PFTX @ RBPID @ PF-NO-VARIANT s" f0" 1 0 1 0 CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ RBPID @ PF-NO-VARIANT s" f1" 1 1 1 CELL CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
PFTX @ TF-FIELD:CLOSE

\ nested rollback scrubs the retired provisional row to canonical zero.
TF-FIELD:OPEN PFTX !
PFTX @ RBPID @ PF-NO-VARIANT s" prov" 1 6 1 6 cells CELL CELL PF-FLAGS-NONE TF-FIELD:ADD PFTX !
TYPE-FIELD:COUNT RBIDX !            \ provisional row index (== committed high-water)
RBIDX @ 5 TWX-PF-RAW@ 6 T=          \ SLOT field (cell 5) is written = 6 while provisional
PFTX @ TF-FIELD:ROLLBACK
RBIDX @ 0 TWX-PF-RAW@ 0 T=          \ FAM scrubbed
RBIDX @ 5 TWX-PF-RAW@ 0 T=          \ SLOT scrubbed
RBIDX @ 10 TWX-PF-RAW@ 0 T=         \ FLAGS scrubbed

\ persistence zero-fills the unused capacity: plant garbage in the first unused
\ tail slot, persist, and prove it bakes as canonical zero while committed rows
\ survive the bake (AOT restore fidelity).
TYPE-FIELD:COUNT RBCN !
PF-CAP RBCN @ > -1 T=               \ there is unused tail capacity to fill
$5eed RBCN @ 3 TWX-PF-RAW!
RBCN @ 3 TWX-PF-RAW@ $5eed T=       \ planted garbage in the unused tail
TFAM-SNAPSHOT-PERSIST
RBCN @ 3 TWX-PF-RAW@ 0 T=           \ persist scrubbed the unused capacity to zero
RBPID @ TYPE-FIELD:NO-VARIANT s" f0" TYPE-FIELD:FIND FOUNDF ! drop FOUNDF @ -1 T=
RBPID @ TYPE-FIELD:NO-VARIANT s" f1" TYPE-FIELD:FIND FOUNDF ! drop FOUNDF @ -1 T=

\ ---------------------------------------------------------------------------
\ The sealed TYPE-NAME owner rejects every reserved variant category and only
\ families in the global or active package scope. An unrelated package does not
\ reserve the same tail.
\ ---------------------------------------------------------------------------
s" " CHECKER-PACKAGE-PUBLIC s" global-variant" 0 TK-ENUM TFAM-DECL drop
s" variant-name-test" CHECKER-PACKAGE-PUBLIC s" local-variant" 0 TK-ENUM TFAM-DECL drop
s" other-variant-test" CHECKER-PACKAGE-PUBLIC s" foreign-variant" 0 TK-ENUM TFAM-DECL drop

package variant-name-test

VALUE-RECORD variant-record payload n END-VALUE-RECORD

s" " ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7107 T=
s" Bad" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7101 T=
s" n" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" q" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" variant-record" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" field" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" bool" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" space-x" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" fresh-mask-x" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" if" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" variant" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" global-variant" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" local-variant" ' TYPE-NAME:VARIANT-REQUIRE catch TC ! 2drop
TC @ 7110 T=
s" foreign-variant" TYPE-NAME:VARIANT-REQUIRE
s" ready" TYPE-NAME:VARIANT-REQUIRE

;package

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-family-suite: failures" 1 die ;
REPORT

;using
;using
