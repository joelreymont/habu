\ maki/extent-test.f - checked tests for EXTENT: (maki/extent.f).
\
\ Proves: the runtime extent value binding, the ix<extent> roundtrip crossing, that
\ two extents are DISTINCT nominal types (flip = checker reject), that a bare n never
\ crosses into an index implicitly, and that a tail collision throws a NAMED reject.
\ Uses its own extent names (#EA/#EB -> extea/exteb) so no other suite's extents
\ collide when maki/test.f loads every file into one process.

require lib/test.f
require test/checker-assert.f
require maki/extent.f

T-RESET

using TFAM

package MAKI

4 EXTENT: #EA
3 EXTENT: #EB

\ --- runtime extent value binding -------------------------------------------
#EA 4 T=
#EB 3 T=

\ --- explicit ix<->n crossing roundtrip (value preserved) -------------------
3 >#EA IX>N 3 T=
2 >#EB IX>N 2 T=

\ --- the injector runtime-guards the bound: crossing outside [0, extent) throws.
: EA-HI ( -- ) #EA >#EA drop ;     \ n == #EA -> out of [0,#EA)
: EA-LO ( -- ) -1 >#EA drop ;
' EA-HI E-EXT-RANGE TTHROWS
' EA-LO E-EXT-RANGE TTHROWS

\ --- two extents are DISTINCT nominal types (identity-body probes) -----------
\ CHECK-QUIET-CANDIDATE!: -1 accept, 0 reject.
s" XID  ( ix<extea> -- ix<extea> )"  CHECK-QUIET-CANDIDATE! -1 T=   \ same extent flows
s" XFL  ( ix<extea> -- ix<exteb> )"  CHECK-QUIET-CANDIDATE!  0 T=   \ flip = reject
s" XFL2 ( ix<exteb> -- ix<extea> )"  CHECK-QUIET-CANDIDATE!  0 T=

\ --- a bare n is never an index (crossing must be explicit) ------------------
s" XNIN ( n -- ix<extea> )"          CHECK-QUIET-CANDIDATE!  0 T=
s" XNOUT ( ix<extea> -- n )"         CHECK-QUIET-CANDIDATE!  0 T=

\ --- a non-positive extent size is a NAMED throw, and mints nothing -----------
\ EXTENT: parses its name at execution, so the wrapper runs the declaration and
\ the #name to parse sits inline after the call site (consumed by parse-name).
: TRY-NEG  ( -- ) -5 EXTENT: ;
' TRY-NEG  E-EXT-VALUE TTHROWS #XNEG
: TRY-ZERO ( -- ) 0 EXTENT: ;
' TRY-ZERO E-EXT-VALUE TTHROWS #XZERO
\ XR-FIND now returns option<xr-slot>; a bad name resolves to none.
: XR-ABSENT? ( ptr u8 n -- bool )
   XR-FIND MATCH option  none OF true ENDOF  some OF drop false ENDOF ;MATCH ;
s" #XNEG"  XR-ABSENT? -1 T=          \ nothing minted: bad names absent from the registry
s" #XZERO" XR-ABSENT? -1 T=
\ the shared decimal emitter itself fails closed on a negative (defense in depth).
: TRY-XGI  ( -- ) -3 XG-INT ;
' TRY-XGI  E-EXT-VALUE TTHROWS

\ --- a mangling collision is a NAMED throw (E-TFAM-DUP), never a silent rename.
\ #EA above minted family tail `extea`; the exact CHECKER-DEFFAMILY line EXTENT:
\ runs throws on that duplicate tail (proven here without EXTENT:'s stream parse).
: TRY-DUP ( -- )  s" extea" s" 0" CHECKER-DEFFAMILY ;
\ redirect the expected declaration diagnostic to a scratch buffer so the asserted
\ throw does not print "duplicate family" noise into the suite transcript.
create ET-DIAG 4096 allot  ET-DIAG 4096 DIAG-BUFFER!
' TRY-DUP E-TFAM-DUP TTHROWS
DIAG-BUFFER-OFF

\ --- (c) the registry slot index is its own type: a raw n cannot address a column.
\ CHECK-QUIET-CANDIDATE!: -1 accept, 0 reject.
s" XSOK  ( xr-slot -- n ) XR-VAL@ "            CHECK-QUIET-CANDIDATE! -1 T=   \ a real slot reads a column
s" XSRAW ( n -- n ) XR-VAL@ "                  CHECK-QUIET-CANDIDATE!  0 T=   \ raw n as a slot: reject
s" XSMK  ( n -- n ) >XR-SLOT XR-VAL@ "         CHECK-QUIET-CANDIDATE! -1 T=   \ explicit crossing restores it

\ --- (c) surface-name length and tail length are DISTINCT: wrong column = reject.
s" XLOK  ( xr-slot -- xr-surf-len ) XR-SLEN@ " CHECK-QUIET-CANDIDATE! -1 T=   \ right length column
s" XLBAD ( xr-slot -- xr-surf-len ) XR-TLEN@ " CHECK-QUIET-CANDIDATE!  0 T=   \ tail length where surface length wanted

\ --- BTC-7 extent-role product / factorization surface (EXTPROD: / >RED, dot
\ habu-extent-role-product-8e364885). The checker half (product former, ordered/
\ mismatch/split-retype, the free-vs-inner contraction rule) is the routed regression
\ test/extent-product-test.f; here is the candidate-B SURFACE: the generated
\ fold/split/join round-trip, the E-EXT-FACTOR value reject, and the contraction rule
\ wired end to end through the shipped words.
8   EXTENT: #PB     \ free / batch factor
16  EXTENT: #PT     \ in-block / sequence factor
128 EXTENT: #PR     \ folded rows = B*T
EXTPROD: #PR ( #PB #PT )   \ verify 128 == 8*16, mark #PB free, derive #PR-FOLD/SPLIT/JOIN

\ (b) legal fold / split / join round-trip type-checks (the shipped generated words).
s" PFO ( n -- ix<extprod<extpb,extpt>> ) #PR-FOLD "                                  CHECK-QUIET-CANDIDATE! -1 T=
s" PRT ( ix<extprod<extpb,extpt>> -- ix<extprod<extpb,extpt>> ) #PR-SPLIT #PR-JOIN " CHECK-QUIET-CANDIDATE! -1 T=
s" PSP ( ix<extprod<extpb,extpt>> -- ix<extpb> ix<extpt> ) #PR-SPLIT "               CHECK-QUIET-CANDIDATE! -1 T=

\ (c) end to end: a contraction (>RED) over the in-block extent type-checks; over the
\ free #B factor or the whole product it is a load-time reject (the cross-sequence leak).
s" PCI ( ix<extpt> -- redx<extpt> ) >RED "                                CHECK-QUIET-CANDIDATE! -1 T=
s" PCB ( ix<extpb> -- redx<extpb> ) >RED "                                CHECK-QUIET-CANDIDATE!  0 T=
s" PCP ( ix<extprod<extpb,extpt>> -- redx<extprod<extpb,extpt>> ) >RED "  CHECK-QUIET-CANDIDATE!  0 T=

\ (e) product-mismatch: a folded-rows extent whose asserted factors' product differs
\ from its size is an E-EXT-FACTOR load reject (#PRBAD = 20 != 8*16). The inline names
\ after TTHROWS are consumed by EXTPROD:'s parse at run time.
20 EXTENT: #PRBAD
: PR-BAD ( -- ) EXTPROD: ;
' PR-BAD E-EXT-FACTOR TTHROWS #PRBAD ( #PB #PT )

;package

T-REPORT

;using
