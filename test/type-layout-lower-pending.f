\ type-layout-lower-pending.f — STAGED slice-3 fixtures for TFAM 12 width-aware
\ lowering (habu-tfam-12-layout, docs/type-families.md §17-18). Run BY THE
\ ENGINE over stdin, standalone — deliberately NOT wired into any suite yet:
\     bin/hb < test/type-layout-lower-pending.f
\ It is green today: every TLP-* word below is a REAL checked definition (the
\ compile subject slice 3 dumps through the native/JIT emitters), and the
\ width-fact asserts after each definition pin the exact per-op contract the
\ emitters consume (operand position 0=top, family-id, registry logical width;
\ absent row = one-cell operand).
\
\ Slice-3 activation checklist:
\   1. capture the native/JIT shuffle goldens for each TLP-* subject via the
\      image/JIT dumpers (tools/jitdump-core.f) and pin them here — goldens are
\      captured from the implemented emitters, never invented beforehand;
\   2. add execution bundle-preservation T{ }T rows once the checked test-entry
\      seeding (docs §25.5) exists alongside LAYOUT-PUSH-FIELDS;
\   3. wire this file into the engine gate slice next to test/type-decl-suite.f;
\   4. flip the parity fixtures staged in tools/check-all-errors-test.f
\      (CAE-TEST-CONST-CARRY) and tools/public-signatures-test.f
\      (PST-TEST-CONST-CARRY), and flip TD12-CONST in test/type-decl-suite.f to
\      the shape-carry expectation.

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

\ ---------------------------------------------------------------------------
\ layout families under test: width 2 (1 payload slot + tag), width 4
\ (3 payload slots + tag), and a zero-payload enum-shaped sum (width 1).
\ ---------------------------------------------------------------------------
SUMTYPE tlp-res 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
SUMTYPE tlp-mix 2
  VARIANT small a ;VARIANT
  VARIANT big a b n ;VARIANT
;SUMTYPE
SUMTYPE tlp-en 0
  VARIANT lit  ;VARIANT
  VARIANT dark ;VARIANT
;SUMTYPE

variable TLOK   variable TLF   variable TLX
s" " s" tlp-res" TFAM-FIND-IN TLOK ! TLF !
TLOK @ -1 T=
TLF @ TFAM-WIDTH@ 2 T=
s" " s" tlp-mix" TFAM-FIND-IN TLOK ! TLX !
TLOK @ -1 T=
TLX @ TFAM-WIDTH@ 4 T=

\ ---------------------------------------------------------------------------
\ transport matrix on the width-2 family. Each subject is a real checked
\ definition; reaching the next line proves certification (a reject kills the
\ load), and the width facts of ITS check are read immediately after. Facts
\ are per-CHECK scratch, so each assert block runs before the next definition.
\ ---------------------------------------------------------------------------
: TLP-DUP ( tlp-res<n,n> -- tlp-res<n,n> tlp-res<n,n> ) dup ;
WF-N@ 1 T=  0 WF-POS@ 0 T=  0 WF-FAM@ TLF @ T=  0 WF-WIDTH@ 2 T=
: TLP-DROP ( tlp-res<n,n> -- ) drop ;
WF-N@ 1 T=  0 WF-POS@ 0 T=
: TLP-SWAP ( tlp-res<n,n> n -- n tlp-res<n,n> ) swap ;
WF-N@ 1 T=  0 WF-POS@ 1 T=  0 WF-WIDTH@ 2 T=
: TLP-OVER ( tlp-res<n,n> n -- tlp-res<n,n> n tlp-res<n,n> ) over ;
WF-N@ 1 T=  0 WF-POS@ 1 T=
: TLP-NIP ( tlp-res<n,n> n -- n ) nip ;
WF-N@ 1 T=  0 WF-POS@ 1 T=
: TLP-TUCK ( n tlp-res<n,n> -- tlp-res<n,n> n tlp-res<n,n> ) tuck ;
WF-N@ 1 T=  0 WF-POS@ 0 T=
: TLP-ROT ( tlp-res<n,n> n n -- n n tlp-res<n,n> ) rot ;
WF-N@ 1 T=  0 WF-POS@ 2 T=
: TLP-MROT ( n n tlp-res<n,n> -- tlp-res<n,n> n n ) -rot ;
WF-N@ 1 T=  0 WF-POS@ 0 T=
: TLP-2DUP ( tlp-res<n,n> n -- tlp-res<n,n> n tlp-res<n,n> n ) 2dup ;
WF-N@ 1 T=  0 WF-POS@ 1 T=
: TLP-2DROP ( tlp-res<n,n> n -- ) 2drop ;
WF-N@ 1 T=  0 WF-POS@ 1 T=
: TLP-2SWAP ( tlp-res<n,n> n n n -- n n tlp-res<n,n> n ) 2swap ;
WF-N@ 1 T=  0 WF-POS@ 3 T=
: TLP-2OVER ( tlp-res<n,n> n n n -- tlp-res<n,n> n n n tlp-res<n,n> n ) 2over ;
WF-N@ 1 T=  0 WF-POS@ 3 T=

\ return-stack transfers: one fact per op, from the row the op consumes.
: TLP-TOR ( tlp-res<n,n> -- tlp-res<n,n> ) >r r> ;
WF-N@ 2 T=  0 WF-TOKIX@ 1 T=  1 WF-TOKIX@ 2 T=  1 WF-POS@ 0 T=
: TLP-RAT ( tlp-res<n,n> -- tlp-res<n,n> tlp-res<n,n> ) >r r@ r> ;
WF-N@ 3 T=  1 WF-TOKIX@ 2 T=  2 WF-TOKIX@ 3 T=
: TLP-2TOR ( tlp-res<n,n> n -- tlp-res<n,n> n ) 2>r 2r> ;
WF-N@ 2 T=  0 WF-POS@ 1 T=  1 WF-POS@ 1 T=
: TLP-2RAT ( tlp-res<n,n> n -- tlp-res<n,n> n tlp-res<n,n> n ) 2>r 2r@ 2r> ;
WF-N@ 3 T=  0 WF-POS@ 1 T=  1 WF-POS@ 1 T=  2 WF-POS@ 1 T=

\ locals capture: the whole group records at the :} token. `x` binds the layout
\ value; locals annotations cannot express family types yet (capability dotted:
\ habu-typed-locals-for-b06b6707), so the entry effect carries the detailed type.
\ typed-local-lint: allow-bare-local
: TLP-LOCAL ( tlp-res<n,n> n -- n ) {: x y:n :} y ;
WF-N@ 1 T=  0 WF-POS@ 1 T=  0 WF-FAM@ TLF @ T=  0 WF-WIDTH@ 2 T=

\ ---------------------------------------------------------------------------
\ width-4 spot checks and multi-fact ordering: facts scan top position first.
\ ---------------------------------------------------------------------------
: TLP-MIX-DUP ( tlp-mix<n,n> -- tlp-mix<n,n> tlp-mix<n,n> ) dup ;
WF-N@ 1 T=  0 WF-FAM@ TLX @ T=  0 WF-WIDTH@ 4 T=
: TLP-MIX-SWAP ( tlp-mix<n,n> n -- n tlp-mix<n,n> ) swap ;
WF-N@ 1 T=  0 WF-POS@ 1 T=  0 WF-WIDTH@ 4 T=
: TLP-DUAL-2SWAP ( tlp-res<n,n> n tlp-mix<n,n> n -- tlp-mix<n,n> n tlp-res<n,n> n ) 2swap ;
WF-N@ 2 T=
0 WF-POS@ 1 T=  0 WF-FAM@ TLX @ T=  0 WF-WIDTH@ 4 T=
1 WF-POS@ 3 T=  1 WF-FAM@ TLF @ T=  1 WF-WIDTH@ 2 T=

\ a zero-payload enum-shaped sum is width 1: emitters keep one-cell lowering.
: TLP-EN-DUP ( tlp-en -- tlp-en tlp-en ) dup ;
WF-N@ 1 T=  0 WF-WIDTH@ 1 T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-layout-lower-pending: failures" 1 die ;
REPORT
