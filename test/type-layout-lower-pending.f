\ type-layout-lower-pending.f — TFAM 12 width-aware lowering suite
\ (habu-tfam-12-layout, docs/type-families.md §17-18). Run BY THE ENGINE over
\ stdin, standalone or through the engine gate (GE-TYPE-LAYOUT-SUITE in
\ test/gate-engine-lib.f, next to the type-decl suite):
\     bin/hb < test/type-layout-lower-pending.f
\ Three sections:
\   1. width-fact contracts — every TLP-* subject is a REAL checked definition;
\      the asserts after each pin the per-op fact the emitter consumes
\      (operand position 0=top, family-id, registry logical width; absent row
\      = one-cell operand).
\   2. emitted-lowering goldens — exact u32 instruction sequences of five
\      representative subjects, captured FROM the implemented pass-2 emitter
\      (EM-COMPILE-P2WIDE / LP2COPY / LP2ROT / LP2RS / EM-P2-CARVE, habu2.f).
\      The emitter is deterministic (the build's two-build byte-compare pins
\      it), so the goldens are exact; they move only when the lowering does.
\   3. execution rows — TRUSTED seed makers push raw payload+tag cells
\      (docs §25.5-style seeding; the maker's declared layout effect is the
\      premise), whole-bundle transports run at RUNTIME, and TRUSTED unpackers
\      surface the cells for value asserts: dup/swap/over/nip/tuck/rot/-rot/
\      2dup/2drop/2swap/2over, the return-stack transfers, and wide locals.
\
\ Remaining follow-up (constant shape-carry, habu-tfam-12-layout dot): flip the
\ parity fixtures staged in tools/check-all-errors-test.f (CAE-TEST-CONST-CARRY)
\ and tools/public-signatures-test.f (PST-TEST-CONST-CARRY), and flip TD12-CONST
\ in test/type-decl-suite.f to the shape-carry expectation.

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
\ emitted-lowering goldens: exact u32 sequences of five subjects, captured
\ from the implemented pass-2 emitter. Layout invariants pinned here:
\   TLP-DUP     w2 dup     = spill-free copy loop (movz #2; src = top-2 cells)
\   TLP-SWAP    w2-under-scalar swap = triple in-place reversal of the top 3
\               cells (rotate the bottom 2-cell group to the top)
\   TLP-MIX-DUP w4 dup     = the same copy loop shape with #4/top-4
\   TLP-TOR     w2 >r r>   = 2-cell block moves to/from [x20+RSTK-OFF]
\   TLP-LOCAL   w2 bundle local = 3-cell frame (align16 -> 32 bytes), capture
\               pops tag-first into slots 3,2 (bundle) and 1 (scalar), the
\               reference reloads slot 1
\ ---------------------------------------------------------------------------
\ TLP-W32: read one emitted instruction word out of a compiled subject.
\ Tested boundary (TRUSTED): reinterprets an xt as the byte base for the four
\ c@ loads of one u32 — test-only code introspection, the same class as the
\ imgdump/jitdump readers; every use sits directly under the golden asserts.
TRUSTED: TLP-W32 ( n n -- n )
   + dup c@ over 1 + c@ 8 lshift or over 2 + c@ 16 lshift or swap 3 + c@ 24 lshift or ;

variable GXT
: GG ( n n -- ) {: ix:n want:n :}   \ golden: instruction ix of subject GXT
   GXT @ ix 4 * TLP-W32  want T= ;

' TLP-DUP GXT !
0 $D10043FF GG  1 $F90003FE GG                          \ prologue
2 $D2800049 GG  3 $D100426A GG                          \ movz x9,#2 ; sub x10,x19,#16
4 $F940014B GG  5 $9100214A GG  6 $F900026B GG          \ copy loop: ldr/add/str
7 $91002273 GG  8 $F1000529 GG  9 $54FFFF61 GG          \ push/subs/b.ne -5
10 $F94003FE GG  11 $910043FF GG  12 $D65F03C0 GG       \ epilogue

' TLP-SWAP GXT !
0 $D10043FF GG  1 $F90003FE GG                          \ prologue
2 $D100626A GG  3 $D100426B GG                          \ rev1: [top-24, top-16]
4 $EB0B015F GG  5 $54000102 GG  6 $F940014C GG  7 $F940016D GG
8 $F900014D GG  9 $F900016C GG  10 $9100214A GG  11 $D100216B GG  12 $17FFFFF8 GG
13 $D100226A GG  14 $D100226B GG                        \ rev2: [top-8, top-8]
15 $EB0B015F GG  16 $54000102 GG  17 $F940014C GG  18 $F940016D GG
19 $F900014D GG  20 $F900016C GG  21 $9100214A GG  22 $D100216B GG  23 $17FFFFF8 GG
24 $D100626A GG  25 $D100226B GG                        \ rev3: whole 3-cell span
26 $EB0B015F GG  27 $54000102 GG  28 $F940014C GG  29 $F940016D GG
30 $F900014D GG  31 $F900016C GG  32 $9100214A GG  33 $D100216B GG  34 $17FFFFF8 GG
35 $F94003FE GG  36 $910043FF GG  37 $D65F03C0 GG       \ epilogue

' TLP-MIX-DUP GXT !
0 $D10043FF GG  1 $F90003FE GG
2 $D2800089 GG  3 $D100826A GG                          \ movz x9,#4 ; sub x10,x19,#32
4 $F940014B GG  5 $9100214A GG  6 $F900026B GG
7 $91002273 GG  8 $F1000529 GG  9 $54FFFF61 GG
10 $F94003FE GG  11 $910043FF GG  12 $D65F03C0 GG

' TLP-TOR GXT !
0 $D10043FF GG  1 $F90003FE GG
2 $F942B68A GG  3 $8B0A0E8B GG                          \ >r: ldr rsp ; block base
4 $D100426C GG  5 $D2800049 GG                          \ src = top-16 ; movz #2
6 $F940018D GG  7 $9100218C GG  8 $F914016D GG          \ data->rstk loop
9 $9100216B GG  10 $F1000529 GG  11 $54FFFF61 GG
12 $D1004273 GG  13 $9100094A GG  14 $F902B68A GG       \ pop 2 ; rsp += 2 ; store
15 $F942B68A GG  16 $D100094A GG  17 $8B0A0E8B GG       \ r>: rsp -= 2 ; block base
18 $D2800049 GG  19 $F954016D GG  20 $9100216B GG       \ rstk->data loop
21 $F900026D GG  22 $91002273 GG  23 $F1000529 GG  24 $54FFFF61 GG
25 $F902B68A GG                                         \ commit rsp
26 $F94003FE GG  27 $910043FF GG  28 $D65F03C0 GG

' TLP-LOCAL GXT !
0 $D10043FF GG  1 $F90003FE GG
2 $D10083FF GG                                          \ sub sp,sp,#32 (3 cells + pad)
3 $D1002273 GG  4 $F9400269 GG  5 $F90007E9 GG          \ pop y -> slot 1
6 $D1002273 GG  7 $F9400269 GG  8 $F9000FE9 GG          \ pop x tag -> slot 3
9 $D1002273 GG  10 $F9400269 GG  11 $F9000BE9 GG        \ pop x slot0 -> slot 2
12 $F94007E9 GG  13 $F9000269 GG  14 $91002273 GG       \ ref y: slot 1 push
15 $910083FF GG                                         \ drop-locals: add sp,#32
16 $F94003FE GG  17 $910043FF GG  18 $D65F03C0 GG

\ ---------------------------------------------------------------------------
\ execution rows: whole-bundle transports at RUNTIME. The TRUSTED makers push
\ raw payload+tag cells under a declared layout effect (docs §25.5-style
\ seeding — the declared effect is the test's premise, the audited boundary);
\ the TRUSTED unpackers surface the cells so plain value asserts can prove
\ the bundle survived the transport. Wide locals unpack the deeper results.
\ ---------------------------------------------------------------------------
\ Tested boundary (TRUSTED): raw 2-cell seeding of tlp-res<n,n> (payload 7,
\ tag 9) — the physical layout the checker's hidden-field expansion declares.
TRUSTED: TLP-MK2 ( -- tlp-res<n,n> ) 7 9 ;
\ Tested boundary (TRUSTED): the matching 2-cell unpack (payload, tag).
TRUSTED: TLP-UN2 ( tlp-res<n,n> -- n n ) ;
\ Tested boundary (TRUSTED): raw 4-cell seeding of tlp-mix<n,n> (1 2 3, tag 4).
TRUSTED: TLP-MK4 ( -- tlp-mix<n,n> ) 1 2 3 4 ;
\ Tested boundary (TRUSTED): the matching 4-cell unpack.
TRUSTED: TLP-UN4 ( tlp-mix<n,n> -- n n n n ) ;

\ typed-local-lint: allow-bare-local
: TLPX-DUP ( -- n n n n ) TLP-MK2 dup {: a b :} a TLP-UN2 b TLP-UN2 ;
TLPX-DUP 9 T= 7 T= 9 T= 7 T=
: TLPX-DROP ( -- n ) 5 TLP-MK2 drop ;
TLPX-DROP 5 T=
\ typed-local-lint: allow-bare-local
: TLPX-SWAP ( -- n n n ) TLP-MK2 5 swap {: s:n r :} s r TLP-UN2 ;
TLPX-SWAP 9 T= 7 T= 5 T=
\ typed-local-lint: allow-bare-local
: TLPX-OVER ( -- n n n n n ) TLP-MK2 5 over {: r1 s:n r2 :} r1 TLP-UN2 s r2 TLP-UN2 ;
TLPX-OVER 9 T= 7 T= 5 T= 9 T= 7 T=
: TLPX-NIP ( -- n n ) 5 TLP-MK2 nip TLP-UN2 ;
TLPX-NIP 9 T= 7 T=
\ typed-local-lint: allow-bare-local
: TLPX-TUCK ( -- n n n n n ) 5 TLP-MK2 tuck {: r1 s:n r2 :} r1 TLP-UN2 s r2 TLP-UN2 ;
TLPX-TUCK 9 T= 7 T= 5 T= 9 T= 7 T=
\ typed-local-lint: allow-bare-local
: TLPX-ROT ( -- n n n n ) TLP-MK2 5 6 rot {: s1:n s2:n r :} s1 s2 r TLP-UN2 ;
TLPX-ROT 9 T= 7 T= 6 T= 5 T=
\ typed-local-lint: allow-bare-local
: TLPX-MROT ( -- n n n n ) 5 6 TLP-MK2 -rot {: r s1:n s2:n :} r TLP-UN2 s1 s2 ;
TLPX-MROT 6 T= 5 T= 9 T= 7 T=
\ typed-local-lint: allow-bare-local
: TLPX-2DUP ( -- n n n n n n ) TLP-MK2 5 2dup {: r1 s1:n r2 s2:n :} r1 TLP-UN2 s1 r2 TLP-UN2 s2 ;
TLPX-2DUP 5 T= 9 T= 7 T= 5 T= 9 T= 7 T=
: TLPX-2DROP ( -- n ) 6 TLP-MK2 5 2drop ;
TLPX-2DROP 6 T=
\ typed-local-lint: allow-bare-local
: TLPX-2SWAP ( -- n n n n n n n n ) TLP-MK2 5 TLP-MK4 6 2swap {: m s2:n r s1:n :} m TLP-UN4 s2 r TLP-UN2 s1 ;
TLPX-2SWAP 5 T= 9 T= 7 T= 6 T= 4 T= 3 T= 2 T= 1 T=
\ typed-local-lint: allow-bare-local
: TLPX-2OVER ( -- n n n n n n n n n n n ) TLP-MK2 5 TLP-MK4 6 2over {: r1 s1:n m1 s2:n r2 s3:n :} r1 TLP-UN2 s1 m1 TLP-UN4 s2 r2 TLP-UN2 s3 ;
TLPX-2OVER 5 T= 9 T= 7 T= 6 T= 4 T= 3 T= 2 T= 1 T= 5 T= 9 T= 7 T=
: TLPX-TOR ( -- n n n ) TLP-MK2 >r 5 r> TLP-UN2 ;
TLPX-TOR 9 T= 7 T= 5 T=
\ typed-local-lint: allow-bare-local
: TLPX-RAT ( -- n n n n ) TLP-MK2 >r r@ {: c :} r> TLP-UN2 c TLP-UN2 ;
TLPX-RAT 9 T= 7 T= 9 T= 7 T=
\ typed-local-lint: allow-bare-local
: TLPX-2TOR ( -- n n n ) TLP-MK2 5 2>r 2r> {: r s:n :} r TLP-UN2 s ;
TLPX-2TOR 5 T= 9 T= 7 T=
\ typed-local-lint: allow-bare-local
: TLPX-2RAT ( -- n n n n n n ) TLP-MK2 5 2>r 2r@ {: c cs:n :}
\ typed-local-lint: allow-bare-local
   2r> {: r s:n :} c TLP-UN2 cs r TLP-UN2 s ;
TLPX-2RAT 5 T= 9 T= 7 T= 5 T= 9 T= 7 T=
\ typed-local-lint: allow-bare-local
: TLPX-MIX-DUP ( -- n n n n n n n n ) TLP-MK4 dup {: a b :} a TLP-UN4 b TLP-UN4 ;
TLPX-MIX-DUP 4 T= 3 T= 2 T= 1 T= 4 T= 3 T= 2 T= 1 T=
\ typed-local-lint: allow-bare-local
: TLPX-MIX-SWAP ( -- n n n n n ) TLP-MK4 5 swap {: s:n m :} s m TLP-UN4 ;
TLPX-MIX-SWAP 4 T= 3 T= 2 T= 1 T= 5 T=
\ typed-local-lint: allow-bare-local
: TLPX-LOCAL ( -- n n n n n n n n n ) 5 TLP-MK4 {: y:n z :} z TLP-UN4 y z TLP-UN4 ;
TLPX-LOCAL 4 T= 3 T= 2 T= 1 T= 5 T= 4 T= 3 T= 2 T= 1 T=

\ supported pass-2 boundary: a wide local bound at TOP LEVEL and REFERENCED inside
\ both arms of a branch lowers and runs. (Binding a bundle local INSIDE branch
\ scope is rejected by the checker — E-LAYOUT-BRANCH-LOCAL, test/type-decl-suite.f
\ TD12-BRLOC-*; dot habu-tfam-12-pass-a77a24ce lifts it — but referencing a
\ top-level bundle local from a branch is fine: #LOC still covers index 0.)
\ typed-local-lint: allow-bare-local
: TLPX-REF-BRANCH ( n -- n n ) TLP-MK2 {: a :} 0 > if a TLP-UN2 else a TLP-UN2 then ;
5 TLPX-REF-BRANCH 9 T= 7 T=
-3 TLPX-REF-BRANCH 9 T= 7 T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-layout-lower-pending: failures" 1 die ;
REPORT
