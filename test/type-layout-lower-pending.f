\ type-layout-lower-pending.f — TFAM 12 width-aware lowering suite
\ (habu-tfam-12-layout, docs/type-families.md §17-18). Run BY THE ENGINE over
\ stdin, standalone or through the native registry:
\     bin/hb < test/type-layout-lower-pending.f
\ Three sections:
\   1. width-fact contracts — every TLP-* subject is a REAL checked definition;
\      the asserts after each pin the per-op fact the emitter consumes
\      (operand position 0=top, family-id, registry logical width; absent row
\      = one-cell operand).
\   2. emitted-lowering goldens — exact u32 instruction sequences of six
\      representative subjects, captured FROM the implemented pass-2 emitter
\      (EM-COMPILE-P2WIDE / LP2COPY / LP2ROT / LP2RS / EM-P2-CARVE, habu2.f).
\      The fetch validator's ASLR-dependent absolute call target is opcode-mask
\      checked; every other word is exact and moves only when lowering does.
\   3. execution rows — generated constructors (TLP--RES:ERR / TLP--MIX:BIG)
\      seed the bundles in checked code, whole-bundle transports run at
\      RUNTIME, and TRUSTED unpackers surface the cells for value asserts:
\      dup/swap/over/nip/tuck/rot/-rot/2dup/2drop/2swap/2over, the
\      return-stack transfers, and wide locals.
\
\ Constant contract (TFAM 12 verdict 2026-07-09): `constant` keeps the one-cell
\ `-- a` model permanently — checked-body layout pops reject (TD12-CONST), the
\ top-level pop never sees a wide value (DNAME-WIDE dispatch gate), and the
\ staged shape-carry fixtures were deleted as unsound (a one-cell bake cannot
\ carry a multi-cell shape). Parity: check-all-errors-test const-layout-narrow.

require src/compiler/native/codewalk.f

using TFAM

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
\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): the internal registry
\ probe goes through a named trusted shim.
TRUSTED: TWX-TFAM-FIND-IN ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN ;
s" " s" tlp-res" TWX-TFAM-FIND-IN TLOK ! TLF !
TLOK @ -1 T=
TLF @ TFAM-WIDTH@ 2 T=
s" " s" tlp-mix" TWX-TFAM-FIND-IN TLOK ! TLX !
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
WF-N@ 2 T=  0 WF-OFF@ 41 T=  1 WF-OFF@ 44 T=  1 WF-POS@ 0 T=
: TLP-RAT ( tlp-res<n,n> -- tlp-res<n,n> tlp-res<n,n> ) >r r@ r> ;
WF-N@ 3 T=  1 WF-OFF@ 57 T=  2 WF-OFF@ 60 T=
: TLP-2TOR ( tlp-res<n,n> n -- tlp-res<n,n> n ) 2>r 2r> ;
WF-N@ 2 T=  0 WF-POS@ 1 T=  1 WF-POS@ 1 T=
: TLP-2RAT ( tlp-res<n,n> n -- tlp-res<n,n> n tlp-res<n,n> n ) 2>r 2r@ 2r> ;
WF-N@ 3 T=  0 WF-POS@ 1 T=  1 WF-POS@ 1 T=  2 WF-POS@ 1 T=

\ locals capture: the whole group records at the :} token. `x` binds the layout
\ value; locals annotations cannot express family types yet (capability dotted:
\ habu-typed-locals-for-b06b6707), so the entry effect carries the detailed type.
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

\ Layout memory operations record one operation-width fact at position 0.
\ W=2 therefore triggers pass 2; the scalar address itself remains one cell.
1 LAYOUT-BUFFER TLP-MEM2-BUF tlp-res<n,n>
: TLP-MEM2-P ( -- ptr tlp-res<n,n> ) 0 TLP-MEM2-BUF ;
: TLP-STORE2 ( tlp-res<n,n> -- ) TLP-MEM2-P ! ;
WF-N@ 1 T=  0 WF-OFF@ 42 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 2 T=
: TLP-FETCH2 ( -- tlp-res<n,n> ) TLP-MEM2-P @ ;
WF-N@ 1 T=  0 WF-OFF@ 42 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 2 T=
1 LAYOUT-BUFFER TLP-MEM4-BUF tlp-mix<n,n>
: TLP-MEM4-P ( -- ptr tlp-mix<n,n> ) 0 TLP-MEM4-BUF ;
: TLP-STORE4 ( tlp-mix<n,n> -- ) TLP-MEM4-P ! ;
WF-N@ 1 T=  0 WF-OFF@ 42 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 4 T=
: TLP-FETCH4 ( -- tlp-mix<n,n> ) TLP-MEM4-P @ ;
WF-N@ 1 T=  0 WF-OFF@ 42 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 4 T=

\ Address-input subjects keep the memory lowering golden free of embedded
\ CREATE addresses. Their only body token is the wide memory operation.
: TLP-STORE2-G ( tlp-res<n,n> ptr tlp-res<n,n> -- ) ! ;
WF-N@ 1 T=  0 WF-OFF@ 50 T=  0 WF-WIDTH@ 2 T=
: TLP-FETCH2-G ( ptr tlp-res<n,n> -- tlp-res<n,n> ) @ ;
WF-N@ 1 T=  0 WF-OFF@ 50 T=  0 WF-WIDTH@ 2 T=

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
\ Retirement owner: habu-interpret-wide-gate-1d70acf7.
TRUSTED: TLP-W32 ( n n -- n )
   + dup c@ over 1 + c@ 8 lshift or over 2 + c@ 16 lshift or swap 3 + c@ 24 lshift or ;

\ Tested boundary (TRUSTED): the golden subjects carry wide effects, so their
\ dict records are DNAME-WIDE (habu-tfam-12-interpret) and interpret `'`
\ correctly fails closed on them. The goldens only READ code bytes, so the xt
\ comes from the raw-xt introspection boundary (search-wl, wordlist 0) — the
\ same test-only class as TLP-W32 and the documented unchecked residual.
\ Retirement owner: habu-interpret-wide-gate-1d70acf7.
TRUSTED: TLP-XT ( ptr u8 n -- n ) 0 search-wl ;

variable GXT

\ The goldens index the subject's OWN instructions. The engine's stack guard
\ (src/compiler/native/codewalk.f, eleven instructions around one BL) precedes
\ every complete transfer and follows every returning call; the raw index of
\ own instruction k skips whole guards on the way there.
512 constant GBOUND                  \ raw instructions a subject is read within
: GWORD ( n -- n ) GXT @ swap 4 * TLP-W32 ;
: GRAW ( n -- n ) {: own:n :}
   0 0 begin over own < while                \ ( seen raw )
      GBOUND over [: GWORD ;] NWALK:GUARD-AT? if
         NWALK:GUARD-INSNS +
      else
         swap 1 + swap 1 +
      then
   repeat
   begin GBOUND over [: GWORD ;] NWALK:GUARD-AT? while NWALK:GUARD-INSNS + repeat
   nip ;

: GG ( n n -- ) {: ix:n want:n :}   \ golden: own instruction ix of subject GXT
   GXT @ ix GRAW 4 * TLP-W32  want T= ;

: GM ( n n n -- ) {: ix:n mask:n want:n :}   \ masked golden instruction
   GXT @ ix GRAW 4 * TLP-W32 mask and  want T= ;

\ The engine helper call is one direct `BL imm26` when the JIT region maps within BL
\ range of __text (native bin/hb, dot habu-aot-repl-bl), and the absolute movz/movk/movk
\ x16 + blr x16 chain when the region is far (the Gforth stage0 seed). Verify whichever
\ form is present and leave GN = the instruction index right after the call.
variable GN
: GCALL ( n -- ) {: ix:n :}
   GXT @ ix 4 * TLP-W32 $FC000000 and $94000000 = if
      ix $FC000000 $94000000 GM  ix 1 + GN !  exit
   then
   ix     $FFE0001F $D2800010 GM
   ix 1 + $FFE0001F $F2A00010 GM
   ix 2 + $FFE0001F $F2C00010 GM
   ix 3 + $FFFFFFFF $D63F0200 GM
   ix 4 + GN ! ;

: TLP-EXIT-W32 ( -- n )
   HB-TARGET-LINUX? if $D2800BC8 exit then
   HB-TARGET-MACOS? if $D2800030 exit then
   s" type-layout-lower: unknown target" 76 die ;
: TLP-SVC-W32 ( -- n )
   HB-TARGET-LINUX? if $D4000001 exit then
   HB-TARGET-MACOS? if $D4001001 exit then
   s" type-layout-lower: unknown target" 76 die ;

\ Every subject's own instructions, guards read past (GRAW). Whole-group
\ transports now synthesize their byte counts into a register ahead of the
\ register-form move (habu2.f LP2COPY / LP2DROPN / LP2REV / LP2RS, x16 for the
\ locals frame), which is what moved these words from their earlier immediate
\ forms; the loop shapes are unchanged.
s" TLP-DUP" TLP-XT GXT !
0 $D10043FF GG                          \ sub sp,sp,#16
1 $F90003FE GG                          \ str x30,[sp,#0]
2 $D2800049 GG                          \ movz x9,#2
3 $D280020A GG                          \ movz x10,#16
4 $CB0A026A GG                          \ sub x10,x19,x10
5 $F940014B GG                          \ ldr x11,[x10,#0]
6 $9100214A GG                          \ add x10,x10,#8
7 $F900026B GG                          \ str x11,[x19,#0]
8 $91002273 GG                          \ add x19,x19,#8
9 $F1000529 GG                          \ subs x9,x9,#1
10 $54FFFF61 GG                         \ b.ne -5
11 $F94003FE GG                         \ ldr x30,[sp,#0]
12 $910043FF GG                         \ add sp,sp,#16
13 $D65F03C0 GG                         \ ret

s" TLP-SWAP" TLP-XT GXT !
0 $D10043FF GG                          \ sub sp,sp,#16
1 $F90003FE GG                          \ str x30,[sp,#0]
2 $D280030A GG                          \ movz x10,#24
3 $D280020B GG                          \ movz x11,#16
4 $CB0A026A GG                          \ sub x10,x19,x10
5 $CB0B026B GG                          \ sub x11,x19,x11
6 $EB0B015F GG                          \ cmp x10,x11
7 $54000102 GG                          \ b.cs +8
8 $F940014C GG                          \ ldr x12,[x10,#0]
9 $F940016D GG                          \ ldr x13,[x11,#0]
10 $F900014D GG                         \ str x13,[x10,#0]
11 $F900016C GG                         \ str x12,[x11,#0]
12 $9100214A GG                         \ add x10,x10,#8
13 $D100216B GG                         \ sub x11,x11,#8
14 $17FFFFF8 GG                         \ b -8
15 $D280030A GG                         \ movz x10,#24
16 $D280010B GG                         \ movz x11,#8
17 $CB0A026A GG                         \ sub x10,x19,x10
18 $CB0B026B GG                         \ sub x11,x19,x11
19 $EB0B015F GG                         \ cmp x10,x11
20 $54000102 GG                         \ b.cs +8
21 $F940014C GG                         \ ldr x12,[x10,#0]
22 $F940016D GG                         \ ldr x13,[x11,#0]
23 $F900014D GG                         \ str x13,[x10,#0]
24 $F900016C GG                         \ str x12,[x11,#0]
25 $9100214A GG                         \ add x10,x10,#8
26 $D100216B GG                         \ sub x11,x11,#8
27 $17FFFFF8 GG                         \ b -8
28 $F94003FE GG                         \ ldr x30,[sp,#0]
29 $910043FF GG                         \ add sp,sp,#16
30 $D65F03C0 GG                         \ ret

s" TLP-MIX-DUP" TLP-XT GXT !
0 $D10043FF GG                          \ sub sp,sp,#16
1 $F90003FE GG                          \ str x30,[sp,#0]
2 $D2800089 GG                          \ movz x9,#4
3 $D280040A GG                          \ movz x10,#32
4 $CB0A026A GG                          \ sub x10,x19,x10
5 $F940014B GG                          \ ldr x11,[x10,#0]
6 $9100214A GG                          \ add x10,x10,#8
7 $F900026B GG                          \ str x11,[x19,#0]
8 $91002273 GG                          \ add x19,x19,#8
9 $F1000529 GG                          \ subs x9,x9,#1
10 $54FFFF61 GG                         \ b.ne -5
11 $F94003FE GG                         \ ldr x30,[sp,#0]
12 $910043FF GG                         \ add sp,sp,#16
13 $D65F03C0 GG                         \ ret

s" TLP-TOR" TLP-XT GXT !
0 $D10043FF GG                          \ sub sp,sp,#16
1 $F90003FE GG                          \ str x30,[sp,#0]
2 $F942B68A GG                          \ ldr x10,[x20,#1384]
3 $8B0A0E8B GG                          \ add x11,x20,x10,lsl #3
4 $D280020C GG                          \ movz x12,#16
5 $CB0C026C GG                          \ sub x12,x19,x12
6 $AA0C03EE GG                          \ mov x14,x12
7 $D2800049 GG                          \ movz x9,#2
8 $AA0903EF GG                          \ mov x15,x9
9 $F940018D GG                          \ ldr x13,[x12,#0]
10 $9100218C GG                         \ add x12,x12,#8
11 $F914016D GG                         \ str x13,[x11,#10240]
12 $9100216B GG                         \ add x11,x11,#8
13 $F1000529 GG                         \ subs x9,x9,#1
14 $54FFFF61 GG                         \ b.ne -5
15 $AA0E03F3 GG                         \ mov x19,x14
16 $8B0F014A GG                         \ add x10,x10,x15
17 $F902B68A GG                         \ str x10,[x20,#1384]
18 $F942B68A GG                         \ ldr x10,[x20,#1384]
19 $D2800049 GG                         \ movz x9,#2
20 $CB09014A GG                         \ sub x10,x10,x9
21 $8B0A0E8B GG                         \ add x11,x20,x10,lsl #3
22 $F954016D GG                         \ ldr x13,[x11,#10240]
23 $9100216B GG                         \ add x11,x11,#8
24 $F900026D GG                         \ str x13,[x19,#0]
25 $91002273 GG                         \ add x19,x19,#8
26 $F1000529 GG                         \ subs x9,x9,#1
27 $54FFFF61 GG                         \ b.ne -5
28 $F902B68A GG                         \ str x10,[x20,#1384]
29 $F94003FE GG                         \ ldr x30,[sp,#0]
30 $910043FF GG                         \ add sp,sp,#16
31 $D65F03C0 GG                         \ ret

\ The locals frame is reserved and released through x16, which the value
\ allocator never pools, so the frame size travels as one movz ahead of the
\ extended-register move (habu2.f EM-P2-CARVE and C-EMIT-DROP-X12).
s" TLP-LOCAL" TLP-XT GXT !
0 $D10043FF GG                          \ sub sp,sp,#16
1 $F90003FE GG                          \ str x30,[sp,#0]
2 $D2800410 GG                          \ movz x16,#32
3 $CB3063FF GG                          \ sub sp,sp,x16
4 $D1002273 GG                          \ sub x19,x19,#8
5 $F9400269 GG                          \ ldr x9,[x19,#0]
6 $F90007E9 GG                          \ str x9,[sp,#8]
7 $D1002273 GG                          \ sub x19,x19,#8
8 $F9400269 GG                          \ ldr x9,[x19,#0]
9 $F9000FE9 GG                          \ str x9,[sp,#24]
10 $D1002273 GG                         \ sub x19,x19,#8
11 $F9400269 GG                         \ ldr x9,[x19,#0]
12 $F9000BE9 GG                         \ str x9,[sp,#16]
13 $F94007E9 GG                         \ ldr x9,[sp,#8]
14 $F9000269 GG                         \ str x9,[x19,#0]
15 $91002273 GG                         \ add x19,x19,#8
16 $D2800410 GG                         \ movz x16,#32
17 $8B3063FF GG                         \ add sp,sp,x16
18 $F94003FE GG                         \ ldr x30,[sp,#0]
19 $910043FF GG                         \ add sp,sp,#16
20 $D65F03C0 GG                         \ ret

\ TLP-STORE2-G: pop the typed address, call the whole-span LPROTSPAN ABI before
\ mutation, copy slot0 then tag, and pop both source cells.
s" TLP-STORE2-G" TLP-XT GXT !
0 $D10043FF GG                          \ sub sp,sp,#16
1 $F90003FE GG                          \ str x30,[sp,#0]
2 $D1002273 GG                          \ sub x19,x19,#8
3 $F940026A GG                          \ ldr x10,[x19,#0]
4 $D280020E GG                          \ movz x14,#16
5 $CB0E026E GG                          \ sub x14,x19,x14
6 $D2800049 GG                          \ movz x9,#2
7 $D280020B GG                          \ movz x11,#16
8 GCALL                                                  \ engine helper call (BL or chain)
GN @ $F94001CF GG                       \ ldr x15,[x14,#0]
GN @ 1 + $F900014F GG                   \ str x15,[x10,#0]
GN @ 2 + $910021CE GG                   \ add x14,x14,#8
GN @ 3 + $9100214A GG                   \ add x10,x10,#8
GN @ 4 + $F1000529 GG                   \ subs x9,x9,#1
GN @ 5 + $54FFFF61 GG                   \ b.ne -5
GN @ 6 + $D2800209 GG                   \ movz x9,#16
GN @ 7 + $CB090273 GG                   \ sub x19,x19,x9
GN @ 8 + $F94003FE GG                   \ ldr x30,[sp,#0]
GN @ 9 + $910043FF GG                   \ add sp,sp,#16
GN @ 10 + $D65F03C0 GG                  \ ret

\ TLP-FETCH2-G: validate the inline descriptor before the typed address is
\ popped, then read slot0 and tag in canonical bundle order. The absolute call
\ target changes under ASLR, so its four-instruction opcode shape is masked.
s" TLP-FETCH2-G" TLP-XT GXT !
0 $D10043FF GG                          \ sub sp,sp,#16
1 $F90003FE GG                          \ str x30,[sp,#0]
2 GCALL                                                  \ engine helper call (BL or chain)
GN @ $14000009 GG                       \ b +9
GN @ 1 + $00000001 GG                   \ descriptor u32 1
GN @ 2 + $00000000 GG                   \ descriptor u32 0
GN @ 3 + $00000001 GG                   \ descriptor u32 1
GN @ 4 + $00000000 GG                   \ descriptor u32 0
GN @ 5 + $00000002 GG                   \ descriptor u32 2
GN @ 6 + $00000000 GG                   \ descriptor u32 0
GN @ 7 + $00000000 GG                   \ descriptor u32 0
GN @ 8 + $00000000 GG                   \ descriptor u32 0
GN @ 9 + $D1002273 GG                   \ sub x19,x19,#8
GN @ 10 + $F940026A GG                  \ ldr x10,[x19,#0]
GN @ 11 + $D2800049 GG                  \ movz x9,#2
GN @ 12 + $F940014B GG                  \ ldr x11,[x10,#0]
GN @ 13 + $9100214A GG                  \ add x10,x10,#8
GN @ 14 + $F900026B GG                  \ str x11,[x19,#0]
GN @ 15 + $91002273 GG                  \ add x19,x19,#8
GN @ 16 + $F1000529 GG                  \ subs x9,x9,#1
GN @ 17 + $54FFFF61 GG                  \ b.ne -5
GN @ 18 + $F94003FE GG                  \ ldr x30,[sp,#0]
GN @ 19 + $910043FF GG                  \ add sp,sp,#16
GN @ 20 + $D65F03C0 GG                  \ ret

\ ---------------------------------------------------------------------------
\ execution rows: whole-bundle transports at RUNTIME. The seeds are the REAL
\ generated constructors (item 8/11: `tlp-res` derives package TLP--RES,
\ `tlp-mix` derives TLP--MIX — tail hyphens escape as `--`), so the physical
\ cells (payload, zero pads, tag) come from checked constructor bodies, not
\ trusted raw pushes. TLP-MK2 = 7 TLP--RES:ERR -> (7, tag 1); TLP-MK4 =
\ 91 92 93 TLP--MIX:BIG -> (91, 92, 93, tag 1). Only the UNPACKERS remain a
\ tested TRUSTED boundary: surfacing bundle cells for value asserts needs a
\ destructor, which is item 9's MATCH (dot habu-retire-tlp-mk2-ac7760d2).
\ Both raw unpackers retire with habu-retire-tlp-mk2-ac7760d2 when checked
\ MATCH/destructuring can expose their payload cells.
\ ---------------------------------------------------------------------------
: TLP-MK2 ( -- tlp-res<n,n> ) 7 TLP--RES:ERR ;
: TLP-MK2B ( -- tlp-res<n,n> ) 8 TLP--RES:OK ;
\ Tested boundary (TRUSTED): the matching 2-cell unpack (payload, tag).
TRUSTED: TLP-UN2 ( tlp-res<n,n> -- n n ) ;
: TLP-MK4 ( -- tlp-mix<n,n> ) 91 92 93 TLP--MIX:BIG ;
\ Tested boundary (TRUSTED): the matching 4-cell unpack.
TRUSTED: TLP-UN4 ( tlp-mix<n,n> -- n n n n ) ;

\ Executed memory lowering: constructor-produced bundles cross typed addresses
\ and return with payload, padding, and tag order intact.
: TLPX-STORE2 ( -- ) TLP-MK2 TLP-STORE2 ;
: TLPX-FETCH2 ( -- n n ) TLP-FETCH2 TLP-UN2 ;
TLPX-STORE2
TLPX-FETCH2 1 T= 7 T=
: TLPX-STORE4 ( -- ) TLP-MK4 TLP-STORE4 ;
: TLPX-FETCH4 ( -- n n n n ) TLP-FETCH4 TLP-UN4 ;
TLPX-STORE4
TLPX-FETCH4 1 T= 93 T= 92 T= 91 T=

: TLPX-DUP ( -- n n n n ) TLP-MK2 dup {: a b :} a TLP-UN2 b TLP-UN2 ;
TLPX-DUP 1 T= 7 T= 1 T= 7 T=
: TLPX-DROP ( -- n ) 5 TLP-MK2 drop ;
TLPX-DROP 5 T=
: TLPX-SWAP ( -- n n n ) TLP-MK2 5 swap {: s:n r :} s r TLP-UN2 ;
TLPX-SWAP 1 T= 7 T= 5 T=
: TLPX-OVER ( -- n n n n n ) TLP-MK2 5 over {: r1 s:n r2 :} r1 TLP-UN2 s r2 TLP-UN2 ;
TLPX-OVER 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-NIP ( -- n n ) 5 TLP-MK2 nip TLP-UN2 ;
TLPX-NIP 1 T= 7 T=
: TLPX-TUCK ( -- n n n n n ) 5 TLP-MK2 tuck {: r1 s:n r2 :} r1 TLP-UN2 s r2 TLP-UN2 ;
TLPX-TUCK 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-ROT ( -- n n n n ) TLP-MK2 5 6 rot {: s1:n s2:n r :} s1 s2 r TLP-UN2 ;
TLPX-ROT 1 T= 7 T= 6 T= 5 T=
: TLPX-MROT ( -- n n n n ) 5 6 TLP-MK2 -rot {: r s1:n s2:n :} r TLP-UN2 s1 s2 ;
TLPX-MROT 6 T= 5 T= 1 T= 7 T=
: TLPX-2DUP ( -- n n n n n n ) TLP-MK2 5 2dup {: r1 s1:n r2 s2:n :} r1 TLP-UN2 s1 r2 TLP-UN2 s2 ;
TLPX-2DUP 5 T= 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-2DROP ( -- n ) 6 TLP-MK2 5 2drop ;
TLPX-2DROP 6 T=
: TLPX-2SWAP ( -- n n n n n n n n ) TLP-MK2 5 TLP-MK4 6 2swap {: m s2:n r s1:n :} m TLP-UN4 s2 r TLP-UN2 s1 ;
TLPX-2SWAP 5 T= 1 T= 7 T= 6 T= 1 T= 93 T= 92 T= 91 T=
: TLPX-2OVER ( -- n n n n n n n n n n n ) TLP-MK2 5 TLP-MK4 6 2over {: r1 s1:n m1 s2:n r2 s3:n :} r1 TLP-UN2 s1 m1 TLP-UN4 s2 r2 TLP-UN2 s3 ;
TLPX-2OVER 5 T= 1 T= 7 T= 6 T= 1 T= 93 T= 92 T= 91 T= 5 T= 1 T= 7 T=
: TLPX-TOR ( -- n n n ) TLP-MK2 >r 5 r> TLP-UN2 ;
TLPX-TOR 1 T= 7 T= 5 T=
: TLPX-RAT ( -- n n n n ) TLP-MK2 >r r@ {: c :} r> TLP-UN2 c TLP-UN2 ;
TLPX-RAT 1 T= 7 T= 1 T= 7 T=
: TLPX-2TOR ( -- n n n ) TLP-MK2 5 2>r 2r> {: r s:n :} r TLP-UN2 s ;
TLPX-2TOR 5 T= 1 T= 7 T=
: TLPX-2RAT ( -- n n n n n n ) TLP-MK2 5 2>r 2r@ {: c cs:n :}
   2r> {: r s:n :} c TLP-UN2 cs r TLP-UN2 s ;
TLPX-2RAT 5 T= 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-MIX-DUP ( -- n n n n n n n n ) TLP-MK4 dup {: a b :} a TLP-UN4 b TLP-UN4 ;
TLPX-MIX-DUP 1 T= 93 T= 92 T= 91 T= 1 T= 93 T= 92 T= 91 T=
: TLPX-MIX-SWAP ( -- n n n n n ) TLP-MK4 5 swap {: s:n m :} s m TLP-UN4 ;
TLPX-MIX-SWAP 1 T= 93 T= 92 T= 91 T= 5 T=
: TLPX-LOCAL ( -- n n n n n n n n n ) 5 TLP-MK4 {: y:n z :} z TLP-UN4 y z TLP-UN4 ;
TLPX-LOCAL 1 T= 93 T= 92 T= 91 T= 5 T= 1 T= 93 T= 92 T= 91 T=
\ two distinct wide locals in one carve group pin declaration-order bind replay
\ against operand-position-sorted width evidence.
package TLP-LOCAL-TEST
public
: DUAL ( -- n n n n n n )
   TLP-MK2 TLP-MK4 {: r m :} r TLP-UN2 m TLP-UN4 ;
: DEEP ( -- n n n n n n )
   TLP-MK2 1 2 3 4 {: r a:n b:n c:n d:n :}
   a b c d r TLP-UN2 ;
;package
TLP-LOCAL-TEST:DUAL 1 T= 93 T= 92 T= 91 T= 1 T= 7 T=
TLP-LOCAL-TEST:DEEP 1 T= 7 T= 4 T= 3 T= 2 T= 1 T=

\ a wide local bound at TOP LEVEL and REFERENCED inside both arms of a branch.
: TLPX-REF-BRANCH ( n -- n n ) TLP-MK2 {: a :} 0 > if a TLP-UN2 else a TLP-UN2 then ;
5 TLPX-REF-BRANCH 1 T= 7 T=
-3 TLPX-REF-BRANCH 1 T= 7 T=

\ ---------------------------------------------------------------------------
\ branch-scoped bundle locals (habu-tfam-12-pass): a bundle local BOUND inside
\ a branch arm lowers by bind sequence (checker LOCW-HW + the P2-CARVE-W live
\ replay), so sibling arms may reuse the same frame slot — at different widths.
\ Each subject runs BOTH arms; values prove whole-bundle capture + reference.
\ ---------------------------------------------------------------------------
: TLPX-BRIF ( n -- n n ) 0 > if TLP-MK2 {: r :} r TLP-UN2
   else TLP-MK2B {: r :} r TLP-UN2 then ;
5 TLPX-BRIF 1 T= 7 T=
-3 TLPX-BRIF 0 T= 8 T=
: TLPX-BRCASE ( n -- n n ) case
     1 of TLP-MK2 {: r :} r TLP-UN2 endof
     2 of TLP-MK2B {: r :} r TLP-UN2 endof
     0 0 rot
   endcase ;
1 TLPX-BRCASE 1 T= 7 T=
2 TLPX-BRCASE 0 T= 8 T=
9 TLPX-BRCASE 0 T= 0 T=
\ sibling arms reuse frame slot 0 at width 2 vs width 4.
: TLPX-BRW ( n -- n n n n ) 0 > if TLP-MK2 {: r :} r TLP-UN2 0 0
   else TLP-MK4 {: m :} m TLP-UN4 then ;
5 TLPX-BRW 0 T= 0 T= 1 T= 7 T=
-3 TLPX-BRW 1 T= 93 T= 92 T= 91 T=
\ a mixed scalar+wide group inside a branch arm.
: TLPX-BRMIX ( n -- n n n ) 0 > if TLP-MK2 5 {: r s:n :} s r TLP-UN2 else 6 7 8 then ;
1 TLPX-BRMIX 1 T= 7 T= 5 T=
0 TLPX-BRMIX 8 T= 7 T= 6 T=
\ an OUTER wide local below a branch-scoped wide local: the branch carve's
\ cumulative spans the live width-4 entry, and the outer local survives the join.
: TLPX-BROUTER ( n -- n n n n ) TLP-MK4 {: m :}
   0 > if TLP-MK2 {: r :} r TLP-UN2 drop drop then m TLP-UN4 ;
1 TLPX-BROUTER 1 T= 93 T= 92 T= 91 T=
0 TLPX-BROUTER 1 T= 93 T= 92 T= 91 T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-layout-lower-pending: failures" 1 die ;
REPORT

;using
